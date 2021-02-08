use core::panic;
use std::{collections::HashMap, mem::swap, todo, usize, vec};

use crate::{
    chunks::Chunk,
    db::CodegenDatabase,
    ir::{self},
    object::{RawObject, StringObject},
    value::Value,
    vm::VM,
};
use errors::{FileId, WithError};

use semant::{
    hir::{
        BinOp, Class, Expr, ExprId, Function, FunctionAstMap, Literal, Name, NameId, PatId,
        Pattern, Stmt, StmtId, UnaryOp,
    },
    IndexMap, InferDataMap, Span, StackedMap, Type, TypeCon,
};

#[derive(Debug, Clone, Copy)]
struct LoopDescription {
    /// The index of the start label
    start: usize,
    /// The index of the end label
    end: usize,
}

#[derive(Debug)]
pub(crate) struct CodegenBuilder<'map, DB> {
    db: DB,
    type_map: &'map InferDataMap,
    chunk: Chunk,
    current_loop: Option<LoopDescription>,
    current_call: Option<NameId>,
    ///  A linked list of all the objects allocated. This
    /// is passed to the vm so runtime collection can be done
    pub objects: RawObject,
    /// The stack slot of the variable
    slots: u32,
    line: u32,
    locals: StackedMap<NameId, usize>,
    params: HashMap<NameId, usize>,
}

impl<'a, 'map, DB> CodegenBuilder<'map, &'a DB>
where
    DB: CodegenDatabase,
{
    pub(crate) fn build_function(&mut self, function: &Function) -> ir::Function {
        for param in &function.params {
            let param = function.ast_map.param(&param.item);

            let pat = param.pat.item;

            let _ = self.get_slot_for_pat(pat, &function.ast_map, None, true);
        }

        if let Some(body) = &function.body {
            body.iter().for_each(|stmt| {
                self.codegen_statement(stmt, &function.ast_map);
            })
        }

        let mut chunk = Chunk::new();
        let mut params = HashMap::new();

        std::mem::swap(&mut chunk, &mut self.chunk);
        std::mem::swap(&mut params, &mut self.params);

        self.slots = 0;
        self.current_loop = None;

        ir::Function {
            name: function.name.item,
            params: params,
            body: chunk,
        }
    }

    pub(crate) fn build_class(&mut self, class: &Class) -> ir::Class {
        let mut methods = HashMap::default();

        for method in class.methods.iter() {
            methods.insert(method.name.item, self.build_function(method));
        }

        ir::Class {
            methods,
            name: class.name.item,
        }
    }

    pub(crate) fn emit_byte(&mut self, byte: u8) {
        self.chunk.write(byte, self.line)
    }

    pub fn new_slot(&mut self) -> u32 {
        let slot = self.slots;
        self.slots += 1;
        slot
    }

    pub fn begin_scope(&mut self) {
        self.locals.begin_scope();
    }

    pub fn end_scope(&mut self) {
        let mut dummy = StackedMap::new();

        std::mem::swap(&mut self.locals, &mut dummy);

        dummy
            .end_scope_iter()
            .for_each(|_| self.emit_byte(opcode::POP));

        std::mem::swap(&mut self.locals, &mut dummy);
    }

    pub fn get_slot_for_pat(
        &mut self,
        id: PatId,
        map: &FunctionAstMap,
        slot: Option<u32>,
        param: bool,
    ) -> u32 {
        let pat = map.pat(&id);

        let slot = slot.unwrap_or(self.new_slot());

        match pat {
            Pattern::Bind { name } => {
                if param {
                    self.params.insert(name.item, slot as usize);
                } else {
                    self.locals.insert(name.item, slot as usize);
                }
            }
            Pattern::Placeholder => {
                // Todo check if this is the correct behavior
            }
            Pattern::Tuple(pats) => {
                for pat in pats {
                    let _ = self.get_slot_for_pat(pat.item, map, Some(slot), param);
                }
            }
            Pattern::Literal(_) => {}
        }

        slot
    }

    pub fn get_ident(&mut self, id: ExprId, map: &FunctionAstMap) -> usize {
        let expr = map.expr(&id);

        match expr {
            Expr::Field(_) => {
                todo!()
            }
            Expr::Ident(ident) => *self.locals.get(&ident.item).expect("Undefined var"),
            _ => todo!(),
        }
    }

    pub fn patch_jump(&mut self, offset: usize) {
        // -2 to adjust for the bytecode for the jump offset itself.
        let jump = self.chunk.code.len() - offset - 2;

        self.chunk.code[offset] = ((jump >> 8) & 0xff) as u8;
        self.chunk.code[offset + 1] = (jump & 0xff) as u8;
    }

    pub fn emit_jump(&mut self, byte: u8) -> usize {
        self.emit_byte(byte);
        self.emit_bytes(0xff, 0xff);
        self.chunk.code.len() - 2
    }

    pub fn emit_loop(&mut self, loop_start: usize) {
        self.emit_byte(opcode::LOOP);

        let offset = self.chunk.code.len() - loop_start + 2;

        self.emit_bytes(((offset >> 8) & 0xff) as u8, (offset & 0xff) as u8)
    }

    pub fn emit_bytes(&mut self, byte1: u8, byte2: u8) {
        self.emit_byte(byte1);
        self.emit_byte(byte2);
    }

    pub fn emit_constant(&mut self, constant: Value) {
        let value = self.make_constant(constant);
        self.emit_bytes(opcode::CONSTANT, value);
    }

    pub fn emit_object(&mut self, object: RawObject) {
        self.objects = object;
        self.emit_constant(Value::object(object))
    }

    pub fn make_constant(&mut self, value: Value) -> u8 {
        let index = self.chunk.add_constant(value);

        if index > 256 {
            todo!("Support more than 256 constants in a chunk")
        }
        index as u8
    }

    pub fn codegen_statement(&mut self, id: &Span<StmtId>, map: &FunctionAstMap) {
        let stmt = map.stmt(&id.item);

        match stmt {
            Stmt::Let {
                pat, initializer, ..
            } => {
                if let Some(expr) = initializer {
                    self.codegen_expr(expr, map);
                } else {
                    self.emit_constant(Value::nil());
                }

                let slot = self.get_slot_for_pat(pat.item, map, None, false);

                self.emit_bytes(opcode::SETLOCAL, slot as u8);
            }
            Stmt::Expr(expr) => self.codegen_expr(expr, map),
            Stmt::Error => {
                self.emit_byte(opcode::IGL);
            }
        }
    }

    pub fn codegen_expr(&mut self, id: &Span<ExprId>, map: &FunctionAstMap) {
        let expr = map.expr(&id.item);

        let ty = self.type_map.expr_to_type.get(&id.item).unwrap();

        match expr {
            Expr::Array(exprs) => {
                for expr in exprs {
                    self.codegen_expr(expr, map)
                }

                self.emit_byte(opcode::ARRAY);
            }
            Expr::Binary { lhs, op, rhs } => match op {
                BinOp::And => self.codegen_and(lhs, rhs, map),
                BinOp::Or => self.codegen_or(lhs, rhs, map),
                BinOp::Equal => {
                    let ident = self.get_ident(lhs.item, map);

                    self.codegen_expr(rhs, map);

                    self.emit_bytes(opcode::SETLOCAL, ident as u8);
                }
                BinOp::PlusEqual => {
                    let ident = self.get_ident(lhs.item, map);

                    self.emit_bytes(opcode::GETLOCAL, ident as u8);

                    self.codegen_expr(rhs, map);

                    match ty {
                        Type::Con(TypeCon::Int) => self.emit_byte(opcode::ADD),
                        Type::Con(TypeCon::Float) => self.emit_byte(opcode::ADDF),
                        _ => panic!("Unsupported assignment types"),
                    }

                    self.emit_bytes(opcode::SETLOCAL, ident as u8);
                }
                BinOp::MinusEqual => {
                    let ident = self.get_ident(lhs.item, map);

                    self.emit_bytes(opcode::GETLOCAL, ident as u8);

                    match ty {
                        Type::Con(TypeCon::Int) => self.emit_byte(opcode::SUB),
                        Type::Con(TypeCon::Float) => self.emit_byte(opcode::SUBF),
                        _ => panic!("Unsupported assignment types"),
                    }

                    self.codegen_expr(rhs, map);

                    self.emit_bytes(opcode::SETLOCAL, ident as u8);
                }
                BinOp::MultEqual => {
                    let ident = self.get_ident(lhs.item, map);

                    self.emit_bytes(opcode::GETLOCAL, ident as u8);

                    match ty {
                        Type::Con(TypeCon::Int) => self.emit_byte(opcode::MUL),
                        Type::Con(TypeCon::Float) => self.emit_byte(opcode::MULF),
                        _ => panic!("Unsupported assignment types"),
                    }

                    self.codegen_expr(rhs, map);

                    self.emit_bytes(opcode::SETLOCAL, ident as u8);
                }
                BinOp::DivEqual => {
                    let ident = self.get_ident(lhs.item, map);

                    self.emit_bytes(opcode::GETLOCAL, ident as u8);

                    match ty {
                        Type::Con(TypeCon::Int) => self.emit_byte(opcode::DIV),
                        Type::Con(TypeCon::Float) => self.emit_byte(opcode::DIVF),
                        _ => panic!("Unsupported assignment types"),
                    }

                    self.codegen_expr(rhs, map);

                    self.emit_bytes(opcode::SETLOCAL, ident as u8);
                }
                op => {
                    self.codegen_expr(lhs, map);
                    self.codegen_expr(rhs, map);

                    match (ty, op) {
                        (Type::Con(TypeCon::Int), BinOp::Plus) => self.emit_byte(opcode::ADD),
                        (Type::Con(TypeCon::Float), BinOp::Plus) => self.emit_byte(opcode::ADDF),

                        (Type::Con(TypeCon::Int), BinOp::Minus) => self.emit_byte(opcode::SUB),
                        (Type::Con(TypeCon::Float), BinOp::Minus) => self.emit_byte(opcode::SUBF),

                        (Type::Con(TypeCon::Int), BinOp::Mult) => self.emit_byte(opcode::MUL),
                        (Type::Con(TypeCon::Float), BinOp::Mult) => self.emit_byte(opcode::MULF),

                        (Type::Con(TypeCon::Int), BinOp::Div) => self.emit_byte(opcode::DIV),
                        (Type::Con(TypeCon::Float), BinOp::Div) => self.emit_byte(opcode::DIVF),

                        (Type::Con(TypeCon::Bool), BinOp::GreaterThan) => {
                            let lhs_ty = self.type_map.expr_to_type.get(&lhs.item).unwrap();

                            match lhs_ty {
                                Type::Con(TypeCon::Int) => self.emit_byte(opcode::GREATER),
                                Type::Con(TypeCon::Float) => self.emit_byte(opcode::GREATERF),
                                _ => {
                                    panic!(
                                        "Invalid comparison types for {:?} lhs {:?} rhs {:?}",
                                        op,
                                        lhs_ty,
                                        self.type_map.expr_to_type.get(&rhs.item).unwrap()
                                    )
                                }
                            }
                        }

                        (Type::Con(TypeCon::Bool), BinOp::GreaterThanEqual) => {
                            let lhs_ty = self.type_map.expr_to_type.get(&lhs.item).unwrap();

                            match lhs_ty {
                                Type::Con(TypeCon::Int) => {
                                    self.emit_bytes(opcode::GREATER, opcode::NOT)
                                }
                                Type::Con(TypeCon::Float) => {
                                    self.emit_bytes(opcode::GREATERF, opcode::NOT)
                                }
                                _ => {
                                    panic!(
                                        "Invalid comparison types for {:?} lhs {:?} rhs {:?}",
                                        op,
                                        lhs_ty,
                                        self.type_map.expr_to_type.get(&rhs.item).unwrap()
                                    )
                                }
                            }
                        }

                        (Type::Con(TypeCon::Bool), BinOp::LessThan) => {
                            let lhs_ty = self.type_map.expr_to_type.get(&lhs.item).unwrap();

                            match lhs_ty {
                                Type::Con(TypeCon::Int) => self.emit_byte(opcode::LESS),
                                Type::Con(TypeCon::Float) => self.emit_byte(opcode::LESSF),
                                _ => {
                                    panic!(
                                        "Invalid comparison types for {:?} lhs {:?} rhs {:?}",
                                        op,
                                        lhs_ty,
                                        self.type_map.expr_to_type.get(&rhs.item).unwrap()
                                    )
                                }
                            }
                        }

                        (Type::Con(TypeCon::Bool), BinOp::LessThanEqual) => {
                            let lhs_ty = self.type_map.expr_to_type.get(&lhs.item).unwrap();

                            match lhs_ty {
                                Type::Con(TypeCon::Int) => {
                                    self.emit_bytes(opcode::LESS, opcode::NOT)
                                }
                                Type::Con(TypeCon::Float) => {
                                    self.emit_bytes(opcode::LESSF, opcode::NOT)
                                }
                                _ => {
                                    panic!(
                                        "Invalid comparison types for {:?} lhs {:?} rhs {:?}",
                                        op,
                                        lhs_ty,
                                        self.type_map.expr_to_type.get(&rhs.item).unwrap()
                                    )
                                }
                            }
                        }

                        (Type::Con(TypeCon::Str), BinOp::Plus) => self.emit_byte(opcode::CONCAT),

                        (_, BinOp::Equal) => self.emit_byte(opcode::EQUAL),
                        (_, BinOp::NotEqual) => self.emit_bytes(opcode::ENUM, opcode::NOT),

                        _ => {
                            todo!(
                                "OP {:?} LHS {:?} RHS {:?}",
                                op,
                                self.type_map.expr_to_type.get(&lhs.item).unwrap(),
                                self.type_map.expr_to_type.get(&rhs.item).unwrap()
                            )
                        }
                    }
                }
            },
            Expr::Block(block_id, _) => {
                let block = map.block(block_id);

                self.begin_scope();

                for stmt in block.0.iter() {
                    self.codegen_statement(stmt, map)
                }

                self.end_scope();
            }

            Expr::Break => {
                let description = self.current_loop.expect("Using break outside a loop");

                self.emit_bytes(opcode::JUMP, description.end as u8);
            }

            Expr::Continue => {
                let description = self.current_loop.expect("Using break outside a loop");
                self.emit_bytes(opcode::JUMP, description.start as u8);
            }

            Expr::Call { callee, args, .. } => {
                for arg in args {
                    self.codegen_expr(arg, map);
                }

                self.codegen_expr(callee, map);

                match self.current_call {
                    Some(name) => {
                        let print = self.db.intern_name(Name::new("print"));

                        if name == print {
                            self.emit_bytes(opcode::PRINT, args.len() as u8);
                        } else {
                            self.emit_byte(opcode::CALL);
                            self.emit_bytes(args.len() as u8, name.0.as_u32() as u8)
                        }
                    }
                    None => {}
                };
            }
            Expr::Cast { expr, ty } => {}
            Expr::Closure { .. } => {}
            Expr::If {
                cond,
                then_branch,
                else_branch: None,
            } => {
                self.codegen_expr(cond, map);

                let false_label = self.emit_jump(opcode::JUMPNOT);

                self.emit_byte(opcode::POP);

                self.codegen_expr(then_branch, map);

                self.patch_jump(false_label);

                self.emit_byte(opcode::POP);
            }
            Expr::If {
                cond,
                then_branch,
                else_branch: Some(else_branch),
            } => {
                self.codegen_expr(cond, map);

                let false_label = self.emit_jump(opcode::JUMPNOT);

                self.emit_byte(opcode::POP);

                self.codegen_expr(then_branch, map);

                let end_label = self.emit_jump(opcode::JUMP);

                self.patch_jump(false_label);

                self.emit_byte(opcode::POP);

                self.codegen_expr(else_branch, map);

                self.patch_jump(end_label);
            }
            Expr::Ident(index) => {
                if let Some(pos) = self.locals.get(&index.item).cloned() {
                    self.emit_bytes(opcode::GETLOCAL, pos as u8);
                } else if let Some(pos) = self.params.get(&index.item).cloned() {
                    self.emit_bytes(opcode::GETPARAM, pos as u8);
                } else {
                    let print = self.db.intern_name(Name::new("print"));

                    if index.item == print {
                        self.current_call = Some(print);
                    } else {
                        self.current_call = Some(index.item);
                    }
                }
            }
            Expr::Index { base, index } => match ty {
                Type::Con(TypeCon::Str) => {
                    self.codegen_expr(base, map);
                    self.codegen_expr(index, map);

                    self.emit_byte(opcode::INDEXSTRING);
                }
                Type::Con(TypeCon::Array { .. }) => {
                    self.codegen_expr(base, map);
                    self.codegen_expr(index, map);
                    self.emit_byte(opcode::INDEXARRAY);
                }

                _ => panic!("Tried to index a {:?}", ty),
            },
            Expr::While { cond, body } => {
                let start_label = self.chunk.code.len();

                self.codegen_expr(cond, map);

                let out = self.emit_jump(opcode::JUMPNOT);

                self.current_loop = Some(LoopDescription {
                    start: start_label,
                    end: out,
                });

                self.emit_byte(opcode::POP);

                let block = map.block(body);

                for stmt in block.0.iter() {
                    self.codegen_statement(stmt, map)
                }

                self.emit_loop(start_label); // Jumps back to the start

                self.patch_jump(out); // the outer label

                self.emit_byte(opcode::POP); //removes cond from stack
            }
            Expr::Literal(literal) => {
                let literal = self.db.lookup_intern_literal(*literal);

                match literal {
                    Literal::True => self.emit_byte(opcode::TRUE),
                    Literal::False => self.emit_byte(opcode::FALSE),
                    Literal::Int(int) => {
                        let int = int.parse::<i64>().unwrap();
                        self.emit_constant(Value::int(int));
                    }
                    Literal::Float(float) => {
                        let float = float.parse::<f64>().unwrap();
                        self.emit_constant(Value::float(float));
                    }
                    Literal::String(string) => {
                        let object = StringObject::new(&string, self.objects);

                        self.emit_object(object)
                    }
                    Literal::Nil => self.emit_byte(opcode::NIL),
                }
            }
            Expr::Paren(expr) => self.codegen_expr(expr, map),
            Expr::Tuple(exprs) => {
                for expr in exprs {
                    self.codegen_expr(expr, map)
                }

                self.emit_bytes(opcode::TUPLE, exprs.len() as u8);
            }
            Expr::Unary { op, expr } => {
                self.codegen_expr(expr, map);
                match op {
                    UnaryOp::Minus => match ty {
                        Type::Con(TypeCon::Int) => self.emit_byte(opcode::NEGATE),
                        Type::Con(TypeCon::Float) => self.emit_byte(opcode::NEGATEF),
                        _ => panic!("Tried using {:?} on an unsupported type {:?}", op, ty),
                    },
                    UnaryOp::Excl => {
                        self.emit_byte(opcode::NOT);
                    }
                }
            }
            Expr::Return(expr) => {
                if let Some(expr) = expr {
                    self.codegen_expr(expr, map);
                }

                self.emit_byte(opcode::RETURN)
            }
            Expr::Match { expr, arms } => {}
            Expr::Enum { def, variant, expr } => {}
            Expr::RecordLiteral { def, fields } => {
                // generate the expressions
                for (_, value) in fields.iter().rev() {
                    self.codegen_expr(value, map);
                }

                self.emit_bytes(opcode::CLASSINSTANCE, def.item.0.as_u32() as u8);

                self.emit_byte(fields.len() as u8);

                for (field, _) in fields.iter().rev() {
                    self.emit_byte(field.item.0.as_u32() as u8);
                }
            }
            Expr::Field(_) => {}
        }
    }

    fn codegen_and(&mut self, lhs: &Span<ExprId>, rhs: &Span<ExprId>, map: &FunctionAstMap) {
        self.codegen_expr(lhs, map);

        let false_label = self.emit_jump(opcode::JUMPNOT);

        self.codegen_expr(rhs, map);

        self.patch_jump(false_label);
    }

    fn codegen_or(&mut self, lhs: &Span<ExprId>, rhs: &Span<ExprId>, map: &FunctionAstMap) {
        self.codegen_expr(lhs, map);

        let else_label = self.emit_jump(opcode::JUMPIF);

        self.codegen_expr(rhs, map);

        self.patch_jump(else_label);

        self.emit_byte(opcode::POP);
    }
}

fn new() -> () {
    todo!()
}

pub fn codegen_query(
    db: &impl CodegenDatabase,
    file: FileId,
) -> WithError<(ir::Program, RawObject)> {
    let WithError(program, mut errors) = db.lower(file);
    let WithError(type_map, error) = db.infer(file);
    errors.extend(error);

    let mut bytecode = ir::Program {
        functions: HashMap::new(),
        classes: HashMap::new(),
    };

    let mut builder = CodegenBuilder {
        db,
        type_map: &InferDataMap {
            expr_to_type: IndexMap::new(),
            stmt_to_type: IndexMap::new(),
        },
        chunk: Chunk::new(),
        current_loop: None,
        current_call: None,
        objects: std::ptr::null::<RawObject>() as RawObject,
        slots: 0,
        line: 0,
        locals: StackedMap::new(),
        params: HashMap::new(),
    };

    for function in &program.functions {
        builder.type_map = type_map.get(&function.name.item).unwrap();

        let func = builder.build_function(function);

        func.body
            .disassemble(&db.lookup_intern_name(function.name.item));

        bytecode.functions.insert(function.name.item, func);
    }

    for class in &program.classes {
        bytecode
            .classes
            .insert(class.name.item, builder.build_class(class));
    }

    WithError((bytecode, builder.objects), errors)
}
