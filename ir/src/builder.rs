use std::collections::HashMap;

use errors::{FileId, WithError};

use semant::{
    hir::{
        BinOp, Class, Expr, ExprId, Function, FunctionAstMap, Literal, Name, NameId, PatId,
        Pattern, Stmt, StmtId, UnaryOp,
    },
    IndexMap, InferDataMap, Span, StackedMap, Type, TypeCon,
};

use crate::ir::{self, Instruction, Value};
use crate::{db::IrDatabase, ir::Register};
#[derive(Debug)]
struct LoopDescr {}

#[derive(Debug)]
pub(crate) struct IrBuilder<'map, DB> {
    db: DB,
    type_map: &'map InferDataMap,
    register_count: u32,
    current_instructions: Option<Vec<Instruction>>,
    current_loop: Option<LoopDescr>,
    locals: StackedMap<NameId, Register>,
    params: HashMap<NameId, Register>,
}

impl<'a, 'map, DB> IrBuilder<'map, &'a DB>
where
    DB: IrDatabase,
{
    fn register(&mut self) -> ir::Register {
        let count = Register(self.register_count);

        self.register_count += 1;

        count
    }

    fn emit(&mut self, inst: Instruction) {
        self.current_instructions
            .as_mut()
            .expect("Function not started")
            .push(inst)
    }

    fn emit_store(&mut self, dest: Register, src: Register) {
        self.current_instructions
            .as_mut()
            .expect("Function not started")
            .push(Instruction::Store(dest, src))
    }

    fn emit_store_immediate(&mut self, dest: Register, val: Value) {
        self.current_instructions
            .as_mut()
            .expect("Basic block should be started")
            .push(Instruction::StoreI(dest, val))
    }

    fn pat_to_reg(&mut self, id: &PatId, map: &FunctionAstMap) -> Register {
        let pat = map.pat(&id);
        let reg = self.register();

        match pat {
            Pattern::Bind { name } => {
                self.locals.insert(name.item, reg);
            }
            Pattern::Placeholder => {
                // Todo check if this is the correct behavior
            }
            Pattern::Tuple(pats) => {
                // TODO treat tuple like array and access members by offset ?
                for pat in pats {
                    let _ = self.pat_to_reg(&pat.item, map);
                }
            }
            Pattern::Literal(_) => {}
        }

        reg
    }

    fn statement(&mut self, id: &Span<StmtId>, map: &FunctionAstMap) {
        let stmt = map.stmt(&id.item);

        match stmt {
            Stmt::Let {
                pat,
                ascribed_type,
                initializer,
            } => {
                let dest = self.pat_to_reg(&pat.item, map);

                if let Some(expr) = initializer {
                    let res = self.expr(expr, map);
                    self.emit_store(dest, res)
                } else {
                    self.emit_store_immediate(dest, Value::Nil);
                }
            }
            Stmt::Expr(id) => {
                let _ = self.expr(id, map);
            }
            Stmt::Error => self.emit(Instruction::Illegal),
        }
    }

    fn expr(&mut self, id: &Span<ExprId>, map: &FunctionAstMap) -> Register {
        let expr = map.expr(&id.item);

        match expr {
            Expr::Paren(_)
            | Expr::Tuple(_)
            | Expr::Unary { .. }
            | Expr::Return(_)
            | Expr::Match { .. }
            | Expr::Enum { .. }
            | Expr::RecordLiteral { .. }
            | Expr::Field(_)
            | Expr::Array(_)
            | Expr::Block(_, _)
            | Expr::Break
            | Expr::Call { .. }
            | Expr::Cast { .. }
            | Expr::Closure { .. }
            | Expr::Continue
            | Expr::If { .. }
            | Expr::Ident(_)
            | Expr::Index { .. }
            | Expr::While { .. } => unimplemented!(),
            Expr::Binary { lhs, op, rhs } => match op {
                // BinOp::And => {}

                // BinOp::Or => {}
                _ => {
                    let lhs = self.expr(lhs, map);

                    let rhs = self.expr(rhs, map);

                    let result = self.register();

                    self.emit(Instruction::Binary(result, lhs, *op, rhs));

                    result
                }
            },
            Expr::Literal(literal) => {
                let dest = self.register();
                let literal = self.db.lookup_intern_literal(*literal);

                match literal {
                    Literal::True => self.emit_store_immediate(dest, Value::Bool(true)),
                    Literal::False => self.emit_store_immediate(dest, Value::Bool(false)),
                    Literal::Int(int) => {
                        let int = int.parse::<i64>().unwrap();

                        self.emit_store_immediate(dest, Value::Int(int))
                    }
                    Literal::Float(float) => {
                        let float = float.parse::<f64>().unwrap();
                        self.emit_store_immediate(dest, Value::Float(Box::new(float.to_be_bytes())))
                    }
                    Literal::String(string) => {
                        // Support strings
                    }
                    Literal::Nil => self.emit_store_immediate(dest, Value::Nil),
                }

                dest
            }
        }
    }
}

pub fn build_ir_query(db: &impl IrDatabase, file: FileId) -> WithError<Vec<ir::Instruction>> {
    let WithError(program, mut errors) = db.lower(file);
    let WithError(type_map, error) = db.infer(file);
    errors.extend(error);

    let mut builder = IrBuilder {
        db,
        type_map: &InferDataMap {
            expr_to_type: IndexMap::new(),
            stmt_to_type: IndexMap::new(),
        },
        register_count: 0,
        current_instructions: None,
        locals: StackedMap::new(),
        params: HashMap::new(),
        current_loop: None,
    };

    unimplemented!()
}
