use ast as t;
use ir::instructions::*;
use ir::types::*;
use std::collections::HashMap;
use syntax::ast::{Literal, Op, UnaryOp};
use util::pos::Spanned;
use util::symbol::Symbol;
use util::symbol::Symbols;

#[derive(Debug)]
struct Builder<'a> {
    symbols: &'a Symbols<()>,
    locals: HashMap<Symbol, Register>,
    parameters: HashMap<Symbol, Register>,
    current_loop: Option<LoopDescription>,
    blocks: HashMap<BlockID, Block>,
    current_block: Option<(BlockID, Vec<Instruction>)>,
}

impl<'a> Builder<'a> {
    pub fn new(symbols: &'a Symbols<()>) -> Self {
        Builder {
            symbols,
            locals: HashMap::new(),
            parameters: HashMap::new(),
            current_loop: None,
            current_block: None,
            blocks: HashMap::new(),
        }
    }

    pub fn blocks(self) -> HashMap<BlockID, Block> {
        self.blocks
    }

    pub fn new_block(&mut self) -> BlockID {
        BlockID::new()
    }

    pub fn start_block(&mut self, id: BlockID) {
        if self.current_block.is_some() {
            panic!("Block is unfinished");
        }

        self.current_block = Some((id, Vec::new()));
    }

    pub fn end_block(&mut self, end: BlockEnd) {
        
        let (id, inst) = self.current_block.take().unwrap();

        self.blocks.insert(
            id,
            Block {
                instructions: inst,
                end,
            },
        );
    }

    pub fn parameters(&mut self) -> Vec<Register> {
        self.parameters
            .iter()
            .map(|(_, register)| *register)
            .collect()
    }

    pub fn emit_instruction(&mut self, inst: Inst, ty: Type) {
        self.current_block
            .as_mut()
            .expect("Basic Block should be started")
            .1
            .push(Instruction {
                instruction: inst,
                ty,
            });
    }

    pub fn emit_store(&mut self, dest: Value, source: Value, ty: Type) {
         
        self.current_block
            .as_mut()
            .expect("Basic block should be started")
            .1
            .push(Instruction {
                instruction: Inst::Store(dest, source),
                ty,
            })
    }

    pub fn add_param(&mut self, symbol: Symbol) {
        self.parameters.insert(symbol, Register::new());
    }

    pub fn add_local(&mut self, symbol: Symbol) -> Register {
        let reg = Register::new();

        self.locals.insert(symbol, reg);

        reg
    }

    pub fn build_statement(&mut self, s: Spanned<t::TypedStatement>) {
        use self::t::Statement;

        let s = *s.value.statement; // prevents colleteral moves error

        match s.value {
            Statement::Block(statements) => {
                for statement in statements {
                    self.emit_instruction(Inst::StatementStart, Type::Nil);
                    self.build_statement(statement)
                }
            }
            Statement::Break => {
                let description = self.current_loop.expect("Cannot use break outside a loop");

                let new = self.new_block();

                self.end_block(BlockEnd::Jump(description.end()));

                self.start_block(new)
            }

            Statement::Continue => {
                let description = self
                    .current_loop
                    .expect("Cannot use continue outside a loop");

                let new = self.new_block();

                self.end_block(BlockEnd::Jump(description.start()));

                self.start_block(new)
            }

            Statement::Expr(expr) => {
                self.build_expr(expr);
            }

            Statement::If {
                cond,
                then,
                otherwise: Some(otherwise),
            } => {
                let c = self.build_expr(cond);

                let body = BlockID::new(); // then body
                let other = BlockID::new(); // else body
                let after = BlockID::new();

                self.end_block(BlockEnd::Branch(c, body, other));

                self.start_block(body);

                self.build_statement(then);

                self.end_block(BlockEnd::Jump(after));

                self.start_block(other);

                self.build_statement(otherwise);

                self.end_block(BlockEnd::Jump(after));

                self.start_block(after);
            }

            Statement::If {
                cond,
                then,
                otherwise: None,
            } => {
                let c = self.build_expr(cond);

                let body = BlockID::new();

                let after = BlockID::new();

                self.end_block(BlockEnd::Branch(c, body, after));

                self.start_block(body);

                self.build_statement(then);

                self.end_block(BlockEnd::Jump(after));

                self.start_block(after);
            }

            Statement::Let { ident, ty, expr } => {

                
                let reg = self.add_local(ident);

                if let Some(expr) = expr {
                    let expr = self.build_expr(expr);
                    self.emit_store(Value::Register(reg), expr, ty);
                }
            }

            Statement::Print(expr) => {
                let expr = self.build_expr(expr);

                self.emit_instruction(Inst::Print(expr), Type::Nil);
            }

            Statement::Return(expr) => {
                let result = self.build_expr(expr);
                let new = self.new_block();

                self.end_block(BlockEnd::Return(result));

                self.start_block(new);
            }

            Statement::While(cond, body) => {
                let cond_block = BlockID::new();
                let body_block = BlockID::new();
                let after = BlockID::new();

                self.current_loop = Some(LoopDescription {
                    start: cond_block,
                    end: after,
                });

                self.end_block(BlockEnd::Jump(cond_block));

                self.start_block(cond_block);

                let c = self.build_expr(cond);

                self.end_block(BlockEnd::Branch(c, body_block, after));

                self.start_block(body_block);

                self.build_statement(body);

                self.end_block(BlockEnd::Jump(cond_block));

                self.start_block(after);
            }
        }
    }

    fn build_expr(&mut self, expr: Spanned<t::TypedExpression>) -> Value {
        use self::t::Expression;

        let expr = expr.value;

        let ty = expr.ty;
        let expr = expr.expr.value;

        match expr {
            Expression::Array(items) => {
                let temp = Register::new();

                self.emit_instruction(Inst::Array(Value::Register(temp), items.len()), ty.clone());

                for (i, item) in items.into_iter().enumerate() {
                    let result = self.build_expr(item);
                    let offset = Register::new();

                    self.emit_instruction(
                        Inst::Binary(
                            offset,
                            Value::Register(temp),
                            BinaryOp::Plus,
                            Value::Const(i as i64),
                        ),
                        Type::App(TypeCon::Int, vec![]),
                    );

                    self.emit_store(Value::Register(offset), result, Type::Nil);
                }

                Value::Register(temp)
            }

            Expression::Assign(var, op, expr) => {
                let expr = self.build_expr(expr);
                let var = self.build_var(&var).expect("Undefined Variable");

                self.emit_store(var.clone(), expr, ty.clone());

                var
            }

            Expression::Binary(lhs, op, rhs) => match op {
                Op::And => self.build_and(lhs, rhs, ty),
                Op::Or => self.build_or(lhs, rhs, ty),
                _ => {
                    let lhs = self.build_expr(lhs);
                    let rhs = self.build_expr(rhs);

                    let op = gen_bin_op(op);
                    let result = Register::new();

                    self.emit_instruction(Inst::Binary(result, lhs, op, rhs), ty);
                    Value::Register(result)
                }
            },

            Expression::Call(callee, args) => {
                let result = Register::new();
                let ident = Value::Named(callee);
                let mut temps = Vec::with_capacity(args.len()); // Temps where the expressions are stored

                for expr in args {
                    temps.push(self.build_expr(expr))
                }

                self.emit_instruction(Inst::Call(Value::Register(result), ident, temps), ty);

                Value::Register(result)
            }

            Expression::Cast(expr, ty) => {
                let from = expr.value.ty.clone();
                let result = self.build_expr(expr);

                self.emit_instruction(Inst::Cast(result.clone(), from,ty.clone()), ty);

                result
            }

            Expression::ClassLiteral { .. } => unimplemented!(),

            Expression::Closure(_) => unimplemented!(),
            

            Expression::GetProperty {property_name,property} => {
                unimplemented!()
            },

            Expression::GetMethod { method_name,method} => {
                unimplemented!()
            },

            Expression::Index(target,index) => {
                unimplemented!()
            }

            

            Expression::Literal(literal) => {
                let tmp = Register::new();

                match literal {
                    Literal::False(_) => {
                        self.emit_store(Value::Register(tmp), Value::Bool(false), ty);
                    }
                    Literal::Nil => {
                        self.emit_store(Value::Register(tmp), Value::Nil, ty);
                    }

                    Literal::Int(number) => {
                        self.emit_store(Value::Register(tmp), Value::Const(number), ty)
                    }

                    Literal::Float(number) => {
                        self.emit_store(Value::Register(tmp), Value::Float(number), ty)
                    }

                    Literal::Str(string) => {
                        let mut bytes = string.into_bytes();

                        self.emit_store(Value::Register(tmp), Value::Mem(bytes), ty);
                    }

                    Literal::True(_) => {
                        self.emit_store(Value::Register(tmp), Value::Bool(true), ty);
                    }
                };

                Value::Register(tmp)
            },

            Expression::Match {cond,arms,all} => {
                let after_block = self.new_block();

                let cond = self.build_expr(cond);

                let result  = Register::new();

                let block_ids:Vec<BlockID> = (0..arms.value.len()+1).map(|_| self.new_block()).collect();



                self.end_block(BlockEnd::Link(*block_ids.first().unwrap())); //fix empty match

                for (i,arm) in arms.value.into_iter().enumerate() {
                    let result = Register::new();
                    self.start_block(block_ids[i]);
                    let pattern = self.build_expr(arm.value.pattern);
                    self.emit_instruction(Inst::Binary(result,pattern, BinaryOp::Equal,cond.clone()), Type::App(TypeCon::Bool,vec![]));
                    self.build_statement(arm.value.body);
                    self.end_block(BlockEnd::Branch(
                        Value::Register(result),
                        after_block,
                        block_ids[i+1]
                    ));
                }

               

                if let Some(all) = all {
                    self.start_block(*block_ids.last().unwrap());
                    self.build_statement(all);
                    self.end_block(BlockEnd::Link(after_block))
                }
                
                self.start_block(after_block);

                Value::Register(result)
            },

            Expression::Set(name,object,value) => {
                unimplemented!()
            },

            Expression::StaticMethodCall {
                class_name,
                method_name,
                params
            } => {
unimplemented!() 
            }


            Expression::Grouping(expr) => self.build_expr(expr),

            Expression::Var(ref symbol,_) =>  {
                self.build_var(symbol).unwrap()
            }

            ref e => unimplemented!("{:?}", e),
        }
    }

    fn build_and(
        &mut self,
        l: Spanned<t::TypedExpression>,
        r: Spanned<t::TypedExpression>,
        ty: Type,
    ) -> Value {
        let built_lhs = self.build_expr(l);
        let rhs_block = self.new_block();
        let reset_block = self.new_block();
        let after_block = self.new_block();
        let result = Register::new();

        self.end_block(BlockEnd::Branch(built_lhs.clone(), reset_block, rhs_block));

        self.start_block(rhs_block);

        let built_rhs = self.build_expr(r);

        self.emit_store(Value::Register(result), built_rhs.clone(), ty.clone());

        self.end_block(BlockEnd::Jump(after_block));
        self.start_block(reset_block);

        self.emit_store(Value::Register(result), Value::Bool(true), ty);

        self.end_block(BlockEnd::Jump(after_block));

        self.start_block(after_block);

        Value::Register(result)
    }

    fn build_or(
        &mut self,
        l: Spanned<t::TypedExpression>,
        r: Spanned<t::TypedExpression>,
        ty: Type,
    ) -> Value {
        unimplemented!()
    }

    fn build_var(&self, var: &Symbol) -> Option<Value> {
        if let Some(register) = self.locals.get(var) {
            Some(Value::Register(*register))
        } else if let Some(register) = self.parameters.get(var) {
            Some(Value::Register(*register))
        } else {
            None
        }
    }
}

fn build_function(function: t::Function, symbols: &Symbols<()>) -> Function {
    let mut builder = Builder::new(symbols);

    for param in function.params {
        builder.add_param(param.name);
    }

    let start = builder.new_block();

    builder.start_block(start);
    builder.build_statement(*function.body);

    if builder.current_block.is_some() {
        builder.end_block(BlockEnd::End);
    }

    Function {
        name: function.name,
        params: builder.parameters(),
        start_block: start,
        blocks: builder.blocks(),
    }
}

pub fn build_program(symbols: &Symbols<()>, old_program: t::Program) -> Program {
    let mut new_program = Program {
        functions: vec![],
        classes: vec![],
    };

    for function in old_program.functions {
        new_program
            .functions
            .push(build_function(function, symbols));
    }

    new_program
}

fn gen_un_op(op: UnaryOp) -> UnaryOp {
    match op {
        UnaryOp::Minus => UnaryOp::Minus,
        UnaryOp::Bang => UnaryOp::Bang,
    }
}

fn gen_bin_op(op: Op) -> BinaryOp {
    match op {
        Op::Plus => BinaryOp::Plus,
        Op::Minus => BinaryOp::Minus,
        Op::Star => BinaryOp::Mul,
        Op::Slash => BinaryOp::Div,
        Op::LessThan => BinaryOp::Lt,
        Op::GreaterThan => BinaryOp::Gt,
        Op::LessThanEqual => BinaryOp::Lte,
        Op::GreaterThanEqual => BinaryOp::Gte,
        Op::EqualEqual => BinaryOp::Equal,
        Op::BangEqual => BinaryOp::NotEqual,
        // Op::And => BinaryOp::And,
        // Op::Or => BinaryOp::Or,
        _ => unreachable!(),
    }
}
