use std::collections::HashMap;

use errors::{FileId, WithError};
use semant::{Function, IndexMap, InferDataMap, Span, StackedMap, Typed};
use semant::hir::{Literal, Name, NameId, UnaryOp,self,};
use semant::typed::{Expr, Pattern, Stmt};

use crate::{block::{Block, BlockEnd, BlockID}, db::IrDatabase, ir::Register};
use crate::block::LoopDescription;
use crate::ir::{self, Instruction, Value, BinOp, FloatParts};

#[derive(Debug)]
pub(crate) struct IrBuilder<'map, DB> {
    db: DB,
    type_map: &'map InferDataMap,
    register_count: u32,
    block_count: u32,
    current_loop: Option<LoopDescription>,
    current_block: Option<(BlockID, Vec<Instruction>)>,
    blocks: Vec<(BlockID, Block)>,
    locals: StackedMap<NameId, Register>,
    params: HashMap<NameId, Register>,
}

impl<'a, 'map, DB> IrBuilder<'map, &'a DB>
    where
        DB: IrDatabase,
{
    fn block(&mut self) -> BlockID {
        let block = BlockID(self.block_count);

        self.block_count += 1;
        block
    }


    fn local(&mut self, name: NameId) -> Register {
        let reg = self.register();
        self.locals.insert(name, reg);
        reg
    }

    pub fn end_block(&mut self, end: BlockEnd) {
        let (id, inst) = self.current_block.take().unwrap();

        self.blocks.push((
            id,
            Block {
                instructions: inst,
                end,
            },
        ));
    }

    fn register(&mut self) -> ir::Register {
        let count = Register(self.register_count);

        self.register_count += 1;

        count
    }



    pub fn start_block(&mut self, id: BlockID) {
        if self.current_block.is_some() {
            panic!("Block is unfinished");
        }

        self.current_block = Some((id, Vec::new()));
    }


    fn build_and(
        &mut self,
        l: &Typed<Expr>,
        r: &Typed<Expr>,
    ) -> Register {
        let built_lhs = self.expr(l);
        let rhs_block = self.block();
        let reset_block = self.block();
        let after_block = self.block();
        let result = self.register();

        self.end_block(BlockEnd::Branch(built_lhs.clone(), rhs_block, reset_block));

        self.start_block(rhs_block);

        let built_rhs = self.expr(r);

        self.emit_store(result, built_rhs.clone());

        self.end_block(BlockEnd::Jump(after_block));
        self.start_block(reset_block);

        self.emit_store_immediate(result, Value::Bool(false));

        self.end_block(BlockEnd::Jump(after_block));

        self.start_block(after_block);

        result
    }

    fn build_or(
        &mut self,
        l: &Typed<Expr>,
        r: &Typed<Expr>,
    ) -> Register {
        let built_lhs = self.expr(l);
        let rhs_block = self.block();
        let reset_block = self.block();
        let after_block = self.block();
        let result = self.register();

        self.end_block(BlockEnd::Branch(built_lhs, reset_block, rhs_block));

        self.start_block(rhs_block);

        let built_rhs = self.expr(r);

        self.emit_store(result, built_rhs);

        self.end_block(BlockEnd::Jump(after_block));
        self.start_block(reset_block);

        self.emit_store_immediate(result, Value::Bool(true));

        self.end_block(BlockEnd::Jump(after_block));

        self.start_block(after_block);

        result
    }

    fn emit(&mut self, inst: Instruction) {
        self.current_block
            .as_mut()
            .expect("Basic block should be started").1
            .push(inst)
    }

    fn emit_store(&mut self, dest: Register, src: Register) {
        self.current_block
            .as_mut()
            .expect("Basic block should be started").1
            .push(Instruction::Store(dest, src))
    }

    fn emit_store_immediate(&mut self, dest: Register, val: Value) {
        self.current_block
            .as_mut()
            .expect("Basic block should be started").1
            .push(Instruction::StoreI(dest, val))
    }

    fn expr(&mut self, expr: &Typed<Expr>) -> Register {
        match &expr.item {
            Expr::Block(stmts, has_value) => {
                let exprs = stmts.iter().map(|expr| self.statement(expr)).collect::<Vec<_>>();

                if *has_value {
                    exprs[exprs.len() - 1]
                } else {
                    let rhs = self.register();

                    self.emit_store_immediate(rhs, Value::Nil);
                    rhs
                }
            }

            Expr::Break => {
                let description = self.current_loop.expect("Cannot use break outside a loop");

                let new = self.block();

                self.end_block(BlockEnd::Jump(description.end()));

                self.start_block(new);

                self.register()
            },
            Expr::Continue => {
                let description = self
                    .current_loop
                    .expect("Cannot use continue outside a loop");

                let new = self.block();

                self.end_block(BlockEnd::Jump(description.start()));

                self.start_block(new);
                self.register()}

            Expr::Return(ret_expr) => {
                let result  =  if let Some(expr) = ret_expr {
                    self.expr(expr)
                }else {

                    let rhs = self.register();

                    self.emit_store_immediate(rhs,Value::Nil);
                    rhs
                };

                let new = self.block();


                self.end_block(BlockEnd::Return(result));

                self.start_block(new);

                result
            },

            Expr::While { cond, body } => {
                let cond_block = self.block();
                let body_block = self.block();
                let after = self.block();

                let outer_loop = self.current_loop.take();

                self.current_loop = Some(LoopDescription {
                    start: cond_block,
                    end: after,
                });

                self.end_block(BlockEnd::Jump(cond_block));

                self.start_block(cond_block);

                let c = self.expr(cond);

                self.end_block(BlockEnd::Branch(c, body_block, after));

                self.start_block(body_block);

                let result =  self.expr(body);

                self.end_block(BlockEnd::Jump(cond_block));

                self.current_loop = outer_loop;

                self.start_block(after);

               result
            },

            Expr::If {
                cond, then_branch, else_branch
            } => {
                let c = self.expr(cond);

                let body = self.block(); // then body
                let other = self.block(); // else body
                let after = self.block();

                self.end_block(BlockEnd::Branch(c, body, other));

                self.start_block(body);

                let then = self.expr(then_branch);

                self.end_block(BlockEnd::Jump(after));

                self.start_block(other);

               if let Some(expr) = else_branch {
                    self.expr(expr)l;
                }else {

                    let rhs = self.register();

                    self.emit_store_immediate(rhs,Value::Nil);
                };

                self.end_block(BlockEnd::Jump(after));

                self.start_block(after);
                self.register()
            },
            Expr::Binary { lhs, op, rhs } => {
                match (op,&expr.ty) {
                    (hir::BinOp::Equal | hir::BinOp::EqualEqual, _) => {
                        let lhs = self.expr(lhs);

                        let rhs = self.expr(rhs);


                        let res = self.register();


                        self.emit_store(lhs,rhs);

                        res
                    },
                    (hir::BinOp::And,_) => {
                        self.build_and(lhs,rhs)
                    },
                    (hir::BinOp::Or,_) => {
                        self.build_or(lhs,rhs)
                    },

                    _ => unreachable!()

                }
            }
            Expr::Unary { op, expr } => {
                let res = self.register();


                let lhs = self.register();
                self.emit_store_immediate(lhs,Value::Int(0));
                let rhs = self.expr(expr);

                match  op {
                    UnaryOp::Minus => {
                        self.emit(Instruction::Binary(res,lhs,BinOp::Minus,rhs));
                    }
                    UnaryOp::Excl => {
                        self.emit(Instruction::Binary(res,rhs,BinOp::Equal,lhs));
                    }
                }
                res
            }

            Expr::Paren(expr) => self.expr(expr),
            Expr::Literal(literal) => {
                let res = self.register();

                let literal = self.db.lookup_intern_literal(*literal);

                match literal {
                    Literal::True => {
                        self.emit_store_immediate(res, Value::Int(1));
                    }
                    Literal::False => {
                        self.emit_store_immediate(res, Value::Int(0));
                    }
                    Literal::Int(int) => {
                        let int = int.parse::<i64>().unwrap();
                        self.emit_store_immediate(res, Value::Int(int));
                    }
                    Literal::Float(float) => {
                        let float = float.parse::<f64>().unwrap();
                        let u = unsafe { std::mem::transmute::<f64, FloatParts>(float) };
                        self.emit_store_immediate(res, Value::Float {f_bits: u.f_bits,i_bits:u.i_bits});
                    },
                    Literal::Nil => {
                        self.emit_store_immediate(res, Value::Nil);
                    }
                    Literal::String(string) => {


                        // TODO call malloc and create a new string
                        // Support strings
                        unimplemented!()
                    }

                }

                res
            },

            _ => unimplemented!()
        }
    }

    fn statement(&mut self, stmt: &Typed<Stmt>) -> Register {
        match &stmt.item {
            Stmt::Let {
                pat, initializer, ..
            } => {
                let rhs = if let Some(expr) = initializer {
                    self.expr(expr)
                } else {
                    let register = self.register();
                    self.emit_store_immediate(register, Value::Nil);
                    register
                };

                match &pat.item {
                    Pattern::Bind { name } => {
                        let lhs = self.local(name.item);
                        self.emit_store(lhs, rhs);
                    }
                    Pattern::Placeholder => {
                        let lhs = self.register();
                        self.emit_store(lhs, rhs);
                    }
                    Pattern::Tuple(pats) => {
                        /* TODO
                        let  (a,b) = (0,10) will become
                        let _a0 = 0,
                        let _b0 = 10;
                        etc.. */

                        todo!()
                    }
                    Pattern::Literal(_) => panic!("Invalid lhs val! Should've been caught in semant")
                }

                self.register() // we don't access this value so this is basically a no-op
            }
            Stmt::Expr(expr) => self.expr(expr),
            Stmt::Error => panic!("Generating IR for invalid code")
        }
    }


    pub fn build_function(&mut self, function: &Typed<Function>) {
        for (i, param) in function.params.iter().enumerate() {
            let reg = self.register();
            self.emit_store_immediate(reg, Value::Int(i as i64))
        }

        for stmt in &function.item.body {
            self.statement(stmt);
        }
    }
}

pub fn build_ir_query(db: &impl IrDatabase, file: FileId) -> WithError<()> {
    let WithError(typed_program, error) = db.infer(file);
    let mut builder = IrBuilder {
        db,
        type_map: &InferDataMap {
            expr_to_type: IndexMap::new(),
            stmt_to_type: IndexMap::new(),
        },
        blocks: Vec::new(),
        locals: StackedMap::new(),
        params: HashMap::new(),
        current_loop: None,
        current_block: None,
        register_count: 0,
        block_count: 0,
    };


    let start = builder.block();

    builder.start_block(start);


    for function in &typed_program.functions {
        let _ = builder.build_function(function);
    };

    if builder.current_block.is_some() {
        builder.end_block(BlockEnd::Jump(start))
    }

    WithError((),error)
}
