use ast as t;
use ir::instructions::*;
use ir::types::Type;
use std::collections::HashMap;
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

    pub fn build_statement(&mut self, s: Spanned<t::Statement>) {
        use self::t::Statement;

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

                self.build_statement(*then);

                self.end_block(BlockEnd::Jump(after));

                self.start_block(other);

                self.build_statement(*otherwise);

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

                self.build_statement(*then);

                self.end_block(BlockEnd::Jump(after));

                self.start_block(after);
            }

            Statement::Var { ident, ty, expr } => {
                let reg = self.add_local(ident);

                if let Some(expr) = expr {
                    let expr = self.build_expr(expr);
                    self.emit_store(Value::Register(reg), expr, ty);
                }
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

                self.build_statement(*body);

                self.end_block(BlockEnd::Jump(cond_block));

                self.start_block(after);
            }

            _ => unimplemented!(),
        }
    }

    fn build_expr(&mut self,expr: Spanned<t::TypedExpression>) -> Value {
        unimplemented!()
    }
}
