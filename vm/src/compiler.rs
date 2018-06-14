// use syntax::ast::statement::Statement;
use chunk::Chunk;
use op::OpCode;
use syntax::ast::expr::*;
use syntax::ast::statement::Statement;

#[derive(Debug)]
struct Compiler {
    chunk: Chunk,
    line: usize,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            chunk: Chunk::new(),
            line: 0,
        }
    }

    fn build_op(&mut self, op: OpCode) {
        self.chunk.write(op, self.line)
    }

    // fn build_expr(&mut self, expr:&TypedExpresion)
}
