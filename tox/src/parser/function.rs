// use crate::macros::T;
use crate::parser::Parser;
use crate::pos::Span;
use crate::token::{
    SyntaxKind::{self, *},
    Token,
};

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = Span<Token>>,
{
    pub(crate) fn parse_program(&mut self) -> {
        let checkpoint = self.builder.checkpoint();

        builder.start
    }
    pub(crate) fn parse_function(&mut self) {
        self.start_node(NODE_FUNCTION);

        if self.peek(|t| t == T![extern]) {
            self.bump();
        }

        self.expect(FUNCTION, "Expected `fn`");

        self.expect(IDENTIFIER, "Expected and identifier");

        // if self.peek(|t| t == LESS) {
        //     self.parse_type_params();
        // }

        // self.parse_params();

        // self.parse_block();

        self.finish_node()
    }
}
