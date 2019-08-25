use crate::T;

use crate::parser::Parser;

use crate::{Span, SyntaxKind::*, Token};

use crate::parser::Precedence;

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = Span<Token>>,
{
    pub(crate) fn parse_while_expr(&mut self) {
        self.start_node(WHILE_EXPR);

        self.expect(T![while], "Expected `while`");

        self.parse_expression(Precedence::Assignment);

        self.parse_block();

        self.finish_node()
    }
}
