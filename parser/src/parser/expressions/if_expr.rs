use syntax::T;

use crate::parser::Parser;

use crate::{Span, SyntaxKind::*, Token};

use crate::parser::Precedence;

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = Span<Token>>,
{
    pub(crate) fn parse_if_expr(&mut self) {
        self.start_node(IF_EXPR);

        self.expect(T![if], "Expected `if`");

        self.parse_expression(Precedence::Assignment);

        self.parse_block();

        if self.current() == T![else] {
            self.parse_else()
        }

        self.finish_node()
    }

    fn parse_else(&mut self) {
        self.bump(); // eat the `else`
        if self.current() == T![if] {
            self.parse_if_expr()
        } else {
            self.parse_block();
        }
    }
}
