use syntax::T;

use crate::parser::Parser;

use crate::{Span, SyntaxKind::*, Token};

use crate::parser::Precedence;

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = Span<Token>>,
{
    pub(crate) fn parse_do_expr(&mut self) {
        self.start_node(DO_EXPR);

        self.expect(T![do], "Expected `if`");

        self.parse_block();

        self.expect(T![while], "Expected `while`");

        self.parse_expression(Precedence::Assignment);

        self.finish_node()
    }
}
