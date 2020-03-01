use syntax::T;

use crate::parser::{Parser, Precedence, Restrictions};

use crate::{Span, SyntaxKind::*, Token};

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = Span<Token>>,
{
    pub(crate) fn parse_if_expr(&mut self) {
        self.start_node(IF_EXPR);

        self.expect(T![if], "Expected `if`");

        self.start_node(CONDITION);
        self.parse_expression(Precedence::Assignment, Restrictions::no_records());
        self.finish_node();
        self.parse_block();

        if self.current() == T![else] {
            self.parse_else()
        }

        self.finish_node()
    }

    fn parse_else(&mut self) {
        self.bump(); // eat the `else`
        self.skip_whitespace();
        if self.current() == T![if] {
            self.parse_if_expr()
        } else {
            self.parse_block();
        }
    }
}

#[cfg(test)]
mod test {
    test_parser! {parse_empty_if,"fn main() { if true {}}"}
    test_parser! {parse_if_and_else,"fn main() { if true {} else {}}"}
    test_parser! {parse_chained_if,"fn main() { if true {} else if false {} else if true {} else if false {} }"}
    test_parser! {parse_chained_if_and_else,"fn main() { if true {} else if false {} else if true {} else {} }"}
}
