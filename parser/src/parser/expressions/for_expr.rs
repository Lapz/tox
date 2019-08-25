use crate::T;

use crate::parser::Parser;

use crate::{Span, SyntaxKind::*, Token};

use crate::parser::Precedence;

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = Span<Token>>,
{
    pub(crate) fn parse_for_expr(&mut self) {
        self.start_node(FOR_EXPR);

        self.expect(T![for], "Expected `for`");

        self.expect(T!["("], "Expected `(`");

        if self.at(T![;]) {
            self.bump()
        } else if self.at(T![let]) {
            self.parse_let_expr();
        } else {
            self.parse_expression(Precedence::Assignment);
            self.expect(T![;], "Expected `;`");
        }

        if self.at(T![;]) {
            self.bump()
        } else {
            self.parse_expression(Precedence::Comparison);
            self.expect(T![;], "Expected `;`");
        }

        if self.at(T![;]) {
            self.bump()
        } else {
            self.parse_expression(Precedence::Assignment);
            self.expect(T![;], "Expected `;`");
        }

        self.expect(T![")"], "Expect `)`");

        self.parse_block();

        self.finish_node()
    }
}

mod test {
    test_parser! {parse_empty_expr,"fn main() {for(;;;) {}}"}
    test_parser! {parse_empty_return_expr,"fn main() {return;}"}
}
