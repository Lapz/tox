use syntax::T;

use crate::parser::pratt::{Precedence, PrefixParser};
use crate::parser::{Parser, Restrictions};

use crate::SyntaxKind::*;

impl<'a> Parser<'a> {
    pub(crate) fn parse_unary_op(&mut self) {
        match self.current() {
            T![-] | T![!] => self.bump(),
            _ => self.error(
                "Expected one of `-` | `!`",
                format!(
                    "Expected one of `-` | `!` but instead found `{:?}`",
                    self.current_string()
                ),
            ),
        }
    }
}
#[derive(Debug)]
pub struct UnaryParselet;

impl PrefixParser for UnaryParselet {
    fn parse(&self, parser: &mut Parser) {
        parser.start_node(PREFIX_EXPR);

        parser.parse_unary_op();

        parser.parse_expression(Precedence::Unary, Restrictions::default());

        parser.finish_node();
    }
}

#[cfg(test)]
mod tests {
    test_parser! {parse_unary_expr,"fn main() {!true;-1}"}
    test_parser! {parse_nested_unary_expr,"fn main() {!!true;}"}
}
