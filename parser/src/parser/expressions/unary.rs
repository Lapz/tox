use syntax::T;

use crate::parser::pratt::{Precedence, PrefixParser};
use crate::parser::Parser;

use crate::{Span, SyntaxKind::*, Token};

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = Span<Token>>,
{
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

impl<I: Iterator<Item = Span<Token>>> PrefixParser<I> for UnaryParselet {
    fn parse(&self, parser: &mut Parser<I>)
    where
        I: Iterator<Item = Span<Token>>,
    {
        parser.start_node(PREFIX_EXPR);

        parser.parse_unary_op();

        parser.parse_expression(Precedence::Unary);

        parser.finish_node();
    }
}

#[cfg(test)]
mod test {
    test_parser! {parse_unary_expr,"fn main() {!true;-1}"}
    test_parser! {parse_nested_unary_expr,"fn main() {!!true;}"}
}
