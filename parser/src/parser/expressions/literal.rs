use crate::T;

use crate::parser::pratt::{Precedence, PrefixParser};
use crate::parser::Parser;

use crate::{Span, SyntaxKind::*, Token};

#[derive(Debug)]
pub struct LiteralParselet;

impl<I: Iterator<Item = Span<Token>>> PrefixParser<I> for LiteralParselet {
    fn parse(&self, parser: &mut Parser<I>)
    where
        I: Iterator<Item = Span<Token>>,
    {
        match parser.current() {
            INT_NUMBER  | FLOAT_NUMBER | STRING | NIL_KW |TRUE_KW |FALSE_KW => {
                parser.start_node(LITERAL);
                parser.bump();
                parser.finish_node();
            }

            _ => parser.error("Expected `{{int}}` or `{{nil}}` or `{{true|false}}` or `{{ident}}` or `{{string}}` found `{}`")
        }
    }
}

#[cfg(test)]
mod test {
    test_parser! {parse_int_literal,"fn main() {1;}"}
    test_parser! {parse_float_literal,"fn main() {1.0;}"}
    test_parser! {parse_string_literal,"fn main() {\"abc\";}"}
    test_parser! {parse_nil_literal,"fn main() {nil};"}
    test_parser! {parse_bool_literal,"fn main() {true;false;}"}
}
