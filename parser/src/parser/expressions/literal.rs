use syntax::T;

use crate::parser::pratt::PrefixParser;
use crate::parser::Parser;

use crate::SyntaxKind::*;

#[derive(Debug)]
pub struct LiteralParselet;

impl PrefixParser for LiteralParselet {
    fn parse(&self, parser: &mut Parser) {
        match parser.current() {
            INT_NUMBER  | FLOAT_NUMBER | STRING | T![nil] |T![true]|T![false] => {
                parser.start_node(LITERAL);
                parser.bump();
                parser.finish_node();
            }

            _ => parser.error("Expected `{{int}}` or `{{nil}}` or `{{true|false}}` or `{{ident}}` or `{{string}}`",format!("Expected `{{int}}` or `{{nil}}` or `{{true|false}}` or `{{ident}}` or `{{string}}` found `{}`",parser.current_string()))
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
