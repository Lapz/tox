use syntax::T;

use crate::parser::{
    pratt::{Precedence, PrefixParser},
    Parser, Restrictions,
};

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
            },


            T!["["] => {
                parser.start_node(ARRAY_EXPR);
                parser.bump(); // Eats the `[`

                if parser.at(T!["]"]) {
                    parser.bump(); // Eats the `]`
                    parser.finish_node();
                    return
                }

                  while !parser.at(EOF) && !parser.at(T!["]"]) {
                    parser.parse_expression(Precedence::Assignment, Restrictions::default());
                    if !parser.at(T!["]"]) && !parser.expected(T![,]) {
                        break;
                    }
                }

                 parser.expect(T!["]"]);


                parser.finish_node()


            },

            _ => parser.error("Expected `{{int}}` or `{{nil}}` or `{{true|false}}` or `{{ident}}` or `{{string}}`",format!("Expected `{{int}}` or `{{nil}}` or `{{true|false}}` or `{{ident}}` or `{{string}}` found `{}`",parser.current_string()))
        }
    }
}

#[cfg(test)]
mod tests {
    test_parser! {parse_int_literal,"fn main() {1;}"}
    test_parser! {parse_float_literal,"fn main() {1.0;}"}
    test_parser! {parse_string_literal,"fn main() {\"abc\";}"}
    test_parser! {parse_nil_literal,"fn main() {nil};"}
    test_parser! {parse_bool_literal,"fn main() {true;false;}"}
}
