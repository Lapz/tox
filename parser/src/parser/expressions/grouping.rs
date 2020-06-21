use syntax::T;

use crate::parser::pratt::{Precedence, PrefixParser};
use crate::parser::{Parser, Restrictions};

use crate::SyntaxKind::*;

#[derive(Debug)]
pub struct GroupingParselet;

impl PrefixParser for GroupingParselet {
    fn parse(&self, parser: &mut Parser) {
        let checkpoint = parser.checkpoint();

        let mut seen_comma = false;

        parser.bump(); // Eats the `(`

        parser.parse_expression(Precedence::Assignment, Restrictions::default());

        if parser.at(T![,]) {
            seen_comma = true;
            parser.bump();

            while !parser.at(EOF) && !parser.at(T![")"]) {
                parser.parse_expression(Precedence::Assignment, Restrictions::default());
                if !parser.at(T![")"]) && !parser.expected(T![,]) {
                    break;
                }
            }
        }

        parser.expect(T![")"]);

        if seen_comma {
            parser.start_node_at(checkpoint, TUPLE_EXPR);
        } else {
            parser.start_node_at(checkpoint, PAREN_EXPR)
        }

        parser.finish_node();
    }
}

#[cfg(test)]
mod tests {
    test_parser! {parse_grouping_expr,"fn main() {(1+10-1);}"}
    test_parser! {parse_tuple_expr,"fn main() {(1,2,3,4);}"}
}
