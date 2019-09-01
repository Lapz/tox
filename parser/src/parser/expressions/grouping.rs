use crate::T;

use crate::parser::pratt::{Precedence, PrefixParser};
use crate::parser::Parser;

use crate::{Span, SyntaxKind::*, Token};

#[derive(Debug)]
pub struct GroupingParselet;

impl<I: Iterator<Item = Span<Token>>> PrefixParser<I> for GroupingParselet {
    fn parse(&self, parser: &mut Parser<I>)
    where
        I: Iterator<Item = Span<Token>>,
    {
        parser.start_node(PAREN_EXPR);

        let checkpoint = parser.checkpoint();

        parser.bump(); // Eats the `(`

        parser.parse_expression(Precedence::Assignment);

        if parser.at(T![,]) {
            parser.bump();
            parser.start_node_at(checkpoint, TUPLE_EXPR);
            while !parser.at(EOF) && !parser.at(T![")"]) {
                parser.parse_expression(Precedence::Assignment);
                if !parser.at(T![")"]) && !parser.expected(T![,]) {
                    break;
                }
            }
        }

        parser.expect(T![")"], "Expected `)`");

        parser.finish_node();
    }
}
