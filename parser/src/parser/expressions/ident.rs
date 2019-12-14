use syntax::T;

use crate::parser::pratt::{Precedence, PrefixParser};
use crate::parser::Parser;
use crate::{Span, SyntaxKind::*, Token};

#[derive(Debug)]
pub struct IdentParselet;

impl<I: Iterator<Item = Span<Token>>> PrefixParser<I> for IdentParselet {
    fn parse(&self, parser: &mut Parser<I>)
    where
        I: Iterator<Item = Span<Token>>,
    {
        parser.start_node(NAME);

        parser.expect(IDENT, "Expected an identifer");

        if parser.at(T![::]) {
            parser.bump();
            parser.parse_type_params(true);
        };

        parser.finish_node();
    }
}
