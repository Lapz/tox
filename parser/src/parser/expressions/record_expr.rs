use syntax::T;

use crate::parser::pratt::{InfixParser, Precedence};
use crate::parser::{Parser, Restrictions};

use crate::{Span, SyntaxKind::*, Token};

#[derive(Debug)]
pub struct RecordParselet(pub Precedence);

impl InfixParser for RecordParselet {
    fn parse(&self, parser: &mut Parser, checkpoint: rowan::Checkpoint) {
        parser.start_node_at(checkpoint, RECORD_LITERAL_EXPR);

        parser.start_node(RECORD_LITERAL_FIELD_LIST);

        parser.expect(T!["{"], "Expected `{`");

        while !parser.at(EOF) && !parser.at(T!["}"]) {
            parser.parse_record_field();

            if !parser.at(T!["}"]) && !parser.expected(T![,]) {
                break;
            }
        }

        parser.expect(T!["}"], "Expected `}`");
        parser.finish_node();

        parser.finish_node();
    }

    fn pred(&self) -> Precedence {
        self.0
    }
}

impl<'a> Parser<'a> {
    pub(crate) fn parse_record_field(&mut self) {
        self.start_node(RECORD_LITERAL_FIELD);

        self.expect(IDENT, "");

        if self.at(T![:]) {
            self.bump();
            self.parse_expression(Precedence::Assignment, Restrictions::default());
        }

        self.finish_node()
    }
}

#[cfg(test)]
mod test {
    test_parser! {parse_empty_record_literal,"fn main() { foo{};}"}
    test_parser! {parse_shorthand_record_literal,"fn main() { foo{ shorthand,short,shortymcshort};}"}
    test_parser! {parse_longhand_record_literal,"fn main() { foo{ longhand:longhand,long:long,longymclong:longymclong};}"}
    test_parser! {parse_mixed_record_literal,"fn main() { foo{ longhand:longhand,longymclong,long:long};}"}
}
