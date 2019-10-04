use crate::T;

use crate::parser::pratt::{InfixParser, Precedence};
use crate::parser::Parser;

use crate::{Span, SyntaxKind::*, Token};

#[derive(Debug)]
pub struct CallParselet(pub Precedence);

impl<I: Iterator<Item = Span<Token>>> InfixParser<I> for CallParselet {
    fn parse(&self, parser: &mut Parser<I>, checkpoint: rowan::Checkpoint) {
        // parser.parse_expression(Precedence::Assignment);
        // // foo::<>();
        // if parser.at(COLON_COLON) {
        //     parser.bump(); // Eats the ::
        //     parser.parse_type_params(true); // Eats the <i32,i32>
        //
        // }

        parser.start_node(ARG_LIST);

        parser.expect(T!["("], "Expected `(`");

        while !parser.at(EOF) && !parser.at(T![")"]) {
            parser.parse_expression(Precedence::Assignment);

            if !parser.at(T![")"]) && !parser.expected(T![,]) {
                break;
            }
        }

        parser.expect(T![")"], "Expected `)`");

        parser.finish_node();
    }

    fn pred(&self) -> Precedence {
        self.0
    }
}
