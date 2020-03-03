use syntax::T;

use crate::parser::pratt::{InfixParser, Precedence};
use crate::parser::{Parser, Restrictions};

use crate::{Span, SyntaxKind::*, Token};

#[derive(Debug)]
pub struct CallParselet(pub Precedence);

impl InfixParser for CallParselet {
    fn parse(&self, parser: &mut Parser, checkpoint: rowan::Checkpoint) {
        parser.start_node_at(checkpoint, CALL_EXPR);
        parser.start_node(ARG_LIST);

        parser.expect(T!["("], "Expected `(`");

        while !parser.at(EOF) && !parser.at(T![")"]) {
            parser.parse_expression(Precedence::Assignment, Restrictions::default());

            if !parser.at(T![")"]) && !parser.expected(T![,]) {
                break;
            }
        }

        parser.expect(T![")"], "Expected `)`");

        parser.finish_node();
        parser.finish_node();
    }

    fn pred(&self) -> Precedence {
        self.0
    }
}

#[cfg(test)]
mod test {
    test_parser! {parse_simple_call_expr,"fn main() {a(1,2);}"}
    test_parser! {parse_call_no_args_expr,"fn main() {a();}"}
    test_parser! {parse_call_generic_params_expr,"fn main() {a::<i32>(a,b);}"}
    test_parser! {parse_call_trailing_comma,"fn main() {a(a,b,);}"}
    test_parser! {parse_call_ife,"fn main() {a(a,b,)();}"}
    test_parser! {parse_call_ife_with_args,"fn main() {a(a,b,)(1,2,3);}"}
}
