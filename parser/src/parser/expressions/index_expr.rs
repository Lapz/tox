use syntax::T;

use crate::parser::pratt::{InfixParser, Precedence};
use crate::parser::{Parser, Restrictions};

use crate::SyntaxKind::*;

#[derive(Debug)]
pub struct IndexParselet(pub Precedence);

impl InfixParser for IndexParselet {
    fn parse(&self, parser: &mut Parser, checkpoint: rowan::Checkpoint) {
        parser.start_node_at(checkpoint, INDEX_EXPR);

        parser.expect(T!["["]);

        parser.parse_expression(Precedence::Assignment, Restrictions::default());

        parser.expect(T!["]"]);

        parser.finish_node();
    }

    fn pred(&self) -> Precedence {
        self.0
    }
}

#[cfg(test)]
mod test {
    test_parser! {parse_simple_index_expr,"fn main() {a[1+2];}"}
    test_parser! {parse_nested_index_expr,"fn main() {a[0][1];}"}
}
