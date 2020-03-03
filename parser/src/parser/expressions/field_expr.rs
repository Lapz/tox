use syntax::T;

use crate::parser::pratt::{InfixParser, Precedence};
use crate::parser::{Parser, Restrictions};

use crate::{Span, SyntaxKind::*, Token};

#[derive(Debug)]
pub struct FieldParselet(pub Precedence);

impl InfixParser for FieldParselet {
    fn parse(&self, parser: &mut Parser, checkpoint: rowan::Checkpoint) {
        parser.start_node_at(checkpoint, FIELD_EXPR);

        parser.expect(T![.], "");

        parser.parse_expression(Precedence::Assignment, Restrictions::default());

        parser.finish_node();
    }

    fn pred(&self) -> Precedence {
        self.0
    }
}

#[cfg(test)]
mod test {
    test_parser! {parse_field_access,"fn main(){a.b;}"}
    test_parser! {parse_field_access_chain,"fn main(){a.b.c.d.e.f;}"}
    test_parser! {parse_field_access_method,"fn main(){a.b();}"}
    test_parser! {parse_field_access_method_chain,"fn main(){a.b.c.d.e();}"}
}
