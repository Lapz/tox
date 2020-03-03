use syntax::T;

use crate::parser::Parser;

use crate::{Span, SyntaxKind::*, Token};

impl<'a> Parser<'a> {
    pub(crate) fn parse_type_alias(&mut self, checkpoint: rowan::Checkpoint) {
        self.start_node_at(checkpoint, TYPE_ALIAS_DEF);

        self.expect(T![type], "Expected `type`");

        self.ident();

        if self.at(L_ANGLE) {
            self.parse_type_params(false);
        }

        self.expect(EQ, "Expected `=`");

        self.parse_type();

        self.expect(SEMI, "Expected `;`");

        self.finish_node()
    }
}

#[cfg(test)]
mod test {
    test_parser! {parse_type_alias,"type Foo = i32;"}
    test_parser! {parse_type_alias_params,"type ParseResult<T> = Result<T,void>;"}
}
