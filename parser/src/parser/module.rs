use syntax::T;

use crate::parser::Parser;

use crate::SyntaxKind::*;

impl<'a> Parser<'a> {
    pub(crate) fn parse_module(&mut self) {
        self.start_node(MOD_DEF);

        self.expect(T![mod]);

        self.ident();

        self.expect(T![;]);

        self.finish_node();
    }
}

#[cfg(test)]
mod test {
    test_parser! {parse_single_mod,"mod foo;"}
    test_parser! {parse_many_mods,"mod bar; mod foo;"}
}
