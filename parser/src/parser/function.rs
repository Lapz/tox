use syntax::T;

use crate::parser::Parser;

use crate::SyntaxKind::*;

impl<'a> Parser<'a> {
    pub(crate) fn parse_function(&mut self, checkpoint: rowan::Checkpoint) {
        self.start_node_at(checkpoint, FN_DEF);

        self.expect(T![fn]);

        self.ident();

        if self.current() == T![<] {
            self.parse_type_params(false);
        }

        if self.current() == T!["("] {
            self.parse_func_params(T!(")"));
        }

        if self.current() == T![->] {
            self.parse_return_type();
        }

        self.parse_block();

        self.finish_node()
    }

    pub(crate) fn parse_return_type(&mut self) {
        self.start_node(RET_TYPE);
        self.expect(T![->]);
        self.parse_type();
        self.finish_node()
    }
}

#[cfg(test)]
mod tests {
    test_parser! {parse_function,"fn main() {}"}
    test_parser! {parse_exported_function,"export fn main() {}"}
}
