use crate::ast::*;
use crate::T;

use crate::parser::Parser;

use crate::{Span, SyntaxKind::*, Token};

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = Span<Token>>,
{
    pub(crate) fn parse_function(&mut self) {
        self.start_node(FN_DEF);

        if self.peek(|t| t == T![export]) {
            self.parse_visibility();
        }

        self.expect(FN_KW, "Expected `fn`");

        self.expect(IDENT, "Expected and identifier");

        if self.peek(|t| t == L_ANGLE) {
            self.parse_type_params();
        }

        if self.peek(|t| t == T!["("]) {
            self.parse_func_params();
        }

        // self.parse_block();

        self.finish_node()
    }
}

#[cfg(test)]
mod test {
    test_parser! {parse_function,"fn main() {}"}
}
