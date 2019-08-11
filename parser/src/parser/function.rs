use crate::ast::*;
use crate::T;

use crate::parser::Parser;

use crate::{Span, SyntaxKind::*, Token};

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = Span<Token>>,
{
    pub(crate) fn parse_function(&mut self, has_visibility: bool) {
        self.start_node(FN_DEF);

        if has_visibility {
            self.parse_visibility();
        }

        self.expect(FN_KW, "Expected `fn`");

        self.expect(IDENT, "Expected and identifier");

        if self.is_ahead(|t| t == L_ANGLE) {
            self.parse_type_params(false);
        }

        if self.is_ahead(|t| t == T!["("]) {
            self.parse_func_params();
        }

        self.finish_node()
    }
}

#[cfg(test)]
mod test {
    test_parser! {parse_function,"fn main() {}"}
}
