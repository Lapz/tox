use syntax::T;

use crate::parser::Parser;

use crate::{Span, SyntaxKind::*, Token};

impl<'a> Parser<'a>

{
    pub(crate) fn parse_continue_expr(&mut self) {
        self.start_node(CONTINUE_EXPR);

        self.expect(T![continue], "Expected `continue`");

        self.finish_node()
    }
}

#[cfg(test)]
mod test {
    test_parser! {parse_continue_expr,"fn main() {continue;}"}
}
