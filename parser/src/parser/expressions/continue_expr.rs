use syntax::T;

use crate::parser::Parser;

use crate::SyntaxKind::*;

impl<'a> Parser<'a> {
    pub(crate) fn parse_continue_expr(&mut self) {
        self.start_node(CONTINUE_EXPR);

        self.expect(T![continue]);

        self.finish_node()
    }
}

#[cfg(test)]
mod tests {
    test_parser! {parse_continue_expr,"fn main() {continue;}"}
}
