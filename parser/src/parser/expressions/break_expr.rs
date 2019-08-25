use crate::T;

use crate::parser::Parser;

use crate::{Span, SyntaxKind::*, Token};

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = Span<Token>>,
{
    pub(crate) fn parse_break_expr(&mut self) {
        self.start_node(BREAK_EXPR);

        self.expect(T![break], "Expected `break`");

        self.finish_node()
    }
}

#[cfg(test)]
mod test {
    test_parser! {parse_break_expr,"fn main() {continue;}"}
}
