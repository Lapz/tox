use crate::T;

use crate::parser::Parser;

use crate::{Span, SyntaxKind::*, Token};

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = Span<Token>>,
{
    pub(crate) fn parse_break(&mut self) {
        self.start_node(CONTINUE_EXPR);

        self.expect(T![continue], "Expected `continue`");

        self.finish_node()
    }
}

#[cfg(test)]
mod test {}
