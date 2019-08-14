use crate::T;

use crate::parser::Parser;

use crate::{Span, SyntaxKind::*, Token};

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = Span<Token>>,
{
    pub(crate) fn parse_block(&mut self) {
        self.start_node(BLOCK);

        self.expect(T!["{"], "Expected `{`");

        while !self.at(EOF) && !self.at(T!["}"]) {}

        self.expect(T!["}"], "Expected `}`");

        self.finish_node()
    }
}

#[cfg(test)]
mod test {}
