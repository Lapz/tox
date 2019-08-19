use crate::T;

use crate::parser::{pratt::Precedence, Parser};

use crate::{Span, SyntaxKind::*, Token};

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = Span<Token>>,
{
    pub(crate) fn parse_block(&mut self) {
        self.start_node(BLOCK);

        self.expect(T!["{"], "Expected `{`");

        while !self.at(EOF) && !self.at(T!["}"]) {
            match self.current() {
                LET_KW => unimplemented!(),
                _ => {
                    self.start_node(EXPR_STMT);
                    self.parse_expression(Precedence::Assignment);
                    self.finish_node();
                }
            }

            self.expect(SEMI, "Expected a `;`");
        }

        self.expect(T!["}"], "Expected `}`");

        self.finish_node()
    }
}

#[cfg(test)]
mod test {}
