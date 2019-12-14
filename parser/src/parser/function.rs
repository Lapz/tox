use syntax::T;

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

        self.expect(T![fn], "Expected `fn`");

        self.ident();

        if self.current() == T![<] {
            self.parse_type_params(false);
        }

        if self.current() == T!["("] {
            self.parse_func_params();
        }

        if self.current() == T![->] {
            self.parse_return_type();
        }

        self.parse_block();

        self.finish_node()
    }

    fn parse_return_type(&mut self) {
        self.start_node(RET_TYPE);
        self.expect(T![->], "Expected `->`");
        self.parse_type();
        self.finish_node()
    }
}

#[cfg(test)]
mod test {
    test_parser! {parse_function,"fn main() {}"}
}
