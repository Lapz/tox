use crate::parser::Parser;
use syntax::T;

use crate::{Span, SyntaxKind::*, Token};

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = Span<Token>>,
{
    pub(crate) fn parse_func_params(&mut self) {
        self.start_node(PARAM_LIST);

        self.bump();

        while !self.at(EOF) && !self.at(T![")"]) {
            self.func_param();

            if !self.at(T![")"]) && !self.expected(T![,]) {
                break;
            }
        }

        self.expect(T![")"], "Expected `)` to close type params");

        self.finish_node()
    }

    fn func_param(&mut self) {
        self.start_node(PARAM);
        self.parse_pattern(false);

        if self.at(T![:]) {
            self.expect(T![:], "Expected `:`");
            self.parse_type();
        }

        self.finish_node();
    }
}
