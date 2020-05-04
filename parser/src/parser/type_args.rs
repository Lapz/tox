use crate::parser::Parser;
use crate::SyntaxKind::*;
use syntax::T;

impl<'a> Parser<'a> {
    pub(crate) fn parse_type_args(&mut self) {
        self.start_node(TYPE_ARG_LIST);

        self.bump();

        while !self.at(EOF) && !self.at(T![>]) {
            self.parse_type();

            if !self.at(T![>]) && !self.expected(T![,]) {
                break;
            }
        }

        self.expect(T![>]);

        self.finish_node();
    }
}
