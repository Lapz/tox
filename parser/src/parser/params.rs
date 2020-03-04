use crate::parser::Parser;
use syntax::T;

use crate::SyntaxKind::*;

impl<'a> Parser<'a> {
    pub(crate) fn parse_func_params(&mut self) {
        self.start_node(PARAM_LIST);

        self.bump();

        while !self.at(EOF) && !self.at(T![")"]) {
            self.func_param();

            if !self.at(T![")"]) && !self.expected(T![,]) {
                break;
            }
        }

        self.expect(T![")"]);

        self.finish_node()
    }

    pub(crate) fn func_param(&mut self) {
        self.start_node(PARAM);
        self.parse_pattern(false);

        if self.at(T![:]) {
            self.expect(T![:]);
            self.parse_type();
        }

        self.finish_node();
    }
}
