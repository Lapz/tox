use crate::parser::Parser;
use syntax::T;

use crate::SyntaxKind::{self, *};

impl<'a> Parser<'a> {
    pub(crate) fn parse_func_params(&mut self, flavour: SyntaxKind) {
        self.start_node(PARAM_LIST);

        // We assume that we have parsed the opening token

        self.bump();

        while !self.at(EOF) && !self.at(flavour) {
            self.func_param();

            if !self.at(flavour) && !self.expected(T![,]) {
                break;
            }
        }

        self.expect(flavour);

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
