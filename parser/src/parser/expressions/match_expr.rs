use syntax::T;

use crate::parser::{Parser, Restrictions};

use crate::SyntaxKind::*;

use crate::parser::Precedence;

impl<'a> Parser<'a> {
    pub(crate) fn parse_match_expr(&mut self) {
        self.start_node(MATCH_EXPR);

        self.expect(T![match]);

        self.parse_expression(Precedence::Call, Restrictions::no_records()); // TODO Forbid struct literal

        self.expect(T!["{"]);

        self.parse_match_arms();

        self.expect(T!["}"]);

        self.finish_node()
    }

    fn parse_match_arms(&mut self) {
        self.start_node(MATCH_ARM_LIST);

        while !self.at(EOF) && !self.at(T!["}"]) {
            self.parse_match_arm();
            if !self.at(T!["}"]) && !self.expected(T![,]) {
                break;
            }
        }

        self.finish_node();
    }

    fn parse_match_arm(&mut self) {
        self.start_node(MATCH_ARM);
        self.parse_pat_list();

        self.expected(T![=>]);

        if self.at(T!["{"]) {
            self.parse_block()
        } else {
            self.parse_expression(Precedence::Call, Restrictions::default())
        }
        self.finish_node();
    }

    fn parse_pat_list(&mut self) {
        if self.at(T![|]) {
            self.bump();
        }

        self.parse_pattern(true);

        while self.at(T![|]) {
            self.bump();
            self.parse_pattern(true)
        }
    }
}

mod tests {
    test_parser! {parse_empty_match_expr,"fn main() { match a {};}"}
    test_parser! {parse_match_expr,"fn main() { match (a,b) { x => 10,1 =>3};}"}
}
