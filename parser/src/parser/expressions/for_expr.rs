use syntax::T;

use crate::parser::{Parser, Precedence, Restrictions};
use crate::SyntaxKind::*;

impl<'a> Parser<'a> {
    pub(crate) fn parse_for_expr(&mut self) {
        self.start_node(FOR_EXPR);

        self.expect(T![for], "Expected `for`");

        self.expect(T!["("], "Expected `(`");

        if self.at(T![;]) {
            self.bump()
        } else if self.at(T![let]) {
            self.parse_let_expr();
            self.expect(T![;], "Expected `;`");
        } else {
            self.parse_expression(Precedence::Assignment, Restrictions::default());
            self.expect(T![;], "Expected `;`");
        }

        if self.at(T![;]) {
            self.bump()
        } else {
            self.parse_expression(Precedence::Comparison, Restrictions::default());
            self.expect(T![;], "Expected `;`");
        }

        if self.at(T![;]) {
            self.bump();
        } else if self.at(T![")"]) {
        } else {
            self.parse_expression(Precedence::Assignment, Restrictions::default());
        }

        self.expect(T![")"], "Expect `)`");

        self.parse_block();

        self.finish_node()
    }
}

mod test {
    test_parser! {parse_empty_for_expr,"fn main() {for(;;;) {}}"}
    test_parser! {parse_for_expr,"
                    fn main() {
                        for(let x = 10; x< 10; x=x+1) {
                            println(\"it works \")
                        };}"
    }
    test_parser! {parse_for_no_incr,"fn main() { for (let x=10;x<10;;){};}"}
    test_parser! {parse_for_no_cond,"fn main() { for (let x=10;;x+=10){};}"}
    test_parser! {parse_for_no_init,"fn main() { for (;x<10;x+=10){};}"}
}
