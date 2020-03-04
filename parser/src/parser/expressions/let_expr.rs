use syntax::T;

use crate::parser::{Parser, Restrictions};

use crate::SyntaxKind::*;

use crate::parser::Precedence;

impl<'a> Parser<'a> {
    pub(crate) fn parse_let_expr(&mut self) {
        self.start_node(LET_STMT);

        self.expect(T![let]);

        self.parse_pattern(false);

        if self.at(T![:]) {
            self.bump();
            self.parse_type();
        }

        if self.at(T![;]) {
            self.finish_node();
            return;
        }

        self.expect(T![=]);

        if self.at(T!["{"]) {
            self.parse_block()
        } else {
            self.parse_expression(Precedence::Assignment, Restrictions::default());
        }

        self.finish_node()
    }
}

mod test {
    test_parser! {parse_let_expr,"fn main() {let foo = 10; let bar = \"a\"}"}
    test_parser! {parse_empty_let_expr,"fn main() {let foo;}"}
    test_parser! {parse_let_type_expr,"fn main() {let foo:i32; let bar: (i32,i32);}"}
    test_parser! {parse_let_patter_expr,"fn main() {let (a,b); let _ = 10;}"}
}
