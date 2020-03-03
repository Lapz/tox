use syntax::T;

use crate::parser::{Parser, Restrictions};

use crate::SyntaxKind::*;

use crate::parser::Precedence;

impl<'a> Parser<'a> {
    pub(crate) fn parse_while_expr(&mut self) {
        self.start_node(WHILE_EXPR);

        self.expect(T![while], "Expected `while`");

        self.start_node(CONDITION);
        self.parse_expression(Precedence::Assignment, Restrictions::no_records());
        self.finish_node();

        self.parse_block();

        self.finish_node()
    }
}

#[cfg(test)]
mod test {
    test_parser! {parse_empty_while_expr,"fn main(){while true {}}"}
    test_parser! {parse_while_expr,"fn main(){while true {print(\"it works\")}}"}
}
