use syntax::T;

use crate::parser::{Parser, Restrictions};

use crate::SyntaxKind::*;

use crate::parser::Precedence;

impl<'a> Parser<'a> {
    pub(crate) fn parse_do_expr(&mut self) {
        self.start_node(DO_EXPR);

        self.expect(T![do], "Expected `do`");

        self.parse_block();

        self.expect(T![while], "Expected `while`");

        self.parse_expression(Precedence::Assignment, Restrictions::no_records());

        self.finish_node()
    }
}

#[cfg(test)]
mod test {
    test_parser! {parse_do_expr,"fn main() { do {println(x); x=x+1;} while x<10;}"}
}
