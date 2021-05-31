use syntax::T;

use crate::parser::pratt::{InfixParser, Precedence};
use crate::parser::{Parser, Restrictions};

use crate::SyntaxKind::*;

impl<'a> Parser<'a> {
    pub(crate) fn parse_op(&mut self) {
        match self.current() {
            T![-]
            | T![+]
            | T![*]
            | T![/]
            | T![&&]
            | T![||]
            |T![<]
            |T![>]
            // | T![^]
            // | T![%]
            | T![=]
            | T![==]
            | T![!]
            | T![!=]
            | T![<=]
            | T![>=]
            | T![+=]
            | T![-=]
            | T![*=]
            | T![/=] => self.bump(),
            
            _ => self.error("Expected an operator",format!("Expected one of `-` | `+` |`*`| `/` | `&&` | `||` | `<` | `>` | `==` | `!` | `!=` | `>=` | `<=` | `+=` | `-=` | `*=` | `/=`  instead found `{}`",self.current_string())),
        }
    }
}
#[derive(Debug)]
pub struct BinaryParselet(pub Precedence);

impl InfixParser for BinaryParselet {
    fn pred(&self) -> Precedence {
        self.0
    }

    fn parse(&self, parser: &mut Parser, checkpoint: rowan::Checkpoint) {
        parser.start_node_at(checkpoint, BIN_EXPR);

        parser.parse_op();

        let restriction = parser.restriction.unwrap_or(Restrictions::default());
        parser.parse_expression(self.0.higher(), restriction);

        parser.finish_node();
    }
}

#[cfg(test)]
mod tests {
    test_parser! {parse_bin_expr,"fn main() {1+1;}"}
    test_parser! {parse_wrapped_bin_expr_literal,"fn main() {1.0+2.0+2.0;}"}
    test_parser! {parse_assign_bin_expr_,"fn main() {x=10;x+=10;x-=10;x/=10;x*=10;}"}
    test_parser! {parse_chained_assign_bin_expr_,"fn main() {x=10=10=2=3;}"}
}
