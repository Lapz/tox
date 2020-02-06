use syntax::T;

use crate::parser::pratt::{InfixParser, Precedence};
use crate::parser::{Parser, Restrictions};

use crate::{Span, SyntaxKind::*, Token};

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = Span<Token>>,
{
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

impl<I: Iterator<Item = Span<Token>>> InfixParser<I> for BinaryParselet {
    fn pred(&self) -> Precedence {
        self.0
    }

    fn parse(&self, parser: &mut Parser<I>, checkpoint: rowan::Checkpoint)
    where
        I: Iterator<Item = Span<Token>>,
    {
        parser.start_node_at(checkpoint, BIN_EXPR);

        parser.parse_op();

        parser.parse_expression(self.0.higher(), Restrictions::default());

        parser.finish_node();
    }
}

#[cfg(test)]
mod test {
    test_parser! {parse_bin_expr,"fn main() {1+1;}"}
    test_parser! {parse_wrapped_bin_expr_literal,"fn main() {1.0+2.0+2.0;}"}
    test_parser! {parse_assign_bin_expr_,"fn main() {x=10;x+=10;x-=10;x/=10;x*=10;}"}
    test_parser! {parse_chained_assign_bin_expr_,"fn main() {x=10=10=2=3;}"}
}
