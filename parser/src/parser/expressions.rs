use crate::parser::{
    pratt::{Precedence, Rule},
    Parser,
    SyntaxKind::*,
};
use crate::{Span, Token};
mod binary;
mod block;
mod break_expr;
mod continue_expr;
mod do_expr;
mod if_expr;
mod literal;
mod unary;

pub use binary::BinaryParselet;
pub use literal::LiteralParselet;
pub use unary::UnaryParselet;

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = Span<Token>>,
{
    pub(crate) fn parse_expression(&mut self, precedence: Precedence) {
        let check_point = self.builder.checkpoint();

        let token = self.current();
        let mut rule = token.rule();

        let parser = self.prefix.get(&rule);

        let parser = if parser.is_none() {
            self.error("Expected an expression instead found `{}`");
            return;
        } else {
            parser.unwrap()
        };

        parser.parse(self);

        while precedence <= self.precedence() {
            {
                let token = self.current();
                rule = token.rule();
            }

            let parser = self.infix.get(&rule);

            let parser = if parser.is_some() {
                parser.unwrap()
            } else {
                break;
            };

            parser.parse(self, check_point);
        }
    }
}
