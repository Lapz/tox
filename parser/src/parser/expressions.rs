use crate::parser::{
    pratt::{Precedence, Rule},
    Parser,
};
use crate::{Span, Token};
mod binary;
mod block;
mod break_expr;
mod continue_expr;
mod do_expr;
mod for_expr;
mod grouping;
mod if_expr;
mod let_expr;
mod literal;
mod return_expr;
mod tuple_expr;
mod unary;
mod while_expr;

pub use binary::BinaryParselet;
pub use grouping::GroupingParselet;
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
            self.error(
                "Expected an expression",
                format!(
                    "Expected an expression but instead found `{}`",
                    self.current_string()
                ),
            );
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
                self.error(
                    "Expected an expression",
                    format!(
                        "Expected an expression but instead found `{}`",
                        self.current_string()
                    ),
                );
                break;
            };

            println!("{:?}", parser);

            parser.parse(self, check_point);
        }

        if precedence == Precedence::None {
            self.recover();
        }
    }
}
