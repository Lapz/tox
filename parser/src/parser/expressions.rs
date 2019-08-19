use crate::parser::{
    pratt::{Precedence, Rule},
    Parser,
    SyntaxKind::*,
};
use crate::{Span, Token};
mod block;
mod break_expr;
mod continue_expr;
mod literal;

pub use literal::LiteralParselet;

#[derive(Debug)]
pub struct UnaryParselet;

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = Span<Token>>,
{
    pub(crate) fn parse_expression(&mut self, precedence: Precedence) {
        self.start_node(EXPR_STMT);
        let mut token = self.current();
        let mut rule = token.rule();

        let parser = self.prefix.get(&rule);

        let parser = if parser.is_none() {
            self.error("Expected an expression instead found `{}`");
            return;
        } else {
            parser.unwrap()
        };

        parser.parse(self);

        // while precedence <= self.precedence() {
        //     {
        //         let token = self.peek();
        //         rule = token.rule();
        //     }

        //     let parser = self.infix.get(&rule);

        //     let parser = if parser.is_some() {
        //         parser.unwrap();
        //     } else {
        //         break;
        //     };

        //     parser.parse(self);
        // }

        self.finish_node()
    }
}
