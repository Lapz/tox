use crate::parser::{
    pratt::{Precedence, Rule, RuleToken},
    Parser, Restrictions,
};
use crate::{Span, Token};

use syntax::T;

mod binary;
mod block;
mod break_expr;
mod call_expr;
mod closure_expr;
mod continue_expr;
mod do_expr;
mod field_expr;
mod for_expr;
mod grouping;
mod ident;
mod if_expr;
mod index_expr;
mod let_expr;
mod literal;
mod match_expr;
mod record_expr;
mod return_expr;
mod unary;
mod while_expr;

pub use binary::BinaryParselet;
pub use call_expr::CallParselet;
pub use closure_expr::ClosureParselet;
pub use field_expr::FieldParselet;
pub use grouping::GroupingParselet;
pub use ident::IdentParselet;
pub use index_expr::IndexParselet;
pub use literal::LiteralParselet;
pub use record_expr::RecordParselet;
pub use unary::UnaryParselet;

impl<'a> Parser<'a> {
    pub(crate) fn parse_expression(&mut self, precedence: Precedence, restrictions: Restrictions) {
        let check_point = self.builder.checkpoint();

        let token = self.current();

        let mut rule = token.rule();

        let parser = self.prefix.get(&rule);

        let parser = if let Some(parser) = parser {
            parser
        } else {
            self.error(
                "Expected an expression",
                format!(
                    "Expected an expression but instead found `{}`",
                    self.current_string()
                ),
            );
            return;
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

            if restrictions.forbid_record && rule == RuleToken::LBrace {
                break;
            }
            parser.parse(self, check_point);
        }

        if precedence == Precedence::None {
            self.recover_until(T![;]);
        }
    }
}
