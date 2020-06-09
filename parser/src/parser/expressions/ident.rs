use syntax::T;

use crate::parser::pratt::{Precedence, PrefixParser};
use crate::parser::{restrictions::Restrictions, Parser};
use crate::SyntaxKind::*;

#[derive(Debug)]
pub struct IdentParselet;

impl PrefixParser for IdentParselet {
    fn parse(&self, parser: &mut Parser) {
        let c = parser.checkpoint();
        parser.start_node(IDENT_EXPR);

        parser.start_node(NAME);

        parser.expect(IDENT);

        if parser.at(T![::]) {
            parser.finish_node(); // close name

            parser.bump();

            if parser.at(T![<]) {
                parser.parse_type_args();
                parser.finish_node(); // close ident_expr
            } else {
                parser.finish_node();

                parser.start_node(IDENT_EXPR);

                parser.start_node(NAME);
                parser.expect(IDENT);

                parser.finish_node();
                parser.finish_node();

                if !parser.at(T![;]) {
                    parser.parse_expression(Precedence::Assignment, Restrictions::default());
                }

                parser.start_node_at(c, ENUM_EXPR);
                parser.finish_node();
            }
        } else {
            parser.finish_node();
            parser.finish_node();
        }
    }
}

#[cfg(test)]
mod tests {
    test_parser! {
        parse_ident_expr,"fn main(){a;}"
    }
    test_parser! {
        parse_generic_ident_expr,"fn main(){a::<i32>;}"
    }
}
