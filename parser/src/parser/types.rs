use crate::ast::*;
use crate::macros::*;
use crate::parser::Parser;

use crate::{Span, SyntaxKind::*, Token};

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = Span<Token>>,
{
    pub(crate) fn parse_type(&mut self) {
        match self.current() {
            IDENT => self.parse_ident_type(),
            L_BRACK => self.parse_array_type(),
            L_PAREN => self.parse_paren_type(),
            FN_KW => self.parse_fn_type(),
            _ => self.error("Expected a type parameter"),
        };
    }

    fn parse_ident_type(&mut self) {
        self.start_node(IDENT_TYPE);

        self.expect(IDENT, "Expected an identifier/type");

        self.finish_node();
    }

    fn parse_array_type(&mut self) {
        self.start_node(ARRAY_TYPE);
        self.bump(); //Eat `[`

        self.parse_type();

        self.expect(T!["]"], "Expected `[`");

        self.finish_node();
    }

    fn parse_paren_type(&mut self) {
        self.start_node(PAREN_TYPE);
        self.bump(); //Eat `(`

        while !self.at(EOF) && !self.at(T![")"]) {
            self.parse_type();
            if !self.at(T![")"]) && !self.expected(T![,]) {
                break;
            }
        }

        self.expect(T![")"], "Expected `)`");

        self.finish_node();
    }

    fn parse_fn_type(&mut self) {
        self.start_node(FN_TYPE);
        self.bump(); //Eat `fn`

        self.expect(T!["("], "Expected `(`");

        while !self.at(EOF) && !self.at(T![")"]) {
            self.parse_type();
            if !self.at(T![")"]) && !self.expected(T![,]) {
                break;
            }
        }

        self.expect(T![")"], "Expected `)`");

        if self.peek(|t| t == T![->]) {
            self.bump();

            self.parse_type();
        }

        self.finish_node();
    }
}
