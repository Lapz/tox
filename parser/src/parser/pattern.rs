use crate::ast::*;
use crate::macros::*;
use crate::parser::Parser;
use rowan::GreenNodeBuilder;

use crate::{
    AstNode, Span,
    SyntaxKind::{self, *},
    SyntaxNode, Token,
};

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = Span<Token>>,
{
    pub(crate) fn parse_pattern(&mut self, allow_literal: bool) {
        match self.current() {
            L_PAREN => self.parse_tuple_pattern(allow_literal),
            UNDERSCORE => self.parse_placeholder_pattern(),
            IDENT => self.parse_binding_pattern(),
            _ => {
                if allow_literal {
                    // self.parse_literal()
                } else {
                    self.error("Expected a literal pattern")
                }
            }
        }
    }

    fn parse_tuple_pattern(&mut self, allow_literal: bool) {
        self.start_node(TUPLE_PAT);

        self.bump(); // Eat the `(`

        while !self.at(EOF) && !self.at(T![")"]) {
            self.parse_pattern(allow_literal);
            if !self.at(T![>]) && !self.expected(T![,]) {
                break;
            }
        }

        self.expect(T![")"], "Expected `>` to close type params");
        self.finish_node();
    }

    fn parse_placeholder_pattern(&mut self) {
        self.start_node(PLACEHOLDER_PAT);
        self.expect(UNDERSCORE, "Expected `_`");
        self.finish_node();
    }

    fn parse_binding_pattern(&mut self) {
        self.start_node(PLACEHOLDER_PAT);
        self.ident();
        self.finish_node();
    }
}

#[cfg(test)]
mod test {

    use crate::utils::dump_debug;
    use crate::{Parser, Span, Token};
    use insta::assert_debug_snapshot_matches;
    use std::io::Write;
    use std::vec::IntoIter;
    use syntax::Lexer;

    test_parser! {parse_placeholder_pattern,"fn main(_:i32) {}"}

    test_parser! {parse_tuple_pattern,"fn main((x,y):i32) {}"}

    test_parser! {parse_binding_pattern,"fn main((x:i32) {}"}

    test_parser! {parse_nested_tuple_pattern,"fn main((x,(y,_)):i32) {}"}

    fn parse(input: &str) -> Parser<IntoIter<Span<Token>>> {
        let mut lexer = Lexer::new(input);
        let parser = Parser::new(lexer.lex().into_iter(), input);

        parser
    }
}
