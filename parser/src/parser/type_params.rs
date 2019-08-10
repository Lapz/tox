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
    pub(crate) fn parse_type_params(&mut self) {
        self.start_node(TYPE_PARAM_LIST);
        self.bump();

        while !self.at(EOF) && !self.at(T![>]) {
            match self.current() {
                IDENT => self.type_param(),

                ident => {
                    println!("{:?}", ident);
                    self.error("Expected a type param")
                }
            }

            if !self.at(T![>]) && !self.expected(T![,]) {
                break;
            }
        }

        println!("current {:?}", self.lookahead);

        self.expect(T![>], "Expected `>` to close type params");
        self.finish_node()
    }

    fn type_param(&mut self) {
        self.start_node(TYPE_PARAM);
        self.ident();
        self.finish_node();
    }
}
