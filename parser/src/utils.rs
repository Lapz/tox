use crate::AstNode;
use crate::{Parser, Span, Token};



use std::vec::IntoIter;
use syntax::Lexer;
pub fn dump_debug<T: AstNode>(item: &T) -> String {
    format!("{:#?}", item.syntax())
}

pub fn parse(input: &str) -> Parser<IntoIter<Span<Token>>> {
    let mut lexer = Lexer::new(input);
    let parser = Parser::new(lexer.lex().into_iter(), input);

    parser
}
