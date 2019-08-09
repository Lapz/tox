use crate::AstNode;
use crate::{Parser, Span, Token};
use std::fs::File;
use std::io::Read;
use std::path::Path;
use std::vec::IntoIter;
use syntax::Lexer;
pub fn dump_debug<T: AstNode>(item: &T) -> String {
    format!("{:#?}", item.syntax())
}

pub fn test_data(path: &str) -> String {
    let path = Path::new(path);
    let mut buf = String::new();
    let contents = File::open(path).unwrap().read_to_string(&mut buf);

    buf
}

pub fn parse(input: &str) -> Parser<IntoIter<Span<Token>>> {
    let mut lexer = Lexer::new(input);
    let parser = Parser::new(lexer.lex().into_iter(), input);

    parser
}
