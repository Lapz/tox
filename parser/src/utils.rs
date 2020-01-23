use crate::AstNode;
#[cfg(test)]
use crate::{Parser, Span, Token};
#[cfg(test)]
use std::vec::IntoIter;
#[cfg(test)]
use syntax::Lexer;

pub fn dump_debug<T: AstNode>(item: &T) -> String {
    format!("{:#?}", item.syntax())
}

#[cfg(test)]
pub fn parse(input: &str) -> Parser<IntoIter<Span<Token>>> {
    let mut files = errors::Files::new();
    let file_id = files.add("testing", input);
    let reporter = errors::Reporter::new(files, file_id);
    let mut lexer = Lexer::new(input, reporter.clone());
    Parser::new(lexer.lex().into_iter(), reporter, input)
}
