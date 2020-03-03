use crate::AstNode;
#[cfg(test)]
use crate::Parser;

#[cfg(test)]
use syntax::{ast::SourceFile, Lexer};

pub fn dump_debug<T: AstNode>(item: &T) -> String {
    format!("{:#?}", item.syntax())
}

#[cfg(test)]
pub fn parse<'a>(input: &'a str) -> SourceFile {
    let mut files = errors::Files::new();
    let file_id = files.add("testing", input);
    let reporter = errors::Reporter::new(file_id);
    let tokens = Lexer::new(input, reporter.clone()).lex();

    Parser::new(&tokens, reporter, input).parse_program()
}
