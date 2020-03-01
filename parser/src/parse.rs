use crate::db::ParseDatabase;
use crate::Parser;
use errors::{FileId, Reporter};
use syntax::{ast::SourceFile, Lexer};

pub fn parse_query(db: &impl ParseDatabase, file: FileId) -> SourceFile {
    let reporter = Reporter::new(file);
    let source = db.source(file);
    let mut lexer = Lexer::new(source, reporter.clone());
    let mut parser = Parser::new(lexer.lex().into_iter(), reporter.clone(), source);
    let program = parser.parse_program();
    program
}
