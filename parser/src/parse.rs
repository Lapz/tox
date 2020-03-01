use crate::db::ParseDatabase;
use crate::Parser;
use errors::{pos::Span, FileId, Reporter, WithError};
use syntax::{ast::SourceFile, Lexer, Token};

pub fn lex_query(db: &impl ParseDatabase, file: FileId) -> WithError<Vec<Span<Token>>> {
    let reporter = Reporter::new(file);
    let source = db.source(file);
    let mut lexer = Lexer::new(source, reporter.clone());
    let tokens = lexer.lex();

    if reporter.has_errors() {
        Err(reporter.finish())
    } else {
        Ok(tokens)
    }
}

pub fn parse_query(db: &impl ParseDatabase, file: FileId) -> WithError<SourceFile> {
    let reporter = Reporter::new(file);

    let source = db.source(file);

    let tokens = db.lex(file)?;
    let mut parser = Parser::new(tokens.into_iter(), reporter.clone(), source);
    let program = parser.parse_program();

    if reporter.has_errors() {
        Err(reporter.finish())
    } else {
        Ok(program)
    }
}
