use errors::pos::Span;
use errors::FileId;
use errors::WithError;
use salsa;
use std::path::PathBuf;
use std::sync::Arc;
use syntax::ast::SourceFile;
use syntax::Token;

#[salsa::query_group(ParseDatabaseStorage)]
pub trait ParseDatabase: FilesExt {
    #[salsa::invoke(crate::parse::parse_query)]
    fn parse(&self, file: FileId) -> WithError<SourceFile>;

    #[salsa::invoke(crate::parse::lex_query)]
    fn lex(&self, file: FileId) -> WithError<Vec<Span<Token>>>;
}

pub trait FilesExt: salsa::Database {
    fn source(&self, file: FileId) -> &Arc<str>;
    fn load_file(&mut self, path: &PathBuf) -> FileId;
}
