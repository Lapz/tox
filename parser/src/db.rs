use errors::pos::Span;
use errors::FileDatabase;
use errors::FileId;
use errors::WithError;
use syntax::ast::SourceFile;
use syntax::Token;

#[salsa::query_group(ParseDatabaseStorage)]
pub trait ParseDatabase: FileDatabase {
    #[salsa::invoke(crate::parse::parse_query)]
    fn parse(&self, file: FileId) -> WithError<SourceFile>;

    #[salsa::invoke(crate::parse::lex_query)]
    fn lex(&self, file: FileId) -> WithError<Vec<Span<Token>>>;
}
