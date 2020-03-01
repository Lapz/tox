use errors::FileId;
use salsa;
use std::path::PathBuf;
use std::sync::Arc;
use syntax::ast::SourceFile;

#[salsa::query_group(ParseDatabaseStorage)]
pub trait ParseDatabase: FilesExt {
    #[salsa::invoke(crate::parse::parse_query)]
    fn parse(&self, file: FileId) -> SourceFile;
}

pub trait FilesExt: salsa::Database {
    fn source(&self, file: FileId) -> &Arc<str>;
    fn load_file(&mut self, path: &PathBuf) -> FileId;
}
