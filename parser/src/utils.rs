use crate::AstNode;
#[cfg(test)]
use errors::WithError;
#[cfg(test)]
use syntax::ast::SourceFile;

pub fn dump_debug<T: AstNode>(item: &T) -> String {
    format!("{:#?}", item.syntax())
}

#[cfg(test)]
#[salsa::database(errors::FileDatabaseStorage, crate::ParseDatabaseStorage)]
#[derive(Debug, Default)]
pub struct MockDatabaseImpl {
    runtime: salsa::Runtime<MockDatabaseImpl>,
}

#[cfg(test)]
impl salsa::Database for MockDatabaseImpl {
    fn salsa_runtime(&self) -> &salsa::Runtime<MockDatabaseImpl> {
        &self.runtime
    }

    fn salsa_runtime_mut(&mut self) -> &mut salsa::Runtime<MockDatabaseImpl> {
        &mut self.runtime
    }
}

#[cfg(test)]
pub fn parse<'a>(input: &'a str) -> WithError<SourceFile> {
    use crate::ParseDatabase;
    use errors::FileDatabase;
    use std::io::Write;
    use tempfile::NamedTempFile;

    let mut file = NamedTempFile::new().unwrap();
    write!(file, "{}", input).unwrap();
    let db = MockDatabaseImpl::default();
    let handle = db.intern_file(file.path().to_path_buf());

    db.parse(handle)
}
