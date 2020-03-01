use crate::read_file;
use errors::FileId;
use parser::FilesExt;
use std::default::Default;
use std::path::PathBuf;
use std::sync::Arc;
#[salsa::database(
    semant::HirDatabaseStorage,
    semant::InternDatabaseStorage,
    parser::ParseDatabaseStorage
)]
#[derive(Debug)]
pub struct DatabaseImpl {
    runtime: salsa::Runtime<DatabaseImpl>,
    files: errors::Files<Arc<str>>,
}

impl FilesExt for DatabaseImpl {
    fn source(&self, file: FileId) -> &Arc<str> {
        self.files.source(file)
    }

    fn load_file(&mut self, path: &PathBuf) -> FileId {
        let source = read_file(path).expect("Couldn't read a file");
        self.files.add(path, source.into())
    }
}

impl salsa::Database for DatabaseImpl {
    fn salsa_runtime(&self) -> &salsa::Runtime<DatabaseImpl> {
        &self.runtime
    }

    fn salsa_runtime_mut(&mut self) -> &mut salsa::Runtime<DatabaseImpl> {
        &mut self.runtime
    }
}

impl Default for DatabaseImpl {
    fn default() -> Self {
        Self {
            runtime: Default::default(),
            files: errors::Files::<Arc<str>>::new(),
        }
    }
}
