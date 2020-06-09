extern crate console_error_panic_hook;
extern crate wasm_bindgen;
use errors::{emit, ColorChoice, Config, Diagnostic, FileDatabase, FileId, Files, StandardStream};
use semant::HirDatabase;
use std::default::Default;
use std::io::{self};
use std::ops::Range;
use wasm_bindgen::prelude::*;

use console_error_panic_hook::set_once as set_panic_hook;
#[salsa::database(
    semant::HirDatabaseStorage,
    semant::InternDatabaseStorage,
    parser::ParseDatabaseStorage,
    errors::FileDatabaseStorage
)]
#[derive(Debug, Default)]
pub struct DatabaseImpl {
    runtime: salsa::Runtime<DatabaseImpl>,
}

pub(crate) trait Diagnostics {
    fn emit(&self, diagnostics: &mut Vec<Diagnostic<FileId>>) -> io::Result<()>;
}

impl Diagnostics for DatabaseImpl {
    fn emit(&self, diagnostics: &mut Vec<Diagnostic<FileId>>) -> io::Result<()> {
        let writer = StandardStream::stderr(ColorChoice::Auto);
        let mut writer = writer.lock();
        let config = Config::default();

        while let Some(diagnostic) = diagnostics.pop() {
            emit(&mut writer, &config, self, &diagnostic)?
        }

        Ok(())
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

impl<'files> Files<'files> for DatabaseImpl {
    type FileId = FileId;
    type Name = String;
    type Source = String;

    fn name(&self, file_id: FileId) -> Option<Self::Name> {
        Some((*errors::db::FileDatabase::file(self, file_id).name).clone())
    }

    fn source(&self, file_id: FileId) -> Option<Self::Source> {
        Some((*errors::db::FileDatabase::file(self, file_id).source).clone())
    }

    fn line_index(&self, file_id: FileId, byte_index: usize) -> Option<usize> {
        errors::db::FileDatabase::line_index(self, file_id, byte_index)
    }

    fn line_range(&self, file_id: FileId, line_index: usize) -> Option<Range<usize>> {
        errors::db::FileDatabase::line_range(self, file_id, line_index)
    }
}

#[wasm_bindgen]
pub fn parse(contents: String) {
    set_panic_hook();
    let db = DatabaseImpl::default();
    let mut errors = Vec::new();

    let handle = db.intern_content(contents);

    match db.resolve_source_file(handle) {
        Ok(_) => {}
        Err(more_errs) => errors.extend(more_errs),
    }

    db.emit(&mut errors).unwrap();
}
