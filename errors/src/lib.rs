pub mod db;
mod files;
pub mod pos;
mod reporter;
pub use crate::reporter::Reporter;
pub use codespan_reporting::{
    diagnostic::Diagnostic,
    files::Files,
    term::{
        emit,
        termcolor::{ColorChoice, StandardStream},
        Config,
    },
};

pub use pos::Span;

pub use db::{FileDatabase, FileDatabaseStorage, FileId};
pub type WithError<T> = Result<T, Vec<Diagnostic<FileId>>>;
