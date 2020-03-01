pub mod pos;
mod reporter;

pub use crate::reporter::Reporter;
pub use codespan::{FileId, Files};
pub use codespan_reporting::{
    diagnostic::Diagnostic,
    term::{
        emit,
        termcolor::{ColorChoice, StandardStream},
        Config,
    },
};

pub type WithError<T> = Result<T, Vec<Diagnostic<FileId>>>;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
