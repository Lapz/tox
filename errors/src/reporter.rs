use codespan::{FileId, Files, Span};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::term::{
    emit,
    termcolor::{ColorChoice, StandardStream},
    Config,
};
use std::cell::RefCell;
use std::hash::{Hash, Hasher};
use std::io::{self};
use std::rc::Rc;
use std::sync::Arc;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Reporter {
    files: Files<Arc<str>>,
    file: FileId,
    diagnostics: Rc<RefCell<Vec<Diagnostic>>>,
}

impl Reporter {
    pub fn new(files: Files<Arc<str>>, file: FileId) -> Self {
        Self {
            file,
            files,
            diagnostics: Rc::new(RefCell::new(Vec::new())),
        }
    }

    pub fn error(
        &mut self,
        message: impl Into<String>,
        additional_info: impl Into<String>,
        span: (impl Into<u32>, impl Into<u32>),
    ) {
        let span = Span::new(span.0.into(), span.1.into());
        let label = Label::new(self.file, span, message);
        let diagnostic = Diagnostic::new_error(additional_info, label);
        self.diagnostics.borrow_mut().push(diagnostic)
    }

    pub fn warn(
        &mut self,
        message: impl Into<String>,
        additional_info: impl Into<String>,
        span: (impl Into<u32>, impl Into<u32>),
    ) {
        let span = Span::new(span.0.into(), span.1.into());
        let label = Label::new(self.file, span, message);
        let diagnostic = Diagnostic::new_warning(additional_info, label);
        self.diagnostics.borrow_mut().push(diagnostic)
    }

    pub fn emit(&self) -> io::Result<()> {
        let writer = StandardStream::stderr(ColorChoice::Auto);
        let mut writer = writer.lock();
        let config = Config::default();

        while let Some(diagnostic) = self.diagnostics.borrow_mut().pop() {
            emit(&mut writer, &config, &self.files, &diagnostic)?
        }

        Ok(())
    }

    pub fn has_errors(&self) -> bool {
        !self.diagnostics.borrow().is_empty()
    }
}

impl Hash for Reporter {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.file.hash(state);
        self.files.hash(state);
        self.diagnostics.borrow().hash(state);
    }
}

// impl PartialEq for Reporter {
//     fn eq(&self, other: &Self) -> bool {
//         self.file == other.file
//             && self.files == other.files
//             && &self.diagnostics.borrow() == other.diagnostics.borrow()
//     }
// }
