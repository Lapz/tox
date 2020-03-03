use codespan::{FileId, Files};
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

#[derive(Clone, PartialEq, Eq)]
pub struct Reporter {
    file: FileId,
    diagnostics: Rc<RefCell<Vec<Diagnostic<FileId>>>>,
}

impl Reporter {
    pub fn new(file: FileId) -> Self {
        Self {
            file,
            diagnostics: Rc::new(RefCell::new(Vec::new())),
        }
    }

    pub fn finish(self) -> Vec<Diagnostic<FileId>> {
        let mut diagnostics = Vec::new();

        while let Some(diagnostic) = self.diagnostics.borrow_mut().pop() {
            diagnostics.push(diagnostic);
        }

        diagnostics
    }

    pub fn error(
        &mut self,
        message: impl Into<String>,
        additional_info: impl Into<String>,
        span: (impl Into<usize>, impl Into<usize>),
    ) {
        let span = span.0.into()..span.1.into();
        let label = Label::new(self.file, span, message);
        let diagnostic = Diagnostic::new_error(additional_info, label);
        self.diagnostics.borrow_mut().push(diagnostic)
    }

    pub fn warn(
        &mut self,
        message: impl Into<String>,
        additional_info: impl Into<String>,
        span: (impl Into<usize>, impl Into<usize>),
    ) {
        let span = span.0.into()..span.1.into();
        let label = Label::new(self.file, span, message);
        let diagnostic = Diagnostic::new_warning(additional_info, label);
        self.diagnostics.borrow_mut().push(diagnostic)
    }

    pub fn emit(&self, files: &Files<Arc<str>>) -> io::Result<()> {
        let writer = StandardStream::stderr(ColorChoice::Auto);
        let mut writer = writer.lock();
        let config = Config::default();

        while let Some(diagnostic) = self.diagnostics.borrow_mut().pop() {
            emit(&mut writer, &config, files, &diagnostic)?
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

        self.diagnostics.borrow().hash(state);
    }
}

impl std::fmt::Debug for Reporter {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("Reporter")
            .field("file", &self.file)
            .finish()
    }
}

// impl PartialEq for Reporter {
//     fn eq(&self, other: &Self) -> bool {
//         self.file == other.file
//             && self.files == other.files
//             && &self.diagnostics.borrow() == other.diagnostics.borrow()
//     }
// }
