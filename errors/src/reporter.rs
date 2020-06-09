use codespan_reporting::diagnostic::{Diagnostic, Label, LabelStyle};

use crate::FileId;
use std::cell::RefCell;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

#[derive(Clone, Eq)]
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

    pub fn extend(&mut self, s: Vec<Diagnostic<FileId>>) {
        self.diagnostics.borrow_mut().extend(s.into_iter())
    }

    pub fn error(
        &mut self,
        message: impl Into<String>,
        additional_info: impl Into<String>,
        span: (impl Into<usize>, impl Into<usize>),
    ) {
        let span = span.0.into()..span.1.into();
        let label = Label::new(LabelStyle::Primary, self.file, span);
        let diagnostic = Diagnostic::error()
            .with_message(message)
            .with_notes(vec![additional_info.into()])
            .with_labels(vec![label]);
        self.diagnostics.borrow_mut().push(diagnostic)
    }

    pub fn warn(
        &mut self,
        message: impl Into<String>,
        additional_info: impl Into<String>,
        span: (impl Into<usize>, impl Into<usize>),
    ) {
        let span = span.0.into()..span.1.into();
        let label = Label::new(LabelStyle::Primary, self.file, span);
        let diagnostic = Diagnostic::warning()
            .with_message(message)
            .with_notes(vec![additional_info.into()])
            .with_labels(vec![label]);
        self.diagnostics.borrow_mut().push(diagnostic)
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

impl PartialEq for Reporter {
    fn eq(&self, other: &Self) -> bool {
        self.file == other.file && self.diagnostics == other.diagnostics
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
