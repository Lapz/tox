use crate::pos::Position;
use codespan::{FileId, Files, Span};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::term::{
    emit,
    termcolor::{ColorChoice, StandardStream},
    Config,
};
use std::cell::RefCell;
use std::io::{self};
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Reporter {
    files: Files,
    file: FileId,
    diagnostics: Rc<RefCell<Vec<Diagnostic>>>,
}

impl Reporter {
    pub fn new(files: Files, file: FileId) -> Self {
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
        span: (Position, Position),
    ) {
        let span = Span::new(span.0.absolute, span.1.absolute);
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

        for diagnostic in &*self.diagnostics.borrow() {
            emit(&mut writer, &config, &self.files, &diagnostic)?
        }

        Ok(())
    }

    pub fn has_errors(&self) -> bool {
        !self.diagnostics.borrow().is_empty()
    }
}
