/// Each of these marks the beginning of a new source line
use std::fmt::{self, Display};
use util::pos::Span;

#[derive(PartialEq, Debug, Copy, Clone)]
pub struct LineStart {
    pub offset: usize,
    pub line: u32,
}

impl Display for LineStart {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.line)
    }
}

impl LineStart {
    pub fn new(pos: Span) -> Self {
        LineStart {
            offset: pos.end.absolute,
            line: pos.end.line,
        }
    }
}
