use std::hash::Hash;
use syntax::{AstNode, TextRange, TextUnit};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span<T> {
    pub item: T,
    pub start: TextUnit,
    pub end: TextUnit,
}

impl<T> Span<T>
where
    T: std::fmt::Debug + Clone + Hash,
{
    pub fn new(item: T, start: TextUnit, end: TextUnit) -> Self {
        Self { item, start, end }
    }

    pub fn from_ast<N: AstNode>(item: T, node: &N) -> Self {
        let range = node.syntax().text_range();
        Span {
            item,
            start: range.start(),
            end: range.end(),
        }
    }

    pub fn from_range(item: T, range: TextRange) -> Self {
        Span {
            item,
            start: range.start(),
            end: range.end(),
        }
    }

    pub fn start(&self) -> TextUnit {
        self.start
    }

    pub fn end(&self) -> TextUnit {
        self.end
    }

    pub fn as_reporter_span(&self) -> (usize, usize) {
        (self.start.to_usize(), self.end.to_usize())
    }
}
