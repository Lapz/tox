//! This module provides struct that are used to keep tracks of the location of items within the source code

use std::fmt::{self, Display};
use std::str::Chars;

#[derive(Debug, Clone)]
pub struct CharPosition<'a> {
    pub pos: Position,
    pub chars: Chars<'a>,
}

/// Represents a Span in the source file along with its value
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Spanned<T: PartialEq> {
    pub span: Span,
    pub value: T,
}

/// A span between two locations in a source file
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

pub const EMPTYSPAN: Span = Span {
    start: Position {
        line: 1,
        column: 0,
        absolute: 1,
    },
    end: Position {
        line: 1,
        column: 0,
        absolute: 1,
    },
};

#[derive(Debug, Copy, PartialOrd, Clone, PartialEq)]
pub struct Position {
    pub line: i64,
    pub column: i64,
    pub absolute: usize,
}

impl<'a> CharPosition<'a> {
    pub fn new(input: &'a str) -> Self {
        CharPosition {
            pos: Position {
                line: 1,
                column: 1,
                absolute: 0,
            },
            chars: input.chars(),
        }
    }
}

impl<'a> Iterator for CharPosition<'a> {
    type Item = (Position, char);

    fn next(&mut self) -> Option<(Position, char)> {
        self.chars.next().map(|ch| {
            let pos = self.pos;
            self.pos = self.pos.shift(ch);
            (pos, ch)
        })
    }
}

impl Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{},{}", self.line, self.column)
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} -> {}", self.start, self.end)
    }
}

impl Span {
    pub fn to(mut self, other: Span) -> Self {
        self.end.line += other.end.line;
        self.end.column += other.end.column;
        self
    }
}

impl<T: PartialEq> Spanned<T> {
    pub fn new(value: T, span: Span) -> Self {
        Spanned { span, value }
    }
    pub fn get_span(&self) -> Span {
        self.span
    }
}

impl Position {
    pub fn shift(mut self, ch: char) -> Self {
        if ch == '\n' {
            self.line += 1;
            self.column = 1;
        } else if ch == '\t' {
            self.column += 4;
        } else {
            self.column += 1;
        }

        self.absolute += ch.len_utf8();
        self
    }
}
