//! This module provides struct that are used to keep tracks of the location of items within the source code

use itertools::multipeek;
use itertools::structs::MultiPeek;
use std::fmt::{self, Display};
use std::str::Bytes;
use std::str::Chars;
#[derive(Debug, Clone)]
pub struct CharPosition<'a> {
    pub pos: Position,
    pub chars: MultiPeek<Chars<'a>>,
}

pub struct BytePosition<'a> {
    pub pos: Position,
    pub bytes: Bytes<'a>,
}

/// Represents a Span in the source file along with its value
#[derive(Debug, Clone)]
pub struct Spanned<T> {
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

#[derive(Debug, Copy, PartialOrd, Clone, PartialEq, Eq, Ord)]
pub struct Position {
    pub line: u32,
    pub column: u32,
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
            chars: multipeek(input.chars()),
        }
    }
}

impl<'a> BytePosition<'a> {
    pub fn new(input: &'a str) -> Self {
        BytePosition {
            pos: Position {
                line: 1,
                column: 1,
                absolute: 0,
            },
            bytes: input.bytes(),
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

impl<'a> Iterator for BytePosition<'a> {
    type Item = (Position, u8);

    fn next(&mut self) -> Option<(Position, u8)> {
        self.bytes.next().map(|ch| {
            let pos = self.pos;
            self.pos = self.pos.shift_byte(ch);
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
    pub fn to(self, other: Span) -> Self {
        use std::cmp;
        Span {
            start: cmp::min(self.start, other.end),
            end: cmp::max(self.end, other.end),
        }
    }
}

impl<T> Spanned<T> {
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

    pub fn shift_byte(mut self, ch: u8) -> Self {
        if ch == b'\n' {
            self.line += 1;
            self.column = 1;
        } else if ch == b'\t' {
            self.column += 4;
        } else {
            self.column += 1;
        }

        self.absolute += (ch as char).len_utf8();
        self
    }
}
