use itertools::multipeek;
use itertools::structs::MultiPeek;
use std::str::Chars;

pub type ColumnIndex = u32;
pub type LineIndex = u32;
pub type ByteIndex = u32;

#[derive(Debug, Clone)]
/// An iterator over the characters within a
pub struct CharPosition<'a> {
    pub pos: Position,
    pub chars: MultiPeek<Chars<'a>>,
}
#[derive(Debug, Copy, PartialOrd, Clone, PartialEq, Eq, Ord)]
pub struct Position {
    pub column: ColumnIndex,
    pub line: LineIndex,
    pub absolute: ByteIndex,
}

#[derive(Debug, Clone)]
pub struct Span<T> {
    pub value: T,
    pub start: Position,
    pub end: Position,
}

impl<T> Span<T>
where
    T: std::fmt::Debug + Clone,
{
    pub fn new(value: T, start: Position, end: Position) -> Self {
        Self { value, start, end }
    }
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

        self.absolute += ch.len_utf8() as u32;
        self
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

impl Into<u32> for Position {
    fn into(self) -> u32 {
        self.absolute
    }
}
