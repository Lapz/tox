use std::str::Chars;
use std::fmt::{Display, Formatter};
use std::fmt;

#[derive(Debug, Clone)]
pub struct CharPosition<'a> {
    pub pos: Postition,
    pub chars: Chars<'a>,
}

#[derive(Debug)]
pub struct WithPos<T> {
    pub node:T,
    pos:Postition
}

impl <T> WithPos<T> {
   pub fn new(node:T,pos:Postition) -> Self {
        WithPos{
            node,
            pos
        }
    }
}

impl<'a> CharPosition<'a> {
    pub fn new(input: &'a str) -> Self {
        CharPosition {
            pos: Postition {
                line: 1,
                column: 1,
                absolute: 0,
            },
            chars: input.chars(),
        }
    }
}
impl<'a> Iterator for CharPosition<'a> {
    type Item = (Postition, char);

    fn next(&mut self) -> Option<(Postition, char)> {
        self.chars.next().map(|ch| {
            let pos = self.pos;
            self.pos = self.pos.shift(ch);
            (pos, ch)
        })
    }
}


#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Postition {
    pub line: i64,
    pub column: i64,
    pub absolute: usize,
}

impl Display for Postition {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "line {}, column {}", self.line, self.column)
    }
}

impl Postition {
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
