extern crate util;

use util::pos::Span;

pub trait TryFrom<T>: Sized {
    /// The type returned in the event of a conversion error.
    type Error;

    /// Performs the conversion.
    fn try_from(T) -> Result<Self, Self::Error>;
}

#[derive(Clone, Copy, Debug)]
pub enum OpCode {
    Return = 1,
    Constant = 2,
}

impl Into<u8> for OpCode {
    fn into(self) -> u8 {
        self as u8
    }
}

#[cfg(feature = "debug")]
pub fn simple_instruction(name: &str, offset: usize) -> usize {
    println!("{:?}", name);
    offset + 1
}

impl TryFrom<u8> for OpCode {
    type Error = ();

    fn try_from(original: u8) -> Result<Self, Self::Error> {
        match original {
            1 => Ok(OpCode::Return),
            2 => Ok(OpCode::Constant),
            _ => Err(()),
        }
    }
}

#[derive(Default)]
pub struct Chunk {
    code: Vec<u8>,
    constants: Vec<f64>,
    lines: Vec<Span>,
}

impl Chunk {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn write<T: Into<u8>>(&mut self, byte: T, span: Span) {
        self.code.push(byte.into());
        self.lines.push(span)
    }
    /// Add a constant and returns the index where it was appended
    ///so that we can locate that same constant later.
    pub fn add_constant(&mut self, value: f64) -> u8 {
        self.constants.push(value);
        (self.constants.len() - 1) as u8
    }

    #[cfg(feature = "debug")]
    pub fn dissassemble(&mut self, name: &str) {
        println!("== {} ==", name);

        let mut i = 0;
        while i < self.code.len() {
            i = self.dissassemble_instruction(i);
        }
    }

    #[cfg(feature = "debug")]
    pub fn dissassemble_instruction(&self, offset: usize) -> usize {
        print!("{:04} ", offset);

        if offset > 0 && self.lines[offset] == self.lines[offset - 1] {
            print!("   | ");
        } else {
            print!("{:04} ", self.lines[offset]);
        }

        let instruction = self.code[offset];

        match OpCode::try_from(instruction) {
            Ok(OpCode::Return) => simple_instruction("OP_RETURN", offset),
            Ok(OpCode::Constant) => self.constant_instruction("OP_CONSTANT", offset as usize),
            _ => {
                println!("Unknown opcode {}", instruction);
                offset + 1
            }
        }
    }

    #[cfg(feature = "debug")]
    /// Pulls out the `OpCode::Constant` and prints that out
    /// Matches on the instruction and uses that pointer offset for
    /// were the value is stored
    pub fn constant_instruction(&self, name: &str, offset: usize) -> usize {
        let constant = self.code[offset + 1];

        println!(
            "{:16} {:04} {}",
            name, constant, self.constants[constant as usize]
        );

        offset as usize + 2
    }
}

#[cfg(test)]
#[cfg(feature = "debug")]
mod tests {

    use util::pos::{Position, Span};
    use super::{Chunk, OpCode};

    #[test]
    fn it_works() {
        let mut chunk = Chunk::new();

        let constant = chunk.add_constant(1.2);

        chunk.write(
            OpCode::Constant,
            Span {
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
            },
        );

        chunk.write(
            constant,
            Span {
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
            },
        );

        chunk.write(
            OpCode::Return,
            Span {
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
            },
        );
        chunk.dissassemble("test chunk")
    }
}
