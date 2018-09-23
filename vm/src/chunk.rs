/// A sequence of bytecode instrutcions, constant pool and
/// the line from which an instruction orginated from
use op::{OpCode, TryFrom};
use std::ops::{Index, Range};

type Line = usize;
#[derive(Debug, Clone, Default)]
pub struct Chunk {
    code: Vec<u8>,
    pub constants: Vec<u8>,
    lines: Vec<Line>,
}

#[cfg(feature = "debug")]
pub fn simple_2_instruction(name: &str, offset: usize) -> usize {
    println!("{}", name);
    offset + 2
}

#[cfg(feature = "debug")]
pub fn simple_instruction(name: &str, offset: usize) -> usize {
    println!("{}", name);
    offset + 1
}

#[cfg(feature = "debug")]
macro_rules! to_num {
    ([$stack:expr, $top:expr] => $type:ty) => {{
        use std::default;
        use std::mem;

        let mut b: [u8; mem::size_of::<$type>()] = default::Default::default();

        b.copy_from_slice($stack);
        unsafe { mem::transmute::<_, $type>(b) }
    }};
}

impl Chunk {
    pub fn new() -> Self {
        Self { ..Self::default() }
    }

    /// Write a single byte to this chunk
    pub fn write<T: Into<u8>>(&mut self, byte: T, line: Line) {
        self.lines.push(line);
        self.code.push(byte.into())
    }

    /// Write multiple bytes to this chunk
    pub fn write_values(&mut self, bytes: &[u8], line: Line) {
        self.lines.push(line);
        self.constants.extend(bytes)
    }

    /// Adds a constant to the constant pool.
    /// Constant must be in bytes.
    /// Returns the offset at which the constant is stored
    pub fn add_constant(&mut self, bytes: &[u8], line: Line) -> usize {
        let len = self.constants.len();
        self.write_values(bytes, line);
        len
    }

    /// Adds a constant to the constant pool.
    /// Constant must be in bytes.
    /// Returns the offset at which the string is stored
    pub fn add_string(&mut self, bytes: &[u8], line: Line) -> usize {
        let len = self.constants.len();
        self.write_values(bytes, line);
        len
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
            Ok(OpCode::Return) => simple_2_instruction("OP_RETURN", offset),
            Ok(OpCode::Int) => self.constant_instruction("OP_INT", offset as usize),
            Ok(OpCode::Float) => self.float_instruction("OP_FLOAT", offset as usize),
            Ok(OpCode::String) => self.string_instruction("OP_STRING", offset),
            Ok(OpCode::Nil) => simple_instruction("OP_NIL", offset),
            Ok(OpCode::NegInt) => simple_instruction("OP_NEGINT", offset),
            Ok(OpCode::AddInt) => simple_instruction("OP_ADDINT", offset),
            Ok(OpCode::SubtractInt) => simple_instruction("OP_SUBTRACTINT", offset),
            Ok(OpCode::MultiplyInt) => simple_instruction("OP_MULTIPLYINT", offset),
            Ok(OpCode::DivideInt) => simple_instruction("OP_DIVINT", offset),
            Ok(OpCode::NegFloat) => simple_instruction("OP_NEGFLOAT", offset),
            Ok(OpCode::AddFloat) => simple_instruction("OP_ADDFLOAT", offset),
            Ok(OpCode::SubtractFloat) => simple_instruction("OP_SUBTRACTFLOAT", offset),
            Ok(OpCode::MultiplyFloat) => simple_instruction("OP_MULTIPLYFLOAT", offset),
            Ok(OpCode::DivideFloat) => simple_instruction("OP_DIVFLOAT", offset),
            Ok(OpCode::IntNeq) => simple_instruction("OP_INTNEQ", offset),
            Ok(OpCode::IntEq) => simple_instruction("OP_INTEQ", offset),
            Ok(OpCode::IntLt) => simple_instruction("OP_INTLT", offset),
            Ok(OpCode::IntLtE) => simple_instruction("OP_INTLTE", offset),
            Ok(OpCode::IntGt) => simple_instruction("OP_INTGT", offset),
            Ok(OpCode::IntGtE) => simple_instruction("OP_INTGTE", offset),
            Ok(OpCode::FloatNeq) => simple_instruction("OP_FLOATNEQ", offset),
            Ok(OpCode::FloatEq) => simple_instruction("OP_FLOATEQ", offset),
            Ok(OpCode::FloatLt) => simple_instruction("OP_FLOATLT", offset),
            Ok(OpCode::FloatLtE) => simple_instruction("OP_FLOATLTE", offset),
            Ok(OpCode::FloatGt) => simple_instruction("OP_FLOATGT", offset),
            Ok(OpCode::FloatGtE) => simple_instruction("OP_FLOATGTE", offset),
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
    fn constant_instruction(&self, name: &str, offset: usize) -> usize {
        // opcode start

        let index = self.code[offset + 1] as usize;

        println!(
            "{:16} {:04}",
            name,
            to_num!([&self.constants[index..index+8],offset] => i64)
        );

        offset as usize + 2
    }

    #[cfg(feature = "debug")]
    fn float_instruction(&self, name: &str, offset: usize) -> usize {
        // opcode start

        let index = self.code[offset + 1] as usize;

        println!(
            "{:16} {:04}",
            name,
            to_num!([&self.constants[index..index+8],offset] => f64)
        );

        offset as usize + 2
    }

    #[cfg(feature = "debug")]
    fn string_instruction(&self, name: &str, offset: usize) -> usize {
        // opcode start

        let index = self.code[offset + 1] as usize;

        let len = self.code[offset + 2] as usize;

        use std::str;
        let string = unsafe { str::from_utf8_unchecked(&self.constants[index..index + len]) };

        println!("{:16} {}", name, string);

        offset as usize + 2
    }
}

impl Index<usize> for Chunk {
    type Output = u8;

    fn index(&self, index: usize) -> &Self::Output {
        &self.code[index]
    }
}

impl Index<Range<usize>> for Chunk {
    type Output = [u8];

    fn index(&self, index: Range<usize>) -> &Self::Output {
        self.code.index(index)
    }
}

#[cfg(test)]
#[cfg(feature = "debug")]
mod test {

    use chunk::Chunk;
    use op::OpCode;

    #[test]
    fn it_work() {
        let mut chunk = Chunk::new();
        chunk.add_string(&[104, 101, 108, 108, 111, 32, 119, 111, 114, 108, 100], 1);

        chunk.write(OpCode::String, 1); // string
        chunk.write(11, 1); // length

        let mut constant = chunk.add_constant(&[0, 0, 0, 0, 0, 0, 40, 64], 1);

        chunk.write(OpCode::Float, 1); //Float

        chunk.write(constant as u8, 1); //index

        constant = chunk.add_constant(&[0, 0, 0, 0, 0, 0, 57, 64], 1);

        chunk.write(OpCode::Float, 1); //Float
        chunk.write(constant as u8, 1); //index

        chunk.write(OpCode::AddFloat, 2); // Add

        chunk.write(OpCode::Return, 2); // Return
        chunk.write(1, 2);

        println!("{:?}", chunk);

        chunk.dissassemble("test");
    }
}
