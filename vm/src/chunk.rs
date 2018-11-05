#[cfg(feature = "debug")]
use opcode;
use value::Value;

type Line = u32;

#[derive(Debug)]
/// A wrapper around an array of bytes
pub struct Chunk {
    pub code: Vec<u8>,
    pub constants: Vec<Value>,
    pub lines: Vec<Line>,
}

impl Chunk {
    pub fn new() -> Self {
        Chunk {
            code: Vec::new(),
            constants: Vec::new(),
            lines: Vec::new(),
        }
    }

    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }

    pub fn write(&mut self, byte: u8, line: Line) {
        self.code.push(byte);
        self.lines.push(line)
    }

    #[cfg(feature = "debug")]
    pub fn disassemble(&self, name: &str) {
        println!("== {} ==\n", name);

        let mut i = 0;

        while i < self.code.len() {
            i = self.disassemble_instruction(i);
        }
    }

    #[cfg(feature = "debug")]
    pub fn disassemble_instruction(&self, offset: usize) -> usize {
        print!("{:04}", offset);

        if offset > 0 && self.lines[offset] == self.lines[offset - 1] {
            print!("   | ")
        } else {
            print!("{:4} ", self.lines[offset])
        }

        let instruction = self.code[offset];

        match instruction {
            opcode::IGL => simple_instruction("OPCODE::IGL", offset),
            opcode::HLT => simple_instruction("OPCODE::HLT", offset),
            opcode::RETURN => simple_instruction("OPCODE::RETURN", offset),
            opcode::CONSTANT => simple_instruction("OPCODE::CONSTANT", offset),
            opcode::PRINT => simple_instruction("OPCODE::PRINT",offset),
            
            opcode::NEGATE => simple_instruction("OPCODE::NEGATE", offset),
            opcode::NEGATEF => simple_instruction("OPCODE::NEGATEF", offset),
            opcode::NIL => simple_instruction("OPCODE::NIL", offset),
            opcode::TRUE => simple_instruction("OPCODE::TRUE", offset),
            opcode::FALSE => simple_instruction("OPCODE::FALSE", offset),
            opcode::NOT => simple_instruction("OPCODE::NOT", offset),
            opcode::EQUAL => simple_instruction("OPCODE::EQUAL", offset),
            opcode::GREATER => simple_instruction("OPCODE::GREATER", offset),
            opcode::GREATERF => simple_instruction("OPCODE::GREATERF", offset),
            opcode::LESS => simple_instruction("OPCODE::LESS", offset),
            opcode::LESSF => simple_instruction("OPCODE::LESSF", offset),
            opcode::ADD => simple_instruction("OPCODE::ADD", offset),
            opcode::ADDF => simple_instruction("OPCODE::ADDF", offset),
            opcode::SUB => simple_instruction("OPCODE::SUB", offset),
            opcode::SUBF => simple_instruction("OPCODE::SUBF", offset),
            opcode::MUL => simple_instruction("OPCODE::MUL", offset),
            opcode::MULF => simple_instruction("OPCODE::MULF", offset),
            opcode::DIV => simple_instruction("OPCODE::DIV", offset),
            opcode::DIVF => simple_instruction("OPCODE::DIVF", offset),
            opcode::JUMP => simple_instruction("OPCODE::JUMP",offset),
            opcode::GETLOCAL => simple_instruction("OPCODE::GETLOCAL",offset),
            opcode::SETLOCAL => simple_instruction("OPCODE::SETLOCAL",offset),
            opcode::CALL => simple_instruction("OPCODE::CALL",offset),
            opcode::JUMPIF => self.jump_instruction("OPCODE::JUMPIF",offset),
            opcode::JUMPNOT => self.jump_instruction("OPCODE::JUMPNOT",offset),
            opcode::LOOP => self.jump_instruction("OPCODE::LOOP",offset),
            
            
            _ => {
                println!("UNKOWN OPCODE {}", instruction);
                offset + 1
            }
        }
    }

    #[cfg(feature = "debug")]
    pub fn constant_instruction(&self, name: &str, offset: usize) -> usize {
        let constant = self.code[offset + 1];
        println!(
            "{:16}{:4} '{}' ",
            name, constant, self.constants[constant as usize]
        );
        offset + 2
    }

    pub fn jump_instruction(&self,name:&str,offset:usize) -> usize {
        let dest = (self.code[offset+1] as u16) << 8 | self.code[offset+2] as u16;
        
        println!(
            "{:16}{:4}",
            name, dest,
        );

        offset+3
    }
}

#[cfg(feature = "debug")]
pub fn simple_instruction(name: &str, offset: usize) -> usize {
    println!("{}", name);
    offset + 1
}
