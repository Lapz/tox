use opcode;
use vm::VM;

impl VM {
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
        print!("{:04} \t", offset);

        let instruction = self.code[offset];

        match instruction {
            opcode::ADD => simple_instruction("OPCODE::ADD", offset),
            opcode::SUB => simple_instruction("OPCODE::SUB", offset),
            opcode::DIV => simple_instruction("OPCODE::DIV", offset),
            opcode::MUL => simple_instruction("OPCODE::MUL", offset),
            opcode::NOT => simple_instruction("OPCODE::NOT", offset),
            opcode::EQUAL => simple_instruction("OPCODE::EQUAL", offset),
            opcode::LESS => simple_instruction("OPCODE::LESS", offset),
            opcode::GREATER => simple_instruction("OPCODE:GREATER", offset),
            opcode::LOAD => simple_instruction("OPCODE::LOAD", offset),
            opcode::STORE => simple_instruction("OPCODE::STORE", offset),
            opcode::JMPF => simple_instruction("OPCODE::JMPF", offset),
            opcode::JMPB => simple_instruction("OPCODE::JMPB", offset),
            opcode::JMPEQ => simple_instruction("OPCODE::JMPEQ", offset),
            opcode::JMPNEQ => simple_instruction("OPCODE::JMPNEQ", offset),
            opcode::HLT => simple_instruction("OPCODE::HLT", offset),
            opcode::ALLOC => simple_instruction("OPCODE::ALLOC", offset),
            opcode::FREE => simple_instruction("OPCODE::FREE", offset),
            opcode::INC => simple_instruction("OPCODE::INC", offset),
            opcode::DEC => simple_instruction("OPCODE::DEC", offset),
            opcode::PUSH => simple_instruction("OPCODE::PUSH", offset),
            opcode::POP => simple_instruction("OPCODE::POP", offset),
            _ => {
                println!("UNKOWN OPCODE {}", instruction);
                offset + 1
            }
        }
    }
}

#[cfg(feature = "debug")]
pub fn simple_instruction(name: &str, offset: usize) -> usize {
    println!("{}", name);
    offset + 4
}
