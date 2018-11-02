#[cfg(feature = "debug")]
use opcode;
#[cfg(feature = "debug")]
use vm::VM;

//     #[cfg(feature = "debug")]
//     pub fn disassemble_instruction(&self, offset: usize) -> usize {
//         print!("{:04} \t", offset);

//         let instruction = self.code[offset];

//         match instruction {
//             opcode::IGL => simple_instruction("OPCODE::IGL",offset),
//             opcode::ADD => simple_instruction("OPCODE::ADD", offset),
//             opcode::SUB => simple_instruction("OPCODE::SUB", offset),
//             opcode::DIV => simple_instruction("OPCODE::DIV", offset),
//             opcode::MUL => simple_instruction("OPCODE::MUL", offset),
//             opcode::NOT => simple_instruction("OPCODE::NOT", offset),
//             opcode::EQUAL => simple_instruction("OPCODE::EQUAL", offset),
//             opcode::LESS => simple_instruction("OPCODE::LESS", offset),
//             opcode::GREATER => simple_instruction("OPCODE:GREATER", offset),
//             opcode::LOAD => simple_instruction("OPCODE::LOAD", offset),
//             opcode::STORE => simple_instruction("OPCODE::STORE", offset),
//             opcode::JMPF => simple_instruction("OPCODE::JMPF", offset),
//             opcode::JMPB => simple_instruction("OPCODE::JMPB", offset),
//             opcode::JMPEQ => simple_instruction("OPCODE::JMPEQ", offset),
//             opcode::JMPNEQ => simple_instruction("OPCODE::JMPNEQ", offset),
//             opcode::HLT => simple_instruction("OPCODE::HLT", offset),
//             opcode::ALLOC => simple_instruction("OPCODE::ALLOC", offset),
//             opcode::FREE => simple_instruction("OPCODE::FREE", offset),
//             opcode::INC => simple_instruction("OPCODE::INC", offset),
//             opcode::DEC => simple_instruction("OPCODE::DEC", offset),
//             opcode::PUSH => simple_instruction("OPCODE::PUSH", offset),
//             opcode::POP => simple_instruction("OPCODE::POP", offset),
//             opcode::MOD => simple_instruction("OPCODE::MOD", offset),
//             opcode::EXPON => simple_instruction("OPCODE::EXPON", offset),
//             opcode::SET => simple_instruction("OPCODE::SET", offset),

//             _ => {
//                 println!("UNKOWN OPCODE {}", instruction);
//                 offset + 1
//             }
//         }
//     }
// }

// #[cfg(feature = "debug")]
// pub fn simple_instruction(name: &str, offset: usize) -> usize {
//     println!("{}", name);
//     offset + 4
// }
