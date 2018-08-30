#[macro_use]
mod parsers;
mod token;

pub use self::parsers::file;
use self::token::Token;

#[derive(Debug, PartialEq)]
pub struct AssemblerInstruction {
    opcode: Option<Token>,
    label: Option<Token>,
    directive: Option<Token>,
    operand1: Option<Token>,
    operand2: Option<Token>,
    operand3: Option<Token>,
}

#[derive(Debug, PartialEq)]
pub struct Program {
    pub instructions: Vec<AssemblerInstruction>,
}
impl AssemblerInstruction {
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut results = Vec::with_capacity(4);

        match self.opcode {
            Some(Token::Op(ref code)) => results.push(*code),

            _ => {
                // panic!("Non-opcode found in opcode field");
            }
        }

        for operand in &[&self.operand1, &self.operand2, &self.operand3] {
            match operand {
                Some(ref op) => AssemblerInstruction::extract_operand(op, &mut results),
                None => (),
            }
        }

        while results.len() < 4 {
            results.push(0);
        }

        results
    }

    fn extract_operand(t: &Token, results: &mut Vec<u8>) {
        match t {
            Token::Register(ref reg) => results.push(*reg),
            Token::Number(ref num) => {
                let converted = *num as u16;
                let byte2 = converted >> 8;

                results.push(byte2 as u8);
                results.push(converted as u8);
            }
            _ => panic!("opcode found in operand field"),
        }
    }
}
