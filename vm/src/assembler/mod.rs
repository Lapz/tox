#[macro_use]
mod parsers;
mod symbols;
mod token;

pub use self::parsers::file;
use self::symbols::{Symbol, SymbolTable, SymbolType};
use self::token::Token;
use nom::types::CompleteStr;

#[derive(Debug)]
pub struct Assembler {
    phase: AssemblerPhase,
    symbols: SymbolTable,
}
#[derive(Debug)]
pub enum AssemblerPhase {
    First,
}

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

impl Assembler {
    pub fn new() -> Assembler {
        Assembler {
            phase: AssemblerPhase::First,
            symbols: SymbolTable::new(),
        }
    }

    pub fn assemble(&mut self, raw: &str) -> Option<Vec<u8>> {
        match file(CompleteStr(raw)) {
            Ok((_, program)) => {
                self.extract_labels(&program);

                Some(program.to_bytes(&self.symbols))
            }

            Err(e) => {
                println!("There was an error assembling the code: {:?}", e);
                None
            }
        }
    }

    fn extract_labels(&mut self, p: &Program) {
        for (i, instruction) in p.instructions.iter().enumerate() {
            if instruction.is_label() {
                if let Some(name) = instruction.label_name() {
                    self.symbols.add(name, i * 4, SymbolType::Label);
                }
            }
        }
    }
}

impl AssemblerInstruction {
    pub fn to_bytes(&self, symbols: &SymbolTable) -> Vec<u8> {
        let mut results = Vec::with_capacity(4);

        match self.opcode {
            Some(Token::Op(ref code)) => results.push(*code),

            _ => {
                // panic!("Non-opcode found in opcode field");
            }
        }

        for operand in &[&self.operand1, &self.operand2, &self.operand3] {
            match operand {
                Some(ref op) => AssemblerInstruction::extract_operand(op, &mut results, symbols),
                None => (),
            }
        }

        while results.len() < 4 {
            results.push(0);
        }

        results
    }

    fn extract_operand(t: &Token, results: &mut Vec<u8>, symbols: &SymbolTable) {
        match t {
            Token::Register(ref reg) => results.push(*reg),
            Token::Number(ref num) => {
                let byte1 = *num as u16;
                let byte2 = byte1 >> 8;

                results.push(byte2 as u8);
                results.push(byte1 as u8);
            }

            Token::LabelDeclaration(ref label) => {
                if let Some(offset) = symbols.offset(label) {
                    let byte1 = offset;
                    let byte2 = offset >> 8;
                    results.push(byte2 as u8);
                    results.push(byte1 as u8);
                }
            }
            _ => panic!("opcode found in operand field"),
        }
    }

    fn is_label(&self) -> bool {
        self.label.is_some()
    }

    fn label_name(&self) -> Option<String> {
        match self.label.as_ref() {
            Some(&Token::LabelDeclaration(ref string)) => Some(string.clone()),
            _ => None,
        }
    }
}

impl Program {
    pub fn to_bytes(&self, symbols: &SymbolTable) -> Vec<u8> {
        let mut program = Vec::with_capacity(self.instructions.len() * 4);

        for inst in self.instructions.iter() {
            program.append(&mut inst.to_bytes(symbols));
        }

        program
    }
}
