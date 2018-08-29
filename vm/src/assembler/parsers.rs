use nom::{digit,alpha1};
use nom::types::CompleteStr;
use opcode::{self, OpCode};


pub struct Input<'a>(pub CompleteStr<'a>);


#[derive(Debug, PartialEq)]
pub enum Token {
    Op(OpCode),
    Register(u8),
    Number(i32),
}

#[derive(Debug, PartialEq)]
pub struct AssemblerInstruction {
    opcode: Token,
    operand1: Option<Token>,
    operand2: Option<Token>,
    operand3: Option<Token>,
}

#[derive(Debug, PartialEq)]
pub struct Program {
    instructions:Vec<AssemblerInstruction>
}



named!(opcode<CompleteStr,Token>,
    do_parse!(
        opcode:alpha1 >>
        (
            Token::Op(u8::opcode(opcode))
        )
    )
);

named!(
    opcode_load<CompleteStr,Token>,
    do_parse!(
        alt!(tag!("load")| tag!("LOAD")) >> (Token::Op(opcode::LOAD))
    )
);

named!(
    opcode_hlt<CompleteStr,Token>,
    do_parse!(
        alt!(tag!("hlt")| tag!("HLT")) >> (Token::Op(opcode::HLT))
    )
);


named!(register<CompleteStr,Token>,
    ws!(
        do_parse!(
            tag!("$") >>
            register: digit >> (
                Token::Register(register.parse::<u8>().unwrap())
            )
        )
    )
);

named!(integer_operand<CompleteStr,Token>,
    ws!(
        do_parse!(
            tag!("#") >>
                number:digit >> (
                    Token::Number(number.parse::<i32>().unwrap())
                )

        )
    )
);

/// Handles instructions of the following form:
/// LOAD $0 #100
named!(instruction_one<CompleteStr,AssemblerInstruction>,
    do_parse!(
        o: opcode >>
        r: register >>
        i : integer_operand >>
        (
            AssemblerInstruction {
                opcode:o,
                operand1:Some(r),
                operand2:Some(i),
                operand3:None
            }
        )
    )
);

named!(instruction_none<CompleteStr,AssemblerInstruction>,
    do_parse!(
        o: opcode_hlt >>
        (
            AssemblerInstruction {
                opcode:o,
                operand1:None,
                operand2:None,
                operand3:None
            }
        )
    )
);

named!(pub file<CompleteStr,Program>,

    do_parse!(
        instructions: ws!(
            many1!(
                alt!(instruction_one | instruction_none)
            )
        ) >> (
            Program {
                instructions
            }
        )
    )

);

impl Program {
    pub fn to_bytes(&self) -> Vec<u8> {

        let mut program = Vec::with_capacity(self.instructions.len() *4);

        for inst in self.instructions.iter() {
            program.append(&mut inst.to_bytes());
        }

        program
    }
}

impl AssemblerInstruction {
    pub fn to_bytes(&self) -> Vec<u8> {

        let mut results = Vec::with_capacity(4);

        match self.opcode {
            Token::Op(ref code) => results.push(*code),
            _ => {
                panic!("Non-opcode found in opcode field");
            }
        }

        for operand in &[&self.operand1,&self.operand2,&self.operand3] {
            match operand {
                Some(ref op) => AssemblerInstruction::extract_operand(op,&mut results),
                None => (),
            }
        }

        results
    }

    fn extract_operand(t:&Token,results:&mut Vec<u8>) {
        match t {
            Token::Register(ref reg) => results.push(*reg),
            Token::Number(ref num) => {
                let converted = *num as u16;
                let byte2 = converted >> 8;

                results.push(byte2 as u8);
                results.push(converted as u8);
            },
            _ => {
                panic!("opcode found in operand field")
            }
        }
    }


}


pub trait FromInput<T> {
    fn opcode(v:T) -> Self;
}

impl <'a> FromInput<CompleteStr<'a>> for u8 {
    fn opcode(v:CompleteStr<'a>) -> Self {
        match v {
            CompleteStr("load") => opcode::LOAD,
            CompleteStr("add") => opcode::ADD,
            CompleteStr("sub") => opcode::SUB,
            CompleteStr("mul") => opcode::MUL,
            CompleteStr("div") => opcode::DIV,
            CompleteStr("hlt") => opcode::HLT,
            CompleteStr("jmp") => opcode::JMP,
            CompleteStr("jmpf") => opcode::JMPF,
            CompleteStr("jmpb") => opcode::JMPB,
            CompleteStr("equal") => opcode::EQUAL,
            CompleteStr("not") => opcode::NOT,
            CompleteStr("greater") => opcode::GREATER,
            CompleteStr("less") => opcode::LESS,
            CompleteStr("jmpe") => opcode::JMPEQ,
            CompleteStr("LOAD") => opcode::LOAD,
            CompleteStr("ADD") => opcode::ADD,
            CompleteStr("SUB") => opcode::SUB,
            CompleteStr("MUL") => opcode::MUL,
            CompleteStr("DIV") => opcode::DIV,
            CompleteStr("HLT") => opcode::HLT,
            CompleteStr("JMP") => opcode::JMP,
            CompleteStr("JMPF") => opcode::JMPF,
            CompleteStr("JMPB") => opcode::JMPB,
            CompleteStr("EQUAL") => opcode::EQUAL,
            CompleteStr("NOT") => opcode::NOT,
            CompleteStr("GREATER") => opcode::GREATER,
            CompleteStr("LESS") => opcode::LESS,
            CompleteStr("JMPE") => opcode::JMPEQ,
            _ => opcode::IGL,
        }
    }
}


impl <'a> Input <'a> {
    pub fn new(input:&'a str) -> Self {
        Input(CompleteStr(input))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn parse_load() {
        let result = opcode_load(CompleteStr("load"));
        assert!(result.is_ok());

        let result = opcode_load(CompleteStr("LOAD"));
        assert!(result.is_ok());

        let (rest, token) = result.unwrap();
        assert_eq!(token, Token::Op(opcode::LOAD));

        let result = opcode_load(CompleteStr("aold"));
        let (rest, token) = result.unwrap();
        assert_eq!(token, Token::Op(opcode::IGL));
    }

    #[test]
    fn parse_register() {
        let result = register(CompleteStr("$10"));

        assert!(result.is_ok());

        let (rest, token) = result.unwrap();
        assert_eq!(token, Token::Register(10));

        let result = register(CompleteStr("0"));
        assert!(result.is_err());

        let result = register(CompleteStr("$a"));
        assert!(result.is_err());
    }

    #[test]
    fn parse_number() {
        let result = integer_operand(CompleteStr("#10"));

        assert!(result.is_ok());

        let (rest, token) = result.unwrap();
        assert_eq!(token, Token::Number(10));

        let result = integer_operand(CompleteStr("10"));
        assert!(result.is_err());
    }

    #[test]
    fn parse_file() {
        let result = file(CompleteStr("LOAD $0 #10"));

        assert!(result.is_ok());

        let (rest, program) = result.unwrap();

        let instructions = vec![AssemblerInstruction {
            opcode: Token::Op(opcode::LOAD),
            operand1: Some(Token::Register(0)),
            operand2: Some(Token::Number(10)),
            operand3: None
        }];

        assert_eq!(
            program,
            Program {
                instructions,
            }

        );



        let result = file(CompleteStr("load $0 #10"));

        assert!(result.is_ok());

        let instructions = vec![AssemblerInstruction {
            opcode: Token::Op(opcode::LOAD),
            operand1: Some(Token::Register(0)),
            operand2: Some(Token::Number(10)),
            operand3: None
        }];

        let (rest, token) = result.unwrap();
        assert_eq!(
            program,
            Program {
                instructions
            }
        );
    }
}
