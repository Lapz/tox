use nom::digit;
use nom::types::CompleteStr;
use opcode::{self, OpCode};

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

named!(
    opcode_load<CompleteStr,Token>,
    do_parse!(
        alt!(tag!("load")| tag!("LOAD")) >> (Token::Op(opcode::LOAD))
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
        o: opcode_load >>
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

named!(pub file<CompleteStr,Vec<AssemblerInstruction>>,
    ws!(
        many0!(
            alt!(instruction_one)
        )
    )
);

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
        assert!(result.is_err());
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

        let (rest, token) = result.unwrap();

        assert_eq!(
            token,
            vec![AssemblerInstruction {
                opcode: Token::Op(opcode::LOAD),
                operand1: Some(Token::Register(0)),
                operand2: Some(Token::Number(10)),
                operand3: None
            }]
        );

        let result = file(CompleteStr("load $0 #10"));

        assert!(result.is_ok());

        let (rest, token) = result.unwrap();
        assert_eq!(
            token,
            vec![AssemblerInstruction {
                opcode: Token::Op(opcode::LOAD),
                operand1: Some(Token::Register(0)),
                operand2: Some(Token::Number(10)),
                operand3: None
            }]
        );
    }
}
