use assembler::token::Token;
use assembler::{AssemblerInstruction, Program};
use nom::types::CompleteStr;
use nom::{alpha1, alphanumeric, digit, multispace};
use opcode;

fn not_line_end(ch: char) -> bool {
    !(ch == '\n')
}

named!(operand<CompleteStr,Token>,
    alt!(
        integer_operand
        | string_operand
        | register
        | label_usage
    )
);

named!(opcode<CompleteStr,Token>,
    do_parse!(
        opcode:alpha1 >>
        (
            Token::Op(u8::opcode(opcode))
        )
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

named!(string_operand<CompleteStr,Token>,
    ws!(
        do_parse!(
            alt!(tag!("\'")| tag!("\"")) >>
            string:alt!(take_until!("'") | take_until!("\"")) >>
            alt!(tag!("\'")| tag!("\"")) >>
            (
                Token::String(string.to_string())
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

named!(comment<CompleteStr,()>,
    do_parse!(
        tag!("//") >>
        take_while!(not_line_end) >> ()
    )
);

named!(directive_declaration<CompleteStr,Token>,
    do_parse!(
            tag!(".") >>
            name:alpha1 >>
            ( Token::Directive(name.to_string()))
    )
);

named!(label_declaration<CompleteStr,Token>,
    ws!(
        do_parse!(
            name: alphanumeric >>
            tag!(":")         >>
            ( Token::LabelDeclaration(name.to_string()))
        )
    )
);

named!(label_usage<CompleteStr,Token>,
    ws!(
        do_parse!(
            tag!("@")          >>
            name: alphanumeric >>
            opt!(multispace)   >>
            (
                Token::LabelUsage(name.to_string())
            )
        )
    )
);

/// Handles instructions of the following form:
/// LOAD $0 #100
named!(instruction_combined<CompleteStr,AssemblerInstruction>,
    do_parse!(
        label:opt!(label_declaration) >>
        opcode: opcode >>
        operand1: opt!(operand) >>
        operand2:  opt!(operand) >>
        operand3: opt!(operand) >>
        (
            AssemblerInstruction {
                label,
                opcode:Some(opcode),
                operand1,
                operand2,
                operand3,
                directive:None,
            }
        )
    )
);

named!(directive_combined<CompleteStr,AssemblerInstruction>,
    ws!(
        do_parse!(
            label: opt!(label_declaration) >>
            name: directive_declaration >>
            operand1: opt!(operand) >>
            operand2:  opt!(operand) >>
            operand3: opt!(operand) >>
            (
                AssemblerInstruction {
                    label,
                    directive:Some(name),
                    opcode:None,
                    operand1,
                    operand2,
                    operand3,
                }
            )
        )
    )
);

named!(directive<CompleteStr,AssemblerInstruction>,
    do_parse!(
        inst : directive_combined >>
        opt!(comment) >>

        (inst)
    )
);

named!(instruction<CompleteStr,AssemblerInstruction>,
    do_parse!(
        inst:instruction_combined >>
        opt!(comment) >>
        (inst)
    )
);

named!(pub file<CompleteStr,Program>,
    do_parse!(
        instructions: ws!(
                many1!(
                    alt!(instruction | directive)
                )
        )
        >> (
            Program {
                instructions
            }
        )
    )
);

pub trait FromInput<T> {
    fn opcode(v: T) -> Self;
}

impl<'a> FromInput<CompleteStr<'a>> for u8 {
    fn opcode(v: CompleteStr<'a>) -> Self {
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
            CompleteStr("jmpeq") => opcode::JMPEQ,
            CompleteStr("jmpneq") => opcode::JMPNEQ,
            CompleteStr("store") => opcode::STORE,
            CompleteStr("alloc") => opcode::ALLOC,
            CompleteStr("free") => opcode::FREE,
            CompleteStr("inc") => opcode::INC,
            CompleteStr("push") => opcode::PUSH,
            CompleteStr("pop") => opcode::POP,
            CompleteStr("mod") => opcode::MOD,
            CompleteStr("expon") => opcode::EXPON,
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
            CompleteStr("JMPEQ") => opcode::JMPEQ,
            CompleteStr("JMPNEQ") => opcode::JMPNEQ,
            CompleteStr("STORE") => opcode::STORE,
            CompleteStr("ALLOC") => opcode::ALLOC,
            CompleteStr("FREE") => opcode::FREE,
            CompleteStr("INC") => opcode::INC,
            CompleteStr("PUSH") => opcode::PUSH,
            CompleteStr("POP") => opcode::POP,
            CompleteStr("MOD") => opcode::MOD,
            CompleteStr("EXPON") => opcode::EXPON,
            CompleteStr("SET") => opcode::SET,
            CompleteStr("PRNT") => opcode::PRNT,
            ref e => {
                println!("Unkown opcode: {:?}",e);
                opcode::IGL
            },
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]

    fn parse_string_directive() {
        let result = directive_combined(CompleteStr("test:.asciiz 'Hello'"));

        assert!(result.is_ok());

        let (_, program) = result.unwrap();

        let expected = AssemblerInstruction {
            opcode: None,
            label: Some(Token::LabelDeclaration("test".into())),
            directive: Some(Token::Directive("asciiz".into())),
            operand1: Some(Token::String("Hello".into())),
            operand2: None,
            operand3: None,
        };

        assert_eq!(program, expected);
    }

    #[test]
    fn parse_string() {
        let result = string_operand(CompleteStr("\"Hello World\""));

        assert!(result.is_ok());

        let (_, token) = result.unwrap();

        assert_eq!(token, Token::String("Hello World".into()));

        let result = string_operand(CompleteStr("'This is a test'"));

        assert!(result.is_ok());

        let (_, token) = result.unwrap();

        assert_eq!(token, Token::String("This is a test".into()))
    }

    #[test]
    fn parse_load() {
        let result = opcode(CompleteStr("load"));
        assert!(result.is_ok());

        let result = opcode(CompleteStr("LOAD"));
        assert!(result.is_ok());

        let (_, token) = result.unwrap();
        assert_eq!(token, Token::Op(opcode::LOAD));

        let result = opcode(CompleteStr("aold"));
        let (_, token) = result.unwrap();
        assert_eq!(token, Token::Op(opcode::IGL));
    }

    #[test]
    fn parse_register() {
        let result = register(CompleteStr("$10"));

        assert!(result.is_ok());

        let (_, token) = result.unwrap();
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

        let (_, token) = result.unwrap();
        assert_eq!(token, Token::Number(10));

        let result = integer_operand(CompleteStr("10"));
        assert!(result.is_err());
    }

    #[test]
    fn parse_label_declaration() {
        let result = label_declaration(CompleteStr("test:"));

        assert!(result.is_ok());

        let (_, token) = result.unwrap();

        assert_eq!(token, Token::LabelDeclaration("test".into()));

        let result = label_declaration(CompleteStr("test"));

        assert!(result.is_err());
    }
    #[test]
    fn parse_label_usage() {
        let result = label_usage(CompleteStr("@test"));

        assert!(result.is_ok());

        let (_, token) = result.unwrap();

        assert_eq!(token, Token::LabelUsage("test".into()));

        let result = label_usage(CompleteStr("test"));

        assert!(result.is_err());
    }

    #[test]
    fn parse_complete_program() {
        let result = file(CompleteStr(
            ".data\nhello: .asciiz 'Hello everyone!'\n.code\nhlt",
        ));
        assert!(result.is_ok())
    }

    #[test]
    fn parse_file() {
        let result = file(CompleteStr("LOAD $0 #10"));

        assert!(result.is_ok());

        let (_, program) = result.unwrap();

        let instructions = vec![AssemblerInstruction {
            opcode: Some(Token::Op(opcode::LOAD)),
            operand1: Some(Token::Register(0)),
            operand2: Some(Token::Number(10)),
            operand3: None,
            directive: None,
            label: None,
        }];

        assert_eq!(program, Program { instructions });

        let result = file(CompleteStr("load $0 #10"));

        assert!(result.is_ok());

        let instructions = vec![AssemblerInstruction {
            opcode: Some(Token::Op(opcode::LOAD)),
            operand1: Some(Token::Register(0)),
            operand2: Some(Token::Number(10)),
            operand3: None,
            directive: None,
            label: None,
        }];

        let (_, program) = result.unwrap();
        assert_eq!(program, Program { instructions });
    }
}
