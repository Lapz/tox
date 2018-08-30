use opcode::OpCode;

#[derive(Debug, PartialEq)]
pub enum Token {
    Op(OpCode),
    Register(u8),
    Number(i32),
    LabelDeclaration(String),
    LabelUsage(String),
    Directive(String),
}
