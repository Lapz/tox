use crate::ast::SyntaxKind;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: SyntaxKind,
    pub len: u32,
}

impl Token {
    pub const fn new(kind: SyntaxKind, len: u32) -> Self {
        Token { kind, len }
    }
}
