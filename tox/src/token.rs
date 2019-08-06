use crate::pos::{CharPosition, Position};

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: SyntaxKind,
    pub len: u32,
}

impl Token {
    pub const fn new(kind: SyntaxKind, len: u32) -> Self {
        Token { kind, len }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)] // Allows the printing of the token
#[repr(u16)]
pub enum SyntaxKind {
    // All the different token types
    IDENTIFIER,
    INT_NUMBER,
    FLOAT_NUMBER,
    STRING,
    CHAR,

    // Assignment
    ASSIGN,       // =
    PLUS_ASSIGN,  // +=
    MINUS_ASSIGN, // -=
    STAR_ASSIGN,  // *=
    SLASH_ASSIGN, // /=
    // Operators
    PLUS,        // +
    MINUS,       // -
    BANG,        // !
    STAR,        // *
    SLASH,       // /
    MODULO,      // %
    EXPONENTIAL, // ^

    // Puntuation
    FRETURN,     // ->
    DOT,         // .
    QUESTION,    // ?
    COLON,       // :
    COMMA,       // ,
    COMMENT,     // //
    SEMICOLON,   // ;
    L_PAREN,     // (
    R_PAREN,     // )
    L_BRACKET,   // [
    R_BRACKET,   // ]
    L_BRACE,     // {
    R_BRACE,     // }
    BAR,         // |
    COLON_COLON, // ::
    MATCHARROW,  // =>
    UNDERSCORE,  // _

    // Comparison
    LESS,        // <
    GREATERTHAN, // >
    EQUALEQUAL,  // ==
    BANG_EQUAL,  // !=
    LESS_EQUAL,  // <=
    GREATER_EQUAL,
    GREATER,   // >
    FAT_ARROW, // =>
    // Keywords,
    FUNCTION,
    BREAK,
    CONTINUE,
    LET,
    IF,
    DO,
    ELSE,
    RETURN,
    TRUE,
    FALSE,
    PRINT,
    CLASS,
    MATCH,
    EXTENDS,
    EXPORT,

    FOR,
    WHILE,
    AND,
    OR,
    NIL,
    TYPE,
    AS,
    ENUM,

    // Other
    EOF,
    ROOT,
    ERROR,

    NODE_FUNCTION,
}

use SyntaxKind::*;
impl From<SyntaxKind> for rowan::cursor::SyntaxKind {
    fn from(kind: SyntaxKind) -> Self {
        Self(kind as u16)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Lang {}

impl rowan::Language for Lang {
    type Kind = SyntaxKind;
    fn kind_from_raw(raw: rowan::cursor::SyntaxKind) -> Self::Kind {
        assert!(raw.0 <= ROOT as u16);
        unsafe { std::mem::transmute::<u16, SyntaxKind>(raw.0) }
    }
    fn kind_to_raw(kind: Self::Kind) -> rowan::cursor::SyntaxKind {
        kind.into()
    }
}
