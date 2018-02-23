use std::fmt::{Display, Formatter};
use std::fmt;

/// A Token is spat out by the lexer.
/// It contains a Type and the position and column it is found in in the lexer
#[derive(Debug, Clone, PartialEq)]
pub struct Token<'a> {
    pub token: TokenType<'a>,
}

impl<'a> Display for Token<'a> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "\'{}\'", self.token)
    }
}

impl<'a> Display for TokenType<'a> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            TokenType::EOF => write!(f, "\0"),
            TokenType::IDENTIFIER(s) => write!(f, "{}", s),
            TokenType::INT(ref i) => write!(f, "{}", i),
            TokenType::FLOAT(ref float) => write!(f, "{}", float),
            TokenType::ASSIGN => write!(f, "="),
            TokenType::STARASSIGN => write!(f, "*="),
            TokenType::PLUSASSIGN => write!(f, "+="),
            TokenType::MINUSASSIGN => write!(f, "-="),
            TokenType::SLASHASSIGN => write!(f, "/="),
            TokenType::PLUS => write!(f, "+"),
            TokenType::MINUS => write!(f, "-"),
            TokenType::BANG => write!(f, "!"),
            TokenType::STAR => write!(f, "*"),
            TokenType::SLASH => write!(f, "\\"),
            TokenType::MODULO => write!(f, "%"),
            TokenType::EXPONENTIAL => write!(f, "^"),
            TokenType::DOT => write!(f, "."),
            TokenType::COLON => write!(f, ":"),
            TokenType::QUESTION => write!(f, "?"),
            TokenType::LESSTHAN => write!(f, "<"),       // <
            TokenType::GREATERTHAN => write!(f, ">"),    // >
            TokenType::EQUALEQUAL => write!(f, "=="),    // ==
            TokenType::BANGEQUAL => write!(f, "!="),     // !=
            TokenType::LESSTHANEQUAL => write!(f, "<="), // <=
            TokenType::GREATERTHANEQUAL => write!(f, "=>"), // =>
            TokenType::STRING(ref s) => write!(f, "{:?}", s),
            TokenType::COMMA => write!(f, ","),     // ,
            TokenType::COMMENT => write!(f, "//"),  // //
            TokenType::SEMICOLON => write!(f, ";"), //
            TokenType::LPAREN => write!(f, "("),    // (
            TokenType::RPAREN => write!(f, ")"),    // )
            TokenType::LBRACKET => write!(f, "["),  // [
            TokenType::RBRACKET => write!(f, "]"),  // ]
            TokenType::LBRACE => write!(f, "{{"),   // {
            TokenType::RBRACE => write!(f, "}}"),   // }
            TokenType::FRETURN => write!(f, "->"),
            // Keywords,
            TokenType::FUNCTION => write!(f, "fun"),
            TokenType::PRINT => write!(f, "print"),
            TokenType::TYPE => write!(f, "type"),
            TokenType::BREAK => write!(f, "break"),
            TokenType::CONTINUE => write!(f, "continue"),
            TokenType::VAR => write!(f, "var"),
            TokenType::IF => write!(f, "if"),
            TokenType::DO => write!(f, "do"),
            TokenType::ELSE => write!(f, "else"),
            TokenType::RETURN => write!(f, "return"),
            TokenType::TRUE(_) => write!(f, "true"),
            TokenType::FALSE(_) => write!(f, "false"),
            TokenType::THIS => write!(f, "this"),
            TokenType::CLASS => write!(f, "class"),
            TokenType::FOR => write!(f, "for"),
            TokenType::WHILE => write!(f, "while"),
            TokenType::AND => write!(f, "and"),
            TokenType::OR => write!(f, "or"),
            TokenType::NIL => write!(f, "nil"),
            TokenType::EXTERN => write!(f, "extern"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)] // Allows the prinitng of the token
pub enum TokenType<'a> {
    // All the differnet token types
    IDENTIFIER(&'a str),
    INT(i64),
    FLOAT(f64),
    STRING(Vec<u8>),

    // Assignment
    ASSIGN,      // =
    PLUSASSIGN,  // +=
    MINUSASSIGN, // -=
    STARASSIGN,  // *=
    SLASHASSIGN, // /=
    // Operators
    PLUS,        // +
    MINUS,       // -
    BANG,        // !
    STAR,        // *
    SLASH,       // /
    MODULO,      // %
    EXPONENTIAL, // ^

    // Puntuation
    FRETURN,   // ->
    DOT,       // .
    QUESTION,  // ?
    COLON,     // :
    COMMA,     // ,
    COMMENT,   // //
    SEMICOLON, // ;
    LPAREN,    // (
    RPAREN,    // )
    LBRACKET,  // [
    RBRACKET,  // ]
    LBRACE,    // {
    RBRACE,    // }

    // Comparison
    LESSTHAN,         // <
    GREATERTHAN,      // >
    EQUALEQUAL,       // ==
    BANGEQUAL,        // !=
    LESSTHANEQUAL,    // <=
    GREATERTHANEQUAL, // =>
    // Keywords,
    FUNCTION,
    BREAK,
    CONTINUE,
    VAR,
    IF,
    DO,
    ELSE,
    RETURN,
    TRUE(bool),
    FALSE(bool),
    PRINT,
    THIS,
    CLASS,

    FOR,
    WHILE,
    AND,
    OR,
    NIL,
    TYPE,
    EXTERN,

    // Other
    EOF,
}
