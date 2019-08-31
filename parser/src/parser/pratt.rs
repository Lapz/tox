use crate::{
    Span,
    SyntaxKind::{self, *},
    Token,
};
use std::fmt::Debug;

use crate::parser::Parser;

pub trait Rule {
    fn rule(&self) -> RuleToken;
}
pub trait PrefixParser<I>: Debug
where
    I: Iterator<Item = Span<Token>>,
{
    fn parse(&self, parser: &mut Parser<I>);
}
pub trait InfixParser<I>: Debug
where
    I: Iterator<Item = Span<Token>>,
{
    fn parse(&self, parser: &mut Parser<I>, checkpoint: rowan::Checkpoint);
    fn pred(&self) -> Precedence;
}

#[derive(Debug, Clone, Copy, PartialOrd, PartialEq)]
pub enum Precedence {
    None,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
}

#[derive(Debug, Eq, Hash, PartialEq, Clone, Copy)]
pub enum RuleToken {
    LParen,
    LBracket,
    Minus,
    Plus,
    Slash,
    Star,
    Literal,
    None,
    Excl,
    Comparison,
    Equality,
    This,
    And,
    Or,
}

impl Precedence {
    pub fn higher(&self) -> Precedence {
        match *self {
            Precedence::None | Precedence::Assignment => Precedence::Or,
            Precedence::Or => Precedence::And,
            Precedence::And => Precedence::Equality,
            Precedence::Equality => Precedence::Comparison,
            Precedence::Comparison => Precedence::Term,
            Precedence::Term => Precedence::Factor,
            Precedence::Factor => Precedence::Unary,
            Precedence::Unary => Precedence::Call,
            Precedence::Call => Precedence::Primary,
            Precedence::Primary => Precedence::Primary,
        }
    }
}

impl Rule for SyntaxKind {
    fn rule(&self) -> RuleToken {
        match self {
            INT_NUMBER | FLOAT_NUMBER | STRING | T![nil] | T![true] | T![false] => {
                RuleToken::Literal
            }
            T![+] => RuleToken::Plus,
            T![!] => RuleToken::Excl,
            T![-] => RuleToken::Minus,
            T!["("] => RuleToken::LParen,
            _ => RuleToken::None,
        }
    }
}
