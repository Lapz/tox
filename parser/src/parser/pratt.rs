use crate::SyntaxKind::{self, *};
use std::any::Any;
use std::fmt::Debug;
use syntax::T;

use crate::parser::Parser;

pub trait Rule {
    fn rule(&self) -> RuleToken;
}
pub trait PrefixParser {
    fn parse(&self, parser: &mut Parser<'_>);
}
pub trait InfixParser: Debug + Any {
    fn parse(&self, parser: &mut Parser<'_>, checkpoint: rowan::Checkpoint);
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
    LBrace,
    Ident,
    Dot,
    LBracket,
    Minus,
    Plus,
    Slash,
    Star,
    Literal,
    Pipe,
    None,
    Excl,
    Comparison,
    Eq,
    EqEq,
    PlusEq,
    MinusEq,
    StarEq,
    SlashEq,
    AmpAmp,
    PipePipe,
}

impl Precedence {
    pub fn higher(self) -> Precedence {
        match self {
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
            IDENT => RuleToken::Ident,
            T![=] => RuleToken::Eq,
            T![.] => RuleToken::Dot,
            T![+=] => RuleToken::PlusEq,
            T![-=] => RuleToken::MinusEq,
            T![/=] => RuleToken::SlashEq,
            T![*=] => RuleToken::MinusEq,
            T![+] => RuleToken::Plus,
            T![!] => RuleToken::Excl,
            T![-] => RuleToken::Minus,
            T!["("] => RuleToken::LParen,
            T!["["] => RuleToken::LBracket,
            T!["{"] => RuleToken::LBrace,
            T![!=] | T![<] | T![>] | T![<=] | T![>=] => RuleToken::Comparison,
            T![&&] => RuleToken::AmpAmp,
            T![||] => RuleToken::PipePipe,
            T![|] => RuleToken::Pipe,
            _ => RuleToken::None,
        }
    }
}
