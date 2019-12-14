#[macro_use]
mod macros;

pub mod ast;
mod lexer;

mod ast_ext;
mod token;
mod traits;
pub use ast::SyntaxKind;
pub use lexer::Lexer;
pub use rowan::{SmolStr, TextRange};
pub use token::Token;
pub use traits::*;

pub type SyntaxNode = rowan::SyntaxNode<ToxLang>;
#[allow(unused)]
pub type SyntaxToken = rowan::SyntaxToken<ToxLang>;
#[allow(unused)]
pub type SyntaxElement = rowan::NodeOrToken<SyntaxNode, SyntaxToken>;

pub type SyntaxNodeChildren = rowan::SyntaxNodeChildren<ToxLang>;
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ToxLang {}
impl rowan::Language for ToxLang {
    type Kind = SyntaxKind;
    fn kind_from_raw(raw: rowan::cursor::SyntaxKind) -> Self::Kind {
        assert!(raw.0 <= SyntaxKind::__LAST as u16);
        unsafe { std::mem::transmute::<u16, SyntaxKind>(raw.0) }
    }
    fn kind_to_raw(kind: Self::Kind) -> rowan::cursor::SyntaxKind {
        kind.into()
    }
}
