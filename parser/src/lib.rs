#[macro_use]
mod macros;
mod parser;
pub(crate) use syntax::{ast, AstNode, Span, SyntaxKind, SyntaxNode, Token};
pub use parser::Parser;