use crate::ast::*;
use crate::macros::*;
use crate::parser::Parser;
use rowan::GreenNodeBuilder;

use crate::{
    AstNode, Span,
    SyntaxKind::{self, *},
    SyntaxNode, Token,
};

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = Span<Token>>,
{
    pub fn parse_program(&mut self) -> SourceFile {
        self.start_node(SOURCE_FILE);

        self.start_node(ITEM_LIST);

        self.parse_function();

        self.finish_node();
        self.finish_node();

        let mut _builder = GreenNodeBuilder::new();

        std::mem::swap(&mut self.builder, &mut _builder);

        let green = _builder.finish();

        let root = SyntaxNode::new_root(green);

        SourceFile::cast(root).unwrap()
    }
}
