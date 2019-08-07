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

    pub(crate) fn parse_function(&mut self) {
        self.start_node(FN_DEF);

        if self.peek(|t| t == T![extern]) {
            self.bump();
        }

        self.expect(FN_KW, "Expected `fn`");

        self.expect(IDENT, "Expected and identifier");

        if self.peek(|t| t == L_ANGLE) {
            self.parse_type_params();
        }

        if self.peek(|t| t == T!["("]) {
            self.parse_func_params();
        }

        // self.parse_func_params();

        // self.parse_block();

        self.finish_node()
    }
}
