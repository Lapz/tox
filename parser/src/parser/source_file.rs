use crate::ast::*;
use crate::parser::Parser;
use rowan::GreenNodeBuilder;
use syntax::T;

use crate::{AstNode, SyntaxKind::*, SyntaxNode};

impl<'a> Parser<'a> {
    pub fn parse_program(&mut self) -> SourceFile {
        self.start_node(SOURCE_FILE);

        while !self.at(EOF) && !self.at(ERROR) {
            let has_visibility = self.has_visibility();
            let checkpoint = self.checkpoint();

            if has_visibility {
                self.parse_visibility();
            }

            match self.current() {
                T![type] => self.parse_type_alias(checkpoint),
                T![fn] => {
                    self.parse_function(checkpoint);
                }
                T![enum] => self.parse_enum(checkpoint),
                T![class] => self.parse_class(checkpoint),
                T!["//"] => {
                    self.bump();
                    continue;
                }
                _ => {
                    self.recover();
                }
            }
        }

        self.finish_node();

        let mut _builder = GreenNodeBuilder::new();

        std::mem::swap(&mut self.builder, &mut _builder);

        let green = _builder.finish();

        let root = SyntaxNode::new_root(green);

        SourceFile::cast(root).unwrap()
    }

    pub(crate) fn has_visibility(&mut self) -> bool {
        match self.current() {
            T![export] => true,
            _ => false,
        }
    }
}
