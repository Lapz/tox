use crate::ast::*;
use crate::parser::Parser;
use crate::T;
use rowan::GreenNodeBuilder;

use crate::{AstNode, Span, SyntaxKind::*, SyntaxNode, Token};

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = Span<Token>>,
{
    pub fn parse_program(&mut self) -> SourceFile {
        self.start_node(SOURCE_FILE);

        while !self.at(EOF) && !self.at(ERROR) {
            let has_visibility = self.has_visibility();

            if has_visibility {
                match self.peek() {
                    T![type] => self.parse_type_alias(has_visibility),
                    T![fn] => self.parse_function(has_visibility),
                    T![enum] => self.parse_enum(has_visibility),
                    _ => self.error("Expected `fn`| `type` | `enum` | `class`| extern`"),
                }
            } else {
                match self.current() {
                    T![type] => self.parse_type_alias(has_visibility),
                    T![fn] => self.parse_function(has_visibility),
                    T![enum] => self.parse_enum(has_visibility),
                    _ => self.error("Expected `fn`| `type` | `enum` | `class`| extern`"),
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

    fn has_visibility(&self) -> bool {
        match self.current() {
            T![export] => true,
            _ => false,
        }
    }
}
