use crate::parser::Parser;
use crate::{
    AstNode, Span,
    SyntaxKind::{self, *},
    SyntaxNode, Token,
};
use syntax::T;

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = Span<Token>>,
{
    pub(crate) fn parse_type_params(&mut self, allow_types: bool) {
        self.start_node(TYPE_PARAM_LIST);
        self.bump();

        while !self.at(EOF) && !self.at(T![>]) {
            if allow_types {
                self.start_node(TYPE_PARAM);
                self.parse_type();
                self.finish_node();
            } else {
                self.type_param();
            }

            if !self.at(T![>]) && !self.expected(T![,]) {
                break;
            }
        }

        self.expect(T![>], "Expected `>` to close type params");
        self.finish_node()
    }

    fn type_param(&mut self) {
        self.start_node(TYPE_PARAM);
        self.ident();
        self.finish_node();
    }
}
