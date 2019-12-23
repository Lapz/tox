use crate::ast;
use crate::{
    children, AstChildren, AstNode,
    SyntaxKind::{self, *},
    SyntaxNode,
};

impl ast::BinExpr {
    pub fn lhs(&self) -> Option<ast::Expr> {
        children(self).nth(0)
    }

    pub fn rhs(&self) -> Option<ast::Expr> {
        children(self).nth(1)
    }

    pub fn op_kind(&self) -> Option<SyntaxKind> {
        self.syntax()
            .children_with_tokens()
            .filter_map(|it| it.into_token())
            .find_map(|c| return Some(c.kind()))
    }
}

impl ast::PrefixExpr {
    pub fn op_kind(&self) -> Option<SyntaxKind> {
        self.syntax()
            .children_with_tokens()
            .filter_map(|it| it.into_token())
            .find_map(|c| return Some(c.kind()))
    }
}

impl ast::IndexExpr {
    pub fn base(&self) -> Option<ast::Expr> {
        children(self).nth(0)
    }

    pub fn index(&self) -> Option<ast::Expr> {
        children(self).nth(1)
    }
}

impl ast::IfExpr {
    fn blocks(&self) -> AstChildren<ast::BlockExpr> {
        children(self)
    }
}

impl ast::Literal {
    pub fn token_kind(&self) -> crate::SyntaxToken {
        self.syntax().first_token().unwrap()
    }
}

impl ast::Stmt {
    pub fn from_expr(syntax: SyntaxNode) -> Option<ast::Stmt> {
        return match syntax.kind() {
            ARRAY_EXPR | PAREN_EXPR | CLOSURE_EXPR | IF_EXPR | FOR_EXPR | WHILE_EXPR
            | CONTINUE_EXPR | BREAK_EXPR | BLOCK_EXPR | RETURN_EXPR | MATCH_EXPR | CLASS_LIT
            | CALL_EXPR | INDEX_EXPR | FIELD_EXPR | CAST_EXPR | PREFIX_EXPR | BIN_EXPR
            | LITERAL => Some(ast::Stmt::ExprStmt(ast::ExprStmt { syntax })),
            _ => None,
        };
    }
}
