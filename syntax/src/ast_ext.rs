use crate::ast::{self};
use crate::{
    child_opt, children, AstChildren, AstNode,
    SyntaxKind::{self, *},
    SyntaxNode,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ElseBranch {
    Block(ast::BlockExpr),
    IfExpr(ast::IfExpr),
}

impl ElseBranch {
    pub fn expr(self) -> ast::Expr {
        match self {
            ElseBranch::Block(e) => ast::Expr::from(e),
            ElseBranch::IfExpr(e) => ast::Expr::from(e),
        }
    }
}

impl ast::ForExpr {
    pub fn init(&self) -> Option<ast::Stmt> {
        children(self).next()
    }

    pub fn cond(&self) -> Option<ast::Expr> {
        children(self).nth(1)
    }

    pub fn increment(&self) -> Option<ast::Expr> {
        children(self).nth(2)
    }
}

impl ast::BinExpr {
    pub fn lhs(&self) -> Option<ast::Expr> {
        children(self).next()
    }

    pub fn rhs(&self) -> Option<ast::Expr> {
        children(self).nth(1)
    }

    pub fn op_kind(&self) -> Option<SyntaxKind> {
        self.syntax()
            .children_with_tokens()
            .filter_map(|it| it.into_token())
            .skip_while(|it| it.kind().is_trivia())
            .find_map(|c| Some(c.kind()))
    }
}

impl ast::PrefixExpr {
    pub fn op_kind(&self) -> Option<SyntaxKind> {
        self.syntax()
            .children_with_tokens()
            .filter_map(|it| it.into_token())
            .find_map(|c| Some(c.kind()))
    }
}

impl ast::IndexExpr {
    pub fn base(&self) -> Option<ast::Expr> {
        children(self).next()
    }

    pub fn index(&self) -> Option<ast::Expr> {
        children(self).nth(1)
    }
}

impl ast::IfExpr {
    pub fn then_branch(&self) -> Option<ast::BlockExpr> {
        self.blocks().next()
    }
    pub fn else_branch(&self) -> Option<ElseBranch> {
        let res = match self.blocks().nth(1) {
            Some(block) => ElseBranch::Block(block),
            None => {
                let elif: ast::IfExpr = child_opt(self)?;
                ElseBranch::IfExpr(elif)
            }
        };
        Some(res)
    }

    pub fn blocks(&self) -> AstChildren<ast::BlockExpr> {
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
        match syntax.kind() {
            ARRAY_EXPR | PAREN_EXPR | CLOSURE_EXPR | IF_EXPR | FOR_EXPR | WHILE_EXPR
            | CONTINUE_EXPR | BREAK_EXPR | BLOCK_EXPR | RETURN_EXPR | MATCH_EXPR | CLASS_LIT
            | CALL_EXPR | INDEX_EXPR | FIELD_EXPR | CAST_EXPR | PREFIX_EXPR | BIN_EXPR
            | LITERAL => Some(ast::Stmt::ExprStmt(ast::ExprStmt { syntax })),
            _ => None,
        }
    }
}

impl ast::CallExpr {
    pub fn type_args(&self) -> Option<ast::TypeArgList> {
        if let Some(id_expr) = children::<_, ast::IdentExpr>(self).nth(0) {
            child_opt(&id_expr)
        } else {
            None
        }
    }
}

impl ast::BlockExpr {
    pub fn has_value(&self) -> bool {
        self.block().map(|b| b.has_value()).unwrap_or(false)
    }
}

impl ast::Block {
    pub fn has_value(&self) -> bool {
        let last = self.statements().last();

        if let Some(stmt) = last {
            match stmt {
                ast::Stmt::ExprStmt(expr) => {
                    if let Some(sibling_or_token) = expr.syntax().next_sibling_or_token() {
                        match sibling_or_token {
                            rowan::NodeOrToken::Node(_) => false,
                            rowan::NodeOrToken::Token(t) => t.kind() != T![;],
                        }
                    } else {
                        false
                    }
                }
                ast::Stmt::LetStmt(_) => false,
            }
        } else {
            false
        }
    }
}
