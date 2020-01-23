use super::{
    Expr, ExprId, Name, Param, ParamId, PatId, Span, Stmt, StmtId, TypeParam, TypeParamId,
};
use indexmap::IndexMap;
use syntax::{ast, AstPtr};

#[derive(Debug, Eq, PartialEq)]
pub struct Function {
    pub(crate) name: Name,
    pub(crate) map: FunctionAstMap,
    pub(crate) params: Vec<ParamId>,
    pub(crate) type_params: Vec<TypeParamId>,
    pub(crate) body: Option<Vec<StmtId>>,
    pub(crate) span: Span,
}

#[derive(Debug, Default, Eq, PartialEq)]
pub(crate) struct FunctionAstMap {
    hir_to_params: IndexMap<ParamId, Param>,
    ast_to_params: IndexMap<ParamId, AstPtr<ast::Param>>,
    hir_to_type_params: IndexMap<TypeParamId, TypeParam>,
    ast_to_type_params: IndexMap<TypeParamId, AstPtr<ast::TypeParam>>,
    hir_to_stmt: IndexMap<StmtId, Stmt>,
    ast_to_stmt: IndexMap<StmtId, AstPtr<ast::Stmt>>,
    hir_to_expr: IndexMap<ExprId, Expr>,
    ast_to_expr: IndexMap<ExprId, AstPtr<ast::Expr>>,
}

impl FunctionAstMap {
    pub fn insert_param(&mut self, id: ParamId, param: Param, node: AstPtr<ast::Param>) {
        self.hir_to_params.insert(id, param);
        self.ast_to_params.insert(id, node);
    }

    pub fn insert_type_param(
        &mut self,
        id: TypeParamId,
        param: TypeParam,
        node: AstPtr<ast::TypeParam>,
    ) {
        self.hir_to_type_params.insert(id, param);
        self.ast_to_type_params.insert(id, node);
    }

    pub fn insert_stmt(&mut self, id: StmtId, stmt: Stmt, node: AstPtr<ast::Stmt>) {
        self.hir_to_stmt.insert(id, stmt);
        self.ast_to_stmt.insert(id, node);
    }

    pub fn insert_expr(&mut self, id: ExprId, expr: Expr, node: AstPtr<ast::Expr>) {
        self.hir_to_expr.insert(id, expr);
        self.ast_to_expr.insert(id, node);
    }

    pub fn get_expr_ptr(&self, id: ExprId) -> AstPtr<ast::Expr> {
        self.ast_to_expr[&id]
    }
}

struct BodyMap {
    exprs: IndexMap<ExprId, AstPtr<ast::Expr>>,
    stmts: IndexMap<StmtId, AstPtr<ast::Stmt>>,
    pattern: IndexMap<PatId, AstPtr<ast::Pat>>,
}

macro_rules! index_data {
    ($ident:ty,$field:ident,$id:ty,$data:path) => {
        impl std::ops::Index<$id> for $ident {
            type Output = $data;

            fn index(&self, id: $id) -> &Self::Output {
                &self.$field[&id]
            }
        }
    };
}

// index_data!(FunctionAstMap, ast_to_params, ParamId, AstPtr<ast::Param>);
index_data!(FunctionAstMap, hir_to_params, ParamId, Param);
index_data!(FunctionAstMap, hir_to_type_params, TypeParamId, TypeParam);
index_data!(BodyMap, exprs, ExprId, AstPtr<ast::Expr>);
index_data!(BodyMap, stmts, StmtId, AstPtr<ast::Stmt>);
index_data!(BodyMap, pattern, PatId, AstPtr<ast::Pat>);
