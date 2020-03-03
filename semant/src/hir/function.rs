use super::{
    Block, BlockId, Expr, ExprId, NameId, Param, ParamId, PatId, Pattern, Span, Stmt, StmtId,
    TypeParam, TypeParamId,
};
use indexmap::IndexMap;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use syntax::{ast, AstPtr};

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct Function {
    pub(crate) exported: bool,
    pub(crate) name: NameId,
    pub(crate) map: FunctionAstMap,
    pub(crate) params: Vec<ParamId>,
    pub(crate) type_params: Vec<TypeParamId>,
    pub(crate) body: Option<Vec<StmtId>>,
    pub(crate) span: Span,
}

#[derive(Debug, Default, Eq, PartialEq, Clone)]
pub(crate) struct FunctionAstMap {
    ast_to_pattern: IndexMap<PatId, AstPtr<ast::Pat>>,
    hir_to_pattern: IndexMap<PatId, Pattern>,
    hir_to_params: IndexMap<ParamId, Param>,
    ast_to_params: IndexMap<ParamId, AstPtr<ast::Param>>,
    hir_to_type_params: IndexMap<TypeParamId, TypeParam>,
    ast_to_type_params: IndexMap<TypeParamId, AstPtr<ast::TypeParam>>,
    hir_to_stmt: IndexMap<StmtId, Stmt>,
    ast_to_stmt: IndexMap<StmtId, AstPtr<ast::Stmt>>,
    hir_to_expr: IndexMap<ExprId, Expr>,
    ast_to_expr: IndexMap<ExprId, AstPtr<ast::Expr>>,
    hir_to_block: IndexMap<BlockId, Block>,
    ast_to_block: IndexMap<BlockId, AstPtr<ast::Block>>,
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

    pub fn insert_block(&mut self, id: BlockId, block: Block, node: AstPtr<ast::Block>) {
        self.hir_to_block.insert(id, block);
        self.ast_to_block.insert(id, node);
    }

    pub fn insert_pat(&mut self, id: PatId, pat: Pattern, node: AstPtr<ast::Pat>) {
        self.hir_to_pattern.insert(id, pat);
        self.ast_to_pattern.insert(id, node);
    }

    pub fn get_expr_ptr(&self, id: ExprId) -> AstPtr<ast::Expr> {
        self.ast_to_expr[&id]
    }

    pub(crate) fn stmt(&self, id: &StmtId) -> &Stmt {
        self.hir_to_stmt.get(id).unwrap()
    }

    pub(crate) fn expr(&self, id: &ExprId) -> &Expr {
        self.hir_to_expr.get(id).unwrap()
    }

    pub(crate) fn expr_span(&self, id: &ExprId) -> Span {
        self.ast_to_expr[id].syntax_node_ptr().range()
    }

    pub(crate) fn block(&self, id: &BlockId) -> &Block {
        self.hir_to_block.get(id).unwrap()
    }

    pub(crate) fn pat(&self, id: &PatId) -> &Pattern {
        &self.hir_to_pattern[id]
    }

    pub(crate) fn pattern_span(&self, id: &PatId) -> Span {
        self.ast_to_pattern[id].syntax_node_ptr().range()
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

macro_rules! hash {
    ($state:expr => $( $field:expr ),*) => {
        {
            $(
            $state.write_u64(
            $field
                .values()
                .map(|kv| {
                    let mut h = DefaultHasher::new();
                    kv.hash(&mut h);
                    h.finish()
                })
                .fold(0, u64::wrapping_add),
            );
        )*
        }
    };

}

impl Hash for FunctionAstMap {
    fn hash<H: Hasher>(&self, state: &mut H) {
        hash!(state => self.hir_to_params,
            self.ast_to_type_params,
            self.hir_to_stmt,
            self.ast_to_stmt,
            self.hir_to_expr,
            self.ast_to_expr,
            self.hir_to_block,
            self.ast_to_block
        )
    }
}

impl Function {
    pub(crate) fn body(&self) -> &Option<Vec<StmtId>> {
        &self.body
    }

    pub(crate) fn map(&self) -> &FunctionAstMap {
        &self.map
    }
}
