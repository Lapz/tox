use super::{
    Block, BlockId, Expr, ExprId, Param, ParamId, PatId, Pattern, Stmt, StmtId, TypeParam,
    TypeParamId,
};
use indexmap::IndexMap;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use syntax::{ast, AstPtr};

/// A map of id's that are a local to an item
/// Allows one to go from id => hir::item
///  and from id => astPtr
#[derive(Debug, Default, Eq, PartialEq, Clone)]
pub(crate) struct FunctionAstMap {
    hir_to_pattern: IndexMap<PatId, Pattern>,
    hir_to_params: IndexMap<ParamId, Param>,
    hir_to_type_params: IndexMap<TypeParamId, TypeParam>,
    hir_to_block: IndexMap<BlockId, Block>,
    hir_to_stmt: IndexMap<StmtId, Stmt>,
    hir_to_expr: IndexMap<ExprId, Expr>,
    ast_to_expr: IndexMap<ExprId, AstPtr<ast::Expr>>,
}

impl FunctionAstMap {
    pub fn insert_param(&mut self, id: ParamId, param: Param) {
        self.hir_to_params.insert(id, param);
    }

    pub fn insert_type_param(&mut self, id: TypeParamId, param: TypeParam) {
        self.hir_to_type_params.insert(id, param);
    }

    pub fn insert_stmt(&mut self, id: StmtId, stmt: Stmt) {
        self.hir_to_stmt.insert(id, stmt);
    }

    pub fn insert_expr(&mut self, id: ExprId, expr: Expr) {
        self.hir_to_expr.insert(id, expr);
    }

    pub fn insert_block(&mut self, id: BlockId, block: Block) {
        self.hir_to_block.insert(id, block);
    }

    pub fn insert_pat(&mut self, id: PatId, pat: Pattern) {
        self.hir_to_pattern.insert(id, pat);
    }

    pub(crate) fn stmt(&self, id: &StmtId) -> &Stmt {
        &self.hir_to_stmt[id]
    }

    pub(crate) fn expr(&self, id: &ExprId) -> &Expr {
        &self.hir_to_expr[id]
    }

    pub(crate) fn block(&self, id: &BlockId) -> &Block {
        &self.hir_to_block[id]
    }

    pub(crate) fn pat(&self, id: &PatId) -> &Pattern {
        &self.hir_to_pattern[id]
    }

    pub(crate) fn type_param(&self, id: &TypeParamId) -> &TypeParam {
        &self.hir_to_type_params[id]
    }

    pub(crate) fn param(&self, id: &ParamId) -> &Param {
        &self.hir_to_params[id]
    }
}

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

            self.hir_to_stmt,
            self.hir_to_expr,
            self.hir_to_block,
            self.hir_to_pattern,
            self.hir_to_type_params,
            self.hir_to_params
        )
    }
}
