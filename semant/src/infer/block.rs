use syntax::TextUnit;

use crate::{
    hir::{BlockId, FunctionAstMap},
    infer::InferDataCollector,
    typed, HirDatabase, Type, TypeCon,
};
impl<'a, DB> InferDataCollector<&'a DB>
where
    DB: HirDatabase,
{
    pub(crate) fn infer_block(
        &mut self,

        block_id: &BlockId,
        has_value: bool,
        span: (TextUnit, TextUnit),
        map: &FunctionAstMap,
    ) -> typed::Typed<typed::Expr> {
        self.env.begin_scope();

        let block = map.block(block_id);

        let mut stmts = Vec::with_capacity(block.0.len());
        let mut returns = Type::Con(TypeCon::Void);

        for (index, stmt) in block.0.iter().enumerate() {
            let stmt = self.infer_statement(stmt, map);
            if has_value && index == block.0.len() - 1 {
                returns = stmt.ty.clone();
            }

            stmts.push(stmt);
        }

        self.env.begin_scope();

        typed::Typed::new(typed::Expr::Block(stmts, has_value), returns, span)
    }
}
