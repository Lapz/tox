use crate::{
    hir::{BlockId, FunctionAstMap},
    infer::{InferDataCollector, Type, TypeCon},
    typed, HirDatabase,
};
impl<'a, DB> InferDataCollector<&'a DB>
where
    DB: HirDatabase,
{
    pub(crate) fn infer_block(
        &mut self,
        map: &FunctionAstMap,
        block_id: &BlockId,
        has_value: bool,
    ) -> typed::Typed<typed::Expr> {
        unimplemented!(); // Todo
                          // self.env.begin_scope();

        // let block = map.block(block_id);

        // let mut returns = Type::Con(TypeCon::Void);

        // for (index, stmt) in block.0.iter().enumerate() {
        //     let ty = self.infer_statement(stmt, map);
        //     if has_value && index == block.0.len() - 1 {
        //         returns = ty;
        //     }
        // }

        // self.env.begin_scope();

        // returns
    }
}
