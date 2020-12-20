use crate::{
    hir::{BinOp, ExprId, FunctionAstMap},
    infer::{InferDataCollector, Type, TypeCon},
    util::Span,
    HirDatabase,
};
impl<'a, DB> InferDataCollector<&'a DB>
where
    DB: HirDatabase,
{
    pub(crate) fn infer_binary(
        &mut self,
        id: &Span<ExprId>,
        lhs: &Span<ExprId>,
        op: &BinOp,
        rhs: &Span<ExprId>,
        map: &FunctionAstMap,
    ) -> Type {
        let inferred_lhs = self.infer_expr(map, lhs);
        let inferred_rhs = self.infer_expr(map, rhs);

        match op {
            BinOp::Plus | BinOp::Minus | BinOp::Mult | BinOp::Div => {
                self.unify(
                    &inferred_lhs,
                    &inferred_rhs,
                    id.as_reporter_span(),
                    Some("`+`,`-`,`*`,`/` operators only works on i32 and f32".into()),
                    true,
                );
                inferred_lhs
            }
            BinOp::And | BinOp::Or => {
                self.unify(
                    &inferred_lhs,
                    &Type::Con(TypeCon::Bool),
                    lhs.as_reporter_span(),
                    Some("Expected a bool".into()),
                    true,
                );
                self.unify(
                    &inferred_rhs,
                    &Type::Con(TypeCon::Bool),
                    rhs.as_reporter_span(),
                    Some("Expected a bool".into()),
                    true,
                );

                Type::Con(TypeCon::Bool)
            }
            BinOp::LessThan
            | BinOp::GreaterThan
            | BinOp::GreaterThanEqual
            | BinOp::LessThanEqual => {
                self.unify(
                    &inferred_lhs,
                    &inferred_rhs,
                    id.as_reporter_span(),
                    Some("Comparison operators only work on numbers".into()),
                    true,
                );
                Type::Con(TypeCon::Bool)
            }
            BinOp::Equal => {
                self.unify(
                    &inferred_lhs,
                    &inferred_rhs,
                    id.as_reporter_span(),
                    None,
                    true,
                );

                inferred_rhs
            }
            BinOp::EqualEqual | BinOp::NotEqual => Type::Con(TypeCon::Bool),
            BinOp::PlusEqual | BinOp::MinusEqual | BinOp::MultEqual | BinOp::DivEqual => {
                self.unify(
                    &inferred_lhs,
                    &inferred_rhs,
                    id.as_reporter_span(),
                    None,
                    true,
                );

                inferred_rhs
            }
        }
    }
}
