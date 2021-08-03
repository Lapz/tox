use crate::{
    hir::{BinOp, ExprId, FunctionAstMap},
    infer::{InferDataCollector, Type, TypeCon},
    typed,
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
    ) -> typed::Typed<typed::Expr> {
        let inferred_lhs = self.infer_expr(map, lhs);
        let lhs_ty = inferred_lhs.ty.clone();
        let inferred_rhs = self.infer_expr(map, rhs);
        let rhs_ty = inferred_rhs.ty.clone();
        //  TODO check if doing operations on ints and floats only
        let ty = match op {
            BinOp::Plus | BinOp::Minus | BinOp::Mult | BinOp::Div => {
                self.unify(
                    &lhs_ty,
                    &rhs_ty,
                    id.as_reporter_span(),
                    Some("`+`,`-`,`*`,`/` operators only works on i32 and f32".into()),
                    true,
                );
                lhs_ty
            }
            BinOp::And | BinOp::Or => {
                self.unify(
                    &Type::Con(TypeCon::Bool),
                    &lhs_ty,
                    lhs.as_reporter_span(),
                    Some("Expected a bool".into()),
                    true,
                );
                self.unify(
                    &Type::Con(TypeCon::Bool),
                    &rhs_ty,
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
                    &lhs_ty,
                    &rhs_ty,
                    id.as_reporter_span(),
                    Some("Comparison operators only work on numbers".into()),
                    true,
                );
                Type::Con(TypeCon::Bool)
            }
            BinOp::Equal => {
                self.unify(&lhs_ty, &rhs_ty, id.as_reporter_span(), None, true);

                rhs_ty
            }
            BinOp::EqualEqual | BinOp::NotEqual => Type::Con(TypeCon::Bool),
            BinOp::PlusEqual | BinOp::MinusEqual | BinOp::MultEqual | BinOp::DivEqual => {
                self.unify(&lhs_ty, &rhs_ty, id.as_reporter_span(), None, true);

                rhs_ty
            }
        };

        typed::Typed::new(
            typed::Expr::Binary {
                lhs: Box::new(inferred_lhs),
                op: *op,
                rhs: Box::new(inferred_rhs),
            },
            ty,
            (id.start, id.end),
        )
    }
}
