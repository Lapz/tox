use ast as t;
use ctx::CompileCtx;
use infer::types::{Type, TypeCon};
use infer::{Infer, InferResult};
use syntax::ast::Expression;
use util::pos::{Span, Spanned};

impl Infer {
    pub(crate) fn infer_ternary(
        &mut self,
        condition: Spanned<Expression>,
        then_branch: Spanned<Expression>,
        else_branch: Spanned<Expression>,
        whole_span: Span,
        ctx: &mut CompileCtx,
    ) -> InferResult<Spanned<t::TypedExpression>> {
        let condition_span = condition.span;
        let cond_tyexpr = self.infer_expr(condition, ctx)?;

        self.unify(
            &Type::App(TypeCon::Bool, vec![]),
            &cond_tyexpr.value.ty,
            condition_span,
            ctx,
        )?;

        let body_span = then_branch.span.to(else_branch.span);
        let then_tyexpr = self.infer_expr(then_branch, ctx)?;
        let else_tyexpr = self.infer_expr(else_branch, ctx)?;

        self.unify(&then_tyexpr.value.ty, &else_tyexpr.value.ty, body_span, ctx)?;
        let ty = then_tyexpr.value.ty.clone();

        let (typed, ty) = (
            Spanned::new(
                t::Expression::Ternary(cond_tyexpr, then_tyexpr, else_tyexpr),
                whole_span,
            ),
            ty,
        );

        Ok(Spanned::new(
            t::TypedExpression {
                expr: Box::new(typed),
                ty,
            },
            whole_span,
        ))
    }
}
