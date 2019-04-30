use crate::ast as t;
use crate::ctx::CompileCtx;

use crate::infer::{Infer, InferResult};
use syntax::ast::Expression;
use util::pos::{Span, Spanned};

impl Infer {
    pub(crate) fn infer_grouping(
        &mut self,
        expr: Spanned<Expression>,
        whole_span: Span,
        ctx: &mut CompileCtx,
    ) -> InferResult<Spanned<t::TypedExpression>> {
        let span = expr.span;
        let ty_expr = self.infer_expr(expr, ctx)?;
        let ty = ty_expr.value.ty.clone();

        Ok(Spanned::new(
            t::TypedExpression {
                expr: Box::new(Spanned::new(t::Expression::Grouping(ty_expr), span)),
                ty,
            },
            whole_span,
        ))
    }
}
