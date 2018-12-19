use ast as t;
use ctx::CompileCtx;
use infer::env::VarEntry;
use infer::types::{Type, TypeCon, TypeVar};
use infer::{Infer, InferResult};
use syntax::ast::{AssignOperator, Expression, Function, Literal, Op, UnaryOp};
use util::pos::{Span, Spanned};
use util::symbol::Symbol;

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
