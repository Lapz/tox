use ast as t;
use ctx::CompileCtx;

use infer::{Infer, InferResult};
use ir::types::{Type, TypeCon};
use syntax::ast::{Expression, UnaryOp};
use util::pos::{Span, Spanned};

impl Infer {
    pub(crate) fn infer_unary(
        &mut self,
        op: Spanned<UnaryOp>,
        expr: Spanned<Expression>,
        whole_span: Span,
        ctx: &mut CompileCtx,
    ) -> InferResult<Spanned<t::TypedExpression>> {
        let expr = self.infer_expr(expr, ctx)?;

        let (typed, ty) = match op.value {
            UnaryOp::Bang => (
                Spanned::new(t::Expression::Unary(op.value, expr), whole_span),
                Type::App(TypeCon::Bool, vec![]),
            ),
            UnaryOp::Minus => {
                if !expr.value.ty.is_int() && !expr.value.ty.is_float() {
                    let msg = format!(
                        "Cannot use `-` operator on type `{}`",
                        expr.value.ty.print(ctx.symbols())
                    );

                    ctx.error(msg, whole_span);
                    return Err(());
                }

                let ty = expr.value.ty.clone();
                (
                    Spanned::new(t::Expression::Unary(op.value, expr), whole_span),
                    ty,
                )
            }
        };

        Ok(Spanned::new(
            t::TypedExpression {
                expr: Box::new(typed),
                ty,
            },
            whole_span,
        ))
    }
}
