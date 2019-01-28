use ast as t;
use ctx::CompileCtx;

use infer::{Infer, InferResult};
use ir::types::{Type, TypeCon};
use syntax::ast::Expression;
use util::pos::{Span, Spanned};

impl Infer {
    pub(crate) fn infer_array(
        &mut self,
        mut items: Vec<Spanned<Expression>>,
        span: Span,
        ctx: &mut CompileCtx,
    ) -> InferResult<Spanned<t::TypedExpression>> {
        let whole_span = span;
        let (typed, ty) = if items.is_empty() {
            (
                Spanned::new(t::Expression::Array(vec![]), span),
                Type::App(TypeCon::Array(Box::new(Type::Nil)), vec![]),
            )
        } else {
            let mut nitems = vec![self.infer_expr(items.remove(0), ctx)?];

            for item in items.into_iter() {
                let span = item.span;
                let ty_expr = self.infer_expr(item, ctx)?;

                self.unify(&nitems[0].value.ty, &ty_expr.value.ty, span, ctx)?;
                nitems.push(ty_expr);
            }

            let ret_ty = nitems[0].value.ty.clone(); // type of an individual element

            (
                Spanned::new(t::Expression::Array(nitems), span),
                Type::App(TypeCon::Array(Box::new(ret_ty)), vec![]),
            )
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
