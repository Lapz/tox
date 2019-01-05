use ast as t;
use ctx::CompileCtx;

use infer::{Infer, InferResult};
use ir::types::{Type, TypeCon};
use syntax::ast::{Expression, Op};
use util::pos::{Span, Spanned};

impl Infer {
    pub(crate) fn infer_binary(
        &mut self,
        lhs: Box<Spanned<Expression>>,
        op: Spanned<Op>,
        rhs: Box<Spanned<Expression>>,
        whole_span: Span,
        ctx: &mut CompileCtx,
    ) -> InferResult<Spanned<t::TypedExpression>> {
        let span = lhs.span.to(rhs.span);

        let lhs = self.infer_expr(*lhs, ctx)?;
        let rhs = self.infer_expr(*rhs, ctx)?;
        let (typed, ty) = match op.value {
            Op::BangEqual | Op::EqualEqual => (
                Spanned::new(t::Expression::Binary(lhs, op.value, rhs), whole_span),
                Type::App(TypeCon::Bool, vec![]),
            ),

            Op::And | Op::Or => {
                self.unify(&lhs.value.ty, &Type::App(TypeCon::Bool, vec![]), span, ctx)?;
                self.unify(&rhs.value.ty, &Type::App(TypeCon::Bool, vec![]), span, ctx)?;

                (
                    Spanned::new(t::Expression::Binary(lhs, op.value, rhs), whole_span),
                    Type::App(TypeCon::Bool, vec![]),
                )
            }

            Op::LessThan | Op::LessThanEqual | Op::GreaterThan | Op::GreaterThanEqual => {
                self.unify(&lhs.value.ty, &rhs.value.ty, span, ctx)?;

                (
                    Spanned::new(t::Expression::Binary(lhs, op.value, rhs), whole_span),
                    Type::App(TypeCon::Bool, vec![]),
                )
            }

            Op::Plus | Op::Slash | Op::Star | Op::Minus | Op::Modulo | Op::Exponential => {
                match self.unify(&lhs.value.ty, &rhs.value.ty, span, ctx) {
                    Ok(()) => (),
                    Err(_) => {
                        match self.unify(&lhs.value.ty, &Type::App(TypeCon::Str, vec![]), span, ctx)
                        {
                            Ok(()) => (),
                            Err(_) => {
                                ctx.remove_error();
                                return Err(());
                            }
                        }
                    }
                }

                let ty = lhs.value.ty.clone();

                (
                    Spanned::new(t::Expression::Binary(lhs, op.value, rhs), whole_span),
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
