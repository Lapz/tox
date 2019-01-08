use ast as t;
use ctx::CompileCtx;

use infer::types::{Type, TypeCon};
use infer::{Infer, InferResult};
use syntax::ast::{Expression, MatchArm, UnaryOp,Statement};
use util::pos::{Span, Spanned};

impl Infer {
    pub(crate) fn infer_match(
        &mut self,
        cond: Spanned<Expression>,
        arms: Spanned<Vec<Spanned<MatchArm>>>,
        all: Option<Box<Spanned<Statement>>>,
        whole_span: Span,
        ctx: &mut CompileCtx,
    ) -> InferResult<Spanned<t::TypedExpression>> {
        let cond = self.infer_expr(cond, ctx)?;

        let pattern_type = cond.value.ty.clone(); // type of the pattern

        let mut return_type = Type::Nil; // Default return type is nill

        let mut typed_arms = Vec::new();
        let arms_span = arms.span;
        let mut arms = arms.value;

        if !arms.is_empty() {
            let first = arms.remove(0);
            let first_span = first.span;
            let first_pattern = self.infer_expr(first.value.pattern, ctx)?;
            let first_body = self.infer_statement(first.value.body, ctx)?;

            return_type = first_body.value.ty.clone();

            typed_arms.push(Spanned::new(
                t::MatchArm {
                    pattern: first_pattern,
                    body: first_body,
                },
                first_span,
            ));

            for arm in arms {
                let span = arm.span;
                let lhs = self.infer_expr(arm.value.pattern, ctx)?;

                self.unify(&lhs.value.ty, &pattern_type, lhs.span, ctx)?;

                let rhs = self.infer_statement(arm.value.body, ctx)?;

                self.unify(&rhs.value.ty, &return_type, rhs.span, ctx)?;

                typed_arms.push(Spanned::new(
                    t::MatchArm {
                        pattern: lhs,
                        body: rhs,
                    },
                    span,
                ));
            }
        }

        let all = if let Some(all) = all {
            let body = self.infer_statement(*all, ctx)?;

            self.unify(&body.value.ty,&return_type,body.span,ctx)?;
            Some(body)
        } else {
            None
        };

        Ok(Spanned::new(
            t::TypedExpression {
                expr: Box::new(Spanned::new(
                    t::Expression::Match {
                        cond,
                        arms: Spanned::new(typed_arms, arms_span),
                        all,
                    },
                    whole_span,
                )),
                ty: return_type,
            },
            whole_span,
        ))
    }
}
