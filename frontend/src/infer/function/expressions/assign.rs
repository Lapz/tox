use ast as t;
use ctx::CompileCtx;

use infer::{Infer, InferResult};
use ir::types::{Type, TypeCon};
use syntax::ast::{AssignOperator, Expression};
use util::pos::{Span, Spanned};
use util::symbol::Symbol;

impl Infer {
    pub(crate) fn infer_assign(
        &mut self,
        name: Spanned<Symbol>,
        kind: Spanned<AssignOperator>,
        value: Box<Spanned<Expression>>,
        whole_span: Span,
        ctx: &mut CompileCtx,
    ) -> InferResult<Spanned<t::TypedExpression>> {
        let span = name.span.to(value.span);

        let ty = self.infer_symbol_type(&name, ctx)?;
        let value_ty = self.infer_expr(*value, ctx)?;
        use syntax::ast::AssignOperator::*;
        match kind.value {
            Equal => {
                self.unify(&ty, &value_ty.value.ty, span, ctx)?;
            }
            MinusEqual | PlusEqual | StarEqual | SlashEqual => {
                match self.unify(&ty, &value_ty.value.ty, span, ctx) {
                    Ok(()) => (),
                    Err(_) => match self.unify(&ty, &Type::App(TypeCon::Str, vec![]), span, ctx) {
                        Ok(()) => (),
                        Err(_) => {
                            ctx.remove_error();
                            return Err(());
                        }
                    },
                }
            }
        }

        let ty = value_ty.value.ty.clone();

        Ok(Spanned::new(
            t::TypedExpression {
                expr: Box::new(Spanned::new(
                    t::Expression::Assign(name.value, kind.value, value_ty),
                    whole_span,
                )),
                ty,
            },
            whole_span,
        ))
    }
}
