use ast as t;
use ctx::CompileCtx;
use infer::{Infer, InferResult};
use infer::types;
use syntax::ast::{Expression, Type};
use util::pos::{Span, Spanned};

impl Infer {
    pub(crate) fn infer_cast(
        &mut self,
        from: Spanned<Expression>,
        to: Spanned<Type>,
        whole_span: Span,
        ctx: &mut CompileCtx,
    ) -> InferResult<Spanned<t::TypedExpression>> {
        let from = self.infer_expr(from, ctx)?;
        let to = self.trans_type(&to, ctx)?;

        match (&from.value.ty, &to) {
            (
                types::Type::App(types::TypeCon::Int, _),
                types::Type::App(types::TypeCon::Float, _),
            ) => {}

            (
                types::Type::App(types::TypeCon::Int, _),
                types::Type::App(types::TypeCon::Str, _),
            ) => {}

            (
                types::Type::App(types::TypeCon::Float, _),
                types::Type::App(types::TypeCon::Str, _),
            ) => {}

            (
                types::Type::App(types::TypeCon::Float, _),
                types::Type::App(types::TypeCon::Int, _),
            ) => {}

            (
                types::Type::App(types::TypeCon::Bool, _),
                types::Type::App(types::TypeCon::Int, _),
            ) => {}

            (lhs, rhs) => {
                let msg = format!(
                    "Cannot cast `{}` to type `{}`",
                    lhs.print(ctx.symbols()),
                    rhs.print(ctx.symbols())
                );
                ctx.error(msg, whole_span);
                return Err(());
            }
        }

        Ok(Spanned::new(
            t::TypedExpression {
                expr: Box::new(Spanned::new(
                    t::Expression::Cast(from, to.clone()),
                    whole_span,
                )),
                ty: to,
            },
            whole_span,
        ))
    }
}
