use ast as t;
use ctx::CompileCtx;
use infer::{Infer, InferResult};
use util::pos::{Span, Spanned};
use util::symbol::Symbol;

impl Infer {
    pub(crate) fn infer_var(
        &mut self,
        symbol: Spanned<Symbol>,
        whole_span: Span,
        ctx: &mut CompileCtx,
    ) -> InferResult<Spanned<t::TypedExpression>> {
        let ty = self.infer_symbol_type(&symbol, ctx)?;
        let typed = Spanned::new(t::Expression::Var(symbol.value, ty.clone()), whole_span);

        Ok(Spanned::new(
            t::TypedExpression {
                expr: Box::new(typed),
                ty,
            },
            whole_span,
        ))
    }
}
