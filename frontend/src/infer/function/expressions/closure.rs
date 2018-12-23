use ast as t;
use ctx::CompileCtx;

use infer::types::{Type, TypeVar};
use infer::{Infer, InferResult};
use syntax::ast::Function;
use util::pos::{Span, Spanned};

impl Infer {
    pub(crate) fn infer_closure(
        &mut self,
        function: Spanned<Function>,
        whole_span: Span,
        ctx: &mut CompileCtx,
    ) -> InferResult<Spanned<t::TypedExpression>> {
        let mut poly_tvs = Vec::with_capacity(function.value.name.value.type_params.len()); // All type parameters

        for ident in &function.value.name.value.type_params {
            let tv = TypeVar::new();

            ctx.add_type(ident.value, Type::Var(tv));
            poly_tvs.push(tv);
        }

        unimplemented!()
    }
}
