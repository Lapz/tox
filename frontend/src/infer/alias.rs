use crate::ctx::CompileCtx;
use crate::infer::types::{Type, TypeVar};
use crate::infer::{Infer, InferResult};
use syntax::ast::TypeAlias;
use util::pos::Spanned;
impl Infer {
    pub fn infer_alias(&self, alias: &Spanned<TypeAlias>, ctx: &mut CompileCtx) -> InferResult<()> {
        if alias.value.alias.value.type_params.is_empty() {
            let ty = self.trans_type(&alias.value.ty, ctx)?;

            ctx.add_type(alias.value.alias.value.name.value, ty);
            return Ok(());
        }

        let mut poly_tvs = Vec::with_capacity(alias.value.alias.value.type_params.len());

        for ident in &alias.value.alias.value.type_params {
            let tv = TypeVar::new();
            ctx.add_type(ident.value, Type::Var(tv));
            poly_tvs.push(tv);
        }

        let entry = Type::Generic(poly_tvs, Box::new(self.trans_type(&alias.value.ty, ctx)?));

        ctx.add_type(alias.value.alias.value.name.value, entry);

        Ok(())
    }
}
