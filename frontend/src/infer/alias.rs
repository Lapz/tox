use ctx::CompileCtx;
use infer::{Infer, InferResult};
use syntax::ast::TypeAlias;
use util::pos::Spanned;

impl Infer {
    pub fn infer_alias(&self, alias: &Spanned<TypeAlias>, ctx: &mut CompileCtx) -> InferResult<()> {
        let ty = self.trans_type(&alias.value.ty, ctx)?;

        ctx.add_type(alias.value.alias.value, ty);

        Ok(())
    }
}
