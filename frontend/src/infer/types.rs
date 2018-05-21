use super::{Infer, InferResult};
use ctx::CompileCtx;
use syntax::ast::expr::Ty as astType;
use types::Type;
use util::pos::Spanned;

impl Infer {
    pub(crate) fn trans_type(
        &self,
        ty: &Spanned<astType>,
        ctx: &mut CompileCtx,
    ) -> InferResult<Type> {
        match ty.value {
            astType::Simple(ref s) => {
                if let Some(ty) = ctx.look_type(s.value) {
                    return Ok(ty.clone());
                }

                let msg = format!("Undefined Type '{}'", ctx.name(s.value));
                ctx.error(msg, ty.span);
                Err(())
            }
            astType::Nil => Ok(Type::Nil),
            astType::Arr(ref s) => Ok(Type::Array(Box::new(self.trans_type(s, ctx)?))),
            astType::Func(ref params, ref returns) => {
                let mut param_tys = Vec::with_capacity(params.len());

                for e_ty in params {
                    param_tys.push(self.trans_type(e_ty, ctx)?)
                }

                if let Some(ref ret) = *returns {
                    Ok(Type::Fun(param_tys, Box::new(self.trans_type(ret, ctx)?)))
                } else {
                    Ok(Type::Fun(param_tys, Box::new(Type::Nil)))
                }
            }
        }
    }
}
