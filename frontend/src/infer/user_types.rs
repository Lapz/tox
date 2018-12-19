///! Transforms the type within the ast into types that are understood by the backend
use super::{Infer, InferResult};
use ctx::CompileCtx;
use infer::types::{Type, TypeCon};
use syntax::ast::Type as astType;
use util::pos::Spanned;
use util::symbol::Symbol;

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
            astType::Arr(ref s) => Ok(Type::App(
                TypeCon::Array(Box::new(self.trans_type(s, ctx)?)),
                vec![],
            )),
            astType::Func(ref params, ref returns) => {
                let mut trans_types = Vec::with_capacity(params.len());

                for ty in params {
                    trans_types.push(self.trans_type(ty, ctx)?)
                }

                let ret = if let Some(ref ret) = *returns {
                    self.trans_type(ret, ctx)?
                } else {
                    Type::Nil
                };

                trans_types.push(ret);

                Ok(Type::App(TypeCon::Arrow, trans_types))
            }
        }
    }

    pub(crate) fn infer_var(
        &self,
        symbol: &Spanned<Symbol>,
        ctx: &mut CompileCtx,
    ) -> InferResult<Type> {
        match ctx.look_var(symbol.value).cloned() {
            Some(ty) => return Ok(ty.clone().get_ty()),
            None => {
                let msg = format!("Undefined variable '{}' ", ctx.name(symbol.value));
                ctx.error(msg, symbol.span);
                Err(())
            }
        }
    }
}
