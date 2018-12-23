///! Transforms the type within the ast into types that are understood by the backend
use super::{Infer, InferResult};
use ctx::CompileCtx;
use infer::types::{Type, TypeCon};
use std::collections::HashMap;
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
            astType::Generic(ref symbol, ref types) => {
                let ty = if let Some(ty) = ctx.look_type(symbol.value).cloned() {
                    ty
                } else {
                    let msg = format!("Undefined Type `{}`", ctx.name(symbol.value));
                    ctx.error(msg, symbol.span);
                    return Err(());
                };

                match ty {
                    Type::Generic(ref tvars, ref ty) => match *ty.clone() {
                        Type::Class(_, ref mut fields, ref methods, unique) => {
                            if tvars.is_empty() {
                                let msg =
                                    format!("Type `{}` is not polymorphic", ctx.name(symbol.value));
                                ctx.error(msg, symbol.span);
                                return Err(());
                            }

                            let mut mappings = HashMap::new();

                            for (tvar, ty) in tvars.iter().zip(types) {
                                mappings.insert(*tvar, self.trans_type(ty, ctx)?);
                            } // First create the mappings

                            for field in &mut fields.iter_mut() {
                                let mut ty = self.subst(&field.ty, &mut mappings);

                                ::std::mem::swap(&mut field.ty, &mut ty);
                            }

                            Ok(Type::Class(
                                symbol.value,
                                fields.clone(),
                                methods.clone(),
                                unique,
                            ))
                        }
                        _ => unreachable!(), // Polymorphic functions are not stored as types they are stored as vars
                    },
                    _ => {
                        let msg = format!("Type `{}` is not polymorphic", ctx.name(symbol.value));
                        ctx.error(msg, symbol.span);
                        Err(())
                    }
                }
            }
        }
    }

    pub(crate) fn infer_symbol_type(
        &self,
        symbol: &Spanned<Symbol>,
        ctx: &mut CompileCtx,
    ) -> InferResult<Type> {
        match ctx.look_var(symbol.value).cloned() {
            Some(ty) => return Ok(ty.clone().get_ty()),
            None => match ctx.look_type(symbol.value).cloned() {
                Some(ty) => return Ok(ty.clone()),
                None => {
                    let msg = format!("Undefined variable '{}' ", ctx.name(symbol.value));
                    ctx.error(msg, symbol.span);
                    Err(())
                }
            },
        }
    }
}
