use crate::{
    infer::{InferDataCollector, Type, TypeCon, TypeVar, Variant},
    HirDatabase,
};
use std::collections::HashMap;
use tracing::instrument;

impl<'a, DB> InferDataCollector<&'a DB>
where
    DB: HirDatabase,
{
    #[instrument(skip(self))]
    pub(crate) fn subst(&mut self, ty: &Type, substitutions: &mut HashMap<TypeVar, Type>) -> Type {
        match ty {
            Type::App(types) => Type::App(
                types
                    .iter()
                    .map(|ty| self.subst(ty, substitutions))
                    .collect(),
            ),
            Type::Tuple(types) => Type::Tuple(
                types
                    .iter()
                    .map(|ty| self.subst(ty, substitutions))
                    .collect(),
            ),
            Type::Poly(ty_vars, u) => Type::Poly(
                ty_vars.iter().map(|_| self.ctx.type_var()).collect(),
                Box::new(self.subst(u, substitutions)),
            ),
            Type::Var(tvar) => {
                if let Some(ty) = substitutions.get(tvar) {
                    ty.clone()
                } else {
                    Type::Var(*tvar)
                }
            }

            Type::Con(con) => Type::Con(match con {
                TypeCon::Bool => TypeCon::Bool,
                TypeCon::Float => TypeCon::Float,
                TypeCon::Int => TypeCon::Int,
                TypeCon::Str => TypeCon::Str,
                TypeCon::Void => TypeCon::Void,
                TypeCon::Array { ty, size } => TypeCon::Array {
                    ty: Box::new(self.subst(ty, substitutions)),
                    size: size.clone(),
                },
            }),
            Type::Enum(name, variants) => Type::Enum(
                *name,
                variants
                    .iter()
                    .map(|(name, v)| {
                        (
                            *name,
                            Variant {
                                tag: v.tag,
                                ty: v.ty.clone().map(|ty| self.subst(&ty, substitutions)),
                            },
                        )
                    })
                    .collect(),
            ),
            Type::Class {
                name,
                fields,
                methods,
            } => Type::Class {
                name: *name,
                fields: fields
                    .iter()
                    .map(|(name, ty)| (*name, self.subst(ty, substitutions)))
                    .collect(),
                methods: methods
                    .iter()
                    .map(|(name, ty)| (*name, self.subst(ty, substitutions)))
                    .collect(),
            },
            Type::Unknown => Type::Unknown,
        }
    }
}
