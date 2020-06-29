use super::infer::InferDataCollector;
use crate::infer::{Subst, Type, Variant};
use crate::HirDatabase;
use std::collections::HashMap;

impl<'a, DB> InferDataCollector<&'a DB>
where
    DB: HirDatabase,
{
    pub(crate) fn subst(&self, ty: &Type, substitutions: &mut Subst) -> Type {
        match ty {
            Type::App(types) | Type::Tuple(types) => Type::App(
                types
                    .iter()
                    .map(|ty| self.subst(ty, substitutions))
                    .collect(),
            ),

            Type::Var(tvar) => {
                if let Some(ty) = substitutions.get(tvar) {
                    ty.clone()
                } else {
                    Type::Var(*tvar)
                }
            }
            Type::Poly(type_vars, u) => {
                let tvars = type_vars
                    .iter()
                    .map(|_| self.resolver.ctx.type_var())
                    .collect();

                Type::Poly(tvars, Box::new(self.subst(u, substitutions)))
            }
            Type::Con(con) => Type::Con(con.clone()),
            Type::Enum(variants) => Type::Enum(
                variants
                    .iter()
                    .map(|(name, variant)| {
                        (
                            *name,
                            Variant {
                                tag: variant.tag,
                                ty: variant.ty.clone().map(|ty| self.subst(&ty, substitutions)),
                            },
                        )
                    })
                    .collect(),
            ),
            Type::Class { fields, methods } => {
                let mut new_fields = HashMap::new();
                let mut new_methods = HashMap::new();

                for (name, ty) in fields {
                    new_fields.insert(*name, self.subst(&ty, substitutions));
                }

                for (name, ty) in methods {
                    new_methods.insert(*name, self.subst(&ty, substitutions));
                }

                Type::Class {
                    fields: new_fields,
                    methods: new_methods,
                }
            }
        }
    }
}
