use super::Infer;
use infer::types::{Method, Property, Type, TypeVar};
use std::collections::HashMap;

impl Infer {
    /// Deals with the subsitution of type variables
    pub fn subst(&self, ty: &Type, substions: &mut HashMap<TypeVar, Type>) -> Type {
        match *ty {
            Type::App(ref tycon, ref types) => Type::App(
                tycon.clone(),
                types.iter().map(|ty| self.subst(ty, substions)).collect(),
            ),

            Type::Class(ref name, ref fields, ref methods, ref unique) => {
                let mut new_fields = Vec::new();
                let mut new_methods = Vec::new();

                for field in fields {
                    new_fields.push(Property {
                        name: field.name,
                        ty: self.subst(&field.ty, substions),
                    });
                }

                for method in methods {
                    new_methods.push(Method {
                        name: method.name,
                        ty: self.subst(&self.subst(&method.ty, substions), substions),
                    })
                }

                Type::Class(*name, new_fields, new_methods, *unique)
            }

            Type::Generic(ref tyvars, ref u) => Type::Generic(
                tyvars.iter().map(|_| TypeVar::new()).collect(),
                Box::new(self.subst(u, substions)),
            ),

            Type::Nil => Type::Nil,
            Type::Var(ref tvar) => {
                if let Some(ty) = substions.get(tvar) {
                    ty.clone()
                } else {
                    Type::Var(*tvar)
                }
            }
        }
    }
}
