use super::Infer;
use infer::types::{Constructor, Method, Property, Type, TypeVar, Variant};
use std::collections::HashMap;

impl Infer {
    /// Deals with the subsitution of type variables
    pub fn subst(&self, ty: &Type, substions: &mut HashMap<TypeVar, Type>) -> Type {
        match *ty {
            Type::App(ref tycon, ref types) => Type::App(
                tycon.clone(),
                types.iter().map(|ty| self.subst(ty, substions)).collect(),
            ),

            Type::Enum {
                ref name,
                ref variants,
            } => {
                let mut n_variants = HashMap::new();

                for (s, variant) in variants {
                    let arity = variant.constructor.arity();
                    let span = variant.constructor.span();
                    let symbol = variant.constructor.symbol();

                    n_variants.insert(
                        *s,
                        Variant {
                            tag: variant.tag,
                            constructor: Constructor::new(
                                symbol,
                                variant
                                    .constructor
                                    .types()
                                    .iter()
                                    .map(|ty| self.subst(ty, substions))
                                    .collect(),
                                arity,
                                span,
                            ),
                        },
                    );
                }

                Type::Enum {
                    name: *name,
                    variants: n_variants,
                }
            }

            Type::Variant(ref c) => {
                let arity = c.arity();
                let span = c.span();
                let symbol = c.symbol();
                Type::Variant(Constructor::new(
                    symbol,
                    c.types()
                        .iter()
                        .map(|ty| self.subst(ty, substions))
                        .collect(),
                    arity,
                    span,
                ))
            }

            // Type::Enum {
            //     name: *name,
            //     variants: variants
            //         .iter()
            //         .map(|(ident, variant)| {
            //             (
            //                 *ident,
            //                 Variant {
            //                     tag: variant.tag,
            //                     inner: variant
            //                         .inner
            //                         .as_ref()
            //                         .map(|inner| self.subst(inner, substions)),
            //                 },
            //             )
            //         })
            //         .collect(),
            // },
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
