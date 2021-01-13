use crate::infer::TypeVar;
use crate::{hir::NameId, resolver::Resolver, HirDatabase};
use errors::Reporter;
use std::{borrow::Borrow, hash::Hash};
use std::{collections::HashMap, rc::Rc};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeCon {
    Bool,
    Float,
    Int,
    Str,
    Void,
    Array { ty: Rc<Type>, size: Option<usize> },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Variant {
    pub tag: usize,
    pub ty: Option<Type>,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    /// (x |-> y) <br/>
    /// Last type in a app is the return type. <br/>
    /// i.e. i32 => App(Con(Int))
    /// and f(a:i32,b:i32) -> f32 <br/>
    /// App(vec&#33;[App(Con(Int)) ,App(Con(Int)),App(Con(Float)) ])
    App(Rc<[Type]>),
    Tuple(Rc<[Type]>),
    Var(TypeVar),
    Con(TypeCon),
    Enum(NameId, HashMap<NameId, Variant>),
    Class {
        name: NameId,
        fields: HashMap<NameId, Type>,
        methods: HashMap<NameId, Type>,
    },
    Unknown,
}

#[derive(Debug)]
struct Scheme {
    vars: HashMap<NameId, TypeVar>,
    ty: Type,
}
#[derive(Debug)]
pub struct Unify {
    pub(crate) context: Map<TypeVar, Type>,
    pub(crate) errors: Vec<Reason>,
    pub(crate) tvar_count: u32,
}
#[derive(Debug)]
pub(crate) struct InferDataCollector<DB> {
    db: DB,
    reporter: Reporter,
    env: HashMap<NameId, Scheme>,
    unify: Unify,
}

/// A rollback map - allows us to revert any changes to the environment that were caused by a failed unification
#[derive(Debug)]
pub struct Map<K, V> {
    committed: HashMap<K, V>,
    new: HashMap<K, V>,
}

impl<K: Eq + Hash, V> Map<K, V> {
    pub fn new() -> Self {
        Map {
            committed: HashMap::new(),
            new: HashMap::new(),
        }
    }

    pub fn insert(&mut self, key: K, value: V) {
        self.new.insert(key, value);
    }

    pub fn get(&self, key: &K) -> Option<&V> {
        self.new.get(key).or_else(|| self.committed.get(key))
    }

    pub fn commit(&mut self) {
        self.committed.extend(self.new.drain())
    }

    pub fn rollback(&mut self) {
        self.new.clear()
    }
}
#[derive(Debug)]
pub struct Reason {
    msg: String,
    notes: Option<String>,
}

impl Unify {
    pub fn type_var(&mut self) -> TypeVar {
        let var = TypeVar(self.tvar_count);
        self.tvar_count += 1;
        var
    }

    pub fn unify(&mut self, lhs: &Type, rhs: &Type) {
        let lhs = self.shallow_normalize(lhs);
        let rhs = self.shallow_normalize(rhs);

        match (&lhs, &rhs) {
            (Type::App(types1), Type::App(types2)) => {
                if types1.len() != types2.len() {
                    self.errors.push(Reason {
                        msg: format!("Expected {} params, found {}", types1.len(), types2.len()),
                        notes: None,
                    });
                    return;
                }

                if types1[types1.len() - 1] != types2[types2.len() - 1] {
                    self.errors.push(Reason {
                        msg: format!(
                            "Expected {:?} found {:?}",
                            types1[types1.len() - 1],
                            types2[types2.len() - 1]
                        ),
                        notes: Some("The return types are different".into()),
                    });

                    return;
                }
            }

            (Type::App(signature), ret) => {
                let last = signature.last().unwrap_or(&Type::Con(TypeCon::Void));
                self.unify(last, ret)
            }
            (Type::Tuple(types1), Type::Tuple(types2)) => {
                if types1.len() != types2.len() {
                    self.errors.push(Reason {
                        msg: format!(
                            "Expected {} tuple elements, found {}",
                            types1.len(),
                            types2.len()
                        ),
                        notes: None,
                    });

                    return;
                }

                for (a, b) in types1.iter().zip(types2.iter()) {
                    let _ = self.unify(a, b);
                }
            }

            (Type::Enum(name1, variants1), Type::Enum(name2, variants2)) => {
                if name1 != name2 {
                    self.errors.push(Reason {
                        msg: format!(
                            "Expected enum `{:?}` but found enum `{:?}`",
                            name1,
                            name2 // self.db.lookup_intern_name(*name1),
                                  // self.db.lookup_intern_name(*name2)
                        ),
                        notes: None,
                    })
                }

                if variants1 != variants2 {
                    self.errors.push(Reason {
                        msg: format!(
                            "Expected enum `{:?}` but found enum `{:?}`",
                            name1,
                            name2 // self.db.lookup_intern_name(*name1),
                                  // self.db.lookup_intern_name(*name2)
                        ),
                        notes: Some("The names are the same but the variants are different".into()),
                    })
                }
            }
            (
                Type::Class {
                    name,
                    fields,
                    methods,
                },
                Type::Class {
                    name: name1,
                    fields: fields1,
                    methods: methods1,
                },
            ) => {
                if name != name1 {
                    self.errors.push(Reason {
                        msg: format!(
                            "Expected class `{:?}` but found enum `{:?}`",
                            name,
                            name1 // self.db.lookup_intern_name(*name1),
                                  // self.db.lookup_intern_name(*name2)
                        ),
                        notes: None,
                    })
                }

                if fields != fields1 && methods != methods1 {
                    self.errors.push(Reason {
                        msg: format!(
                            "Expected enum `{:?}` but found enum `{:?}`",
                            name,
                            name1 // self.db.lookup_intern_name(*name1),
                                  // self.db.lookup_intern_name(*name2)
                        ),
                        notes: Some(
                            "The names are the same but the class fields/methods are different"
                                .into(),
                        ),
                    })
                }
            }

            (Type::Var(a), ty) | (ty, Type::Var(a)) => self.unify_var_type(*a, ty),

            (Type::Con(TypeCon::Int), Type::Con(TypeCon::Float))
            | (Type::Con(TypeCon::Float), Type::Con(TypeCon::Int)) => return,
            (Type::Unknown, _) | (_, Type::Unknown) => return,
            (Type::Con(l), Type::Con(r)) if l == r => return,
            (_, _) => {
                self.errors.push(Reason {
                    msg: format!("Mismatching types, expected `{:?}` found `{:?}`", lhs, rhs),
                    notes: None,
                });
                return;
            }
        }
    }

    pub fn insert(&mut self, var: TypeVar, ty: Type) {
        self.context.insert(var, ty);
    }

    fn unify_var_type(&mut self, var: TypeVar, ty: &Type) {
        match ty {
            Type::Var(tv) if &var == tv => return,
            _ => {}
        }

        if self.occurs(&var, &ty) {
            self.errors.push(Reason {
                msg: format!("Infinite type"),
                notes: None,
            });
            return;
        }

        let var_subst = self.context.get(&var).cloned();
    }

    fn get_var_type(&self, var: &TypeVar) -> Type {
        match self.context.get(var) {
            Some(v) => self.shallow_normalize(v),
            None => Type::Var(*var),
        }
    }

    /// Apply the type substitution that we have already to the supplied type var
    fn shallow_normalize(&self, a: &Type) -> Type {
        match a {
            Type::Var(var) => self.get_var_type(var),
            ty => ty.clone(),
        }
    }

    fn occurs(&self, tv: &TypeVar, ty: &Type) -> bool {
        match ty {
            Type::App(params) => {
                for param in params.iter() {
                    if self.occurs(tv, param) {
                        return true;
                    }
                }

                false
            }
            Type::Tuple(params) => {
                for param in params.iter() {
                    if self.occurs(tv, param) {
                        return true;
                    }
                }

                false
            }
            Type::Var(v) => v == tv,
            Type::Con(con) => match con {
                TypeCon::Bool | TypeCon::Float | TypeCon::Int | TypeCon::Str | TypeCon::Void => {
                    false
                }
                TypeCon::Array { ty, size } => self.occurs(tv, ty),
            },
            Type::Enum(_, variants) => {
                for (_, variant) in variants {
                    if let Some(ty) = variant.ty.as_ref() {
                        if self.occurs(tv, ty) {
                            return true;
                        }
                    }
                }

                false
            }
            Type::Class {
                name,
                fields,
                methods,
            } => {
                for (_, field) in fields {
                    if self.occurs(tv, field) {
                        return true;
                    }
                }

                false
            }
            Type::Unknown => false,
        }
    }

    /// Given a type scheme with fn <T>(v:T) -> T;
    /// If we instantiate it with <T = i32> ;
    /// we should obtain fn (v:i32) -> i32;
    pub fn subst(&self, ty: &Type) -> Type {
        match ty {
            Type::App(types) => Type::App(types.iter().map(|ty| self.subst(ty)).collect()),

            Type::Var(v) => {
                if let Some(t) = self.context.get(v) {
                    t.clone()
                } else {
                    Type::Var(*v)
                }
            }
            Type::Tuple(types) => Type::Tuple(types.iter().map(|ty| self.subst(ty)).collect()),
            Type::Con(con) => Type::Con(match con {
                TypeCon::Bool => TypeCon::Bool,
                TypeCon::Float => TypeCon::Float,
                TypeCon::Int => TypeCon::Int,
                TypeCon::Str => TypeCon::Str,
                TypeCon::Void => TypeCon::Void,
                TypeCon::Array { ty, size } => TypeCon::Array {
                    ty: Rc::new(self.subst(ty)),
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
                                ty: v.ty.clone().map(|ty| self.subst(&ty)),
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
                    .map(|(name, ty)| (*name, self.subst(ty)))
                    .collect(),
                methods: methods
                    .iter()
                    .map(|(name, ty)| (*name, self.subst(ty)))
                    .collect(),
            },
            Type::Unknown => Type::Unknown,
        }
    }
}
