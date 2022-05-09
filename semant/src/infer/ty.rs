use indexmap::IndexMap;

use crate::hir::NameId;

use std::{collections::hash_map::DefaultHasher, hash::Hasher};

/// A type var represent a variable that could be a type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeVar(pub(crate) u32);
/// A unique identifier that is used to distinguish to types with the exact some fields
/// i.e struct Foo {} && struct Bar {} we treat them differently
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Unique(pub(crate) u32);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeCon {
    Bool,
    Float,
    Int,
    Str,
    Void,
    Array { ty: Box<Type>, size: Option<usize> },
}

/// All of of our base types
///
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    /// (x |-> y) <br/>
    /// Last type in a app is the return type. <br/>
    /// i.e. i32 => App(Con(Int))
    /// and f(a:i32,b:i32) -> f32 <br/>
    /// App(vec&#33;[App(Con(Int)) ,App(Con(Int)),App(Con(Float)) ])
    App(Vec<Type>),
    Tuple(Vec<Type>),
    Poly(Vec<TypeVar>, Box<Type>),
    Var(TypeVar),
    Con(TypeCon),
    Enum(NameId, IndexMap<NameId, Variant>),
    Class {
        name: NameId,
        fields: IndexMap<NameId, Type>,
        methods: IndexMap<NameId, Type>,
    },
    Unknown,
}

impl std::hash::Hash for Type {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Type::App(types) => types.iter().for_each(|ty| ty.hash(state)),
            Type::Tuple(types) => types.iter().for_each(|ty| ty.hash(state)),
            Type::Poly(vars, inner) => {
                vars.iter().for_each(|ty| ty.hash(state));
                inner.hash(state)
            }
            Type::Var(var) => var.hash(state),
            Type::Con(con) => con.hash(state),
            Type::Enum(name, fields) => {
                name.hash(state);

                state.write_u64(
                    fields
                        .values()
                        .map(|kv| {
                            let mut h = DefaultHasher::new();
                            kv.hash(&mut h);
                            h.finish()
                        })
                        .fold(0, u64::wrapping_add),
                )
            }
            Type::Class {
                name,
                fields,
                methods,
            } => {
                name.hash(state);
                state.write_u64(
                    fields
                        .values()
                        .map(|kv| {
                            let mut h = DefaultHasher::new();
                            kv.hash(&mut h);
                            h.finish()
                        })
                        .fold(0, u64::wrapping_add),
                );
                state.write_u64(
                    methods
                        .values()
                        .map(|kv| {
                            let mut h = DefaultHasher::new();
                            kv.hash(&mut h);
                            h.finish()
                        })
                        .fold(0, u64::wrapping_add),
                )
            }
            Type::Unknown => std::mem::discriminant(&Type::Unknown).hash(state),
        }
    }
}

impl Type {

    // If a function returns a struct, it is caller's responsibility
    // to allocate a space for the return value.
    // If the return type is a large struct/union, the caller passes
    // a pointer to a buffer as if it were the first argument.
    pub fn allocates(&self) -> bool {
        match self {
            Type::Class {..} => true,
            Type::Poly(_,inner) => inner.allocates(),
            _ => false
        }
    }
    pub fn size(&self) -> isize {
        match self {
            Type::Con(con) => match con {
                TypeCon::Bool => 1,
                TypeCon::Float => 4,
                TypeCon::Int => 8, // todo implement 32 bit floats
                TypeCon::Str => 4, // TODO fix
                TypeCon::Void => 1,
                TypeCon::Array { ty, size } => {
                    if let Some(len) = size {
                        ty.size() * (*len as isize)
                    } else {
                        16
                    }
                }
            },
            Type::App(..) => 8,
            Type::Tuple(_length) => 8,
            Type::Enum(_, fields) => {
                4 + fields.iter().fold(0, |acc, (_, variant)| {
                    acc + variant.ty.as_ref().map_or(4, |ty| ty.size())
                })
            }
            Type::Class { fields, .. } => {
                let mut size = 0;

                for (_, ty) in fields {
                    size += ty.size()
                }

                size
            }

            Type::Poly(_, inner) => inner.size(),
            Type::Var(_) => panic!("Found type var during code generation, this is a bug"),
            Type::Unknown => panic!("Found unknown type during code generation"),
        }
    }

    // We treat a tuple like a struct

    pub fn align(&self) -> isize {
        match self {
            Type::Con(con) => match con {
                TypeCon::Bool => 1,
                TypeCon::Float => 4,
                TypeCon::Int => 8,
                TypeCon::Str => 4, // TODO fix
                TypeCon::Void => 1,
                TypeCon::Array { ty, .. } => ty.align(),
            },
            Type::App(..) => 8,
            Type::Poly(_, inner) => inner.align(),
            Type::Unknown => panic!("Unsized types"),
            Type::Tuple(_length) => 8,
            Type::Enum(_, _) => 4,
            Type::Var(_) => panic!("Unsized type"),
            Type::Class {
                name: _,
                fields,
                methods: _,
            } => {
                if fields.is_empty() {
                    1
                } else {
                    4
                }
            }
        }
    }
}

/// Represent an enum variant
/// ```ignore
/// Foo::Bar => Variant {
//      tag:0, // the number it was declared at
///     inner:None // if it doesn't have an inner type i.e Ok(foo)
///  }
/// ```

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Variant {
    pub tag: usize,
    pub ty: Option<Type>,
}

impl From<u32> for TypeVar {
    fn from(i: u32) -> Self {
        Self(i)
    }
}
