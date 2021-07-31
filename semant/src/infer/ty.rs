use crate::hir::NameId;

use std::collections::HashMap;

/// A type var represent a variable that could be a type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeVar(pub(crate) u32);
/// A unique identifier that is used to distinguish to types with the exact some fields
/// i.e struct Foo {} && struct Bar {} we treat them differently
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Unique(pub(crate) u32);

#[derive(Debug, Clone, PartialEq, Eq)]
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
    Enum(NameId, HashMap<NameId, Variant>),
    Class {
        name: NameId,
        fields: HashMap<NameId, Type>,
        methods: HashMap<NameId, Type>,
    },
    Unknown,
}

impl Type {
    pub fn size(&self) -> usize {
        match self {
            Type::Con(con) => match con {
                TypeCon::Bool => 1,
                TypeCon::Float => 4,
                TypeCon::Int => 4,
                TypeCon::Str => 4, // TODO fix
                TypeCon::Void => 1,
                TypeCon::Array { ty, size } => {
                    if let Some(len) = size {
                        ty.size() * len
                    } else {
                        16
                    }
                }
            },
            Type::App(..) => 8,
            Type::Tuple(length) => 8,
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

    pub fn align(&self) -> usize {
        match self {
            Type::Con(con) => match con {
                TypeCon::Bool => 1,
                TypeCon::Float => 4,
                TypeCon::Int => 4,
                TypeCon::Str => 4, // TODO fix
                TypeCon::Void => 1,
                TypeCon::Array { ty, .. } => ty.align(),
            },
            Type::App(..) => 8,
            Type::Poly(_, inner) => inner.align(),
            Type::Unknown => panic!("Unsized types"),
            Type::Tuple(length) => 8,
            Type::Enum(_, _) => 4,
            Type::Var(_) => panic!("Unsized type"),
            Type::Class {
                name,
                fields,
                methods,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Variant {
    pub tag: usize,
    pub ty: Option<Type>,
}

impl From<u32> for TypeVar {
    fn from(i: u32) -> Self {
        Self(i)
    }
}
