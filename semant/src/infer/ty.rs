use crate::hir::NameId;
use std::collections::HashMap;

/// A type var represent a variable that could be a type
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
    /// (x |-> y)
    /// Last type in a app is the return
    /// type. \n i.e. i32 => App(Con(Int))
    /// and f(a:i32,b:i32) -> f32 => App(vec![App(Con(Int)) ,App(Con(Int)),App(Con(Float)) ])
    App(Vec<Type>),
    Tuple(Vec<Type>),
    Poly(Vec<TypeVar>, Box<Type>),
    Var(TypeVar),
    Con(TypeCon),
    Enum(HashMap<NameId, Variant>),
    Class {
        fields: HashMap<NameId, Type>,
        methods: HashMap<NameId, Type>,
    },
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
