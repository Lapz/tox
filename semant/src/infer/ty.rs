use crate::hir::NameId;
use indexmap::IndexMap;
use std::{
    collections::{hash_map::DefaultHasher, HashMap},
    hash::{Hash, Hasher},
};

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

macro_rules! hash {
    ($state:expr => $( $field:expr ),*) => {
        {
            $(
            $state.write_u64(
            $field
                .values()
                .map(|kv| {
                    let mut h = DefaultHasher::new();
                    kv.hash(&mut h);
                    h.finish()
                })
                .fold(0, u64::wrapping_add),
            );
        )*
        }
    };

}

impl Hash for Type {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Type::App(inner) => inner.hash(state),
            Type::Tuple(inner) => inner.hash(state),
            Type::Poly(l, r) => {
                l.hash(state);
                r.hash(state)
            }
            Type::Var(inner) => inner.hash(state),
            Type::Con(inner) => inner.hash(state),
            Type::Enum(fields) => hash!(state => fields),
            Type::Class { fields, methods } => hash!(state => fields,methods),
        }
    }
}
