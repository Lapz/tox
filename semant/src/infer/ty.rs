use crate::{hir::NameId, HirDatabase};
use indexmap::IndexMap;
use std::{
    collections::{hash_map::DefaultHasher, HashMap},
    fmt::{self, Display, Write},
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
    Enum(NameId, HashMap<NameId, Variant>),
    Class {
        name: NameId,
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

impl Type {
    pub(crate) fn format<'a, DB: HirDatabase>(&self, db: &'a DB) -> String {
        let mut buf = String::new();

        self.format_inner(&mut buf, db).unwrap();

        buf
    }

    fn format_inner<'a, DB: HirDatabase>(&self, f: &mut String, db: &'a DB) -> fmt::Result {
        match self {
            Type::App(types) => {
                let len = types.len() - 1;

                write!(f, "fn (")?;

                for i in 0..types.len() - 1 {
                    if i + 1 == types.len() - 1 {
                        types[i].format_inner(f, db)?;
                    } else {
                        types[i].format_inner(f, db)?;
                        write!(f, ",")?;
                    }
                }

                write!(f, ") -> ")?;

                types.last().unwrap().format_inner(f, db)
            }
            Type::Tuple(types) => {
                write!(f, "(")?;

                for (index, ty) in types.iter().enumerate() {
                    if index + 1 == types.len() - 1 {
                        ty.format_inner(f, db)?;
                    } else {
                        ty.format_inner(f, db)?;
                        write!(f, ",")?;
                    }
                }

                write!(f, ")")
            }
            Type::Var(v) => write!(f, "{}", v),
            Type::Con(con) => match con {
                TypeCon::Array { ty, size } => {
                    write!(f, "[")?;

                    ty.format_inner(f, db)?;

                    if let Some(size) = size {
                        write!(f, ";{}]", size)?
                    } else {
                        write!(f, "]")?
                    }

                    Ok(())
                }
                _ => write!(f, "{}", con),
            },
            Type::Enum(name, _) => write!(f, "enum {}", db.lookup_intern_name(*name)),
            Type::Class { name, .. } => write!(f, "class {}", db.lookup_intern_name(*name)),
            Type::Poly(type_vars, ty) => {
                write!(f, "poly")?;

                if !type_vars.is_empty() {
                    write!(f, "<")?;
                    for (i, var) in type_vars.iter().enumerate() {
                        if i + 1 == type_vars.len() {
                            write!(f, "{}", var)?;
                        } else {
                            write!(f, "{},", var)?;
                        }
                    }
                    write!(f, ">")?;
                }

                ty.format_inner(f, db)
            }
        }
    }
}

impl Display for TypeVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "${}", self.0)
    }
}
impl Display for TypeCon {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeCon::Bool => write!(f, "bool"),
            TypeCon::Float => write!(f, "float"),
            TypeCon::Int => write!(f, "int"),
            TypeCon::Str => write!(f, "str"),
            TypeCon::Void => write!(f, "nil"),
            TypeCon::Array { ty, size } => {
                if let Some(size) = size {
                    write!(f, "[{:?};{}]", ty, size)
                } else {
                    write!(f, "[{:?}]", ty)
                }
            }
        }
    }
}

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
            Type::Enum(name, fields) => {
                name.hash(state);
                hash!(state => fields)
            }
            Type::Class {
                name,
                fields,
                methods,
            } => {
                name.hash(state);
                hash!(state => fields,methods)
            }
        }
    }
}
