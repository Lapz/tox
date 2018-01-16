//! This module provides the types that are used throughout tox for the typeChecking

use super::pos::Postition;
use super::symbol::Symbol;
use env::Entry;
use std::fmt::{Display, Formatter};
use std::fmt;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::cmp::{Ordering, PartialOrd};

#[derive(Debug, Clone)]
pub enum TypeError {
    Expected(Type, Type, Postition),
    UndefindedType(String, Postition),
    UndefindedVar(String, Postition),
    UndefindedClass(String, Postition),
    NotCallable(Postition),
    InvalidIndex(Postition),
    IndexAble(String, Postition),
    NotMethodOrProperty(String, Postition),
    TooManyProperty(Postition),
    TooLittleProperty(Postition),
    ExpectedOneOf(String, Postition),
    NotInstanceOrClass(Type, Symbol, Postition),
    SuperClass(Symbol, Postition),
}

#[derive(Debug, Clone)]
pub enum Type {
    Class {
        name: Symbol,
        methods: HashMap<Symbol, Entry>,
        fields: HashMap<Symbol, Type>,
    },
    This(Symbol, HashMap<Symbol, Type>, HashMap<Symbol, Type>),
    Func(Vec<Type>, Box<Type>),
    Dict(Box<Type>, Box<Type>), // Key, Value
    Array(Box<Type>, usize),
    Name(Symbol, Box<Type>),
    Int,
    Str,
    Bool,
    Nil,
    Float,
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            Type::Class {
                ref name,
                ref methods,
                ref fields,
            } => write!(
                f,
                "Class {} with Fields {:#?} and Methods {:#?}",
                name, fields, methods
            ),

            Type::This(ref name, ref methods, ref fields) => write!(
                f,
                "'This {}' has the fields {:?} and methods {:?}",
                name, methods, fields
            ),
            Type::Func(ref params, ref returns) => write!(
                f,
                "Func with param Types {:?} returns {:?}",
                params, returns
            ),
            Type::Dict(ref key, ref value) => write!(f, "Dictionary<{},{}>", key, value),
            Type::Array(ref a, ref len) => write!(f, "Array of {} with len {}", a, len),
            Type::Name(ref name, ref ty) => write!(f, "Type alias {} = {}", name, ty),
            Type::Int => write!(f, "Int"),
            Type::Str => write!(f, "Str"),
            Type::Bool => write!(f, "Boolean"),
            Type::Nil => write!(f, "Nil"),
            Type::Float => write!(f, "Float"),
        }
    }
}

impl Display for TypeError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            TypeError::Expected(ref expected, ref got, ref pos) => write!(
                f,
                "Expected Type \'{}\' but instead got \'{}\' on {}",
                &expected, got, pos
            ),
            TypeError::IndexAble(ref name, ref pos) => {
                write!(f, "\'{}\' is not an indexable on {}", name, pos)
            }

            TypeError::InvalidIndex(ref pos) => write!(f, "Cannot index on line '{}'", pos),

            TypeError::UndefindedVar(ref name, ref pos) => {
                write!(f, "Undefinded variable \'{}\' on {}", name, pos)
            }
            TypeError::SuperClass(ref name, ref pos) => {
                write!(f, "{} is not a class on {}", name, pos)
            }
            TypeError::UndefindedClass(ref name, ref pos) => {
                write!(f, "Undefinded Class\'{}\' on {}", name, pos)
            }

            TypeError::NotCallable(ref pos) => {
                write!(f, "Can only call functions and classes on {}", pos)
            }

            TypeError::NotInstanceOrClass(ref ty, ref property, ref pos) => write!(
                f,
                "Type {} dosen't have the method/field {} on {}",
                ty, property, pos
            ),

            TypeError::ExpectedOneOf(ref msg, ref pos) => write!(f, "{} on {}", msg, pos),

            TypeError::NotMethodOrProperty(ref name, ref pos) => {
                write!(f, "Undefined method/property \'{}\' on {}", name, pos)
            }

            TypeError::UndefindedType(ref name, ref pos) => {
                write!(f, "Undefined Type \'{}\' on {}", name, pos)
            }

            TypeError::TooLittleProperty(ref pos) => write!(f, "Expected more fields on {}", pos),
            TypeError::TooManyProperty(ref pos) => write!(f, "Expected less fields on {}", pos),
        }
    }
}

impl<'a> Eq for Type {}

impl<'a> PartialEq for Type {
    fn eq(&self, other: &Type) -> bool {
        match (self, other) {
            (&Type::Dict(ref k, ref v), &Type::Dict(ref okey, ref ov)) => k == okey && v == ov,
            (
                &Type::Class {
                    ref name,
                    ref methods,
                    ref fields,
                },
                &Type::Class {
                    name: ref oname,
                    fields: ref ofields,
                    methods: ref omethods,
                },
            ) => name == oname && methods == omethods && fields == ofields,
            (&Type::This(ref n, _, _), &Type::This(ref oname, _, _)) => n == oname,
            (&Type::Func(ref params, ref returns), &Type::Func(ref oparams, ref oreturns)) => {
                params == oparams && returns == oreturns
            }

            (&Type::Name(ref name, ref ty), &Type::Name(ref oname, ref oty)) => {
                name == oname && ty == oty
            }

            (&Type::Array(ref s, ref len), &Type::Array(ref o, ref olen)) => s == o && len == olen,

            (&Type::Nil, &Type::Nil)
            | (&Type::Float, &Type::Float)
            | (&Type::Int, &Type::Int)
            | (&Type::Str, &Type::Str)
            | (&Type::Bool, &Type::Bool) => true,

            _ => false,
        }
    }
}

impl<'a> PartialOrd for Type {
    fn partial_cmp(&self, other: &Type) -> Option<Ordering> {
        match (self, other) {
            (
                &Type::Class { ref name, .. },
                &Type::Class {
                    name: ref oname, ..
                },
            )
            | (&Type::This(ref name, _, _), &Type::This(ref oname, _, _))
            | (&Type::Name(ref name, _), &Type::Name(ref oname, _)) => name.partial_cmp(oname),
            (s @ &Type::Dict(_, _), o @ &Type::Dict(_, _))
            | (s @ &Type::Func(_, _), o @ &Type::Func(_, _)) => s.partial_cmp(o),
            (&Type::Array(ref s, _), &Type::Array(ref o, _)) => s.partial_cmp(o),
            (&Type::Nil, &Type::Nil)
            | (&Type::Float, &Type::Float)
            | (&Type::Int, &Type::Int)
            | (&Type::Str, &Type::Str)
            | (&Type::Bool, &Type::Bool) => Some(Ordering::Equal),
            _ => None,
        }
    }
}

impl Hash for Type {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match *self {
            Type::Class { ref name, .. } | Type::This(ref name, _, _) => name.hash(state),
            Type::Func(ref params, ref returns) => {
                params.hash(state);
                returns.hash(state)
            }
            Type::Name(ref name, ref returns) => {
                name.hash(state);
                returns.hash(state)
            }
            Type::Dict(ref key, ref value) => {
                key.hash(state);
                value.hash(state)
            }
            Type::Array(ref ty, ref len) => {
                ty.hash(state);
                len.hash(state);
            }
            Type::Nil => "nil".hash(state),
            Type::Float => "float".hash(state),
            Type::Int => "int".hash(state),
            Type::Str => "str".hash(state),
            Type::Bool => "bool".hash(state),
        }
    }
}
