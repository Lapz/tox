//! This module provides the types that are used throughout tox for the typeChecking

use super::pos::Span;
use super::symbol::Symbol;
use env::Entry;
use std::cmp::{Ordering, PartialOrd};
use std::collections::HashMap;
use std::fmt;
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};

#[derive(Debug, Clone)]
pub enum TypeError {
    Expected(Type, Type, Span),
    UndefinedType(String, Span),
    UndefinedVar(String, Span),
    UndefinedClass(String, Span),
    NotCallable(Span),
    InvalidIndex(Span),
    IndexAble(String, Span),
    NotMethodOrProperty(String, Span),
    TooManyProperty(Span),
    TooLittleProperty(Span),
    ExpectedOneOf(String, Span),
    NotInstanceOrClass(Type, Symbol, Span),
    SuperClass(Symbol, Span),
}

#[derive(Debug, Clone)]
pub enum Type {
    Class {
        name: Symbol,
        methods: HashMap<Symbol, Entry>,
        fields: HashMap<Symbol, Type>,
    },
   
    Func(Vec<Type>, Box<Type>),
    Dict(Box<Type>, Box<Type>), // Key, Value
    Array(Box<Type>),
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

          
            Type::Func(ref params, ref returns) => write!(
                f,
                "Func with param Types {:?} returns {:?}",
                params, returns
            ),
            Type::Dict(ref key, ref value) => write!(f, "Dictionary<{},{}>", key, value),
            Type::Array(ref a) => write!(f, "Array of {} ", a),
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

            TypeError::UndefinedVar(ref name, ref pos) => {
                write!(f, "Undefined variable \'{}\' on {}", name, pos)
            }
            TypeError::SuperClass(ref name, ref pos) => {
                write!(f, "{} is not a class on {}", name, pos)
            }
            TypeError::UndefinedClass(ref name, ref pos) => {
                write!(f, "Undefined Class\'{}\' on {}", name, pos)
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

            TypeError::UndefinedType(ref name, ref pos) => {
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
           
            (&Type::Func(ref params, ref returns), &Type::Func(ref oparams, ref oreturns)) => {
                params == oparams && returns == oreturns
            }

            (&Type::Func(_, ref returns), unknown) => **returns == *unknown,

            (&Type::Name(ref name, ref ty), &Type::Name(ref oname, ref oty)) => {
                name == oname && ty == oty
            }

            (&Type::Array(ref s), &Type::Array(ref o)) => s == o,

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
            | (&Type::Name(ref name, _), &Type::Name(ref oname, _)) => name.partial_cmp(oname),
            (s @ &Type::Dict(_, _), o @ &Type::Dict(_, _))
            | (s @ &Type::Func(_, _), o @ &Type::Func(_, _))
            | (s @ &Type::Array(_), o @ &Type::Array(_)) => s.partial_cmp(o),
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
            Type::Class { ref name, .. } => name.hash(state),
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
            Type::Array(ref ty) => {
                ty.hash(state);
            }
            Type::Nil => "nil".hash(state),
            Type::Float => "float".hash(state),
            Type::Int => "int".hash(state),
            Type::Str => "str".hash(state),
            Type::Bool => "bool".hash(state),
        }
    }
}
