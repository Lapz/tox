// use std::collections::{HashMap, HashSet};
use pos::Postition;
use symbol::Symbol;
use env::Entry;
use std::fmt::{Display, Formatter};
use std::fmt;

#[derive(Debug, Clone)]
pub enum TypeError {
    Expected(Type, Type, Postition),
    Undefinded,
    UndefindedVar(String, Postition),
    NotSame(String),
    Function,
    InvalidIndex,
    NotArray,
    NotProperty(String, Postition),
    TooManyProperty,
    TooLittleProperty,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum Type {
    Class {
        name: Symbol,
        methods: Vec<(Symbol, Entry)>,
        fields: Vec<(Symbol, Type)>,
    },
    Int,
    Str,
    Bool,
    Nil,
    Float,

    Dict(Box<Type>, Box<Type>), // Key, Value
    Array(Box<Type>),
    Name(Symbol, Box<Type>),
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            Type::Class {
                ref name,
                ref methods,
                ref fields,
            } => unimplemented!(),
            Type::Int => write!(f, "Int"),
            Type::Str => write!(f, "Str"),
            Type::Bool => write!(f, "Boolean"),
            Type::Nil => write!(f, "Nil"),
            Type::Float => write!(f, "Float"),
            Type::Dict(ref key, ref value) => write!(f, "Dictionary<{},{}>", key, value),
            Type::Array(ref a) => write!(f, "Array of {}", a),
            Type::Name(ref name, ref ty) => write!(f, "Type alias {} = {}", name, ty),
        }
    }
}

impl Display for TypeError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            TypeError::Expected(ref expected, ref got, ref pos) => write!(
                f,
                "Expected type \'{}\' but instead got \'{}\' on {}",
                &expected, got, pos
            ),

            TypeError::UndefindedVar(ref name, ref pos) => {
                write!(f, "Undefinded variable \'{}\' on {}", name, pos)
            }

            TypeError::NotProperty(ref name, ref pos) => {
                write!(f, "Undefined property \'{}\' on {}", name, pos)
            }

            _ => unimplemented!(),
        }
    }
}
