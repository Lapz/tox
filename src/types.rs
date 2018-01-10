// use std::collections::{HashMap, HashSet};
use pos::Postition;
use symbol::Symbol;
use env::Entry;
use std::fmt::{Display, Formatter};
use std::fmt;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum TypeError <'a> {
    Expected(Type<'a>, Type<'a>, Postition),
    UndefindedType(String, Postition),
    UndefindedVar(String, Postition),
    UndefindedClass(String, Postition),
    NotCallable(Postition),
    InvalidIndex(Postition),
    IndexAble(String, Postition),
    NotMethodOrProperty(String, Postition),
    TooManyProperty(Postition),
    TooLittleProperty(Postition),
    ExpectedOneOf(String),
    NotInstanceOrClass(Type<'a>, Symbol, Postition),
    SuperClass(Symbol, Postition),
}

#[derive(Debug, PartialEq, PartialOrd, Clone,Hash)]
pub enum Type<'a> {
    Class {
        name: Symbol,
        methods: HashMap<Symbol, &'a Entry<'a>>,
        fields: HashMap<Symbol, &'a Type<'a>>,
    },
    This(Symbol, HashMap<Symbol, &'a Type<'a>>, HashMap<Symbol, &'a Type<'a>>),
    Int,
    Str,
    Bool,
    Nil,
    Float,
    Func(Vec<&'a Type<'a>>, Box<Type<'a>>),
    Dict(Box<Type<'a>>, Box<Type<'a>>), // Key, Value
    Array(Box<Type<'a>>),
    Name(Symbol, Box<Type<'a>>),
}

impl <'a> Display for Type<'a> {
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
            Type::Int => write!(f, "Int"),
            Type::Str => write!(f, "Str"),
            Type::Bool => write!(f, "Boolean"),
            Type::Nil => write!(f, "Nil"),
            Type::Float => write!(f, "Float"),
            Type::This(ref name, ref methods, ref fields) => write!(
                f,
                "'This {}' has the fields {:?} and methods {:?}",
                name, methods, fields
            ),
            Type::Func(ref params, ref returns) => write!(
                f,
                "Func with param Type<'a>s {:?} returns {:?}",
                params, returns
            ),
            Type::Dict(ref key, ref value) => write!(f, "Dictionary<{},{}>", key, value),
            Type::Array(ref a) => write!(f, "Array of {}", a),
            Type::Name(ref name, ref ty) => write!(f, "Type<'a> alias {} = {}", name, ty),
        }
    }
}

impl <'a> Display for TypeError<'a> {
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

            TypeError::ExpectedOneOf(ref msg) => write!(f, "{}", msg),

            TypeError::NotMethodOrProperty(ref name, ref pos) => {
                write!(f, "Undefined method/property \'{}\' on {}", name, pos)
            }

            TypeError::UndefindedType(ref name, ref pos) => {
                write!(f, "Undefined Type<'a> \'{}\' on {}", name, pos)
            }

            TypeError::TooLittleProperty(ref pos) => write!(f, "Expected more fields on {}", pos),
            TypeError::TooManyProperty(ref pos) => write!(f, "Expected less fields on {}", pos),
        }
    }
}
