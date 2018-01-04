// use std::collections::{HashMap, HashSet};
use pos::Postition;
use symbol::Symbol;
use env::Entry;
use std::fmt::{Display, Formatter};
use std::fmt;

#[derive(Debug, Clone)]
pub enum TypeError {
    Expected(Type, Type, Postition),
    UndefindedType(String, Postition),
    UndefindedVar(String, Postition),
    Function(Postition),
    InvalidIndex(Postition),
    IndexAble(String, Postition),
    NotMethodOrProperty(String, Postition),
    TooManyProperty(Postition),
    TooLittleProperty(Postition),
    ExpectedOneOf(String),
    NotInstanceOrClass(Type,Symbol,Postition),
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum Type {
    Class {
        name: Symbol,
        methods: Vec<(Symbol, Entry)>,
        fields: Vec<(Symbol, Type)>,
    },
    This(Vec<(Symbol, Type)>, Vec<(Symbol, Type)>),
    Int,
    Str,
    Bool,
    Nil,
    Float,
    Func(Vec<Type>, Box<Type>),
    Dict(Box<Type>, Box<Type>), // Key, Value
    Array(Box<Type>),
    Name(Symbol, Box<Type>),
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            Type::Class { ref name,ref methods,ref fields } => write!(f, "Class {} with Fields {:#?} and Methods {:#?}", name,fields,methods),
            Type::Int => write!(f, "Int"),
            Type::Str => write!(f, "Str"),
            Type::Bool => write!(f, "Boolean"),
            Type::Nil => write!(f, "Nil"),
            Type::Float => write!(f, "Float"),
            Type::This(ref methods, ref fields) => write!(
                f,
                "'This' has the fields {:?} and methods {:?}",
                methods, fields
            ),
            Type::Func(ref params, ref returns) => write!(
                f,
                "Func with param types {:?} returns {:?}",
                params, returns
            ),
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
            TypeError::IndexAble(ref name, ref pos) => {
                write!(f, "\'{}\' is not an indexable on {}", name, pos)
            }

            TypeError::InvalidIndex(ref pos) => write!(f, "Cannot index on line '{}'", pos),

            TypeError::UndefindedVar(ref name, ref pos) => {
                write!(f, "Undefinded variable \'{}\' on {}", name, pos)
            }

            TypeError::NotInstanceOrClass(ref ty,ref property,ref pos) => {
                write!(f,"Type {} dosen't have the method/field {} on {}",ty,property, pos)
            }

            TypeError::ExpectedOneOf(ref msg) => write!(f, "{}", msg),

            TypeError::NotMethodOrProperty(ref name, ref pos) => {
                write!(f, "Undefined method/property \'{}\' on {}", name, pos)
            }

            TypeError::UndefindedType(ref name, ref pos) => {
                write!(f, "Undefined type \'{}\' on {}", name, pos)
            }
            TypeError::Function(ref pos) => {
                write!(f, "Type should be a variable not a function on {}", pos)
            }

            TypeError::TooLittleProperty(ref pos) => write!(f, "Expected more fields on {}", pos),
            TypeError::TooManyProperty(ref pos) => write!(f, "Expected less fields on {}", pos),
        }
    }
}
