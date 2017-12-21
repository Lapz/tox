// use std::collections::{HashMap, HashSet};
use pos::Postition;
use symbol::Symbol;
use env::Entry;
#[derive(Debug, Clone)]
pub enum TypeError {
    Expected(Type, Postition),
    Undefinded,
    UndefindedVar,
    NotSame(String),
    Function,
    InvalidIndex,
    NotArray,
    NotProperty,
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
