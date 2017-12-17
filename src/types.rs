// use std::collections::{HashMap, HashSet};
use pos::Postition;
#[derive(Debug, Clone)]
pub enum TypeError {
    Expected(Type, Postition),
    Undefinded,
    NotSame,
    Function,
    InvalidIndex,
}


#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum Type {
    Int,
    Str,
    Bool,
    Nil,
    Float,
    Dict(Box<Type>, Box<Type>), // Key, Value
    Array(Box<Type>),
}
