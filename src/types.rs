// use std::collections::{HashMap, HashSet};
use pos::Postition;
#[derive(Debug, Clone)]
pub enum TypeError {
    Recursive,
    Expected(Type, Postition),
    Undefinded,
    NotSame,
    Duplicate,
    Function,
}

pub struct Unique {
    id: i64,
}

// pub struct TypeEnv<'a>(HashMap<Variable<'a>, Type>);

pub struct TypeVar(i64);

impl Unique {
    fn new() -> Self {
        Unique { id: 0 }
    }

    fn next(&mut self) -> TypeVar {
        let v = self.id;

        self.id += 1;
        TypeVar(v)
    }
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
