use std::collections::{HashMap, HashSet};
use ast::expr::Variable;

#[derive(Debug)]
pub enum TypeError {
    Recursive,
    Expected(Type),
}
pub struct Unique {
    id: i64,
}

pub struct TypeEnv<'a>(HashMap<Variable<'a>, Type>);

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

#[derive(Debug, PartialEq)]
pub enum Type {
    Int,
    Str,
    Bool,
    Nil,
    Float,
}
