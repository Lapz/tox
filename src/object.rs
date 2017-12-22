use std::ops::Not;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::cmp::{Ordering, PartialOrd};

#[derive(Debug, PartialEq,Clone)]
pub enum Object {
    Float(f64),
    Int(i64),
    Str(String),
    Bool(bool),
    Return(Box<Object>),
    Array(Vec<Object>),
    Dict(HashMap<Object,Object>),
    Nil,
    None,
}


impl Eq for Object {}

impl Hash for Object {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match *self {
            Object::Int(i) => i.hash(state),
            Object::Float(f) => {
                let hash = f as i64;
                hash.hash(state)
            }
            Object::Bool(ref b) => b.hash(state),
            Object::Str(ref s) => s.hash(state),
            Object::Nil => "nil".hash(state),
            _ => "".hash(state),
        }
    }
}

impl Not for Object {
    type Output = Object;

    fn not(self) -> Object {
        match self {
            Object::Bool(b) => Object::Bool(!b),
            _ => Object::Bool(false),
        }
    }
}


impl PartialOrd for Object {
    fn partial_cmp(&self, other: &Object) -> Option<Ordering> {
        match (self, other) {
            (&Object::Float(ref s), &Object::Float(ref o)) => (s.partial_cmp(o)),
            (&Object::Int(ref s), &Object::Int(ref o)) => (s.partial_cmp(o)),
            (&Object::Str(ref s), &Object::Str(ref o)) => (s.partial_cmp(o)),
            (&Object::Bool(ref s), &Object::Bool(ref o)) => (s.partial_cmp(o)),
            (&Object::Return(ref s), &Object::Return(ref o)) => (s.partial_cmp(o)),
            (&Object::Array(ref a), &Object::Array(ref o)) => (a.partial_cmp(o)),
            (&Object::Dict(ref d), &Object::Dict(ref o)) => (d.iter().partial_cmp(o.iter())),
            (&Object::Nil, &Object::Nil) => Some(Ordering::Equal),
            (&Object::None,&Object::None) => Some(Ordering::Equal),
            (s,o) =>(s.partial_cmp(o)),
        }
    }
}