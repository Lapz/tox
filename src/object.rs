use std::ops::Not;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::cmp::{Ordering, PartialOrd};
use std::fmt::{Display, Formatter};
use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Float(f64),
    Int(i64),
    Str(String),
    Bool(bool),
    Return(Box<Object>),
    Array(Vec<Object>),
    Dict(HashMap<Object, Object>),
    Nil,
    None,
}

impl Object {
    pub fn is_truthy(&self) -> bool {
        match *self {
            Object::Nil => false,
            Object::Bool(b) => b,
            _ => true,
        }
    }

    pub fn as_string(&self) -> String {
        match *self {
            Object::Float(f) => f.to_string(),
            Object::None => "None".into(),
            Object::Return(ref val) => val.as_string(),
            Object::Dict(ref hashmap) => {
                let mut fmt_string = String::new();
                fmt_string.push_str("{");
                for (i, (k, v)) in hashmap.iter().enumerate() {
                    fmt_string.push_str(format!("{} : {}", k, v).as_str());
                    if i < hashmap.len() - 1 {
                        fmt_string.push_str(", ");
                    }
                }
                fmt_string.push_str("}");
                fmt_string
            }

            Object::Int(b) => b.to_string(),
            Object::Bool(b) => b.to_string(),
            Object::Nil => "nil".to_string(),
            Object::Str(ref s) => s.clone(),
            Object::Array(ref v) => {
                let mut fmt_string = String::new();
                fmt_string.push_str("[");
                for (i, o) in v.iter().enumerate() {
                    fmt_string.push_str(format!("{}", o).as_str());
                    if i < v.len() - 1 {
                        fmt_string.push_str(", ");
                    }
                }
                fmt_string.push_str("]");

                fmt_string
            }
        }
    }
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
            (&Object::None, &Object::None) => Some(Ordering::Equal),
            (s, o) => (s.partial_cmp(o)),
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            Object::None => write!(f, "none"),
            Object::Return(ref r) => write!(f, "return {}", r),
            Object::Float(ref n) => write!(f, "{}", n.to_string()),
            Object::Int(ref n) => write!(f, "{}", n.to_string()),
            Object::Bool(ref b) => write!(f, "{}", b.to_string()),
            Object::Array(ref v) => {
                let mut fmt_string = String::new();
                fmt_string.push_str("[");
                for (i, o) in v.iter().enumerate() {
                    fmt_string.push_str(format!("{}", o).as_str());
                    if i < v.len() - 1 {
                        fmt_string.push_str(", ");
                    }
                }
                fmt_string.push_str("]");
                write!(f, "{}", fmt_string)
            }
            Object::Nil => write!(f, "nil"),

            Object::Str(ref s) => write!(f, "{}", s.clone()),

            Object::Dict(ref hashmap) => {
                let mut fmt_string = String::new();
                fmt_string.push_str("{");
                for (i, (k, v)) in hashmap.iter().enumerate() {
                    fmt_string.push_str(format!("{} : {}", k, v).as_str());
                    if i < hashmap.len() - 1 {
                        fmt_string.push_str(", ");
                    }
                }
                fmt_string.push_str("}");
                write!(f, "{}", fmt_string)
            }
        }
    }
}
