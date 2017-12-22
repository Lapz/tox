use std::ops::Not;

#[derive(Debug, PartialEq, Clone, PartialOrd)]
pub enum Object {
    Float(f64),
    Int(i64),
    Str(String),
    Bool(bool),
    Nil,
    None,
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
