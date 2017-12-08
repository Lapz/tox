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
            Object::Nil => Object::Bool(true),
            Object::Bool(b) => Object::Bool(!b),
            _ => Object::Bool(false),
        }
    }
}



// impl PartialOrd for Object {
//     fn partial_cmp(&self,other:&Object) -> Option<Ordering> {
//         match (self,other) {
//             (&Object::Float(l), &Object::Float(r)) => Some(l.cmp(&r)),
//             (&Object::Int(l),&Object::Int(r)) => Some(l.cmp(&r)),
//             (&Object::Str(ref l ), &Object::Str(ref r)) => Some(l.cmp(&r)),
//             _ => None
//         }
//     }
// }

// impl PartialEq for Object {
//     fn eq(&self,other:&Object) -> bool {

//     }
// }
