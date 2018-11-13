use chunk::Chunk;
use std::fmt::{self, Debug, Display};
use std::mem;
use std::ops::Deref;
use value::Value;

pub type RawObject = *mut Object;

#[derive(PartialEq, Debug, Clone, Copy)]
#[repr(C)]
pub enum ObjectType {
    String,
    Func,
    Array,
}

#[derive(Debug, Clone)]
#[repr(C)]
pub struct Object {
    pub ty: ObjectType,
    pub next: RawObject,
}

#[derive(Debug, Clone)]
#[repr(C)]
pub struct StringObject<'a> {
    pub obj: Object,
    pub chars: ObjectValue<'a>,
}

#[derive(Debug, Clone)]
#[repr(C)]
pub struct ArrayObject {
    pub obj: Object,
    pub items: Vec<Value>
}

#[derive(Debug, Clone)]
#[repr(C)]
pub struct FunctionObject {
    pub obj: Object,
    pub arity: usize,
    pub function: super::Function,
}

#[derive(Clone)]
#[repr(C)]
pub enum ObjectValue<'a> {
    /// A pointer to a string
    Str(&'a str),

    /// An actual rust string
    String(String),
}

impl Object {
    pub fn new(ty: ObjectType, next: RawObject) -> Self {
        Object { ty, next }
    }
}

impl FunctionObject {
    pub fn new(arity: usize, function: super::Function, next: RawObject) -> RawObject {
        let func = FunctionObject {
            obj: Object::new(ObjectType::Func, next),
            function,
            arity,
        };

        Box::into_raw(Box::new(func)) as RawObject
    }
}

impl ArrayObject {
     pub fn new(items: Vec<Value>,next: RawObject) -> RawObject {
        let array = ArrayObject {
            obj: Object::new(ObjectType::Array, next),
            items,
        };

        Box::into_raw(Box::new(array)) as RawObject
    }
}
impl<'a> StringObject<'a> {
    /// Create a new string Object that dosen't take ownership of the string passed in
    /// Conserveatly copies the string from the pointer
    pub fn new(string: &'a str, next: RawObject) -> RawObject {
        let s = StringObject {
            obj: Object::new(ObjectType::String, next),
            chars: ObjectValue::Str(string),
        };

        Box::into_raw(Box::new(s)) as RawObject
    }

    /// Creates a new String Object that takes ownership of the string passed in
    pub fn from_owned(chars: String, next: RawObject) -> RawObject {
        let s = StringObject {
            obj: Object::new(ObjectType::String, next),
            chars: ObjectValue::String(chars),
        };

        Box::into_raw(Box::new(s)) as RawObject
    }
}

impl<'a> ObjectValue<'a> {
    pub fn string(&self) -> &str {
        match *self {
            ObjectValue::Str(ref string) => string,
            ObjectValue::String(ref string) => string,
        }
    }
}

impl<'a> Deref for StringObject<'a> {
    type Target = Object;

    fn deref(&self) -> &Self::Target {
        &self.obj
    }
}

impl Drop for Object {
    fn drop(&mut self) {
        match self.ty {
            ObjectType::String => unsafe {
                let string: &StringObject = mem::transmute(self);

                mem::drop(string);
            },

            ObjectType::Func => (),
            ObjectType::Array => {
                ()
            }
        }
    }
}

impl<'a> Drop for StringObject<'a> {
    fn drop(&mut self) {
        match &self.chars {
            ObjectValue::String(string) => mem::drop(string),
            _ => (),
        }
    }
}

impl<'a> Display for ObjectValue<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ObjectValue::Str(ref string) => write!(f, "{}", string)?,
            ObjectValue::String(ref string) => write!(f, "{}", string)?,
        }
        Ok(())
    }
}

impl<'a> Debug for ObjectValue<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ObjectValue::Str(ref string) => write!(f, "static: {:?}", string)?,
            ObjectValue::String(ref string) => write!(f, "new: {:?}", string)?,
        }
        Ok(())
    }
}

impl<'a> Display for StringObject<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.chars)?;

        Ok(())
    }
}
