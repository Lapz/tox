//! Contains all the values that are objects within tox
//! When adding a new object make sure the first field is obj:Object otherwise the transmutes will fail

use super::Function;
use crate::value::Value;
use fnv::FnvHashMap;
use std::fmt::{self, Debug, Display};
use std::ops::Deref;
use util::symbol::Symbol;

pub type RawObject = *mut Object;
pub type NativeFn = fn(*const Value) -> Value;

#[derive(PartialEq, Debug, Clone, Copy)]
#[repr(C)]
pub enum ObjectType {
    String,
    Func,
    Array,
    Class,
    Instance,
    Native,
    Enum,
}

#[derive(Debug, Clone, PartialEq)]
#[repr(C)]
pub struct NativeObject {
    pub obj: Object,
    pub arity: u8,
    pub function: NativeFn,
}

#[derive(Debug, Clone, PartialEq)]
#[repr(C)]
pub struct Object {
    pub ty: ObjectType,
    pub next: RawObject,
}

#[derive(Debug, Clone, PartialEq)]
#[repr(C)]
pub struct EnumObject {
    pub obj: Object,
    pub name: Symbol,
    pub tag: u32,
    pub data: Option<Value>,
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
    pub items: Vec<Value>,
}

#[derive(Debug, Clone, PartialEq)]
#[repr(C)]
pub struct ClassObject {
    pub obj: Object,
    pub methods: FnvHashMap<Symbol, FunctionObject>,
}

#[derive(Debug, Clone)]
#[repr(C)]
pub struct InstanceObject {
    pub obj: Object,
    pub properties: FnvHashMap<Symbol, Value>,
    pub methods: FnvHashMap<Symbol, Function>,
}

#[derive(Debug, Clone, PartialEq)]
#[repr(C)]
pub struct FunctionObject {
    pub obj: Object,
    pub arity: usize,
    pub function: super::Function,
}

#[derive(Clone, PartialEq)]
#[repr(C)]
pub enum ObjectValue<'a> {
    /// A pointer to a string
    Str(&'a str),

    /// A pointer to some bytes
    Mem(&'a [u8]),
    /// An actual rust string
    String(String),
}

impl Object {
    pub fn new(ty: ObjectType, next: RawObject) -> Self {
        Object { ty, next }
    }
}

impl NativeObject {
    pub fn new(arity: u8, function: NativeFn, next: RawObject) -> RawObject {
        let func = NativeObject {
            obj: Object::new(ObjectType::Native, next),
            function,
            arity,
        };

        Box::into_raw(Box::new(func)) as RawObject
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
    pub fn new(items: Vec<Value>, next: RawObject) -> RawObject {
        let array = ArrayObject {
            obj: Object::new(ObjectType::Array, next),
            items,
        };

        Box::into_raw(Box::new(array)) as RawObject
    }
}

impl EnumObject {
    pub fn new(name: Symbol, tag: u32, data: Option<Value>, next: RawObject) -> RawObject {
        let _enum = EnumObject {
            name,
            obj: Object::new(ObjectType::Enum, next),
            tag,
            data,
        };

        Box::into_raw(Box::new(_enum)) as RawObject
    }
}

impl InstanceObject {
    pub fn new(
        methods: FnvHashMap<Symbol, Function>,
        properties: FnvHashMap<Symbol, Value>,
        next: RawObject,
    ) -> RawObject {
        let array = InstanceObject {
            obj: Object::new(ObjectType::Instance, next),
            methods,
            properties,
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

    /// Conserveatly copies the string from the pointer
    pub fn from_bytes(string: &'a [u8], next: RawObject) -> RawObject {
        let s = StringObject {
            obj: Object::new(ObjectType::String, next),
            chars: ObjectValue::Mem(string),
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

    pub fn value(&self) -> &str {
        self.chars.string()
    }
}

impl<'a> ObjectValue<'a> {
    pub fn string(&self) -> &str {
        match *self {
            ObjectValue::Str(ref string) => string,
            ObjectValue::String(ref string) => string,
            ObjectValue::Mem(ref bytes) => std::str::from_utf8(bytes).unwrap(),
        }
    }
}

impl<'a> Deref for StringObject<'a> {
    type Target = Object;

    fn deref(&self) -> &Self::Target {
        &self.obj
    }
}

impl<'a> Display for ObjectValue<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            ObjectValue::Str(ref string) => write!(f, "{}", string),
            ObjectValue::String(ref string) => write!(f, "{}", string),
            ObjectValue::Mem(ref bytes) => write!(f, "{}", std::str::from_utf8(bytes).unwrap()),
        }
    }
}

impl<'a> Debug for ObjectValue<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            ObjectValue::Str(ref string) => write!(f, "static: {:?}", string),
            ObjectValue::String(ref string) => write!(f, "new: {:?}", string),
            ObjectValue::Mem(ref bytes) => {
                write!(f, "static   {:?}", std::str::from_utf8(bytes).unwrap())
            }
        }
    }
}

impl<'a> Display for StringObject<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.chars)
    }
}
