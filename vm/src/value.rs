use object::{FunctionObject, Object, ObjectType, RawObject, StringObject};
use std::fmt::{self, Debug, Display};
use std::mem;
#[derive(Clone, Copy)]
/// A value within the VM
pub struct Value {
    val: As,
    ty: ValueType,
}

/// The possible types of values
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ValueType {
    Int,
    Float,
    Nil,
    Bool,
    Object,
}

/// A union of the values
#[derive(Clone, Copy)]
#[repr(C)]
pub union As {
    boolean: bool,
    float: f64,
    int: i64,
    /// A values whos state is stored on the heap
    object: RawObject,
}

impl Value {
    pub fn bool(value: bool) -> Value {
        Value {
            val: As { boolean: value },
            ty: ValueType::Bool,
        }
    }

    pub fn nil() -> Value {
        Value {
            val: As { int: 0 },
            ty: ValueType::Nil,
        }
    }

    pub fn float(float: f64) -> Value {
        Value {
            val: As { float },
            ty: ValueType::Float,
        }
    }

    pub fn int(int: i64) -> Value {
        Value {
            val: As { int },
            ty: ValueType::Int,
        }
    }

    pub fn object(object: RawObject) -> Value {
        Value {
            val: As { object },
            ty: ValueType::Object,
        }
    }

    pub fn as_bool(&self) -> bool {
        debug_assert_eq!(
            self.ty,
            ValueType::Bool,
            "Value is type `{:?}` instead of {:?}",
            self.ty,
            ValueType::Bool
        );

        unsafe { self.val.boolean }
    }

    pub fn as_int(&self) -> i64 {
        debug_assert_eq!(
            self.ty,
            ValueType::Int,
            "Value is type `{:?}` instead of {:?}",
            self.ty,
            ValueType::Int
        );

        unsafe { self.val.int }
    }

     pub fn as_float(&self) -> f64 {
        debug_assert_eq!(
            self.ty,
            ValueType::Float,
            "Value is type `{:?}` instead of {:?}",
            self.ty,
            ValueType::Float
        );

        unsafe { self.val.float }
    }

    pub fn as_object(&self) -> RawObject {
        debug_assert_eq!(
            self.ty,
            ValueType::Object,
            "Value is type `{:?}` instead of {:?}",
            self.ty,
            ValueType::Object
        );

        unsafe { self.val.object }
    }

    pub fn as_string<'a>(&self) -> &StringObject<'a> {
        let ptr = self.as_object();

        unsafe { mem::transmute(ptr) }
    }

    pub fn as_function<'a>(&self) -> &'a FunctionObject {
        let ptr = self.as_object();

        unsafe { mem::transmute(ptr) }
    }
    #[inline]
    pub fn is_object(&self) -> bool {
        self.ty == ValueType::Object
    }

    pub fn is_string(&self) -> bool {
        unsafe {
            self.is_object()
                && mem::transmute::<RawObject, &Object>(self.as_object()).ty == ObjectType::String
        }
    }

   
}

impl Debug for Value {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "Value {{")?;
        unsafe {

            match self.ty {
                ValueType::Int | ValueType::Nil => {
                    write!(fmt, "val:{:?},", self.val.int)?
                },

                ValueType::Float => {
                    write!(fmt, "val:{:?},", self.val.float)?;
                },

                ValueType::Bool => {
                    write!(fmt, "val:{:?},", self.val.boolean)?;
                },

                ValueType::Object => {
                    if self.is_string() {

                    }else {
                        write!(fmt, "val:{:?}",::std::mem::transmute::<RawObject,&Object,>(self.val.object))?;
                    }
                }
            }
        }
        
        write!(fmt, " ty:{:?}", self.ty)?;
        write!(fmt, "}}")?;
        Ok(())
    }
}

impl Display for Value {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        unsafe {
            if self.ty == ValueType::Int {
                write!(fmt, "{}", self.val.int)?;
            } else if self.ty == ValueType::Nil {
                write!(fmt, "nil")?;
            } else if self.ty == ValueType::Float {
                write!(fmt, "{}", self.val.float)?;
            } else if self.ty == ValueType::Bool {
                write!(fmt, "{}", self.val.boolean)?;
            } else if self.ty == ValueType::Object {
                let obj: &Object = mem::transmute(self.as_object());

                match obj.ty {
                    ObjectType::String => write!(fmt, "{}", self.as_string())?,
                    ObjectType::Func => write!(fmt, " fun")?,
                }
            }
        }

        Ok(())
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Value) -> bool {
        if self.ty != other.ty {
            false
        } else {
            match self.ty {
                ValueType::Bool => self.as_bool() == other.as_bool(),
                ValueType::Nil => false,
                ValueType::Int => self.as_int() == other.as_int(),
                ValueType::Float => self.as_float() == other.as_float(),
                ValueType::Object => unimplemented!(),
            }
        }
    }
}
