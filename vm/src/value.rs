use crate::object::{
    ArrayObject, ClassObject, FunctionObject, InstanceObject, NativeObject, Object, ObjectType,
    RawObject, StringObject,
};

use std::fmt::{self, Debug, Display};
use std::mem;

// A mask that selects the sign bit.
const SIGN_BIT: u64 =  1 << 63; // 1 << 63
// The bits that must be set to indicate a quiet NaN.
const QNAN: u64 = 0x7ffc000000000000 as u64;

const TAG_NIL: u64 = 1;
const TAG_FALSE: u64 = 2;
const TAG_TRUE: u64 = 3;
const TAG_INT: u64 = 4;
const TAG_FLOAT: u64 = 5;

const TRUE_VAL: Value = VMValue(QNAN | TAG_TRUE);
const FALSE_VAL: Value = VMValue(QNAN | TAG_FALSE);
const NIL_VAL: Value = VMValue(QNAN | TAG_NIL);

#[derive(Clone, Copy,PartialEq)]
pub struct VMValue(pub u64);

impl VMValue {
    pub fn inner(&self) -> u64 {
        self.0
    }
}
pub type Value = VMValue;


/// A union of the values
#[derive(Clone, Copy)]
#[repr(C)]
pub union NumberUnion {
    bits64: u64,
    bits: [u32; 2],
    float: f64,
    int: i64,
}


impl VMValue {
    pub fn bool(value: bool) -> Value {
        if value {
            TRUE_VAL
        } else {
            FALSE_VAL
        }
    }

    pub fn nil() -> Value {
        VMValue(QNAN | TAG_NIL)
    }

    pub fn float(float: f64) -> Value {
        unsafe {
            let mut data: NumberUnion = std::mem::uninitialized();
            data.float = float;
            return VMValue(data.bits64);
        }
    }

    pub fn int(int: i64) -> Value {
        unsafe {
            let mut data: NumberUnion = std::mem::uninitialized();
            data.int = int;
            return VMValue(data.bits64);
        }
    }

    pub fn object(object: RawObject) -> Value {
    
        VMValue( (SIGN_BIT | QNAN | object as usize as u64) as u64)
    }

    #[inline]
    pub fn is_obj_type(&self, ty: ObjectType) -> bool {
        unsafe {
            self.is_object() && std::mem::transmute::<RawObject, &Object>(self.as_object()).ty == ty
        }
    }

    #[inline]
    pub fn is_object(&self) -> bool {
        (((self.inner()) & (QNAN | SIGN_BIT)) == (QNAN | SIGN_BIT))
    }

    #[inline]
    pub fn is_class(&self) -> bool {
        self.is_obj_type(ObjectType::Class)
    }

    #[inline]
    pub fn is_string(&self) -> bool {
        self.is_obj_type(ObjectType::String)
    }

    #[inline]
    pub fn is_native(&self) -> bool {
        self.is_obj_type(ObjectType::Native)
    }

    #[inline]
    pub fn is_int(&self) -> bool {
        (((self.inner()) & (QNAN | TAG_INT)) == (QNAN | TAG_INT))
    }

    #[inline]
    pub fn is_float(&self) -> bool {
        (((self.inner()) & (QNAN | TAG_FLOAT)) == (QNAN | TAG_FLOAT))
    }

    #[inline]
    pub fn is_nil(&self) -> bool {
        self == &NIL_VAL
    }

    #[inline]
    pub fn is_bool(&self) -> bool {
        (((self.inner()) & (QNAN | TAG_FALSE)) == (QNAN | TAG_FALSE))
    }

    #[inline]
    pub fn as_bool(&self) -> bool {
        self == &TRUE_VAL
    }

    #[inline]
    pub fn as_int(&self) -> i64 {
        unsafe {
            let mut data: NumberUnion = std::mem::uninitialized();

            data.bits64 = self.inner();

            data.int
        }
    }

    #[inline]
    pub fn as_float(&self) -> f64 {
        unsafe {
            let mut data: NumberUnion = std::mem::uninitialized();

            data.bits64 = self.inner();

            data.float
        }
    }

    #[inline]
    pub fn as_object(&self) -> RawObject {
        (self.inner() & !(SIGN_BIT | QNAN)) as usize as RawObject
    }

    #[inline]
    pub fn as_string<'a>(&self) -> &'a StringObject<'a> {
        let ptr = self.as_object();

        unsafe { mem::transmute(ptr) }
    }

    #[inline]
    pub fn as_array<'a>(&self) -> &'a ArrayObject {
        let ptr = self.as_object();

        unsafe { mem::transmute(ptr) }
    }

    #[inline]
    pub fn as_class<'a>(&self) -> &'a ClassObject {
        let ptr = self.as_object();

        unsafe { mem::transmute(ptr) }
    }

    #[inline]
    pub fn as_function<'a>(&self) -> &'a FunctionObject {
        let ptr = self.as_object();

        unsafe { mem::transmute(ptr) }
    }

    #[inline]
    pub fn as_native<'a>(&self) -> &'a NativeObject {
        let ptr = self.as_object();

        unsafe { mem::transmute(ptr) }
    }

    #[inline]
    pub fn as_instance<'a>(&self) -> &'a InstanceObject {
        let ptr = self.as_object();

        unsafe { mem::transmute(ptr) }
    }

    #[inline]
    pub fn as_mut_instance<'a>(&self) -> &'a mut InstanceObject {
        let ptr = self.as_object();

        unsafe { mem::transmute(ptr) }
    }
}

impl Debug for Value {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "Value {{")?;
        unsafe {
            if self.is_object() {
                let obj: &Object = std::mem::transmute(self.as_object());

                match obj.ty {
                    ObjectType::String => write!(
                        fmt,
                        "{:#?}",
                        ::std::mem::transmute::<RawObject, &StringObject>(self.as_object())
                    )?,

                    ObjectType::Func => write!(
                        fmt,
                        "{:#?}",
                        ::std::mem::transmute::<RawObject, &FunctionObject>(self.as_object())
                    )?,
                    ObjectType::Array => write!(
                        fmt,
                        "{:#?}",
                        ::std::mem::transmute::<RawObject, &ArrayObject>(self.as_object())
                    )?,
                    ObjectType::Class => write!(
                        fmt,
                        "{:#?}",
                        ::std::mem::transmute::<RawObject, &ClassObject>(self.as_object())
                    )?,
                    ObjectType::Instance => write!(
                        fmt,
                        "{:#?}",
                        ::std::mem::transmute::<RawObject, &InstanceObject>(self.as_object())
                    )?,

                    ObjectType::Native => write!(
                        fmt,
                        "{:#?}",
                        ::std::mem::transmute::<RawObject, &NativeObject>(self.as_object())
                    )?,
                }
            }
        }
        //     match self.ty {
        //         ValueType::Int  ValueType::Nil => write!(fmt, "val:{:?},", self.val.int)?,

        //         ValueType::Float => {
        //             write!(fmt, "val:{:?},", self.val.float)?;
        //         }

        //         ValueType::Bool => {
        //             write!(fmt, "val:{:?},", self.val.boolean)?;
        //         }

        //         ValueType::Object => {
        //             let obj: &Object = mem::transmute(self.as_object());

        //             match obj.ty {
        // ObjectType::String =>
        //                 O
        //             }
        //         }
        //     }
        // }

        write!(fmt, "{:?}", self.0)?;
        write!(fmt, "}}")?;
        Ok(())
    }
}

impl Display for Value {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        unsafe {
            if self.is_int() {
                write!(fmt, "{}", self.as_int())?;
            } else if self.is_nil() {
                write!(fmt, "nil")?;
            } else if self.is_float() {
                write!(fmt, "{}", self.as_float())?;
            } else if self.is_bool() {
                write!(fmt, "{}", self.as_bool())?;
            } else if self.is_object() {
                let obj: &Object = mem::transmute(self.as_object());

                match obj.ty {
                    ObjectType::String => write!(fmt, "{}", self.as_string())?,
                    ObjectType::Func => write!(fmt, " fun")?,
                    ObjectType::Array => write!(fmt, "array")?,
                    ObjectType::Class => write!(fmt, "class")?,
                    ObjectType::Instance => write!(fmt, "instance")?,
                    ObjectType::Native => write!(fmt, "native")?,
                }
            }
        }

        Ok(())
    }
}


