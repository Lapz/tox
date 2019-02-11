#[cfg(feature = "NAN-tagging")]
pub use self::NAN_tagging::*;

#[cfg(not(feature = "NAN_tagging"))]
pub use self::normal::*;

#[cfg(feature = "NAN-tagging")]
mod NAN_tagging {

    use crate::object::{
        ArrayObject, ClassObject, FunctionObject, InstanceObject, NativeObject, Object, ObjectType,
        RawObject, StringObject,
    };

    use std::fmt::{self, Debug, Display};
    use std::mem;
    // A mask that selects the sign bit.
    const SIGN_BIT: u64 = 1 << 63; // 1 << 63
                                   // The bits that must be set to indicate a quiet NaN.
    const QNAN: u64 = 0x7ffc000000000000;

    const TAG_NIL: u64 = 1;
    const TAG_FALSE: u64 = 2;
    const TAG_TRUE: u64 = 3;
    const TAG_INT: u64 = 4;
    const TAG_FLOAT: u64 = 5;

    const TRUE_VAL: Value = VMValue(QNAN | TAG_TRUE);
    const FALSE_VAL: Value = VMValue(QNAN | TAG_FALSE);
    const NIL_VAL: Value = VMValue(QNAN | TAG_NIL);

    #[derive(Clone, Copy)]
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
        num: f64,
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

                data.num = float;

                VMValue(TAG_FLOAT | data.bits64)
            }
        }

        pub fn int(int: i64) -> Value {
            unsafe {
                let mut data: NumberUnion = std::mem::uninitialized();

                data.num = int as f64;

                VMValue(TAG_INT | data.bits64)
            }
        }

        pub fn object(object: RawObject) -> Value {
            VMValue((SIGN_BIT | QNAN | object as usize as u64) as u64)
        }

        #[inline]
        pub fn is_object(&self) -> bool {
            (((self.inner()) & (QNAN | SIGN_BIT)) == (QNAN | SIGN_BIT))
        }

        #[inline]
        pub fn is_obj_type(&self, ty: ObjectType) -> bool {
            unsafe {
                self.is_object()
                    && std::mem::transmute::<RawObject, &Object>(self.as_object()).ty == ty
            }
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
            (self.inner() & TAG_INT) == TAG_INT
        }

        #[inline]
        pub fn is_float(&self) -> bool {
            (self.inner() & TAG_FLOAT) == TAG_FLOAT
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

                data.bits64 = self.inner() & !TAG_INT;

                data.num as i64
            }
        }

        #[inline]
        pub fn as_float(&self) -> f64 {
            unsafe {
                let mut data: NumberUnion = std::mem::uninitialized();
                data.bits64 = self.inner() & !TAG_FLOAT;
                println!("{:?}", data.num);
                println!("{:b}", data.num.to_bits());
                data.num
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
                } else if self.is_nil() {
                    write!(fmt, "nil")?
                } else if self.is_bool() {
                    write!(fmt, "{:?}", self.as_bool())?;
                } else if self.is_int() {
                    write!(fmt, "{:?}", self.as_int())?;
                } else if self.is_float() {
                    write!(fmt, "{:?}", self.as_float())?;
                }
            }

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

    impl PartialEq for VMValue {
        fn eq(&self, other: &Self) -> bool {
            if self.inner() == other.inner() {
                true
            } else if !self.is_object() || !other.is_object() {
                false
            } else {
                unsafe {
                    let self_object: &Object = std::mem::transmute(self.as_object());
                    match self_object.ty {
                        ObjectType::String => {
                            let self_string: &StringObject = std::mem::transmute(self.as_object());
                            let other_string: &StringObject =
                                std::mem::transmute(other.as_object());

                            self_string.chars == other_string.chars
                        }

                        _ => false,
                    }
                }
            }
        }
    }
}

#[cfg(not(feature = "NAN_tagging"))]
mod normal {
    use crate::object::{
        ArrayObject, ClassObject, EnumObject, FunctionObject, InstanceObject, NativeObject, Object,
        ObjectType, RawObject, StringObject,
    };

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

        #[inline]
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

        #[inline]
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

        #[inline]
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

        #[inline]
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

        #[inline]
        pub fn as_string<'a>(&self) -> &StringObject<'a> {
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

        #[inline]
        pub fn is_object(&self) -> bool {
            self.ty == ValueType::Object
        }

        #[inline]
        pub fn is_class(&self) -> bool {
            unsafe {
                self.is_object()
                    && mem::transmute::<RawObject, &Object>(self.as_object()).ty
                        == ObjectType::Class
            }
        }

        #[inline]
        pub fn is_string(&self) -> bool {
            unsafe {
                self.is_object()
                    && mem::transmute::<RawObject, &Object>(self.as_object()).ty
                        == ObjectType::String
            }
        }
        #[inline]
        pub fn is_native(&self) -> bool {
            unsafe {
                self.is_object()
                    && mem::transmute::<RawObject, &Object>(self.as_object()).ty
                        == ObjectType::Native
            }
        }
    }

    impl Debug for Value {
        fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(fmt, "Value {{")?;
            unsafe {
                match self.ty {
                    ValueType::Int | ValueType::Nil => write!(fmt, "val:{:?},", self.val.int)?,

                    ValueType::Float => {
                        write!(fmt, "val:{:?},", self.val.float)?;
                    }

                    ValueType::Bool => {
                        write!(fmt, "val:{:?},", self.val.boolean)?;
                    }

                    ValueType::Object => {
                        let obj: &Object = mem::transmute(self.as_object());

                        match obj.ty {
                            ObjectType::String => write!(
                                fmt,
                                "{:#?}",
                                ::std::mem::transmute::<RawObject, &StringObject>(self.val.object)
                            )?,
                            ObjectType::Func => write!(
                                fmt,
                                "{:#?}",
                                ::std::mem::transmute::<RawObject, &FunctionObject>(
                                    self.val.object
                                )
                            )?,
                            ObjectType::Array => write!(
                                fmt,
                                "{:#?}",
                                ::std::mem::transmute::<RawObject, &ArrayObject>(self.val.object)
                            )?,
                            ObjectType::Class => write!(
                                fmt,
                                "{:#?}",
                                ::std::mem::transmute::<RawObject, &ClassObject>(self.val.object)
                            )?,
                            ObjectType::Instance => write!(
                                fmt,
                                "{:#?}",
                                ::std::mem::transmute::<RawObject, &InstanceObject>(
                                    self.val.object
                                )
                            )?,

                            ObjectType::Enum => write!(
                                fmt,
                                "{:#?}",
                                ::std::mem::transmute::<RawObject, &EnumObject>(
                                    self.val.object
                                )
                            )?,

                            ObjectType::Native => write!(
                                fmt,
                                "{:#?}",
                                ::std::mem::transmute::<RawObject, &NativeObject>(self.val.object)
                            )?,
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
        fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
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
                        ObjectType::Array => write!(fmt, "array")?,
                        ObjectType::Class => write!(fmt, "class")?,
                        ObjectType::Instance => write!(fmt, "instance")?,
                        ObjectType::Native => write!(fmt, "native")?,
                        ObjectType::Enum => write!(fmt,"enum")?,
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
                    ValueType::Object => unsafe {
                        let self_object: &Object = std::mem::transmute(self.as_object());

                        match self_object.ty {
                            ObjectType::String => {
                                let self_string: &StringObject =
                                    std::mem::transmute(self.as_object());
                                let other_string: &StringObject =
                                    std::mem::transmute(other.as_object());

                                self_string.chars == other_string.chars
                            }
                            ObjectType::Array => {
                                let self_array: &ArrayObject =
                                    std::mem::transmute(self.as_object());
                                let other_array: &ArrayObject =
                                    std::mem::transmute(other.as_object());

                                self_array.items == other_array.items
                            }

                            ObjectType::Instance => {
                                let self_instance: &InstanceObject =
                                    std::mem::transmute(self.as_object());
                                let other_instance: &InstanceObject =
                                    std::mem::transmute(other.as_object());

                                self_instance.properties == other_instance.properties
                                //shoud we check if methods are the same as well ?
                            }

                            ObjectType::Class => {
                                let self_class: &ClassObject =
                                    std::mem::transmute(self.as_object());
                                let other_class: &ClassObject =
                                    std::mem::transmute(other.as_object());

                                self_class == other_class
                            }
                            ObjectType::Func => {
                                let self_func: &FunctionObject =
                                    std::mem::transmute(self.as_object());
                                let other_func: &FunctionObject =
                                    std::mem::transmute(other.as_object());

                                self_func == other_func
                            }

                            ObjectType::Enum => {
                                let self_enum: &EnumObject = std::mem::transmute(self.as_object());

                                let other_enum: &EnumObject =
                                    std::mem::transmute(other.as_object());

                                self_enum.name == other_enum.name
                                    && self_enum.tag == other_enum.tag
                                    && self_enum.data == other_enum.data
                            }
                            ObjectType::Native => {
                                let self_native: &NativeObject =
                                    std::mem::transmute(self.as_object());
                                let other_native: &NativeObject =
                                    std::mem::transmute(other.as_object());

                                self_native == other_native
                            }
                        }
                    },
                }
            }
        }
    }
}
