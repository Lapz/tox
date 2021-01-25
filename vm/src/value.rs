use std::fmt::{self, Debug, Display};

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

        unsafe { &*(ptr as *const StringObject<'a>) }
    }

    #[inline]
    pub fn as_array<'a>(&self) -> &'a ArrayObject {
        let ptr = self.as_object();

        unsafe { &*(ptr as *const ArrayObject) }
    }

    #[inline]
    pub fn as_class<'a>(&self) -> &'a ClassObject {
        let ptr = self.as_object();

        unsafe { &*(ptr as *const ClassObject) }
    }

    #[inline]
    pub fn as_function<'a>(&self) -> &'a FunctionObject {
        let ptr = self.as_object();

        unsafe { &*(ptr as *const FunctionObject) }
    }

    #[inline]
    pub fn as_native<'a>(&self) -> &'a NativeObject {
        let ptr = self.as_object();

        unsafe { &*(ptr as *const NativeObject) }
    }

    #[inline]
    pub fn as_instance<'a>(&self) -> &'a InstanceObject {
        let ptr = self.as_object();

        unsafe { &*(ptr as *const InstanceObject) }
    }

    #[inline]
    pub fn as_mut_instance<'a>(&self) -> &'a mut InstanceObject {
        let ptr = self.as_object();

        unsafe { &mut *(ptr as *mut InstanceObject) }
    }

    #[inline]
    pub fn is_object(&self) -> bool {
        self.ty == ValueType::Object
    }

    #[inline]
    pub fn is_class(&self) -> bool {
        unsafe { self.is_object() && (*self.as_object()).ty == ObjectType::Class }
    }

    #[inline]
    pub fn is_string(&self) -> bool {
        unsafe { self.is_object() && (*self.as_object()).ty == ObjectType::String }
    }
    #[inline]
    pub fn is_native(&self) -> bool {
        unsafe { self.is_object() && (*self.as_object()).ty == ObjectType::Native }
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
                    let obj: &Object = &*self.as_object();

                    match obj.ty {
                        ObjectType::String => {
                            write!(fmt, "{:#?}", &*(self.val.object as *const StringObject))?
                        }
                        ObjectType::Func => {
                            write!(fmt, "{:#?}", &*(self.val.object as *const FunctionObject))?
                        }
                        ObjectType::Array => {
                            write!(fmt, "{:#?}", &*(self.val.object as *const ArrayObject))?
                        }
                        ObjectType::Class => {
                            write!(fmt, "{:#?}", &*(self.val.object as *const ClassObject))?
                        }
                        ObjectType::Instance => {
                            write!(fmt, "{:#?}", &*(self.val.object as *const InstanceObject))?
                        }

                        ObjectType::Enum => {
                            write!(fmt, "{:#?}", &*(self.val.object as *const EnumObject))?
                        }

                        ObjectType::Native => {
                            write!(fmt, "{:#?}", &*(self.val.object as *const NativeObject))?
                        }
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
                let obj: &Object = &*self.as_object();

                match obj.ty {
                    ObjectType::String => write!(fmt, "{}", self.as_string())?,
                    ObjectType::Func => write!(fmt, " fun")?,
                    ObjectType::Array => write!(fmt, "array")?,
                    ObjectType::Class => write!(fmt, "class")?,
                    ObjectType::Instance => write!(fmt, "instance")?,
                    ObjectType::Native => write!(fmt, "native")?,
                    ObjectType::Enum => write!(fmt, "enum")?,
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
                    let self_object: &Object = &*self.as_object();

                    match self_object.ty {
                        ObjectType::String => {
                            let self_string: &StringObject =
                                &*(self.as_object() as *const StringObject<'_>);
                            let other_string: &StringObject =
                                &*(other.as_object() as *const StringObject<'_>);

                            self_string.chars == other_string.chars
                        }
                        ObjectType::Array => {
                            let self_array: &ArrayObject =
                                &*(self.as_object() as *const ArrayObject);
                            let other_array: &ArrayObject =
                                &*(other.as_object() as *const ArrayObject);

                            self_array.items == other_array.items
                        }

                        ObjectType::Instance => {
                            let self_instance: &InstanceObject =
                                &*(self.as_object() as *const InstanceObject);
                            let other_instance: &InstanceObject =
                                &*(other.as_object() as *const InstanceObject);

                            self_instance.properties == other_instance.properties
                            //shoud we check if methods are the same as well ?
                        }

                        ObjectType::Class => {
                            let self_class: &ClassObject =
                                &*(self.as_object() as *const ClassObject);
                            let other_class: &ClassObject =
                                &*(other.as_object() as *const ClassObject);

                            self_class == other_class
                        }
                        ObjectType::Func => {
                            let self_func: &FunctionObject =
                                &*(self.as_object() as *const FunctionObject);
                            let other_func: &FunctionObject =
                                &*(other.as_object() as *const FunctionObject);

                            self_func == other_func
                        }

                        ObjectType::Enum => {
                            let self_enum: &EnumObject = &*(self.as_object() as *const EnumObject);

                            let other_enum: &EnumObject =
                                &*(other.as_object() as *const EnumObject);

                            self_enum.name == other_enum.name
                                && self_enum.tag == other_enum.tag
                                && self_enum.data == other_enum.data
                        }
                        ObjectType::Native => {
                            let self_native: &NativeObject =
                                &*(self.as_object() as *const NativeObject);
                            let other_native: &NativeObject =
                                &*(other.as_object() as *const NativeObject);

                            self_native == other_native
                        }
                    }
                },
            }
        }
    }
}
