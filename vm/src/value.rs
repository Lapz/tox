use std::fmt::{self, Debug, Display};

#[derive(Clone, Copy)]
pub struct Value {
    val: As,
    ty: ValueType,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ValueType {
    Int,
    Float,
    Nil,
    Bool,
}
#[derive(Clone, Copy)]
#[repr(C)]
pub union As {
    boolean: bool,
    float: f64,
    int: i64,
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
}

impl Debug for Value {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "Value {{")?;
        unsafe {
            if self.ty == ValueType::Int || self.ty == ValueType::Nil {
                write!(fmt, "val:{:?},", self.val.int)?;
            } else if self.ty == ValueType::Float {
                write!(fmt, "val:{:?},", self.val.float)?;
            } else {
                write!(fmt, "val:{:?},", self.val.boolean)?;
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
            if self.ty == ValueType::Int
            {
                write!(fmt, "{}", self.val.int)?;
            } else if self.ty == ValueType::Nil 
            {
                write!(fmt, "nil")?;
            }else if self.ty == ValueType::Float {
                write!(fmt, "{}", self.val.float)?;
            } else if self.ty == ValueType::Bool {
                write!(fmt, "{}", self.val.boolean)?;
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
            }
        }
    }
}
