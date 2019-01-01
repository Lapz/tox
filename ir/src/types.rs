use util::symbol::Symbol;
use std::fmt::{self,Display};

static mut TYPEVAR_COUNT:u32 = 0;
static mut UNIQUE_COUNT:u32 =  0;

/// A type var represent a variable that could be a type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeVar(pub u32);

/// A unique identifier that
#[derive(Clone, Copy, Debug, PartialEq, Default)]
pub struct Unique(pub u32);


#[derive(Debug,Clone,PartialEq)]
pub enum TypeCon {
    Arrow,
    Array(Box<Type>),
    Bool,
    Float,
    Int,
    Str,
    Void
}


#[derive(Debug,Clone,PartialEq)]
pub enum Type {
    App(TypeCon, Vec<Type>),// when type::Con is type::Con::Arrow the last type in the vec of types is the return type
    Class(Symbol, Vec<Property>, Vec<Method>, Unique), // Name, Properties, Methods,Unique
    Generic(Vec<TypeVar>, Box<Type>),
    Nil,
    Var(TypeVar),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Property {
    pub name: Symbol,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Method {
    pub name: Symbol,
    pub ty: Type,
}

impl Property {
    pub fn new(name:Symbol,ty:Type) -> Property {
        Property {
            name,
            ty
        }
    }
}

impl Unique {
    pub fn new() -> Self {
        let value = unsafe { UNIQUE_COUNT };
        unsafe { UNIQUE_COUNT += 1 };
        Unique(value)
    }
}

impl Method {
    pub fn new(name:Symbol,ty:Type) -> Method {
        Method {
            name,
            ty
        }
    }
}

impl TypeVar {
    pub fn new() -> Self {
        let value = unsafe { TYPEVAR_COUNT };
        unsafe { TYPEVAR_COUNT += 1 };
        TypeVar(value)
    }
}

impl Type {
    pub fn is_int(&self) -> bool {
        match *self {
            Type::App(TypeCon::Int, _) => true,
            _ => false,
        }
    }

    pub fn is_float(&self) -> bool {
        match *self {
            Type::App(TypeCon::Float, _) => true,
            _ => false,
        }
    }
}

impl Display for TypeCon {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            TypeCon::Arrow => write!(f, "->"),
            TypeCon::Array(ref inner) => write!(f, "{}", inner),
            TypeCon::Bool => write!(f, "bool"),
            TypeCon::Float => write!(f, "float"),
            TypeCon::Int => write!(f, "int"),
            TypeCon::Str => write!(f, "str"),
            TypeCon::Void => write!(f, "nil"),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Type::App(ref tycon, ref types) => {
                if let TypeCon::Arrow = *tycon {
                    write!(f, "fn (")?;

                    for i in 0..types.len() - 1 {
                        if i + 1 == types.len() - 1 {
                            write!(f, "{}", types[i])?;
                        } else {
                            write!(f, "{},", types[i])?;
                        }
                    }

                    write!(f, ") -> ")?;

                    write!(f, "{}", types.last().unwrap())?;
                }

                write!(f, "{}", tycon)?;

                for (i, ty) in types.iter().enumerate() {
                    if i + 1 == types.len() {
                        write!(f, "{}", ty)?;
                    } else {
                        write!(f, "{},", ty)?;
                    }
                }

                write!(f, "")
            }

            Type::Class(ref name, ref fields, _, _) => {
                write!(f, "{}", name)?;

                if !fields.is_empty() {
                    write!(f, "<")?;
                    for (i, field) in fields.iter().enumerate() {
                        if i + 1 == fields.len() {
                            write!(f, "{}", field.ty)?;
                        } else {
                            write!(f, "{},", field.ty)?;
                        }
                    }

                    write!(f, ">")?;
                }

                Ok(())
            }

            Type::Generic(ref vars, ref ret) => {
                write!(f, "poly")?;

                if !vars.is_empty() {
                    write!(f, "<")?;
                    for (i, var) in vars.iter().enumerate() {
                        if i + 1 == vars.len() {
                            write!(f, "{}", var)?;
                        } else {
                            write!(f, "{},", var)?;
                        }
                    }

                    write!(f, ">")?;
                }

                for (i, var) in vars.iter().enumerate() {
                    if i + 1 == vars.len() {
                        write!(f, "{}", var)?;
                    } else {
                        write!(f, "{},", var)?;
                    }
                }

                write!(f, " {}", ret)
            }

            Type::Nil => write!(f, "nil"),
            Type::Var(ref v) => write!(f, "{}", v),
        }
    }
}

impl Display for TypeVar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "tv{}", self.0)
    }
}
