use std::collections::HashMap;
use std::fmt::{self, Display};
use util::symbol::{Symbol, Symbols};

static mut TYPEVAR_COUNT: u32 = 0;
static mut UNIQUE_COUNT: u32 = 0;

/// A type var represent a variable that could be a type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeVar(pub u32);

/// A unique identifier that
#[derive(Clone, Copy, Debug, PartialEq, Default)]
pub struct Unique(pub u32);

#[derive(Debug, Clone, PartialEq)]
pub enum TypeCon {
    Arrow,
    Array(Box<Type>),
    Bool,
    Float,
    Int,
    Str,
    Void,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    App(TypeCon, Vec<Type>), // when type::Con is type::Con::Arrow the last type in the vec of types is the return type
    Class(Symbol, Vec<Property>, Vec<Method>, Unique), // Name, Properties, Methods,Unique
    Generic(Vec<TypeVar>, Box<Type>),
    Nil,
    Var(TypeVar),
    Enum {
        name: Symbol,
        variants: HashMap<Symbol, Variant>,
    },
}

/// Represent an enum variant
/// ```ignore
/// Foo::Bar => Variant {
//      tag:0, // the number it was declared at
///     inner:None // if it dosen't have an inner type i.e Ok(foo)
///  }
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct Variant {
    pub tag: u32,
    pub inner: Option<Type>,
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
    pub fn new(name: Symbol, ty: Type) -> Property {
        Property { name, ty }
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
    pub fn new(name: Symbol, ty: Type) -> Method {
        Method { name, ty }
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

impl Type {
    pub fn print<'a>(&self, symbols: &'a Symbols<()>) -> String {
        match *self {
            Type::App(ref tycon, ref types) => {
                let mut fmt_string = String::new();

                if let TypeCon::Arrow = *tycon {
                    fmt_string.push_str("fn(");

                    for i in 0..types.len() - 1 {
                        if i + 1 == types.len() - 1 {
                            fmt_string.push_str(&format!("{}", types[i].print(symbols)));
                        } else {
                            fmt_string.push_str(&format!("{},", types[i].print(symbols)));
                        }
                    }

                    fmt_string.push_str(") -> ");

                    fmt_string.push_str(&format!("{}", types.last().unwrap().print(symbols)));

                    return fmt_string;
                }

                fmt_string.push_str(&format!("{}", tycon));

                for (i, ty) in types.iter().enumerate() {
                    if i + 1 == types.len() {
                        fmt_string.push_str(&ty.print(symbols))
                    } else {
                        fmt_string.push_str(&format!("{},", ty.print(symbols)))
                    }
                }

                fmt_string
            }
            Type::Enum { ref name, .. } => {
                let mut fmt_string = String::new();

                fmt_string.push_str(&format!("{}", symbols.name(*name)));

                fmt_string
            }

            Type::Class(ref name, ref properties, _, _) => {
                let mut fmt_string = String::new();
                fmt_string.push_str(&format!("{}", symbols.name(*name)));

                if !properties.is_empty() {
                    fmt_string.push('<');
                    for (i, field) in properties.iter().enumerate() {
                        if i + 1 == properties.len() {
                            fmt_string.push_str(&format!("{}", field.ty.print(symbols)));
                        } else {
                            fmt_string.push_str(&format!("{},", field.ty.print(symbols)));
                        }
                    }

                    fmt_string.push('>');
                }

                fmt_string
            }

            Type::Generic(ref vars, ref ret) => {
                let mut fmt_string = String::new();
                fmt_string.push_str("poly ");

                if !vars.is_empty() {
                    fmt_string.push_str(&ret.print(symbols));

                    fmt_string.push('<');

                    for (i, var) in vars.iter().enumerate() {
                        if i + 1 == vars.len() {
                            fmt_string += &var.0.to_string();
                        } else {
                            fmt_string.push_str(&format!("{},", var.0.to_string()));
                        }
                    }

                    fmt_string.push('>');
                }

                fmt_string
            }

            Type::Nil => "nil".into(),

            Type::Var(ref v) => format!("{{T:{}}}", v),
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

            Type::Enum {
                ref name,
                ref variants,
            } => {
                write!(f, "enum {}", *name)?;

                if !variants.is_empty() {
                    write!(f, "<")?;
                    for (i, variant) in variants.iter().enumerate() {
                        let variant = variant.1;

                        if i + 1 == variants.len() {
                            if let Some(ref inner) = variant.inner {
                                write!(f, "{}:{}", variant.tag, inner)?;
                            } else {
                                write!(f, "{}", variant.tag)?;
                            }
                        } else {
                            if let Some(ref inner) = variant.inner {
                                write!(f, "{}:{},", variant.tag, inner)?;
                            } else {
                                write!(f, "{},", variant.tag)?;
                            }
                        }
                    }

                    write!(f, ">")?;
                }

                Ok(())
            }

            Type::Generic(ref vars, ref ret) => {
                write!(f, "poly")?;
                write!(f, " {}", ret)?;

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

                Ok(())
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
