use crate::hir;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeVar(pub u32);

/// A unique identifier that is used to distinguish to structs with the same fields
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Unique(pub u32);

static mut TYPEVAR_COUNT: u32 = 0;
static mut UNIQUE_COUNT: u32 = 0;

#[derive(Debug, Clone, PartialEq)]
/// A type constructor
pub enum TypeCon {
    /// Used to represent a function
    /// i.e Type::App(TypeCon::Arrow,[Type::Int,Type::Int])
    /// is the same as fn(i32) -> i32
    Arrow,
    /// Used to represent a boolean
    Bool,
    /// Used to represent a float
    Float,
    /// Used to represent an integer
    Int,
    /// Used to represent void
    Void,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Ty {
    /// Used to represent a `lambda`? function
    App(TypeCon, Vec<Ty>),
    /// Used to represent a class along with its propertes and methods
    /// The unique field is used to differentiate two classes with the same methods and fields
    Class(hir::Name, Vec<Property>, Vec<Method>, Unique),
    /// Represent a generic with unbounded free vars the returns bar
    Generic(Vec<TypeVar>, Box<Ty>),
    /// Used to represent a free type variable
    Var(TypeVar),
    /// Used to represent an enum
    /// The unique field is used in the same way as the class variant
    Enum(hir::Name, Vec<EnumVariant>, Unique),
    /// Used to represent a tuple variant
    Tuple(Vec<Ty>),
}

/// Represent an enum variant
/// ```ignore
/// Foo::Bar => Variant {
//      tag:0, // the number it was declared at
///     inner:Vec<Ty> // if it doesn't have an inner type i.e Ok(foo)
///  }
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariant {
    /// The tag of the enum variant
    pub tag: u32,
    /// The type that the enum stores
    pub types: Vec<Ty>,
}

/// Represent a class field property
/// ```ignore
/// class Foo {
///     bar:i32
/// }
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct Property {
    /// The name of the class
    pub name: hir::Name,
    /// The type of the class field
    pub ty: Ty,
}

/// Represent a class method
/// ```ignore
/// class Foo {
///     fn bar() -> i32 {
///
///     }
/// }
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct Method {
    /// The name of the class
    pub name: hir::Name,
    /// The type of the class method
    pub ty: Ty,
}

impl Property {
    pub fn new(name: hir::Name, ty: Ty) -> Self {
        Self { name, ty }
    }
}

impl Method {
    pub fn new(name: hir::Name, ty: Ty) -> Self {
        Self { name, ty }
    }
}

impl Unique {
    pub fn new() -> Self {
        Self::default()
    }
}

impl TypeVar {
    pub fn new() -> Self {
        Self::default()
    }
}

impl Default for Unique {
    fn default() -> Self {
        let value = unsafe { UNIQUE_COUNT };
        unsafe { UNIQUE_COUNT += 1 };
        Unique(value)
    }
}

impl Default for TypeVar {
    fn default() -> Self {
        let value = unsafe { TYPEVAR_COUNT };
        unsafe { TYPEVAR_COUNT += 1 };
        TypeVar(value)
    }
}
