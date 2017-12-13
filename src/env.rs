use types::Type;
use symbol::Symbols;

#[derive(Debug, Clone)]
enum Entry {
    VarEntry(Type),
    FunEntry { parms: Vec<Type>, returns: Type },
}

#[derive(Debug, Clone)]
pub struct Env<'a> {
    types: Symbols<'a, Type>,
    varialbes: Symbols<'a, Entry>,
}
