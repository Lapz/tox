use types::Type;
use symbol::Symbols;

enum Entry {
    VarEntry(Type),
    FunEntry {
        parms:Vec<Type>,
        returns:Type,
    }
}

#[derive(Debug,Clone)]
pub  struct Env {
    types: Symbols<Type>,
    varialbes: Symbols<Entry>,
}