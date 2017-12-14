use types::Type;
use symbol::{Symbol,Symbols};

#[derive(Debug, Clone)]
pub enum Entry {
    VarEntry(Type),
    FunEntry { parms: Vec<Type>, returns: Type },
}

#[derive(Debug, Clone)]
pub struct Env<'a> {
    pub types: Symbols<'a, Type>,
    pub vars: Symbols<'a, Entry>,
}

impl <'a> Env<'a> {
    pub fn look(&mut self,symbol:Symbol) -> Option<&Type> {
        self.types.look(symbol)
    } 
}