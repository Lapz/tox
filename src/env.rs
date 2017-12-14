use types::Type;
use symbol::{Symbol,Symbols,SymbolFactory};
use std::rc::Rc;


#[derive(Debug, Clone)]
pub enum Entry {
    VarEntry(Type),
    FunEntry { parms: Vec<Type>, returns: Type },
}

#[derive(Debug, Clone)]
pub struct Env {
    pub types: Symbols<Type>,
    pub vars: Symbols<Entry>,
}

impl  Env {
    pub fn new(strings:&Rc<SymbolFactory>) -> Self {
        Env {
            types: Symbols::new(Rc::clone(strings)),
            vars:Symbols::new(Rc::clone(strings))
        }
    }
    pub fn look_type(&mut self,symbol:Symbol) -> Option<&Type> {
        self.types.look(symbol)
    } 

    pub fn add_var(&mut self, symbol:Symbol,data:Entry)  {
        self.vars.enter(symbol, data)
    }

    
}