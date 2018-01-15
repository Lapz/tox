//! This module provides an Environment which keeps a track of the mappings between a
//! `Symbol` and a `Type` or an `Entry`

use types::Type;
use super::symbol::{Symbol, SymbolFactory, Table};
use std::rc::Rc;
use super::Unique;

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
pub enum Entry {
    VarEntry(Type), // Vec of (Vec<MethodParam types>,Return Type)
    FunEntry { params: Vec<Type>, returns: Type },
}

#[derive(Debug, Clone)]
pub struct TypeEnv {
    pub types: Table<Type>,
    pub vars: Table<Entry>,
    pub unique: Unique,
}

impl TypeEnv {
    pub fn new(strings: &Rc<SymbolFactory>) -> Self {
        let mut types = Table::new(Rc::clone(strings));
        let string_symbol = types.symbol("str");
        let int_symbol = types.symbol("int");
        let float_symbol = types.symbol("float");
        let nil_symbol = types.symbol("nil");
        let bool_symbol = types.symbol("bool");

        types.enter(int_symbol, Type::Int);
        types.enter(float_symbol, Type::Float);
        types.enter(bool_symbol, Type::Bool);
        types.enter(nil_symbol, Type::Nil);
        types.enter(string_symbol, Type::Str);

        TypeEnv {
            types,
            vars: Table::new(Rc::clone(strings)),
            unique: Unique::new(),
        }
    }
    pub fn look_type(&mut self, symbol: Symbol) -> Option<&Type> {
        self.types.look(symbol)
    }

    pub fn name(&self, symbol: Symbol) -> String {
        self.vars.name(symbol)
    }
    pub fn look_var(&self, symbol: Symbol) -> Option<&Entry> {
        self.vars.look(symbol)
    }

    pub fn unique_id(&mut self) -> Symbol {
        let next = Unique::new().0;
        self.vars.symbol(&next.to_string())
    }

    pub fn begin_scope(&mut self) {
        self.types.begin_scope();
        self.vars.begin_scope();
    }

    pub fn end_scope(&mut self) {
        self.types.end_scope();
        self.vars.end_scope();
    }

    pub fn add_type(&mut self, symbol: Symbol, data: Type) {
        self.types.enter(symbol, data)
    }

    pub fn add_var(&mut self, symbol: Symbol, data: Entry) {
        self.vars.enter(symbol, data)
    }
}
