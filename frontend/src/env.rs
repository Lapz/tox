//! This module provides an Environment which keeps a track of the mappings between a
//! `Symbol` and a `Type` or an `Entry`

use std::rc::Rc;
use types::Type;
use util::symbol::{Symbol, SymbolFactory, Symbols};
use util::Unique;

#[derive(Debug, Clone)]
pub enum Entry {
    Ty(Type),
    Class(Type),
    Fun(Type),
}

#[derive(Debug, Clone)]
pub enum VarEntry {
    Var(Type),
    Fun { ty: Type },
}

// #[derive(Debug, Clone)]
// pub struct TypeEnv {
//     pub types: Symbols<Type>,
//     pub vars: Symbols<Entry>,
//     pub unique: Unique,
// }

// impl TypeEnv {
//     pub fn new(strings: &Rc<SymbolFactory>) -> Self {

//         let mut env = TypeEnv {
//             types,
//             vars: Symbols::new(Rc::clone(strings)),
//             unique: Unique::new(),
//         };

//

//

//         env
//     }
//     pub fn look_type(&mut self, symbol: Symbol) -> Option<&Type> {
//         self.types.look(symbol)
//     }

//     pub fn name(&self, symbol: Symbol) -> String {
//         self.vars.name(symbol)
//     }

//     pub fn look_var(&self, symbol: Symbol) -> Option<&Entry> {
//         self.vars.look(symbol)
//     }

//     pub fn unique_id(&mut self) -> Symbol {
//         let next = Unique::new().0;
//         self.vars.symbol(&next.to_string())
//     }

//     pub fn begin_scope(&mut self) {
//         self.types.begin_scope();
//         self.vars.begin_scope();
//     }

//     pub fn end_scope(&mut self) {
//         self.types.end_scope();
//         self.vars.end_scope();
//     }

//     pub fn add_type(&mut self, symbol: Symbol, data: Type) {
//         self.types.enter(symbol, data)
//     }

//     pub fn add_var(&mut self, symbol: Symbol, data: Entry) {
//         self.vars.enter(symbol, data)
//     }

//     pub fn get_symbol(&mut self, name: &str) -> Symbol {
//         self.vars.symbol(name)
//     }

//     fn add_builtin_class(&mut self, name: &str, methods: Vec<(&str, Entry)>) {
//         let symbol = self.vars.symbol(name);

//     }

//
// }
