//! This module provides an Environment which keeps a track of the mappings between a
//! `Symbol` and a `Type` or an `Entry`

use std::rc::Rc;
use types::Type;
use util::symbol::{Symbol, SymbolFactory, Symbols};
use util::Unique;

#[derive(Debug, Clone, PartialEq)]
pub enum Entry {
    Ty(Type),
    Class(Type),
    Fun(Type),
}

#[derive(Debug, Clone, PartialEq)]
pub enum VarEntry {
    Var(Type),
    Fun { ty: Type },
}

impl Entry {
    pub fn ty(&self) -> &Type {
        match *self {
            Entry::Ty(ref ty) | Entry::Class(ref ty) | Entry::Fun(ref ty) => ty,
        }
    }
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

//     fn add_builtin_class(&mut self, name: &str, methods: Vec<(&str, Entry)>) {
//         let symbol = self.vars.symbol(name);

//     }

//
// }
