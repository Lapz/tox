//! This module provides an Environment which keeps a track of the mappings between a
//! `Symbol` and a `Type` or an `Entry`

use types::Type;
use super::symbol::{Symbol, SymbolFactory, Symbols};
use std::rc::Rc;
use super::Unique;

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
pub enum Entry {
    VarEntry(Type), // Vec of (Vec<MethodParam types>,Return Type)
    FunEntry { params: Vec<Type>, returns: Type },
}

#[derive(Debug, Clone)]
pub struct TypeEnv {
    pub types: Symbols<Type>,
    pub vars: Symbols<Entry>,
    pub unique: Unique,
}

impl TypeEnv {
    pub fn new(strings: &Rc<SymbolFactory>) -> Self {
        let mut types = Symbols::new(Rc::clone(strings));
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

        let mut env = TypeEnv {
            types,
            vars: Symbols::new(Rc::clone(strings)),
            unique: Unique::new(),
        };

        env.add_builtin("clock", vec![], Type::Float);
        env.add_builtin("hex", vec![Type::Int], Type::Str);
        env.add_builtin("oct", vec![Type::Int], Type::Str);
        env.add_builtin("random", vec![Type::Int, Type::Int], Type::Int);
        env.add_builtin("to_int", vec![Type::Str], Type::Int);

        env.add_builtin_class(
            "io",
            vec![
                (
                    "readline",
                    Entry::FunEntry {
                        params: vec![],
                        returns: Type::Str,
                    },
                ),
            ],
        );

        env
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

    pub fn get_symbol(&mut self, name: &str) -> Symbol {
        self.vars.symbol(name)
    }

    fn add_builtin_class(&mut self, name: &str, methods: Vec<(&str, Entry)>) {
        let symbol = self.vars.symbol(name);

        use std::collections::HashMap;

        let mut methods_ty = HashMap::new();

        for method in methods {
            let name = self.vars.symbol(method.0);
            methods_ty.insert(name, method.1);
        }

        let entry = Entry::VarEntry(Type::Class {
            name: symbol,
            methods: methods_ty,
            fields: HashMap::new(),
        });

        self.vars.enter(symbol, entry);
    }

    fn add_builtin(&mut self, name: &str, params: Vec<::types::Type>, returns: Type) {
        let symbol = self.vars.symbol(name);
        self.vars.enter(symbol, Entry::FunEntry { params, returns });
    }
}
