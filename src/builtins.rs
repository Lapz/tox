use std::time::{SystemTime, UNIX_EPOCH};

use std::io::prelude::*;
use std::io;
use env::{Entry, Env};
use types::Type;
use object::Object;
use interpreter::RuntimeError;

pub type BuiltInFunction = fn(&[Object]) -> Result<Object, RuntimeError>;

pub struct BuiltIn;

impl Default for BuiltIn {
    fn default() -> Self {
        Self::new()
    }
}

impl BuiltIn {
    pub fn new() -> Self {
        BuiltIn {}
    }
}

impl Env {
    pub fn get_builtins(&mut self) {
        self.add_builtin("clock", vec![], Type::Float, built_in_clock);
    }
    fn add_builtin(&mut self, name: &str, params: Vec<Type>, returns: Type, func: BuiltInFunction) {
        let symbol = self.vars.symbol(name);
        let entry = Entry::FunEntry { params, returns };
        self.vars.enter(symbol, entry);
        self.add_object(symbol, Object::BuiltIn(symbol, func));
    }
}

fn built_in_clock(_: &[Object]) -> Result<Object, RuntimeError> {
    let time = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_secs();

    Ok(Object::Int(time as i64))
}
