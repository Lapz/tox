#![feature(use_nested_groups)]

extern crate syntax;
extern crate util;
extern crate rand;

mod object;
pub mod interpreter;
mod builtins;

use object::Object;
use syntax::ast::{expr::VariableUseHandle, statement::Statement};
use util::pos::WithPos;
use std::collections::HashMap;
use self::interpreter::{evaluate_statement, RuntimeError, env::Environment};

pub fn interpret(
    statements: &[WithPos<Statement>],
    locals: &HashMap<VariableUseHandle, usize>,
    env: &mut Environment,
) -> Result<Object, RuntimeError> {
    let mut result = Object::None;
    for statement in statements {
        result = evaluate_statement(statement, locals, env)?
    }
    Ok(result)
}
