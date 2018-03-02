extern crate rand;
extern crate syntax;
extern crate util;

mod object;
pub mod interpreter;
mod builtins;

use object::Object;
use syntax::ast::expr::VariableUseHandle;
use syntax::ast::statement::Statement;
use util::pos::Spanned;
use std::collections::HashMap;
use self::interpreter::{evaluate_statement, RuntimeError};
use self::interpreter::env::Environment;

pub fn interpret(
    statements: &[Spanned<Statement>],
    locals: &HashMap<VariableUseHandle, usize>,
    env: &mut Environment,
) -> Result<Object, RuntimeError> {
    let mut result = Object::None;

    for statement in statements {
        result = evaluate_statement(statement, locals, env)?
    }

    Ok(result)
}
