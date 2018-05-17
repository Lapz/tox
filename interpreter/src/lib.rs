extern crate fnv;
extern crate rand;
extern crate syntax;
extern crate util;
mod builtins;
pub mod interpreter;
mod object;

pub use self::interpreter::env::Environment;
use self::interpreter::{evaluate_statement, RuntimeError};
use fnv::FnvHashMap;
use object::Object;
use syntax::ast::expr::VariableUseHandle;
use syntax::ast::statement::Statement;
use util::pos::Spanned;



pub fn interpret(
    statements: &[Spanned<Statement>],
    locals: &FnvHashMap<VariableUseHandle, usize>,
    env: &mut Environment,
) -> Result<Object, RuntimeError> {
    let mut result = Object::None;

    for statement in statements {
        result = evaluate_statement(statement, locals, env)?
    }

    Ok(result)
}
