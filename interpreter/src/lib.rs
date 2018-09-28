extern crate fnv;
extern crate rand;
extern crate syntax;
extern crate util;
mod builtins;
pub mod interpreter;
mod object;

pub use self::interpreter::env::Environment;
use self::interpreter::{evaluate_class, evaluate_function, evaluate_statement, RuntimeError};
//use fnv::FnvHashMap;
use object::Object;
use syntax::ast::Program;
use util::symbol::Symbol;

pub fn interpret(
    program: &Program,
    env: &mut Environment,
    main: Symbol,
) -> Result<Object, RuntimeError> {
    let mut result = Object::None;

    for class in program.classes.iter() {
        evaluate_class(&class.value, env)?;
    }

    for function in program.functions.iter() {

        evaluate_function(&function.value, env)?;

    }

    for function in program.functions.iter() { ;

        if function.value.name.value == main {
            result = evaluate_statement(&function.value.body, env)?; // Evaluate main after everything else
        }
    }



    Ok(result)
}
