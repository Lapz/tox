#[cfg(test)]
#[macro_use]
extern crate pretty_assertions;
mod token;
mod lexer;
mod pos;
mod ast;
mod parser;
mod repl;
// mod object;
//  mod interpreter;
mod inference;
mod types;
mod resolver;
mod symbol;
mod env;
mod pprint;

use repl::repl;

// use interpreter::Interpreter;

fn main() {
    repl(false, true)
}
