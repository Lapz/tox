#[cfg(test)]
#[macro_use]
extern crate pretty_assertions;
extern crate rand;
extern crate util;

use util::symbol;

#[macro_use]
mod macros;

pub mod ast;
pub mod compiler;
pub mod lexer;
// pub mod parser;
// mod pprint;
mod token;
