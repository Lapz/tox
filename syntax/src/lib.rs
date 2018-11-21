#![feature(nll)]
#[cfg(test)]
#[macro_use]
extern crate pretty_assertions;
extern crate rand;
extern crate util;

#[macro_use]
mod macros;

pub mod ast;
pub mod parser;
// mod pprint;
mod token;
