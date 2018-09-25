#[cfg(test)]
extern crate pretty_assertions;

extern crate fnv;
extern crate rand;
extern crate syntax;
extern crate util;

#[macro_use]
// mod semant;
// mod env;
// mod resolver;
// mod test;
mod ast;
mod codegen;
mod ctx;
mod infer;

pub use codegen::Compiler;
pub use infer::Infer;
