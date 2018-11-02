#![feature(nll)]
#[cfg(test)]
extern crate pretty_assertions;

extern crate fnv;
extern crate opcode;
extern crate rand;
extern crate syntax;
extern crate util;
extern crate vm;

#[macro_use]
// mod resolver;
// mod test;
mod ast;
mod codegen;
mod ctx;
mod infer;

// pub use codegen::Compiler;
pub use codegen::compile;
pub use infer::Infer;
