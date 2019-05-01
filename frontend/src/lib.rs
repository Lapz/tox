#[cfg(test)]
extern crate pretty_assertions;

extern crate fnv;
extern crate opcode;
extern crate rand;
extern crate syntax;
extern crate util;
extern crate vm;

mod ast;
mod codegen;
mod ctx;
mod infer;

pub use crate::codegen::compile;
pub use crate::infer::Infer;
