#[cfg(test)]
extern crate pretty_assertions;

extern crate fnv;
extern crate opcode;
extern crate rand;
extern crate syntax;
extern crate util;
extern crate vm;

mod ast;
mod build_cfg;
// mod build_structs;
mod codegen;
mod ctx;
mod infer;

pub use crate::build_cfg::build_program;
pub use crate::codegen::compile;
pub use crate::infer::Infer;
