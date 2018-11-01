#[cfg(test)]
extern crate pretty_assertions;

extern crate fnv;
extern crate rand;
extern crate syntax;
extern crate util;
extern crate vm;
extern crate opcode;

#[macro_use]
// mod resolver;
// mod test;
mod ast;
mod codegen;
mod ctx;
mod infer;

// pub use codegen::Compiler;
pub use infer::Infer;
