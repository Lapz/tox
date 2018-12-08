//! The VM module it contains the vm.
//! The opcodes for the vm can be found in the opcode module
//! a ```VM``` and an ```Assembler``` for the tasm langauage
#![feature(nll)]
extern crate opcode;

extern crate util;

extern crate libc;

#[macro_use]
mod macros;
mod chunk;
mod object;
mod value;
mod vm;
mod native;

pub use chunk::Chunk;
pub use object::{FunctionObject, RawObject, StringObject};
pub use value::Value;
pub use vm::VM;

#[derive(Debug, Clone)]
pub struct Function {
    pub name: ::util::symbol::Symbol,
    pub body: Chunk,
    pub params: ::std::collections::HashMap<::util::symbol::Symbol, usize>,
}

#[derive(Debug, Clone)]
pub struct Class {
    pub name: ::util::symbol::Symbol,
    pub methods: ::std::collections::HashMap<::util::symbol::Symbol, Function>,
}

#[derive(Debug, Clone)]
pub struct Program {
    pub functions: ::std::collections::HashMap<::util::symbol::Symbol, Function>,
    pub classes: ::std::collections::HashMap<::util::symbol::Symbol, Class>,
}
