//! The VM module it contains the vm.
//! The opcodes for the vm can be found in the opcode module
//! a ```VM``` and an ```Assembler``` for the tasm langauage

#[cfg(target_arch = "wasm32")]
#[macro_use]
extern crate wasm_bindgen;
use opcode;

#[macro_use]
mod macros;
mod chunk;
mod native;
mod object;
mod value;
mod vm;

pub use crate::chunk::Chunk;
pub use crate::object::{FunctionObject, RawObject, StringObject};
pub use crate::value::Value;
pub use crate::vm::VM;
use fnv::FnvHashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: ::util::symbol::Symbol,
    pub body: Chunk,
    pub params: FnvHashMap<::util::symbol::Symbol, usize>,
}

#[derive(Debug, Clone)]
pub struct Class {
    pub name: ::util::symbol::Symbol,
    pub methods: FnvHashMap<::util::symbol::Symbol, Function>,
}

#[derive(Debug, Clone)]
pub struct Program {
    pub functions: FnvHashMap<::util::symbol::Symbol, Function>,
    pub classes: FnvHashMap<::util::symbol::Symbol, Class>,
}
