use std::collections::HashMap;

use semant::hir::{BinOp, FunctionId, UnaryOp};
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Register(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Label(pub u32);

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum Value {
    Nil,
    Bool(bool),
    Int(i64),
    Float(Box<[u8; std::mem::size_of::<f64>()]>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Size {
    Bit8,
    Bit32,
    Bit64,
}

pub struct Function {
    id: FunctionId,
    params: Vec<Register>,
    layout: HashMap<Register, ()>,
    instructions: Vec<Instruction>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction {
    /// $dest = $lhs $op $rhs
    Binary(Register, Register, BinOp, Register),
    ///Cast the value from one type to another
    /// $dest = $val as type
    Cast(Register, Register, Size),
    /// Call the function with the provided args in registers and store in in the dest
    /// $dest = call $func($args)
    Call(Register, Register, Vec<Register>),
    /// $dest = val
    StoreI(Register, Value),
    /// $dest = val
    Store(Register, Register),
    /// goto %label
    Jump(Label),
    /// Label: %label
    Label(Label),
    // if $cond then $val jump %label
    Branch(Register, Register, Label),
    Illegal,
}
