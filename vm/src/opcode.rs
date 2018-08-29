//! Instruction in the VM;
//! Each Instruction in the VM is 32 bits and the first 8 bits will
//! contain our opcode
//! The remaing 24 bits will be used for the Operands
//!

/// HLT
/// Stops the running of the vm
pub const HLT: u8 = 0x1;
/// JMP Dest
/// Changes the ip to the value in the register
/// Allows for jumping forward or backwards
pub const JMP: u8 = 0x2;
/// Returns the value stored the in RET register
pub const RETURN: u8 = 0x3;
/// NUMBER
pub const CONSTANT: u8 = 0x4;
/// NEGATE $DEST
/// Unary negate of the dest
pub const NEGATE: u8 = 0x5;
/// ADD SRC SRC DEST
pub const ADD: u8 = 0x6;
/// SUB SRC SRC DEST
pub const SUB: u8 = 0x7;
/// MUL SRC SRC DEST
pub const MUL: u8 = 0x8;
/// DIV SRC SRC DEST
pub const DIV: u8 = 0x9;
/// DIV SRC SRC DEST
pub const NIL: u8 = 0x10;
pub const TRUE: u8 = 0x11;
pub const FALSE: u8 = 0x12;

pub const NOT: u8 = 0x13;
pub const EQUAL: u8 = 0x14;
pub const GREATER: u8 = 0x15;
pub const LESS: u8 = 0x17;
/// LOAD SRC DEST
pub const LOAD: u8 = 0x16;
/// JMPF DEST
pub const JUMPF: u8 = 0x18;
/// JMPB DEST
pub const JUMPB: u8 = 0x19;
