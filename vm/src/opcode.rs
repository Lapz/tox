//! Instruction in the VM;
//! Each Instruction in the VM is 32 bits and the first 8 bits will
//! contain our opcode
//! The remaining 24 bits will be used for the Operands

pub type OpCode = u8;

/// ILLEGAL INST
pub const IGL: u8 = 00;

/// HLT
/// Stops the running of the vm
pub const HLT: u8 = 01;

/// JMP Dest
/// Changes the ip to the value in the register
/// Allows for jumping forward or backwards
pub const JMP: u8 = 02;

/// JMPF DEST
/// increments the `ip` by the value stored in DEST
pub const JMPF: u8 = 03;

/// JMPB DEST
/// decrements the `ip` by the value stored in DEST
pub const JMPB: u8 = 04;

/// JMPS if the equal flag is set;
pub const JMPEQ: u8 = 05;

/// JMPS if the equal flag is not set;
pub const JMPNEQ: u8 = 06;

/// ADD SRC SRC DEST
pub const ADD: u8 = 07;

/// SUB SRC SRC DEST
pub const SUB: u8 = 08;

/// MUL SRC SRC DEST
pub const MUL: u8 = 09;

/// DIV SRC SRC DEST
pub const DIV: u8 = 010;

/// NOT
/// Set the equal_flag to !equal_flag
pub const NOT: u8 = 011;

/// EQUAL SRC SRC
/// Sets the equal_flag to true
pub const EQUAL: u8 = 012;

/// GREATER SRC SRC
/// Sets the equal_flag to true
pub const GREATER: u8 = 013;

/// Less SRC SRC
/// Sets the equal_flag to 1
pub const LESS: u8 = 014;

/// LOAD SRC DEST
pub const LOAD: u8 = 015;

/// STORES $SRC $DEST
/// stores the value in src in dest
pub const STORE: u8 = 016;

/// ALLOC $BYTES
/// Extends the heap by n bytes
pub const ALLOC: u8 = 017;

/// FREE $BYTES
/// Shrinks the heap by n bytes
pub const FREE: u8 = 018;

/// INC $REG
/// Increase the value stored in the register by 1
pub const INC: u8 = 019;

/// DEC $REG
/// Decrease the value stored in the register by 1
pub const DEC: u8 = 020;

/// PUSH $REG
/// Pushes the value onto the stack
pub const PUSH: u8 = 021;

/// POP $REG
/// Pops the value off the top of stack
pub const POP: u8 = 022;

/// MOD SRC SRC DEST
/// CALCULATES SRC1/SRC2 AND STORES IT IN DEST
pub const MOD: u8 = 023;

/// EXPON SRC SRC DEST
/// CALCULATES SRC1^SRC2 AND STORES IT IN DEST
pub const EXPON: u8 = 024;
