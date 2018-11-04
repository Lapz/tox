//! Instruction in the VM;
//! Each Instruction in the VM is 8 bits
pub mod opcode {
    /// ILLEGAL INST
    pub const IGL: u8 = 00;
    /// HLT
    /// Stops the running of the vm
    pub const HLT: u8 = 01;
    pub const RETURN: u8 = 02;
    ///CONSTANT
    /// CAN BE A STR FLOAT OR INT
    pub const CONSTANT: u8 = 03;
    pub const PRINT: u8 = 04;
    pub const NEGATEF: u8 = 05;
    pub const NEGATE: u8 = 06;
    pub const NIL: u8 = 07;
    pub const TRUE: u8 = 08;
    pub const FALSE: u8 = 09;
    pub const NOT: u8 = 010;
    pub const EQUAL: u8 = 011;

    pub const GREATER: u8 = 012;
    pub const GREATERF: u8 = 013;
    pub const LESS: u8 = 014;
    pub const LESSF: u8 = 015;
    /// ADD
    pub const ADD: u8 = 016;
    /// ADD Float
    pub const ADDF: u8 = 017;
    /// SUB
    pub const SUB: u8 = 018;
    /// SUB Float
    pub const SUBF: u8 = 019;

    /// MUL
    pub const MUL: u8 = 020;
    /// MUL Float
    pub const MULF: u8 = 021;
    /// DIV
    pub const DIV: u8 = 022;
    /// DIV Float
    pub const DIVF: u8 = 023;

    pub const JUMP: u8 = 024;

    pub const GETLOCAL:u8 = 025;

    pub const SETLOCAL:u8 = 026;

    pub const CALL:u8 = 027;

    /// Jump to a location if true
    pub const JUMPIF:u8 = 028;
    /// jumps to a location if false
    pub const JUMPNOT:u8 = 029;

    // pub const STORELOCAL:u8
}

// /// JMP Dest
//     /// Changes the ip to the value in the register
//     /// Allows for jumping forward or backwards
//     pub const JMP: u8 = 02;

//     /// JMPF DEST
//     /// increments the `ip` by the value stored in DEST
//     pub const JMPF: u8 = 03;

//     /// JMPB DEST
//     /// decrements the `ip` by the value stored in DEST
//     pub const JMPB: u8 = 04;

//     /// JMPS if the equal flag is set;
//     pub const JMPEQ: u8 = 05;

//     /// JMPS if the equal flag is not set;
//     pub const JMPNEQ: u8 = 06;
