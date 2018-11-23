//! Instruction in the VM;
//! Each Instruction in the VM is 8 bits
pub mod opcode {
    /// ILLEGAL INST
    pub const IGL: u8 = 00;
    /// HLT
    /// Stops the running of the vm
    pub const HLT: u8 = 01;
    /// RETURN
    /// Returns the value on the top of the stack
    pub const RETURN: u8 = 02;
    /// CONSTANT
    /// Allocates a constant into the constant pool
    pub const CONSTANT: u8 = 03;
    /// PRINT
    /// Prints the value on the top of the stack
    pub const PRINT: u8 = 04;
    /// NEGATEF $x
    /// Returns -$x .FLOATS ONLY
    pub const NEGATEF: u8 = 05;
    /// NEGATE $x
    /// Returns -$x
    pub const NEGATE: u8 = 06;
    /// NIL
    /// Places nil onto the stack
    pub const NIL: u8 = 07;
    /// TRUE
    /// Places true onto the stack
    pub const TRUE: u8 = 08;
    /// FALSE
    /// Places false onto the stack
    pub const FALSE: u8 = 09;
    /// NOT $x
    /// Returns !$x
    pub const NOT: u8 = 010;
    /// EQUAl $x $y
    /// Returns $x == $y
    pub const EQUAL: u8 = 011;
    /// GREATER $x $y
    /// Returns $x > $y
    pub const GREATER: u8 = 012;
    pub const GREATERF: u8 = 013;
    /// LESS $x $y
    /// Returns $x < $y
    pub const LESS: u8 = 014;
    pub const LESSF: u8 = 015;
    /// ADD $x $y
    pub const ADD: u8 = 016;
    /// ADD Float
    pub const ADDF: u8 = 017;
    /// SUB $x $y
    pub const SUB: u8 = 018;
    /// SUB Float
    pub const SUBF: u8 = 019;
    /// MUL $x $y
    pub const MUL: u8 = 020;
    /// MUL Float $x $y
    pub const MULF: u8 = 021;
    /// DIV $x $y
    pub const DIV: u8 = 022;
    /// DIV Float $x $y
    pub const DIVF: u8 = 023;

    pub const JUMP: u8 = 024;

    pub const GETLOCAL: u8 = 025;

    pub const SETLOCAL: u8 = 026;

    pub const CALL: u8 = 027;

    /// Jump to a location if true
    pub const JUMPIF: u8 = 028;
    /// jumps to a location if false
    pub const JUMPNOT: u8 = 029;

    pub const LOOP: u8 = 030;

    pub const POP: u8 = 031;

    pub const CONCAT: u8 = 032;

    pub const GETPARAM: u8 = 033;

    pub const SETPARAM: u8 = 034;

    pub const CALLCLOSURE: u8 = 035;

    pub const ARRAY: u8 = 036;

    pub const INDEXARRAY: u8 = 037;

    pub const INDEXSTRING: u8 = 038;

    pub const GETPROPERTY: u8 = 039;

    pub const GETMETHOD: u8 = 040;

    pub const CLASSINSTANCE: u8 = 041;

    pub const SETPROPERTY: u8 = 042;

    pub const CALLMETHOD: u8 = 043;

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
