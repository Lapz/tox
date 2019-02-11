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
    /// JUMP $LOC
    pub const JUMP: u8 = 024;
    /// GETLOCAL $slot
    pub const GETLOCAL: u8 = 025;
    /// SETLOCAL $slot
    pub const SETLOCAL: u8 = 026;
    /// CALL $func $num_args
    pub const CALL: u8 = 027;
    /// Jump to a location if true
    pub const JUMPIF: u8 = 028;
    /// jumps to a location if false
    pub const JUMPNOT: u8 = 029;
    /// LOOP $offset:u16
    /// decrease the ip by offset
    pub const LOOP: u8 = 030;
    /// POP
    /// Remove the value from the stack
    pub const POP: u8 = 031;
    /// CONCAT $x:str, $y:str
    pub const CONCAT: u8 = 032;
    /// GETPARAM $slot
    pub const GETPARAM: u8 = 033;
    /// SETPARAM $slot
    pub const SETPARAM: u8 = 034;
    /// CALLCLOSURE $args
    pub const CALLCLOSURE: u8 = 035;
    /// ARRAY
    /// Create an array object and put it on the stack
    pub const ARRAY: u8 = 036;
    /// INDEXARRAY
    /// Index an array
    pub const INDEXARRAY: u8 = 037;
    /// INDEXSTRING
    /// index a string
    pub const INDEXSTRING: u8 = 038;
    /// GETPROPERTY $id
    /// Get the property of class with $id
    pub const GETPROPERTY: u8 = 039;
    /// SETPROPERTY $id
    /// Set the property of class with $id
    pub const SETPROPERTY: u8 = 040;
    /// GETMETHOD $id
    /// Get the class method with the id of $id
    pub const GETMETHOD: u8 = 041;
    /// CLASSINSTANCE $id $num_properties
    /// Create a new ClassInstance object with the methods
    /// from the class $id and $num_properties
    pub const CLASSINSTANCE: u8 = 042;
    /// CALLINSTANCEMETHOD $id $arg_count
    /// Call the functions $id which takes $arg_count
    pub const CALLINSTANCEMETHOD: u8 = 043;
    /// CALLSTATICMETHOD $id $arg_count
    /// Call the static class methods $id which takes $arg_count
    pub const CALLSTATICMETHOD: u8 = 044;

    pub const CALLNATIVE: u8 = 045;

    /// converts an int(i64)  to float(f64)
    pub const INT2FLOAT: u8 = 046;
    /// converts an float(f64)  to int(i64)
    /// results in a loss of precession
    pub const FLOAT2INT: u8 = 047;
    /// converts a boolean to int(i64)
    pub const BOOL2INT: u8 = 049;

    pub const INT2STR: u8 = 050;

    pub const FLOAT2STR: u8 = 051;

    pub const ENUM: u8 = 052;
}
