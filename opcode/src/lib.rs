/// ILLEGAL INST
pub const IGL: u8 = 0;
/// HLT
/// Stops the running of the vm
pub const HLT: u8 = 1;
/// RETURN
/// Returns the value on the top of the stack
pub const RETURN: u8 = 2;
/// CONSTANT
/// Allocates a constant into the constant pool
pub const CONSTANT: u8 = 3;
/// PRINT
/// Prints the value on the top of the stack
pub const PRINT: u8 = 4;
/// NEGATEF $x
/// Returns -$x .FLOATS ONLY
pub const NEGATEF: u8 = 5;
/// NEGATE $x
/// Returns -$x
pub const NEGATE: u8 = 6;
/// NIL
/// Places nil onto the stack
pub const NIL: u8 = 7;
/// TRUE
/// Places true onto the stack
pub const TRUE: u8 = 8;
/// FALSE
/// Places false onto the stack
pub const FALSE: u8 = 9;
/// NOT $x
/// Returns !$x
pub const NOT: u8 = 10;
/// EQUAl $x $y
/// Returns $x == $y
pub const EQUAL: u8 = 11;
/// GREATER $x $y
/// Returns $x > $y
pub const GREATER: u8 = 12;
pub const GREATERF: u8 = 13;
/// LESS $x $y
/// Returns $x < $y
pub const LESS: u8 = 14;
pub const LESSF: u8 = 15;
/// ADD $x $y
pub const ADD: u8 = 16;
/// ADD Float
pub const ADDF: u8 = 17;
/// SUB $x $y
pub const SUB: u8 = 18;
/// SUB Float
pub const SUBF: u8 = 19;
/// MUL $x $y
pub const MUL: u8 = 20;
/// MUL Float $x $y
pub const MULF: u8 = 21;
/// DIV $x $y
pub const DIV: u8 = 22;
/// DIV Float $x $y
pub const DIVF: u8 = 23;
/// JUMP $LOC
pub const JUMP: u8 = 24;
/// GETLOCAL $slot
pub const GETLOCAL: u8 = 25;
/// SETLOCAL $slot
pub const SETLOCAL: u8 = 26;
/// CALL $func $num_args
pub const CALL: u8 = 27;
/// Jump to a location if true
pub const JUMPIF: u8 = 28;
/// jumps to a location if false
pub const JUMPNOT: u8 = 29;
/// LOOP $offset:u16
/// decrease the ip by offset
pub const LOOP: u8 = 30;
/// POP
/// Remove the value from the stack
pub const POP: u8 = 31;
/// CONCAT $x:str, $y:str
pub const CONCAT: u8 = 32;
/// GETPARAM $slot
pub const GETPARAM: u8 = 33;
/// SETPARAM $slot
pub const SETPARAM: u8 = 34;
/// CALLCLOSURE $args
pub const CALLCLOSURE: u8 = 35;
/// ARRAY
/// Create an array object and put it on the stack
pub const ARRAY: u8 = 36;
/// INDEXARRAY
/// Index an array
pub const INDEXARRAY: u8 = 37;
/// INDEXSTRING
/// index a string
pub const INDEXSTRING: u8 = 38;
/// GETPROPERTY $id
/// Get the property of class with $id
pub const GETPROPERTY: u8 = 39;
/// SETPROPERTY $id
/// Set the property of class with $id
pub const SETPROPERTY: u8 = 40;
/// GETMETHOD $id
/// Get the class method with the id of $id
pub const GETMETHOD: u8 = 41;
/// CLASSINSTANCE $id $num_properties
/// Create a new ClassInstance object with the methods
/// from the class $id and $num_properties
pub const CLASSINSTANCE: u8 = 42;
/// CALLINSTANCEMETHOD $id $arg_count
/// Call the functions $id which takes $arg_count
pub const CALLINSTANCEMETHOD: u8 = 43;
/// CALLSTATICMETHOD $id $arg_count
/// Call the static class methods $id which takes $arg_count
pub const CALLSTATICMETHOD: u8 = 44;

pub const CALLNATIVE: u8 = 45;

/// converts an int(i64)  to float(f64)
pub const INT2FLOAT: u8 = 46;
/// converts an float(f64)  to int(i64)
/// results in a loss of precession
pub const FLOAT2INT: u8 = 47;
/// converts a boolean to int(i64)
pub const BOOL2INT: u8 = 49;

pub const INT2STR: u8 = 50;

pub const FLOAT2STR: u8 = 51;

pub const ENUM: u8 = 52;

// declare an enum with associated data
pub const ENUMDATA: u8 = 53;
