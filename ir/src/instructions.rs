use std::collections::HashMap;
use std::fmt::{self,Debug,Display};
use util::symbol::Symbol;


static mut LABEL_COUNT: u32 = 0;

static mut REGISTER_COUNT: u32 = 0;

static mut BLOCK_COUNT: u32 = 0;

#[derive(Debug, Clone, Hash, PartialEq, Eq, Copy)]
pub struct BlockID(pub u32);

/// A label in the code.
pub struct Label(u32);

/// A temp label that can either be a temp location or a register
#[derive(Clone, Copy, Hash, PartialEq, Default)]
pub struct Register(u32);


pub struct Program {
    pub functions: Vec<Function>,
    pub classes: Vec<Class>
}

pub struct Function {
    pub name:Symbol,
    pub params:Vec<Label>,
    pub body:Vec<Instruction>
}

#[derive(Debug,Clone)]
pub struct Class {
    ident:Symbol,
    fields:Vec<crate::types::Type>,
    methods:Vec<crate::types::Type>

}

#[derive(Debug, Clone)]
pub struct LoopDescription {
    start: BlockID,
    end: BlockID,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub instructions: Vec<Instruction>,
    pub end: BlockEnd,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    /// Integer Constant
    Const(u64),
    /// Float Constant
    Float(f64),
    /// A named variable
    Name(Symbol),
    ///  Contents of a word of memory at address
    Mem(Vec<u8>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum BlockEnd {
    Jump(BlockID),
    Return(Value),
    Branch(Value, BlockID, BlockID),
    End,
}

impl BlockID {
    pub fn new() -> BlockID {
        let count = unsafe { BLOCK_COUNT };

        let id = BlockID(count);

        unsafe {
            BLOCK_COUNT += 1;
        }

        id
    }
}

impl Label {
    pub fn new() -> Label {
        let count = unsafe { LABEL_COUNT };

        let label = Label(count);

        unsafe {
            LABEL_COUNT += 1;
        }

        label
    }
}

impl Register {
    pub fn new() -> Register {
        let count = unsafe { REGISTER_COUNT };

        let temp = Register(count);

        unsafe {
            REGISTER_COUNT += 1;
        }

        temp
    }
}


#[derive(Debug,Clone,PartialEq)]
pub struct Instruction {
    pub instruction:Inst,
    pub ty:crate::types::Type
}

/// Instruction used in the IR
/// Instructions are of the form i <- a op b
#[derive(Debug, Clone, PartialEq)]
pub enum Inst {
    /// A stack allocated array of size whatever
    /// Stored at a location
    Array(Vec<Value>),

    Drop(Register),

    Binary(Register, Value, BinaryOp, Value),

    Cast(Value,crate::types::Type),
    
    Call(Value, Value, Vec<Value>),

    StatementStart,
    
    /// t1 = val
    Store(Value, Value),

    /// t1 = op a
    Unary(Value, Value, UnaryOp),

    Return(Value),
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Plus,
    Minus,
    Mul,
    Div,
    Gt,
    Gte,
    Lt,
    Lte,
    Equal,
    NotEqual,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Bang,
    Minus,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CmpOp {
    LT,
    GT,
    LTE,
    GTE,
    EQ,
    NE,
}

impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::Const(ref v) => write!(f, "{}", v),
            Value::Float(ref v) => write!(f, "{}",v),
            Value::Name(ref name) => write!(f, "{}", name),
            Value::Mem(ref bytes) => {
                write!(f, "[")?;

                for (i, byte) in bytes.iter().enumerate() {
                    if i + 1 == bytes.len() {
                        write!(f, "{}", byte)?;
                    } else {
                        write!(f, "{},", byte)?;
                    }
                }

                write!(f, "]")
            }
        }
    }
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            BinaryOp::Plus => write!(f, "+"),
            BinaryOp::Minus => write!(f, "-"),
            BinaryOp::Mul => write!(f, "*"),
            BinaryOp::Div => write!(f, "/"),
            BinaryOp::Lt => write!(f, "<"),
            BinaryOp::Gt => write!(f, ">"),
            BinaryOp::Lte => write!(f, "<="),
            BinaryOp::Gte => write!(f, ">="),
            BinaryOp::Equal => write!(f, "=="),
            BinaryOp::NotEqual => write!(f, "!="),
        }
    }
}

impl Display for CmpOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            CmpOp::LT => write!(f, "<"),
            CmpOp::LTE => write!(f, "<="),
            CmpOp::GT => write!(f, ">"),
            CmpOp::GTE => write!(f, ">="),
            CmpOp::NE => write!(f, "!="),
            CmpOp::EQ => write!(f, "=="),
        }
    }
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            UnaryOp::Bang => write!(f, "!"),
            UnaryOp::Minus => write!(f, "-"),
        }
    }
}

impl Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "t{}", self.0)
    }
}

impl Debug for Register {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "t{}", self.0)
    }
}

impl Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "l{}", self.0)
    }
}
impl Display for BlockID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "label_{}", self.0)
    }
}

impl Display for BlockEnd {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            BlockEnd::End => write!(f, "end"),
            BlockEnd::Jump(ref id) => write!(f, "goto {}", id),
            BlockEnd::Return(ref id) => write!(f, "return {}", id),
            BlockEnd::Branch(ref v, ref t_branch, ref f_branch) => {
                write!(f, "branch {} {} {}", v, t_branch, f_branch)
            }
        }
    }
}

