use indexmap::map::IndexMap;
use indexmap::set::IndexSet;
use std::collections::HashMap;
use std::fmt::{self, Debug, Display};
use std::hash::{Hash, Hasher};
use util::symbol::{Symbol, Symbols};

static mut LABEL_COUNT: u32 = 0;

static mut REGISTER_COUNT: u32 = 4;

static mut BLOCK_COUNT: u32 = 0;

pub const STACK_POINTER: Register = Register::Register(0);

pub const RAX: Register = Register::Register(1);

pub const RBP: Register = Register::Register(2);

pub const RES: Register = Register::Register(3);

pub const POINTER_WIDTH: usize = 8;

#[derive(Debug, Clone, Hash, PartialEq, Eq, Copy, PartialOrd, Ord)]
pub struct BlockID(pub u32);

/// A label in the code.
pub struct Label(u32);

#[derive(Clone, Copy, Hash, Eq, Ord, PartialOrd)]
pub enum Register {
    /// A random register
    Register(u32),
    Offset(u32, usize),
    Named(Symbol),
}

/// A register

// pub struct Register(pub u32);

pub struct Program {
    pub functions: Vec<Function>,
    pub classes: Vec<Class>,
}

#[derive(Debug)]
pub struct Function {
    pub name: Symbol,
    pub params: Vec<Register>,
    pub locals: Vec<Register>,
    pub blocks: IndexMap<BlockID, Block>,
    pub start_block: BlockID,
    pub registers: IndexMap<Register, usize>,
    pub stack_locs: IndexMap<Register, Register>,
}

impl Function {
    pub fn dummy() -> Self {
        Function {
            name: Symbol(0),
            params: Vec::new(),
            locals: Vec::new(),
            blocks: IndexMap::new(),
            start_block: BlockID(0),
            registers: IndexMap::new(),
            stack_locs: IndexMap::new(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct StructLayout {
    fields: HashMap<Symbol, usize>,
}

#[derive(Debug, Clone)]
pub struct Layout {
    /// the size of the field
    size_of: usize,
    /// the alignment
    align_of: usize,
}
#[derive(Debug, Clone)]
pub struct Class {
    ident: Symbol,
}

#[derive(Debug, Clone, Copy)]
pub struct LoopDescription {
    pub start: BlockID,
    pub end: BlockID,
}

#[derive(Clone)]
pub struct Block {
    pub instructions: Vec<Instruction>,
    pub end: BlockEnd,
}

impl Block {
    pub fn new(instructions: Vec<Instruction>, end: BlockEnd) -> Self {
        Self { instructions, end }
    }
}
#[derive(Debug, Clone, PartialEq)]
pub enum BlockEnd {
    Jump(BlockID),
    Return(Register),
    Branch(Register, BlockID, BlockID),
    Link(BlockID),
    End,
}
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum Size {
    Bit8,
    Bit32,
    Bit64,
}
/// Instruction used in the IR
/// Instructions are of the form i <- a op b
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Instruction {
    /// $dest = $lhs $op $rhs
    Binary(Register, Register, BinaryOp, Register),
    ///Cast the value from one type to another
    /// $dest = $val as type
    Cast(Register, Register, Size),
    /// Call the function with the provided args in registers and store in in the dest
    /// $dest = call $func
    Call(Register, Symbol, Vec<Register>),
    /// A phi node
    /// $dest = φ($lhs,$rhs)
    Phi(Register, Register, Register),
    StatementStart,

    /// $dest = val
    StoreI(Register, Value),
    /// $dest = val
    Store(Register, Register),
    /// $dest = op $val
    Unary(Register, Register, UnaryOp),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    /// Integer Constant
    Const(i64),
    /// Float Constant
    Float(f64),
    /// One of many registers
    Register(Register),
    /// A stack allocated array of size whatever
    Array(usize),
    ///  Contents of a word of memory at address
    Mem(Vec<u8>),

    Bool(bool),

    Nil,
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
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

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
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

impl LoopDescription {
    pub fn start(&self) -> BlockID {
        self.start
    }

    pub fn end(&self) -> BlockID {
        self.end
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

        let temp = Register::Register(count);

        unsafe {
            REGISTER_COUNT += 1;
        }

        temp
    }

    pub fn offset_at(&self, offset: usize) -> Register {
        match self {
            Register::Register(reg) => Register::Offset(*reg, offset),
            Register::Offset(reg, _) => Register::Offset(*reg, offset),
            _ => unimplemented!(),
        }
    }

    pub fn named(symbol: Symbol) -> Register {
        Register::Named(symbol)
    }

    pub fn name(&self, symbols: &Symbols<()>) -> String {
        match self {
            Register::Register(ref v) => format!("t{}", v),
            Register::Offset(ref v, offset) => format!("{}", v),
            Register::Named(ref v) => format!("{}", symbols.name(*v)),
        }
    }

    pub fn pretty(&self, symbols: &Symbols<()>) -> String {
        match self {
            Register::Register(ref v) => format!("%t{}", v),
            Register::Offset(ref v, offset) => format!("(%{}){}", v, offset),
            Register::Named(ref v) => format!("%{}", symbols.name(*v)),
        }
    }
}

impl BlockEnd {
    pub fn pretty(&self, symbols: &Symbols<()>) -> String {
        match *self {
            BlockEnd::End => format!("end"),
            BlockEnd::Jump(ref id) => format!("goto {}", id),
            BlockEnd::Return(ref id) => format!("return {}", id.pretty(symbols)),
            BlockEnd::Branch(ref v, ref t_branch, ref f_branch) => {
                format!("branch {} {} {}", v, t_branch, f_branch)
            }

            BlockEnd::Link(ref link) => format!("next {}", link),
        }
    }
}

impl Layout {
    pub fn new(size_of: usize, align_of: usize) -> Layout {
        Layout { align_of, size_of }
    }
}

impl Instruction {
    pub fn is_move(&self) -> bool {
        match self {
            Instruction::Store(_, _) => true,
            _ => false,
        }
    }

    pub fn is_start(&self) -> bool {
        match self {
            Instruction::StatementStart => true,
            _ => false,
        }
    }

    pub fn is_phi(&self) -> bool {
        match self {
            Instruction::Phi(_, _, _) => true,
            _ => false,
        }
    }

    pub fn rewrite_phi_name(&mut self, new: Register) {
        match self {
            Instruction::Phi(ref mut dest, _, _) => {
                *dest = new;
            }
            _ => {}
        }
    }

    pub fn is_store_i(&self) -> bool {
        match self {
            Instruction::StoreI(_, _) => true,
            _ => false,
        }
    }

    pub fn get_immediate(self) -> Option<Value> {
        match self {
            Instruction::StoreI(_, value) => Some(value),
            _ => None,
        }
    }

    pub fn used(&self) -> IndexSet<Register> {
        let mut used = IndexSet::new();
        use Instruction::*;
        match self {
            Binary(_, ref lhs, _, ref rhs) => {
                used.insert(*lhs);
                used.insert(*rhs);
            }
            Cast(_, ref value, _) => {
                used.insert(*value);
            }

            Call(_, _, ref args) => {
                for arg in args {
                    used.insert(*arg);
                }
            }

            StatementStart => {}

            StoreI(_, _) => {}

            Store(_, ref val) => {
                used.insert(*val);
            }

            Unary(_, ref val, _) => {
                used.insert(*val);
            }

            Phi(_, ref lhs, ref rhs) => {
                used.insert(*lhs);
                used.insert(*rhs);
            }
        }

        used
    }

    pub fn def(&self) -> IndexSet<Register> {
        let mut defined = IndexSet::new();
        use Instruction::*;
        match self {
            Binary(ref dest, _, _, _) => {
                defined.insert(*dest);
            }
            Cast(ref dest, _, _) => {
                defined.insert(*dest);
            }

            Call(ref dest, _, _) => {
                defined.insert(*dest);
            }

            StatementStart => (),

            StoreI(ref dest, _) => {
                defined.insert(*dest);
            }

            Store(ref dest, _) => {
                defined.insert(*dest);
            }

            Unary(ref dest, _, _) => {
                defined.insert(*dest);
            }

            Phi(ref dest, _, _) => {
                defined.insert(*dest);
            }
        }

        defined
    }

    pub fn rewrite_uses(&mut self, old_reg: Register, new_reg: Register) {
        use Instruction::*;
        match self {
            Binary(_, ref mut lhs, _, ref mut rhs) => {
                if *lhs == old_reg {
                    *lhs = new_reg;
                } else if *rhs == old_reg {
                    *rhs = new_reg;
                }
            }
            Cast(_, ref mut value, _) => {
                if *value == old_reg {
                    *value = new_reg;
                }
            }

            Call(_, _, ref mut args) => {
                for arg in args {
                    if *arg == old_reg {
                        *arg = new_reg;
                    }
                }
            }

            StatementStart => (),

            StoreI(_, _) => {}

            Store(_, ref mut value) => {
                if *value == old_reg {
                    *value = new_reg;
                }
            }

            Unary(_, ref mut value, _) => {
                if *value == old_reg {
                    *value = new_reg;
                }
            }

            Phi(_, ref mut lhs, ref mut rhs) => {
                if *lhs == old_reg {
                    *lhs = new_reg;
                } else if *rhs == old_reg {
                    *rhs = new_reg;
                }
            }
        }
    }

    pub fn rewrite_def(&mut self, new_reg: Register) {
        use Instruction::*;
        match self {
            Binary(ref mut dest, _, _, _) => {
                *dest = new_reg;
            }
            Cast(ref mut dest, _, _) => {
                *dest = new_reg;
            }

            Call(ref mut dest, _, _) => {
                *dest = new_reg;
            }

            StatementStart => (),

            StoreI(ref mut dest, _) => {
                *dest = new_reg;
            }

            Store(ref mut dest, _) => {
                *dest = new_reg;
            }

            Unary(ref mut dest, _, _) => {
                *dest = new_reg;
            }

            _ => unimplemented!(),
        }
    }
}

impl PartialEq for Register {
    fn eq(&self, o: &Register) -> bool {
        match (self, o) {
            (Register::Register(ref s), Register::Register(ref o)) => s == o,
            (Register::Offset(ref s, so), Register::Offset(ref o, ref oo)) => s == o && so == oo,
            (Register::Named(ref s), Register::Named(ref o)) => s == o,
            _ => false,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::Const(ref v) => write!(f, "{}", v),
            Value::Float(ref v) => write!(f, "{}", v),
            Value::Register(ref name) => write!(f, "{}", name),
            Value::Array(ref len) => write!(f, "[_:{};]", len),
            Value::Bool(ref b) => write!(f, "{}", b),
            Value::Nil => write!(f, "nil"),
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
        match self {
            Register::Register(ref v) => write!(f, "%t{}", v),
            Register::Offset(ref v, offset) => write!(f, "(%{}){}", v, offset),
            Register::Named(ref v) => write!(f, "%{}", v),
        }
    }
}

impl Debug for Register {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Register::Register(ref v) => write!(f, "%t{:?}", v),
            Register::Offset(ref v, offset) => write!(f, "(%{:?}){:?}", v, offset),
            Register::Named(ref v) => write!(f, "%{}", v),
        }
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

            BlockEnd::Link(ref link) => write!(f, "next {}", link),
        }
    }
}

impl Display for Instruction {
    fn fmt(&self, out: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Instruction::StatementStart => write!(out, ""),
            Instruction::Binary(ref res, ref lhs, ref op, ref rhs) => {
                write!(out, "{} <- {} {} {}", res, lhs, op, rhs)
            }
            Instruction::Store(ref dest, ref source) => write!(out, "{} <- {}", dest, source),
            Instruction::StoreI(ref dest, ref val) => write!(out, "{} <- {}", dest, val),
            Instruction::Cast(ref dest, _, ref ty) => write!(out, "{} as {}", dest, ty),
            Instruction::Unary(ref dest, ref source, ref op) => {
                write!(out, "{} <- {}{}", dest, op, source)
            }
            Instruction::Phi(ref dest, ref lhs, ref rhs) => {
                write!(out, "{} <- φ({},{})", dest, lhs, rhs)
            }
            Instruction::Call(ref dest, ref callee, ref args) => {
                for arg in args {
                    write!(out, "arg: {}\n", arg)?;
                }

                write!(out, "{} <- call {}", dest, callee)?;

                Ok(())
            }
        }
    }
}

impl Debug for Block {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for inst in self.instructions.iter() {
            writeln!(f, "{}", inst)?;
        }

        writeln!(f, "{}", self.end)?;

        Ok(())
    }
}

impl Display for Size {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Size::Bit8 => write!(f, "i8"),
            Size::Bit32 => write!(f, "i32"),
            Size::Bit64 => write!(f, "i64"),
        }
    }
}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Value::Float(ref _value) => {}

            rest @ _ => rest.hash(state),
        }
    }
}

impl Eq for Value {}
