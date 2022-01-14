use crate::ir::{Instruction, Register};

#[derive(Debug, Clone, Hash, PartialEq, Eq, Copy)]
pub struct BlockID(pub u32);

#[derive(Clone,Debug)]
pub struct Block {
    pub instructions: Vec<Instruction>,
    pub end: BlockEnd,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BlockEnd {
    Jump(BlockID),
    Return(Register),
    Branch(Register, BlockID, BlockID),
    Link(BlockID),
    End,
}

#[derive(Debug)]
pub struct LoopDescription {
    pub start: BlockID,
    pub end: BlockID,
}


impl LoopDescription {
    pub fn start(&self) -> BlockID {
        self.start
    }

    pub fn end(&self) -> BlockID {
        self.end
    }
}
