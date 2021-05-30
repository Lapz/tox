use crate::ir::Label;
mod x86_64;
pub trait Frame: Clone {
    type Access: Clone;

    fn new(&mut self, name: Label, formals: Vec<bool>, register_count: u32) -> Self;

    fn name(&self) -> Label;

    fn formals(&self) -> &[Self::Access];

    fn alloc_local(&mut self, escape: bool) -> Self::Access;
}
