use crate::ir::{Label, Register};

use super::Frame;

pub const POINTER_SIZE: i64 = 8;
pub const FUNCTION_SIZE: i64 = 8;

#[derive(Debug, Clone)]
struct X86_64 {
    name: Label,
    formals: Vec<Access>,
    offset: i64,
    register_count: u32,
}
#[derive(Debug, Clone)]
enum Access {
    InFrame(i64),
    InReg(Register),
}

impl Frame for X86_64 {
    type Access = Access;

    fn new(&mut self, name: crate::ir::Label, formals: Vec<bool>, register_count: u32) -> Self {
        let mut frame = Self {
            name,
            formals: Vec::new(),
            offset: 0,
            register_count,
        };

        frame.formals = formals
            .iter()
            .map(|escape| frame.alloc_local(*escape))
            .collect();

        frame
    }

    fn name(&self) -> crate::ir::Label {
        self.name
    }

    fn formals(&self) -> &[Self::Access] {
        &self.formals
    }

    fn alloc_local(&mut self, escape: bool) -> Self::Access {
        if escape {
            self.offset -= POINTER_SIZE;
            Access::InFrame(self.offset)
        } else {
            self.register_count += 1;
            Access::InReg(Register(self.register_count))
        }
    }
}
