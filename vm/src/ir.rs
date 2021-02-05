use std::collections::HashMap;

use semant::hir::{NameId, ParamId};

use crate::chunks::Chunk;

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: NameId,
    pub body: Chunk,
    pub params: HashMap<NameId, usize>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub functions: HashMap<NameId, Function>,
}

impl Eq for Program {}
