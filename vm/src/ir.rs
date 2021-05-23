use std::collections::HashMap;

use semant::hir::{NameId, ParamId};

use crate::{chunks::Chunk, object::RawObject};

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: NameId,
    pub body: Chunk,
    pub params: HashMap<NameId, usize>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Class {
    pub name: NameId,
    pub methods: HashMap<NameId, Function>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub functions: HashMap<usize, RawObject>,
    pub classes: HashMap<NameId, Class>,
}

impl Eq for Program {}
