//! This module provides an Environment which keeps a track of the mappings between a
//! `Symbol` and a `Type` or an `Entry`

use infer::types::Type;

/// A typed variable mapped to a var of a number i.e int | float or otherwise
#[derive(Debug, Clone, PartialEq)]
pub enum VarType {
    Number,
    Other,
}

#[derive(Debug, Clone, PartialEq)]
pub enum VarEntry {
    Var(Type),
    Fun { ty: Type },
}

impl VarEntry {
    pub fn get_ty(self) -> Type {
        match self {
            VarEntry::Fun { ty } => ty,
            VarEntry::Var(ty) => ty,
        }
    }
}
