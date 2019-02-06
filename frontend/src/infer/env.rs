//! This module provides an Environment which keeps a track of the mappings between a
//! `Symbol` and a `Type` or an `Entry`

use infer::types::Type;

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
