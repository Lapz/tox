//! This module provides an Environment which keeps a track of the mappings between a
//! `Symbol` and a `Type` or an `Entry`

use infer::types::Type;

#[derive(Debug, Clone, PartialEq)]
pub enum Entry {
    Ty(Type),
    Class(Type),
    Fun(Type),
}

#[derive(Debug, Clone, PartialEq)]
pub enum VarEntry {
    Var(Type),
    Fun { ty: Type },
}

impl Entry {
    pub fn ty(&self) -> &Type {
        match *self {
            Entry::Ty(ref ty) | Entry::Class(ref ty) | Entry::Fun(ref ty) => ty,
        }
    }

    pub fn get_ty(self) -> Type {
        match self {
            Entry::Ty(ty) | Entry::Class(ty) | Entry::Fun(ty) => ty,
        }
    }
}

impl VarEntry {
    pub fn get_ty(self) -> Type {
        match self {
            VarEntry::Fun { ty } => ty,
            VarEntry::Var(ty) => ty,
        }
    }
}
