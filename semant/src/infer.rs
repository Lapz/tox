mod ctx;
mod infer;
mod stacked_map;
mod subst;
mod ty;
mod unify;

pub use ctx::Ctx;
pub(crate) use infer::infer_query;
pub(crate) use stacked_map::StackedMap;

use std::collections::HashMap;
pub(crate) use ty::{Type, TypeCon, TypeVar, Variant};

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct TypeId(salsa::InternId);
impl salsa::InternKey for TypeId {
    fn from_intern_id(v: salsa::InternId) -> Self {
        TypeId(v)
    }
    fn as_intern_id(&self) -> salsa::InternId {
        self.0
    }
}

pub(crate) type Subst = HashMap<TypeVar, Type>;
