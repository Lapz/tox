
use crate::hir;
use crate::ty;
use crate::util::StackedMap;

use std::default::Default;

#[derive(Debug)]
pub struct Ctx {
    types: StackedMap<hir::NameId, ty::Ty>,
    variables: StackedMap<hir::NameId, ty::VarEntry>,
    type_var_count: u32,
}

impl Ctx {
    pub(crate) fn tv(&mut self) -> ty::TypeVar {
        let value = self.type_var_count;
        self.type_var_count += 1;
        ty::TypeVar(value)
    }

    pub(crate) fn add_type(&mut self, name: hir::NameId, ty: ty::Ty) {
        self.types.insert(name, ty)
    }
}

impl Default for Ctx {
    fn default() -> Self {
        Ctx {
            types: StackedMap::new(),
            variables: StackedMap::new(),
            type_var_count: 0,
        }
    }
}
