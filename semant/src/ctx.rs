use crate::hir::{FunctionId, FunctionMap};

pub struct Context {
    functions: FunctionMap,
}

impl Context {
    pub fn new() -> Self {
        Context {
            functions: FunctionMap::new(),
        }
    }

    pub fn add_function() -> FunctionId {
        unimplemented!()
    }
}
