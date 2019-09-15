use crate::hir::FunctionId;
use std::collections::HashMap;
pub struct Context {
    functions: HashMap<(), ()>,
}

impl Context {
    pub fn new() -> Self {
        Context {
            functions: HashMap::new(),
        }
    }

    pub fn add_function(name: unimplemented!()) -> FunctionId {}
}
