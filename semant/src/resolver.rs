use crate::hir;
use std::collections::HashMap;

#[derive(Debug, PartialEq)]
pub enum State {
    Declared,
    Defined,
    Read,
}

#[derive(Debug, Default)]
pub struct Resolver {
    scopes: Vec<HashMap<hir::NameId, State>>,
}
