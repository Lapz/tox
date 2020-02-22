use crate::hir;
use std::collections::HashMap;

// pub(crate) enum Level {
//     Global,
//     Block(hir::BlockId),
// }
// #[derive(Debug, Clone)]
// pub struct FileTable {
//     symbol_level: HashMap<hir::NameId, Level>,
// }

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
