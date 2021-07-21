#[macro_use]
mod db;
pub mod hir;
mod infer;
// mod infer2;
mod lower;
mod util;
#[macro_use]
mod resolver;
pub use db::{HirDatabase, HirDatabaseStorage, InternDatabaseStorage};
pub use indexmap::IndexMap;
pub use infer::{Ctx, InferDataMap, StackedMap, Type, TypeCon, TypeMap};
pub use resolver::Resolver;
pub use syntax::{SmolStr, TextRange};
pub use util::Span;
