#[macro_use]
mod db;
pub mod hir;
pub mod infer;
// mod infer2;
mod lower;
pub mod typed;
mod util;
#[macro_use]
mod resolver;
pub use db::{HirDatabase, HirDatabaseStorage, InternDatabaseStorage};
pub use indexmap::IndexMap;
pub use infer::{Ctx, InferDataMap, StackedMap, Type, TypeCon};
pub use resolver::Resolver;
pub use syntax::{SmolStr, TextRange};
pub use typed::{Function, Program, Typed};
pub use util::Span;
