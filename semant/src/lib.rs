#[macro_use]
mod db;
mod hir;
mod infer;
// mod infer2;
mod lower;
mod util;
#[macro_use]
mod resolver;
pub use db::{HirDatabase, HirDatabaseStorage, InternDatabaseStorage};
pub use infer::{Ctx, Type, TypeCon, TypeMap};
pub use syntax::TextRange;
