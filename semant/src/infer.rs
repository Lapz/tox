// mod type_alias;
mod alias;
mod ctx;
mod infer;
mod stacked_map;
mod ty;

pub use ctx::Ctx;
pub(crate) use infer::infer_query;

pub(crate) use ty::{Type, TypeCon};
