use crate::{ir::Program, object::RawObject};
use errors::{FileId, WithError};
use semant::HirDatabase;

#[salsa::query_group(CodegenDatabaseStorage)]
pub trait CodegenDatabase: HirDatabase {
    #[salsa::invoke(crate::codegen::codegen_query)]
    fn codegen(&self, file: FileId) -> WithError<(Program, RawObject)>;
}
