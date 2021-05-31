use errors::{db::File, FileId, WithError};
use semant::HirDatabase;

#[salsa::query_group(CodegenDatabaseStorage)]
pub trait CodegenDatabase: HirDatabase {
    /*
    #[salsa::invoke(c)]
    fn escape_analysis(&self,file:FileId) -> WithError<HashMap<NameId,>>;
    */
    #[salsa::invoke(crate::hir::compile_to_asm_query)]
    fn compile_to_asm(&self, file: FileId) -> WithError<()>;
}
