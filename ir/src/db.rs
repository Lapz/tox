use crate::ir::Instruction;
use errors::{FileId, WithError};
use semant::HirDatabase;

#[salsa::query_group(IrDatabaseStorage)]
pub trait IrDatabase: HirDatabase {
    /*
    #[salsa::invoke(c)]
    fn escape_analysis(&self,file:FileId) -> WithError<HashMap<NameId,>>;
    */
    #[salsa::invoke(crate::builder::build_ir_query)]
    fn build_ir(&self, file: FileId) -> WithError<()>;
}
