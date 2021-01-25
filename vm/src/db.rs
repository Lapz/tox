use errors::{FileId, WithError};
use semant::HirDatabase;

fn dummy(db: &impl CodegenDatabase, file: FileId) -> i32 {
    1
}
#[salsa::query_group(CodegenDatabaseStorage)]
pub trait CodegenDatabase: HirDatabase {
    fn dummy(&self, file: FileId) -> i32;
}
