use crate::HirDatabase;
use errors::{FileId, WithError};

pub fn infer_query(db: &impl HirDatabase, file: FileId) -> WithError<()> {
    let program = db.lower(file)?;
    let _ = db.resolve_source_file(file)?;

    for _ in &program.functions {
        //
    }

    Ok(())
}
