use crate::resolver::Resolver;
use crate::HirDatabase;
use errors::{FileId, WithError};
use std::sync::Arc;

#[derive(Debug)]
struct InferDataCollector<DB> {
    db: DB,
    resolver: Arc<Resolver>,
}

pub fn infer_query(db: &impl HirDatabase, file: FileId) -> WithError<()> {
    let program = db.lower(file)?;
    let resolver = db.resolve_source_file(file)?;

    let collector = InferDataCollector { db, resolver };

    for function in &program.functions {
        //
    }

    Ok(())
}
