use crate::hir::Function;
use crate::resolver::Resolver;
use crate::HirDatabase;
use errors::{FileId, WithError};
use std::sync::Arc;

#[derive(Debug)]
struct InferDataCollector<DB> {
    db: DB,
    resolver: Arc<Resolver>,
}

impl<'a, DB> InferDataCollector<&'a DB>
where
    DB: HirDatabase,
{
    fn infer_function(&mut self, function: &Function) {}
}

pub fn infer_query(db: &impl HirDatabase, file: FileId) -> WithError<()> {
    let program = db.lower(file)?;
    let resolver = db.resolve_source_file(file)?;

    let mut collector = InferDataCollector { db, resolver };

    for function in &program.functions {
        collector.infer_function(function);
    }

    Ok(())
}
