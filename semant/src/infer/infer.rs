use crate::hir::Function;
use crate::infer::Type;
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
    fn infer_function(&mut self, function: &Function) {
        let body = if let Some(ty) = self.resolver.get_type(&function.name.item) {
            ty
        } else {
            Type::Unknown
        };

        println!("{:?}", body);
    }
}

pub fn infer_query(db: &impl HirDatabase, file: FileId) -> WithError<()> {
    let WithError(program, mut errors) = db.lower(file);
    let WithError(resolver, error) = db.resolve_source_file(file);

    errors.extend(error);

    let mut collector = InferDataCollector { db, resolver };

    for function in &program.functions {
        collector.infer_function(function);
    }

    WithError((), errors)
}
