mod ctx;
mod infer;
mod pattern_matrix;
mod stacked_map;
mod subst;
mod ty;
mod unify;
use crate::{hir::NameId, resolver::Resolver, HirDatabase};
pub use ctx::Ctx;
use errors::{FileId, Reporter, WithError};
pub(crate) use stacked_map::StackedMap;
use std::sync::Arc;
pub(crate) use ty::{Type, TypeCon, TypeVar, Variant};

#[derive(Debug)]
pub(crate) struct InferDataCollector<DB> {
    db: DB,
    ctx: Ctx,
    resolver: Arc<Resolver>,
    reporter: Reporter,
    returns: Option<Type>,
    possible_returns: Vec<Type>,
    fn_name: Option<NameId>,
}

pub fn infer_query(db: &impl HirDatabase, file: FileId) -> WithError<()> {
    let WithError(program, mut errors) = db.lower(file);
    let WithError(resolver, error) = db.resolve_source_file(file);
    let reporter = Reporter::new(file);
    errors.extend(error);

    let ctx = resolver.ctx.clone();

    let mut collector = InferDataCollector {
        db,
        ctx,
        resolver,
        reporter,
        returns: None,

        fn_name: None,
        possible_returns: Vec::new(),
    };

    for function in &program.functions {
        collector.infer_function(function);
    }

    errors.extend(collector.reporter.finish());

    WithError((), errors)
}
