use crate::db::HirDatabase;
use crate::Ctx;

use crate::resolver::{data::ResolverDataCollector, Resolver};
use errors::{FileId, Reporter, WithError};
use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

pub fn resolve_exports_query(db: &impl HirDatabase, file: FileId) -> WithError<Arc<Resolver>> {
    let program = db.lower(file)?;
    let reporter = Reporter::new(file);
    let ctx = Ctx::new(db);
    let mut collector = ResolverDataCollector {
        db,
        ctx,
        reporter,
        items: HashSet::new(),
        exported_items: HashSet::new(),
        binding_error: false,
        function_data: HashMap::new(),
    };

    for function in &program.functions {
        collector.add_function(function.name, function.exported)
    }

    let (resolver, reporter) = collector.finish();

    if reporter.has_errors() {
        Err(reporter.finish())
    } else {
        Ok(Arc::new(resolver))
    }
}

pub fn resolve_source_file_query(db: &impl HirDatabase, file: FileId) -> WithError<Arc<Resolver>> {
    let source_file = db.lower(file)?;

    let reporter = Reporter::new(file);

    let ctx = Ctx::new(db);

    let mut collector = ResolverDataCollector {
        db,
        ctx,
        reporter,
        items: HashSet::new(),
        exported_items: HashSet::new(),
        binding_error: false,
        function_data: HashMap::new(),
    };

    for import in &source_file.imports {
        db.resolve_import(file, import.id)?
            .into_iter()
            .for_each(|(name, ty, kind)| collector.ctx.insert_type(name, ty, kind));
    }

    // collect the top level definitions first so we can
    // use forward declarations
    for function in &source_file.functions {
        collector.add_function(function.name, function.exported);
    }

    for alias in &source_file.type_alias {
        if let Err(_) = collector.resolve_alias(alias) {
            continue;
        };
    }

    for function in &source_file.functions {
        if let Err(_) = collector.resolve_function(function) {
            continue;
        }
    }

    let (resolver, reporter) = collector.finish();

    if reporter.has_errors() {
        Err(reporter.finish())
    } else {
        Ok(Arc::new(resolver))
    }
}
