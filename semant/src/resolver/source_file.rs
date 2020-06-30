use crate::db::HirDatabase;
use crate::Ctx;

use super::data::ItemKind;
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
        collector.add_item(function.name, ItemKind::Function, function.exported)
    }

    let resolver = collector.finish();

    if resolver.reporter.has_errors() {
        Err(resolver.reporter.finish())
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
    for class in &source_file.classes {
        collector.add_item(class.name, ItemKind::Class, class.exported)
    }

    // collect the top level definitions first so we can
    // use forward declarations
    for function in &source_file.functions {
        collector.add_item(function.name, ItemKind::Function, function.exported);
    }

    // collect the top level definitions first so we can
    // use forward declarations
    for enum_def in &source_file.enums {
        collector.add_item(enum_def.name, ItemKind::Enum, enum_def.exported);
    }

    for alias in &source_file.type_alias {
        if let Err(_) = collector.resolve_alias(alias) {
            continue;
        };
    }

    for enum_def in &source_file.enums {
        if let Err(_) = collector.resolve_enum(enum_def) {
            continue;
        }
    }

    for class in &source_file.classes {
        if let Err(_) = collector.resolve_class(class) {
            continue;
        }
    }

    for function in &source_file.functions {
        if let Err(_) = collector.resolve_function(function) {
            continue;
        }
    }

    let resolver = collector.finish();

    if resolver.reporter.has_errors() {
        Err(resolver.reporter.finish())
    } else {
        Ok(Arc::new(resolver))
    }
}
