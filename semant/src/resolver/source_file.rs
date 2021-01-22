use crate::{
    db::HirDatabase,
    infer::{self, StackedMap},
};
use crate::{hir, Ctx};

use super::data::ItemKind;
use crate::resolver::{data::ResolverDataCollector, Resolver};
use errors::{FileId, Reporter, WithError};
use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

pub fn resolve_exports_query(db: &impl HirDatabase, file: FileId) -> WithError<Arc<Resolver>> {
    let WithError(program, mut errors) = db.lower(file);
    let reporter = Reporter::new(file);
    let mut ctx = Ctx::new(db);
    ctx.begin_scope();

    let mut interned_types = StackedMap::new();
    interned_types.begin_scope();

    let mut collector = ResolverDataCollector {
        db,
        ctx,
        reporter,
        items: HashSet::new(),
        exported_items: HashSet::new(),
        binding_error: false,
        function_data: HashMap::new(),
        interned_types,
    };

    for function in &program.functions {
        collector.add_item(function.name, ItemKind::Function, function.exported)
    }

    let (resolver, reporter) = collector.finish();

    errors.extend(reporter.finish());

    WithError(Arc::new(resolver), errors)
}

pub fn resolve_named_type_query(
    db: &impl HirDatabase,
    file: FileId,
    name: hir::NameId,
) -> Arc<infer::Type> {
    let WithError(resolver, _) = db.resolve_source_file(file);
    if let Some(ty) = resolver.get_type(&name) {
        Arc::new(ty)
    } else {
        Arc::new(infer::Type::Unknown)
    }
}

pub fn resolve_hir_type_query(
    db: &impl HirDatabase,
    file: FileId,
    ty_id: hir::TypeId,
) -> Arc<infer::Type> {
    let WithError(resolver, _) = db.resolve_source_file(file);
    if let Some(ty) = resolver.lookup_intern_type(&ty_id) {
        Arc::new(ty)
    } else {
        Arc::new(infer::Type::Unknown)
    }
}

pub fn resolve_source_file_query(db: &impl HirDatabase, file: FileId) -> WithError<Arc<Resolver>> {
    let WithError(source_file, mut errors) = db.lower(file);

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
        interned_types: StackedMap::new(),
    };

    for import in &source_file.imports {
        let WithError(import, error) = db.resolve_import(file, import.id);

        errors.extend(error);
        import
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

    let (resolver, reporter) = collector.finish();

    errors.extend(reporter.finish());

    WithError(Arc::new(resolver), errors)
}
