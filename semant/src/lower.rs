mod alias;
mod class;
mod function;
mod imports;
mod module;
use crate::db::HirDatabase;
use crate::hir;
use errors::{FileId, WithError};
use std::sync::Arc;
use syntax::{ClassDefOwner, ExternImportDefOwner, FnDefOwner, ModuleDefOwner, TypeAliasDefOwner};

pub(crate) use alias::lower_type_alias_query;
pub(crate) use class::lower_class_query;
pub(crate) use function::lower_function_query;
pub(crate) use imports::lower_import_query;
pub(crate) use module::lower_module_query;

pub(crate) fn lower_query(db: &impl HirDatabase, file: FileId) -> WithError<Arc<hir::SourceFile>> {
    let source = db.parse(file)?;
    let mut program = hir::SourceFile::default();

    for import in source.imports() {
        let id = db.intern_import(import);
        program.imports.push(db.lower_import(file, id));
    }

    for module in source.modules() {
        let id = db.intern_module(module);
        program.modules.push(db.lower_module(file, id));
    }

    for type_alias in source.type_alias() {
        let id = db.intern_type_alias(type_alias);

        program.type_alias.push(db.lower_type_alias(id));
    }

    for class in source.classes() {
        let id = db.intern_class(class);

        program.classes.push(db.lower_class(id));
    }

    for function in source.functions() {
        let id = db.intern_function(function);
        program.functions.push(db.lower_function(id));
    }

    Ok(Arc::new(program))
}
