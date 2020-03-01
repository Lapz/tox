mod alias;
mod function;
use crate::db::HirDatabase;
use crate::hir;
pub(crate) use alias::lower_type_alias_query;
use errors::{FileId, WithError};
pub(crate) use function::lower_function_query;
use std::sync::Arc;

use syntax::{FnDefOwner, TypeAliasDefOwner};

pub(crate) fn lower_query(db: &impl HirDatabase, file: FileId) -> WithError<Arc<hir::Program>> {
    let source = db.parse(file)?;
    let mut program = hir::Program::default();

    for type_alias in source.type_alias() {
        let id = db.intern_type_alias(type_alias);

        program.type_alias.push(db.lower_type_alias(id));
    }

    for function in source.functions() {
        let id = db.intern_function(function);
        program.functions.push(db.lower_function(id));
    }

    Ok(Arc::new(program))
}
