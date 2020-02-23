mod alias;
mod function;
use crate::db::HirDatabase;
use crate::hir;
pub(crate) use alias::lower_type_alias_query;
pub(crate) use function::lower_function_query;
use std::sync::Arc;

use syntax::{ast, FnDefOwner, TypeAliasDefOwner};

pub(crate) fn lower_ast_query(db: &impl HirDatabase, source: ast::SourceFile) -> Arc<hir::Program> {
    let mut program = hir::Program::default();

    for type_alias in source.type_alias() {
        program.type_alias.push(db.lower_type_alias(type_alias));
    }

    for function in source.functions() {
        program.functions.push(db.lower_function(function));
    }

    Arc::new(program)
}
