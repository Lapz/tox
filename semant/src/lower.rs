mod alias;
mod function;
use crate::db::HirDatabase;
pub(crate) use alias::lower_type_alias_query;
pub(crate) use function::lower_function_query;

use syntax::{ast, FnDefOwner, TypeAliasDefOwner};
pub fn lower_ast(source: ast::SourceFile, db: &impl HirDatabase) {
    for type_alias in source.type_alias() {
        db.lower_type_alias(type_alias);
    }

    for function in source.functions() {
        db.lower_function(function);
    }
}
