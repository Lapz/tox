use crate::db::HirDatabase;
use crate::{hir, impl_collector, util};
use std::sync::Arc;
use syntax::{AstNode, NameOwner, TypeParamsOwner, TypesOwner, VisibilityOwner};

#[derive(Debug)]
pub(crate) struct TypeAliasDataCollector<DB> {
    db: DB,
    type_param_count: u64,
    type_params: Vec<util::Span<hir::TypeParamId>>,
    ast_map: hir::FunctionAstMap,
}

impl_collector!(TypeAliasDataCollector);

impl<'a, DB> TypeAliasDataCollector<&'a DB>
where
    DB: HirDatabase,
{
    pub fn finish(
        self,
        name: util::Span<hir::NameId>,
        exported: bool,
        ty: util::Span<hir::TypeId>,
        span: crate::TextRange,
    ) -> hir::TypeAlias {
        let type_params = self.type_params;
        hir::TypeAlias {
            name,
            type_params,
            span,
            ast_map: self.ast_map,
            exported,
            ty,
        }
    }
}
pub(crate) fn lower_type_alias_query(
    db: &impl HirDatabase,
    alias_id: hir::TypeAliasId,
) -> Arc<hir::TypeAlias> {
    let alias = db.lookup_intern_type_alias(alias_id);
    let name = util::Span::from_ast(
        db.intern_name(alias.name().unwrap().into()),
        &alias.name().unwrap(),
    );

    let exported = alias.visibility().is_some();
    let mut collector = TypeAliasDataCollector {
        db,
        type_params: Vec::new(),
        type_param_count: 0,
        ast_map: hir::FunctionAstMap::default(),
    };

    if let Some(type_params_list) = alias.type_param_list() {
        for type_param in type_params_list.type_params() {
            collector.lower_type_param(type_param);
        }
    }

    let ty = collector.lower_type(alias.type_ref().unwrap());
    let span = alias.syntax().text_range();
    Arc::new(collector.finish(name, exported, ty, span))
}
