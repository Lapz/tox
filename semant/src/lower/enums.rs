use crate::impl_collector;
use crate::{
    hir::{self, Enum, FunctionAstMap},
    util, HirDatabase, TextRange,
};

use std::sync::Arc;
use syntax::{ast, AstNode, NameOwner, TypeParamsOwner, TypesOwner, VisibilityOwner};

#[derive(Debug)]
pub(crate) struct EnumDataCollector<DB> {
    db: DB,
    type_param_count: u64,
    type_params: Vec<util::Span<hir::TypeParamId>>,
    variants: Vec<util::Span<hir::EnumVariant>>,
    ast_map: FunctionAstMap,
}

impl_collector!(EnumDataCollector);

impl<'a, DB> EnumDataCollector<&'a DB>
where
    DB: HirDatabase,
{
    pub fn finish(self, name: util::Span<hir::NameId>, exported: bool, span: TextRange) -> Enum {
        let variants = self.variants;
        let ast_map = self.ast_map;
        let type_params = self.type_params;

        Enum {
            exported,
            name,
            ast_map,
            type_params,
            variants,
            span,
        }
    }

    pub(crate) fn lower_variant(
        &mut self,
        variant: ast::EnumVariant,
    ) -> util::Span<hir::EnumVariant> {
        let name = self.db.intern_name(variant.name().unwrap().into());

        let ty = if let Some(ty_ref) = variant.type_ref() {
            Some(self.lower_type(ty_ref))
        } else {
            None
        };

        util::Span::from_ast(hir::EnumVariant { name, ty }, &variant)
    }
}

pub(crate) fn lower_enum_query(db: &impl HirDatabase, enum_id: hir::EnumId) -> Arc<Enum> {
    let enum_ = db.lookup_intern_enum(enum_id);

    let name = util::Span::from_ast(
        db.intern_name(enum_.name().unwrap().into()),
        &enum_.name().unwrap(),
    );

    let mut collector = EnumDataCollector {
        db,
        type_param_count: 0,
        type_params: Vec::new(),
        variants: Vec::new(),
        ast_map: FunctionAstMap::default(),
    };

    let exported = enum_.visibility().is_some();

    if let Some(type_params_list) = enum_.type_param_list() {
        for type_param in type_params_list.type_params() {
            collector.lower_type_param(type_param);
        }
    }

    if let Some(variant_list) = enum_.variant_list() {
        for variant in variant_list.variants() {
            let v = collector.lower_variant(variant);
            collector.variants.push(v);
        }
    }

    let span = enum_.syntax().text_range();

    Arc::new(collector.finish(name, exported, span))
}
