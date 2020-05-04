use crate::db::HirDatabase;
use crate::{hir, util};

use std::sync::Arc;

use syntax::{ast, AstNode, NameOwner, TypeParamsOwner, TypesOwner};
#[derive(Debug)]
pub(crate) struct TypeAliasDataCollector<DB> {
    db: DB,
    type_param_count: u64,
    type_params: Vec<util::Span<hir::TypeParamId>>,
    ast_map: hir::FunctionAstMap,
}

impl<'a, DB> TypeAliasDataCollector<&'a DB>
where
    DB: HirDatabase,
{
    pub fn finish(
        self,
        name: util::Span<hir::NameId>,
        span: crate::TextRange,
        ty: util::Span<hir::TypeId>,
    ) -> hir::TypeAlias {
        let type_params = self.type_params;
        hir::TypeAlias {
            name,
            type_params,
            span,
            ast_map: self.ast_map,
            ty,
        }
    }
    pub(crate) fn lower_type_param(&mut self, type_param: ast::TypeParam) {
        let name = self.db.intern_name(type_param.name().unwrap().into());

        self.add_type_param(&type_param, hir::TypeParam { name });
    }

    pub fn add_type_param(&mut self, ast_node: &ast::TypeParam, type_param: hir::TypeParam) {
        let current = self.type_param_count;

        self.type_param_count += 1;

        let id = hir::TypeParamId(current);

        self.ast_map.insert_type_param(id, type_param);
        self.type_params.push(util::Span::from_ast(id, ast_node));
    }

    pub(crate) fn lower_type(&mut self, ty: ast::TypeRef) -> util::Span<hir::TypeId> {
        let range = ty.syntax().text_range();
        let id = match ty {
            ast::TypeRef::ParenType(paren_ty) => {
                let mut types = Vec::new();

                for c in paren_ty.types() {
                    types.push(self.lower_type(c))
                }

                self.db.intern_type(hir::Type::ParenType(types))
            }
            ast::TypeRef::ArrayType(array_ty) => {
                let ty = self.lower_type(array_ty.type_ref().unwrap());

                self.db.intern_type(hir::Type::ArrayType { ty, size: None })
            }
            ast::TypeRef::IdentType(ident_ty) => {
                if let Some(type_args) = ident_ty.type_args() {
                    let type_args = type_args
                        .types()
                        .map(|ty| self.lower_type(ty))
                        .collect::<Vec<_>>();

                    let name: hir::Name = ident_ty.into();

                    self.db.intern_type(hir::Type::Poly {
                        name: self.db.intern_name(name),
                        type_args,
                    })
                } else {
                    let name: hir::Name = ident_ty.into();

                    let name_id = self.db.intern_name(name);

                    self.db.intern_type(hir::Type::Ident(name_id))
                }
            }

            ast::TypeRef::FnType(fn_ty) => {
                let mut params = Vec::new();

                for param in fn_ty.types() {
                    params.push(self.lower_type(param))
                }

                let ret = fn_ty
                    .ret_type()
                    .and_then(|ret| ret.type_ref().map(|ty| self.lower_type(ty)));

                self.db.intern_type(hir::Type::FnType { params, ret })
            }
        };

        util::Span::from_range(id, range)
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
    Arc::new(collector.finish(name, span, ty))
}
