use crate::db::HirDatabase;
use crate::hir;

use std::sync::Arc;

use syntax::{
    ast, AstNode, AstPtr, NameOwner, TypeParamsOwner, TypesOwner,
};
#[derive(Debug)]
pub(crate) struct TypeAliasDataCollector<DB> {
    db: DB,
    type_param_count: u64,
    type_params: Vec<hir::TypeParamId>,
    ast_map: hir::FunctionAstMap,
}

impl<'a, DB> TypeAliasDataCollector<&'a DB>
where
    DB: HirDatabase,
{
    pub fn finish(self, name: hir::Name, span: hir::Span, ty: hir::TypeId) -> hir::TypeAlias {
        let type_params = self.type_params;
        hir::TypeAlias {
            name,
            type_params,
            span,
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

        self.ast_map
            .insert_type_param(id, type_param, AstPtr::new(ast_node));
    }

    pub(crate) fn lower_type(&mut self, ty: ast::TypeRef) -> hir::TypeId {
        match ty {
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
                let name: hir::Name = ident_ty.into();

                self.db
                    .intern_type(hir::Type::Ident(self.db.intern_name(name)))
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
        }
    }
}
pub(crate) fn lower_type_alias_query(
    db: &impl HirDatabase,
    alias: ast::TypeAliasDef,
) -> Arc<hir::TypeAlias> {
    let name = alias.name().unwrap().into();
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
