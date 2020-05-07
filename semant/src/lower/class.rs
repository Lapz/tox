use crate::{
    hir::{self, Class, Field, Function, FunctionAstMap},
    util, HirDatabase, TextRange,
};

use std::sync::Arc;
use syntax::{
    ast, AstNode, FnDefOwner, NameOwner, NamedFieldsOwner, TypeAscriptionOwner, TypeParamsOwner,
    TypesOwner, VisibilityOwner,
};
#[derive(Debug)]
pub(crate) struct ClassDataCollector<DB> {
    db: DB,
    type_param_count: u64,
    type_params: Vec<util::Span<hir::TypeParamId>>,
    methods: Vec<Arc<hir::Function>>,
    fields: Vec<util::Span<Field>>,
    ast_map: FunctionAstMap,
}

impl<'a, DB> ClassDataCollector<&'a DB>
where
    DB: HirDatabase,
{
    pub fn finish(self, name: util::Span<hir::NameId>, exported: bool, span: TextRange) -> Class {
        let methods = self.methods;
        let ast_map = self.ast_map;
        let type_params = self.type_params;
        let fields = self.fields;
        Class {
            exported,
            name,
            ast_map,
            type_params,
            fields,
            methods,
            span,
        }
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

    pub(crate) fn lower_type_param(&mut self, type_param: ast::TypeParam) {
        let name = self.db.intern_name(type_param.name().unwrap().into());

        self.add_type_param(
            &type_param,
            hir::TypeParam {
                name: util::Span::from_ast(name, &type_param),
            },
        );
    }

    pub(crate) fn lower_field(&mut self, field: ast::NamedFieldDef) -> Field {
        let property = util::Span::from_ast(
            self.db.intern_name(field.name().unwrap().into()),
            &field.name().unwrap(),
        );

        let ty = self.lower_type(field.ascribed_type().unwrap());
        Field { property, ty }
    }

    pub fn add_type_param(&mut self, ast_node: &ast::TypeParam, type_param: hir::TypeParam) {
        let current = self.type_param_count;

        self.type_param_count += 1;

        let id = hir::TypeParamId(current);

        self.ast_map.insert_type_param(id, type_param);
        self.type_params.push(util::Span::from_ast(id, ast_node));
    }

    pub fn lower_method(&mut self, lowered_fn: Arc<Function>) {
        self.methods.push(lowered_fn);
    }
}

pub(crate) fn lower_class_query(db: &impl HirDatabase, class_id: hir::ClassId) -> Arc<hir::Class> {
    let class = db.lookup_intern_class(class_id);

    let name = util::Span::from_ast(
        db.intern_name(class.name().unwrap().into()),
        &class.name().unwrap(),
    );

    let mut collector = ClassDataCollector {
        db,
        type_param_count: 0,
        type_params: Vec::new(),
        methods: Vec::new(),
        fields: Vec::new(),
        ast_map: FunctionAstMap::default(),
    };

    let exported = class.visibility().is_some();

    if let Some(type_params_list) = class.type_param_list() {
        for type_param in type_params_list.type_params() {
            collector.lower_type_param(type_param);
        }
    }

    for field in class.fields() {
        collector.lower_field(field);
    }

    for method in class.functions() {
        let id = db.intern_function(method);
        collector.lower_method(db.lower_function(id));
    }

    let span = class.syntax().text_range();

    Arc::new(collector.finish(name, exported, span))
}
