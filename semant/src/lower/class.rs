use crate::{
    hir::{self, Class, Field, Function, FunctionAstMap},
    impl_collector, util, HirDatabase, TextRange,
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

impl_collector!(ClassDataCollector);

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

    pub(crate) fn lower_field(&mut self, field: ast::NamedFieldDef) {
        let property = util::Span::from_ast(
            self.db.intern_name(field.name().unwrap().into()),
            &field.name().unwrap(),
        );

        let ty = self.lower_type(field.ascribed_type().unwrap());
        self.fields
            .push(util::Span::from_ast(Field { property, ty }, &field));
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
