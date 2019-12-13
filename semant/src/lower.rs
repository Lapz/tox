use crate::db::HirDatabase;
use crate::hir::{self};
use std::collections::HashMap;
use syntax::{
    ast, child, children, AstNode, AstPtr, FnDefOwner, NameOwner, TypeAscriptionOwner,
    TypeParamsOwner, TypesOwner,
};

#[derive(Debug)]
pub(crate) struct FunctionDataCollector<DB> {
    db: DB,
    type_param_count: u64,
    param_id_count: u64,
    ast_map: hir::FunctionAstMap,
    params: Vec<hir::ParamId>, // expressions: HashMap<hir::ExprId, hir::Expr>,
    type_params: Vec<hir::TypeParamId>,
}

impl<'a, DB> FunctionDataCollector<&'a DB>
where
    DB: HirDatabase,
{
    pub fn finish(self) -> hir::function::Function {
        let params = self.params;
        let type_params = self.type_params;

        hir::function::Function {
            params,
            type_params,
            body: None,
            span: unimplemented!(),
        }
    }
    pub fn add_param(&mut self, ast_node: &ast::Param, param: hir::Param) {
        let current = self.param_id_count;

        self.param_id_count += 1;

        let id = hir::ParamId(current);

        self.ast_map.insert_param(id, param, AstPtr::new(ast_node));

        self.params.push(id);
    }

    pub fn add_type_param(&mut self, ast_node: &ast::TypeParam, type_param: hir::TypeParam) {
        let current = self.type_param_count;

        self.type_param_count += 1;

        let id = hir::TypeParamId(current);

        self.ast_map
            .insert_type_param(id, type_param, AstPtr::new(ast_node));
    }

    pub(crate) fn lower_pattern(&mut self, pat: ast::Pat) -> hir::PatId {
        let pattern = match &pat {
            ast::Pat::BindPat(binding) => {
                let name: crate::hir::Name = binding
                    .name()
                    .map(|n| n.into())
                    .unwrap_or(crate::hir::Name::missing());
                crate::hir::Pattern::Bind { name }
            }
            ast::Pat::PlaceholderPat(_) => crate::hir::Pattern::Placeholder,
            ast::Pat::TuplePat(variants) => crate::hir::Pattern::Tuple(
                variants
                    .args()
                    .map(|pat| self.lower_pattern(pat))
                    .collect::<Vec<_>>(),
            ),
            ast::Pat::LiteralPat(literal) => unimplemented!(),
        };

        self.db.intern_pattern(pattern)
    }

    pub(crate) fn lower_param(&mut self, param: ast::Param) {
        let pat = self.lower_pattern(param.pat().unwrap());

        let ty = self.lower_type(param.ascribed_type().unwrap());

        self.add_param(&param, hir::Param { pat, ty });
    }

    pub(crate) fn lower_type_param(&mut self, type_param: ast::TypeParam) {
        let name = self.db.intern_name(type_param.name().unwrap().into());

        self.add_type_param(&type_param, hir::TypeParam { name });
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

pub fn lower_ast(source: ast::SourceFile, db: &impl HirDatabase, reporter: &mut errors::Reporter) {
    for function in source.functions() {
        let mut collector = FunctionDataCollector {
            db,
            param_id_count: 0,
            type_param_count: 0,
            params: Vec::new(),
            type_params: Vec::new(),
            ast_map: hir::FunctionAstMap::default(),
        };

        let name: Option<crate::hir::Name> = function.name().map(|name| name.into());

        if let Some(type_params_list) = function.type_param_list() {
            for type_param in type_params_list.type_params() {
                collector.lower_type_param(type_param);
            }
        }

        if let Some(param_list) = function.param_list() {
            for param in param_list.params() {
                collector.lower_param(param);
            }
        }

        if let Some(body) = function.body() {
            let span = body.syntax().text_range();

            reporter.warn("this is a body", "I am a body", (span.start(), span.end()));

            for statement in body.statements() {
                let span = body.syntax().text_range();

                reporter.warn("this is a stmt", "I am a stmt", (span.start(), span.end()));
            }
        }

        let span = function.syntax().text_range();

        reporter.warn(
            "this is a function",
            "I am a function",
            (span.start(), span.end()),
        );

        println!("{:#?}", collector);
    }
}
