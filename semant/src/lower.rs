use crate::db::HirDatabase;
use crate::hir::{self, Ctx};
use std::collections::HashMap;
use syntax::{
    ast, child, children, AstNode, FnDefOwner, NameOwner, TypeAscriptionOwner, TypesOwner,
};

#[derive(Debug, Default)]
pub(crate) struct FunctionDataCollector<DB> {
    db: DB,
    params: Vec<hir::Param>, // expressions: HashMap<hir::ExprId, hir::Expr>,
}

impl<'a, DB> FunctionDataCollector<&'a DB>
where
    DB: HirDatabase,
{
    pub fn add_param(&mut self, param: hir::Param) {
        self.params.push(param)
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

        self.add_param(hir::Param { pat, ty })
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

    pub(crate) fn lower(mut self, params: Option<ast::ParamList>) {}
}

pub fn lower_ast(source: ast::SourceFile, db: &impl HirDatabase, reporter: &mut errors::Reporter) {
    let mut ctx = Ctx::new();

    for function in source.functions() {
        let mut collector = FunctionDataCollector {
            db,

            params: Vec::new(),
        };

        let name: Option<crate::hir::Name> = function.name().map(|name| name.into());

        if let Some(param_list) = function.param_list() {
            for param in param_list.params() {
                reporter.warn(
                    "this is a test",
                    "testing",
                    (
                        param.syntax().text_range().start(),
                        param.syntax().text_range().end(),
                    ),
                );
                collector.lower_param(param);
            }
        }

        println!("{:#?}", collector);
    }
}

pub(crate) fn lower_pat(pat: ast::Pat) {}
