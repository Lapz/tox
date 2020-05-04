use crate::{
    hir,
    resolver::{ModuleGraph, Resolver},
};
use errors::{FileId, WithError};
use parser::ParseDatabase;

use std::sync::Arc;
use syntax::ast;
#[salsa::query_group(InternDatabaseStorage)]
pub trait InternDatabase {
    #[salsa::interned]
    fn intern_module(&self, mod_def: ast::ModDef) -> hir::ModuleId;

    #[salsa::interned]
    fn intern_import(&self, import_def: ast::ImportDef) -> hir::ImportId;

    #[salsa::interned]
    fn intern_function(&self, fn_def: ast::FnDef) -> hir::FunctionId;

    #[salsa::interned]
    fn intern_name(&self, name: hir::Name) -> hir::NameId;

    #[salsa::interned]
    fn intern_class(&self, class_def: ast::ClassDef) -> hir::ClassId;

    #[salsa::interned]
    fn intern_enum(&self, enum_def: ast::EnumDef) -> hir::EnumId;

    #[salsa::interned]
    fn intern_type_alias(&self, type_alias_def: ast::TypeAliasDef) -> hir::TypeAliasId;

    #[salsa::interned]
    fn intern_type(&self, ty: hir::Type) -> hir::TypeId;

    #[salsa::interned]
    fn intern_literal(&self, lit: hir::Literal) -> hir::LiteralId;
}

#[salsa::query_group(HirDatabaseStorage)]
pub trait HirDatabase: std::fmt::Debug + InternDatabase + ParseDatabase {
    #[salsa::invoke(crate::lower::lower_function_query)]
    fn lower_function(&self, file: FileId, function: hir::FunctionId) -> Arc<hir::Function>;
    #[salsa::invoke(crate::lower::lower_module_query)]
    fn lower_module(&self, file: FileId, module: hir::ModuleId) -> Arc<hir::Module>;
    #[salsa::invoke(crate::lower::lower_import_query)]
    fn lower_import(&self, file: FileId, import: hir::ImportId) -> Arc<hir::Import>;
    #[salsa::invoke(crate::lower::lower_type_alias_query)]
    fn lower_type_alias(&self, alias: hir::TypeAliasId) -> Arc<hir::TypeAlias>;
    #[salsa::invoke(crate::lower::lower_query)]
    fn lower(&self, file: FileId) -> WithError<Arc<hir::SourceFile>>;
    #[salsa::invoke(crate::resolver::resolve_exports_query)]
    fn resolve_exports(&self, file: FileId) -> WithError<Arc<Resolver>>;
    #[salsa::invoke(crate::resolver::resolve_modules_query)]
    fn resolve_modules(&self, file: FileId, module: hir::ModuleId) -> WithError<FileId>;
    #[salsa::invoke(crate::resolver::resolve_source_file_query)]
    fn resolve_source_file(&self, file: FileId) -> WithError<Arc<Resolver>>;
    #[salsa::invoke(crate::resolver::resolve_imports_query)]
    fn resolve_import(&self, file: FileId, import: hir::ImportId) -> WithError<()>;

    #[salsa::invoke(crate::resolver::module_graph_query)]
    fn module_graph(&self, file: FileId) -> WithError<ModuleGraph>;

    #[salsa::invoke(crate::infer::infer_query)]
    fn infer(&self, file: FileId) -> WithError<()>;
}
