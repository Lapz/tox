use crate::hir;
use crate::hir::{Function, FunctionId};
use salsa;
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;
use syntax::ast;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FileId(pub u32);
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SourceRootId(pub u32);

#[derive(Default, Clone, Debug, PartialEq, Eq)]
pub struct SourceRoot {
    files: HashMap<PathBuf, FileId>,
}

impl SourceRoot {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert_file(&mut self, path: PathBuf, file_id: FileId) {
        self.files.insert(path, file_id);
    }
}

// #[salsa::query_group(SourceDatabaseStorage)]
// pub trait SourceDatabase: std::fmt::Debug {
//     #[salsa::input]
//     fn file_text(&self, file_id: FileId) -> Arc<String>;

//     #[salsa::invoke(parse_query)]
//     fn parse(&self, file_id: FileId) -> ast::SourceFile;
//     #[salsa::input]
//     fn source_root(&self, id: SourceRootId) -> Arc<SourceRoot>;
// }

#[salsa::query_group(InternDatabaseStorage)]
pub trait InternDatabase {
    // #[salsa::interned]
    // fn intern_name(&self, ast_nane: ast::Name) -> hir::Name;

    #[salsa::interned]
    fn intern_function(&self, fn_def: ast::FnDef) -> hir::FunctionId;

    #[salsa::interned]
    fn intern_class(&self, class_def: ast::ClassDef) -> hir::ClassId;

    #[salsa::interned]
    fn intern_enum(&self, enum_def: ast::EnumDef) -> hir::EnumId;

    #[salsa::interned]
    fn intern_type_alias(&self, type_alias_def: ast::TypeAliasDef) -> hir::TypeAliasId;
}

#[salsa::query_group(HirDatabaseStorage)]
pub trait HirDatabase: std::fmt::Debug {
    #[salsa::input]
    fn function_data(&self, fn_id: FunctionId) -> Arc<Function>;
}
