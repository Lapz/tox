mod alias;
mod class;
mod enums;
mod function;
mod imports;
mod module;

use crate::{db::HirDatabase, hir};
use errors::{FileId, WithError};
use std::sync::Arc;
use syntax::{
    ClassDefOwner, EnumDefOwner, ExternImportDefOwner, FnDefOwner, ModuleDefOwner,
    TypeAliasDefOwner,
};

pub(crate) use alias::lower_type_alias_query;
pub(crate) use class::lower_class_query;
pub(crate) use enums::lower_enum_query;
pub(crate) use function::lower_function_query;
pub(crate) use imports::lower_import_query;
pub(crate) use module::lower_module_query;

#[macro_export]
macro_rules! impl_collector {
    ($name:ident) => {
        impl<'a, DB> $name<&'a DB>
        where
            DB: $crate::HirDatabase,
        {
            pub(crate) fn lower_type_param(&mut self, type_param: syntax::ast::TypeParam) {
                let name = self.db.intern_name(type_param.name().unwrap().into());

                self.add_type_param(
                    &type_param,
                    $crate::hir::TypeParam {
                        name: util::Span::from_ast(name, &type_param),
                    },
                );
            }

            pub fn add_type_param(
                &mut self,
                ast_node: &syntax::ast::TypeParam,
                type_param: $crate::hir::TypeParam,
            ) {
                let current = self.type_param_count;

                self.type_param_count += 1;

                let id = $crate::hir::TypeParamId(current);

                self.ast_map.insert_type_param(id, type_param);
                self.type_params
                    .push($crate::util::Span::from_ast(id, ast_node));
            }

            pub(crate) fn lower_type(
                &mut self,
                ty: syntax::ast::TypeRef,
            ) -> $crate::util::Span<$crate::hir::TypeId> {
                let range = ty.syntax().text_range();
                let id = match ty {
                    syntax::ast::TypeRef::ParenType(paren_ty) => {
                        let mut types = Vec::new();

                        for c in paren_ty.types() {
                            types.push(self.lower_type(c))
                        }

                        self.db.intern_type($crate::hir::Type::ParenType(types))
                    }
                    syntax::ast::TypeRef::ArrayType(array_ty) => {
                        let ty = self.lower_type(array_ty.type_ref().unwrap());

                        self.db
                            .intern_type($crate::hir::Type::ArrayType { ty, size: None })
                    }
                    syntax::ast::TypeRef::IdentType(ident_ty) => {
                        if let Some(type_args) = ident_ty.type_args() {
                            let type_args = type_args
                                .types()
                                .map(|ty| self.lower_type(ty))
                                .collect::<Vec<_>>();

                            let name: $crate::hir::Name = ident_ty.into();

                            self.db.intern_type($crate::hir::Type::Poly {
                                name: self.db.intern_name(name),
                                type_args,
                            })
                        } else {
                            let name: hir::Name = ident_ty.into();

                            let name_id = self.db.intern_name(name);

                            self.db.intern_type($crate::hir::Type::Ident(name_id))
                        }
                    }

                    syntax::ast::TypeRef::FnType(fn_ty) => {
                        let mut params = Vec::new();

                        for param in fn_ty.types() {
                            params.push(self.lower_type(param))
                        }

                        let ret = fn_ty
                            .ret_type()
                            .and_then(|ret| ret.type_ref().map(|ty| self.lower_type(ty)));

                        self.db
                            .intern_type($crate::hir::Type::FnType { params, ret })
                    }
                };

                $crate::util::Span::from_range(id, range)
            }
        }
    };
}

pub(crate) fn lower_query(db: &impl HirDatabase, file: FileId) -> WithError<Arc<hir::SourceFile>> {
    let WithError(source, errors) = db.parse(file);
    let mut program = hir::SourceFile::default();

    for import in source.imports() {
        let id = db.intern_import(import);
        program.imports.push(db.lower_import(file, id));
    }

    for module in source.modules() {
        let id = db.intern_module(module);
        program.modules.push(db.lower_module(file, id));
    }

    for type_alias in source.type_alias() {
        let id = db.intern_type_alias(type_alias);

        program.type_alias.push(db.lower_type_alias(id));
    }

    for enum_def in source.enums() {
        let id = db.intern_enum(enum_def);

        program.enums.push(db.lower_enum(id))
    }

    for class in source.classes() {
        let id = db.intern_class(class);

        program.classes.push(db.lower_class(id));
    }

    for function in source.functions() {
        let id = db.intern_function(function);
        program.functions.push(db.lower_function(id));
    }

    WithError(Arc::new(program), errors)
}
