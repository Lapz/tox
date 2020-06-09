mod alias;
mod class;
mod data;
mod enums;
mod function;
mod imports;
mod module;
mod module_graph;
mod source_file;
#[macro_use]
#[cfg(test)]
mod tests;

pub(crate) use data::Resolver;
pub(crate) use data::TypeKind;
pub(crate) use imports::resolve_imports_query;
pub(crate) use module::resolve_modules_query;
pub(crate) use module_graph::module_graph_query;
pub(crate) use module_graph::ModuleGraph;
pub(crate) use source_file::resolve_exports_query;
pub(crate) use source_file::resolve_source_file_query;

#[macro_export]
macro_rules! create_test {
    ($filename:ident ,is_err) => {
        $crate::__create_test!($filename, is_err);
    };
    ($filename:ident ) => {
        $crate::__create_test!($filename, is_ok);
    };
}
#[macro_export]
macro_rules! __create_test {
    ($filename:ident,$kind:ident) => {
        #[test]
        fn $filename() -> std::io::Result<()> {
            use errors::db::FileDatabase;
            use $crate::HirDatabase;

            let dir = tempfile::tempdir()?;

            let structure = $crate::resolver::tests::load_file(&format!(
                "{}/src/resolver/tests/{}.ron",
                env!("CARGO_MANIFEST_DIR"),
                stringify!($filename)
            ));

            let mut file_names = Vec::new();

            $crate::resolver::tests::create_structure(&dir.path(), &structure, &mut file_names)?;

            let db = $crate::resolver::tests::MockDatabaseImpl::default();

            let handle = db.intern_file(file_names.remove(0));

            match db.resolve_source_file(handle) {
                Ok(_) => {}
                Err(errors) => println!("{:?}", errors),
            }

            assert!(db.resolve_source_file(handle).$kind());
            Ok(())
        }
    };
}
