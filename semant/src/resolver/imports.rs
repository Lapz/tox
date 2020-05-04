use crate::{hir::ImportId, HirDatabase};
use errors::{FileId, Reporter, WithError};

pub fn resolve_imports_query(
    db: &impl HirDatabase,
    file: FileId,
    import_id: ImportId,
) -> WithError<()> {
    let mut reporter = Reporter::new(file);
    let import = db.lower_import(file, import_id);
    let module_graphs = db.module_graph(file)?;
    let nodes = module_graphs.get_node(&file);
    let mut import_err = String::new();
    let span = (import.span.start().to_usize(), import.span.end().to_usize());

    if nodes.is_none() {
        reporter.error(
            "Trying to reference an import for the a file that is not declared",
            "",
            span,
        );

        return Err(reporter.finish());
    }

    let mut nodes = nodes.unwrap();

    for segment in &import.segments {
        if let Some(module) = nodes.get(&segment.name.item) {
            let next_node = module_graphs.try_get_node(&module);

            let mut next_node = next_node.unwrap();

            std::mem::swap(&mut next_node, &mut nodes);

            import_err.push_str(&format!("{}::", db.lookup_intern_name(segment.name.item)));

            if segment.nested_imports.len() > 0 {
                let exports = db.resolve_exports(*module)?;

                for name in &segment.nested_imports {
                    eprintln!("{:#?}  {:?}", exports.exported_items, name);
                    if !exports.has_export(&name.item) {
                        reporter.error(
                            "Unresolved import",
                            format!(
                                "Couldn't find the import `{}`",
                                format!("{}{}", import_err, db.lookup_intern_name(name.item)),
                            ),
                            span,
                        );
                    }
                }
            }
        } else {
            import_err.push_str(db.lookup_intern_name(segment.name.item).as_str());

            reporter.error(
                "Unresolved module when finding import",
                format!("Couldn't find the import `{}`", import_err),
                span,
            )
        }
    }

    if reporter.has_errors() {
        Err(reporter.finish())
    } else {
        Ok(())
    }
}

#[cfg(test)]
#[macro_use]
mod test {

    macro_rules! create_test {
        ($filename:ident ,is_err) => {
            __create_test!($filename, is_err);
        };
        ($filename:ident ) => {
            __create_test!($filename, is_ok);
        };
    }

    macro_rules! __create_test {
        ($filename:ident,$kind:ident) => {
            #[test]
            fn $filename() -> std::io::Result<()> {
                use crate::HirDatabase;
                use errors::db::FileDatabase;

                let dir = tempfile::tempdir()?;

                let structure = crate::resolver::tests::load_file(&format!(
                    "{}/src/resolver/tests/{}.ron",
                    env!("CARGO_MANIFEST_DIR"),
                    stringify!($filename)
                ));

                let mut file_names = Vec::new();

                crate::resolver::tests::create_structure(&dir.path(), &structure, &mut file_names)?;

                let db = crate::resolver::tests::MockDatabaseImpl::default();

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

    create_test!(import_single);
    create_test!(import_many);

    create_test!(import_no_exported, is_err);

    create_test!(import_dir_and_file, is_err);

    create_test!(import_deep);

    create_test!(import_deep_dirs);
}
