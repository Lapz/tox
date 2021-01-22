use super::TypeKind;
use crate::{
    hir::{ImportId, NameId},
    infer::Type,
    HirDatabase,
};
use errors::{FileId, Reporter, WithError};

pub fn resolve_imports_query(
    db: &impl HirDatabase,
    file: FileId,
    import_id: ImportId,
) -> WithError<Vec<(NameId, Type, TypeKind)>> {
    let mut reporter = Reporter::new(file);
    let import = db.lower_import(file, import_id);
    let WithError(module_graphs, mut errors) = db.module_graph(file);
    let nodes = module_graphs.get_node(&file);
    let mut import_err = String::new();
    let span = (import.span.start().to_usize(), import.span.end().to_usize());

    let mut imported_types = Vec::new();

    if nodes.is_none() {
        reporter.error(
            "Trying to reference an import for the a file that is not declared",
            "",
            span,
        );

        return WithError(imported_types, reporter.finish());
    }

    let mut nodes = nodes.unwrap();

    for segment in &import.segments {
        if let Some(module) = nodes.get(&segment.name.item) {
            let next_node = module_graphs.try_get_node(&module);

            let mut next_node = next_node.unwrap();

            std::mem::swap(&mut next_node, &mut nodes);

            import_err.push_str(&format!("{}::", db.lookup_intern_name(segment.name.item)));

            if segment.nested_imports.len() > 0 {
                let WithError(exports, error) = db.resolve_source_file(*module);

                errors.extend(error);

                for name in &segment.nested_imports {
                    if !exports.has_export(&name.item) {
                        reporter.error(
                            "Unresolved import",
                            format!(
                                "Couldn't find the import `{}`",
                                format!("{}{}", import_err, db.lookup_intern_name(name.item)),
                            ),
                            span,
                        );

                        continue;
                    }

                    if let Some(ty) = exports.ctx.get_type(&name.item) {
                        imported_types.push((
                            name.item,
                            ty,
                            exports
                                .ctx
                                .get_kind(&name.item)
                                .unwrap_or(TypeKind::Function),
                        ))
                    } else {
                        eprintln!(
                            "Found an import but couldn't find its type in the ctx; id {:?} name {}",
                            name,
                            db.lookup_intern_name(name.item)
                        )
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

    WithError(imported_types, reporter.finish())
}

#[cfg(test)]

mod tests {

    use crate::create_test;

    create_test!(import_single);
    create_test!(import_many);

    create_test!(import_no_exported, is_err);

    create_test!(import_dir_and_file, is_err);

    create_test!(import_deep);

    create_test!(import_deep_dirs);
}
