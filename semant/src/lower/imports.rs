use crate::hir;
use crate::{util, HirDatabase};
use errors::FileId;
use std::sync::Arc;

use syntax::{AstNode, ImportSegmentOwner};

pub(crate) fn lower_import_query(
    db: &impl HirDatabase,
    file: FileId,
    import_id: hir::ImportId,
) -> Arc<hir::Import> {
    let import = db.lookup_intern_import(import_id);
    let mut segments = Vec::new();

    for segment in import.segments() {
        let name = segment.name().map(|name| name.into());

        let name = util::Span::from_ast(db.intern_name(name.unwrap()), &segment.name().unwrap());

        let nested_imports = Vec::new();

        segments.push(hir::Segment {
            name,
            nested_imports,
        });
    }

    if let Some(list) = import.import_list() {
        let index = segments.len() - 1;
        let last = &mut segments[index];
        for segment in list.segments() {
            let name = segment.name().map(|name| name.into());

            let name = db.intern_name(name.unwrap());
            last.nested_imports
                .push(util::Span::from_ast(name, &segment.name().unwrap()));
        }
    } else if import.segments().count() > 1 {
        // only when we have import foo::something
        // get the last import
        assert!(segments.len() > 1);
        let actual_import = segments.pop().unwrap();
        let index = segments.len() - 1;

        segments[index].nested_imports.push(actual_import.name);
    }

    let span = import.syntax().text_range();

    Arc::new(hir::Import {
        segments,
        id: import_id,
        file,
        span,
    })
}
