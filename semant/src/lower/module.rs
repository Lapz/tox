use crate::hir;
use crate::util::Span;
use crate::HirDatabase;
use errors::FileId;
use std::sync::Arc;

use syntax::{AstNode, NameOwner};

pub(crate) fn lower_module_query(
    db: &impl HirDatabase,
    file: FileId,
    mod_id: hir::ModuleId,
) -> Arc<hir::Module> {
    let module = db.lookup_intern_module(mod_id);

    let name = module.name().unwrap();
    let range = name.syntax().text_range();
    let name_id = db.intern_name(name.into());
    let name = Span::from_range(name_id, range);

    let span = module.syntax().text_range();
    Arc::new(hir::Module {
        id: mod_id,
        file,
        name,
        span,
    })
}
