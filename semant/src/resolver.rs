use crate::db::HirDatabase;
use crate::hir;
use errors::Reporter;

use std::{collections::HashMap, sync::Arc};

#[derive(Debug)]
pub(crate) struct ResolverDataCollector<DB> {
    db: DB,
    table: FileTable,
    reporter: Reporter,
}
#[derive(Debug, Clone)]
pub(crate) enum Level {
    Global,
    Block(hir::BlockId),
}
#[derive(Debug, Clone)]
pub struct FileTable {
    symbol_level: HashMap<hir::NameId, Level>,
    symbol_exports: HashMap<hir::NameId, bool>,
}

impl FileTable {
    pub fn new() -> Self {
        Self {
            symbol_exports: HashMap::default(),
            symbol_level: HashMap::default(),
        }
    }

    pub fn contains(&self, id: hir::NameId) -> bool {
        self.symbol_level.contains_key(&id)
    }

    pub(crate) fn insert_name(&mut self, id: hir::NameId, level: Level, exported: bool) {
        self.symbol_level.insert(id, level);
        self.symbol_exports.insert(id, exported);
    }
}

impl<'a, DB> ResolverDataCollector<&'a DB>
where
    DB: HirDatabase,
{
    pub(crate) fn insert_top_level(
        &mut self,
        id: hir::NameId,
        level: Level,
        exported: bool,
        span: hir::Span,
    ) {
        if self.table.contains(id) {
            let name = self.db.lookup_intern_name(id);
            let message = format!(
                "A function with the name `{}` has already been defined.",
                name
            );
            self.reporter.error(message, "", (span.start(), span.end()));
        } else {
            self.table.insert_name(id, level, exported)
        }
    }
}

pub fn resolve_program_query(
    db: &impl HirDatabase,
    program: Arc<hir::Program>,
    reporter: Reporter,
) -> () {
    let mut collector = ResolverDataCollector {
        db,
        reporter,
        table: FileTable::new(),
    };

    for function in &program.functions {
        collector.insert_top_level(
            function.name,
            Level::Global,
            function.exported,
            function.span,
        )
    }
}
