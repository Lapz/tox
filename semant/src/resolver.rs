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

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum State {
    Declared,
    Defined,
    Read,
}
#[derive(Default, Debug, Clone)]
pub(crate) struct FunctionData {
    scopes: Vec<HashMap<hir::NameId, State>>,
    locals: HashMap<hir::NameId, usize>,
}

impl FunctionData {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
            ..Self::default()
        }
    }

    pub(crate) fn peek(&self) -> usize {
        self.scopes.len() - 1
    }
}
#[derive(Debug, Clone)]
pub struct FileTable {
    symbol_level: HashMap<hir::NameId, Level>,
    symbol_exports: HashMap<hir::NameId, bool>,
    function_data: HashMap<hir::NameId, FunctionData>,
}

impl FileTable {
    pub fn new() -> Self {
        Self {
            symbol_exports: HashMap::default(),
            symbol_level: HashMap::default(),
            function_data: HashMap::default(),
        }
    }

    pub(crate) fn peek(&self, name: hir::NameId) -> usize {
        self.function_data[&name].peek()
    }

    pub(crate) fn function_data(&self, name: hir::NameId) -> &FunctionData {
        &self.function_data[&name]
    }

    pub(crate) fn function_data_mut(&mut self, name: hir::NameId) -> &mut FunctionData {
        self.function_data.get_mut(&name).unwrap()
    }

    pub fn contains(&self, id: hir::NameId) -> bool {
        self.symbol_level.contains_key(&id)
    }

    pub(crate) fn insert_name(&mut self, id: hir::NameId, level: Level, exported: bool) {
        self.symbol_level.insert(id, level);
        self.symbol_exports.insert(id, exported);
    }

    pub(crate) fn insert_function_data(&mut self, id: hir::NameId, data: FunctionData) {
        self.function_data.insert(id, data);
    }
}

impl<'a, DB> ResolverDataCollector<&'a DB>
where
    DB: HirDatabase,
{
    pub(crate) fn table(self) -> FileTable {
        self.table
    }

    fn peek(&self, name: hir::NameId) -> usize {
        self.table.peek(name)
    }

    pub(crate) fn declare(&mut self, function: hir::NameId, name: hir::NameId, span: hir::Span) {
        let index = self.peek(function);

        if self.table.function_data(function).scopes[index].contains_key(&name) {
            let msg = format!(
                "The name `{}` already exists in this scope.",
                self.db.lookup_intern_name(name)
            );

            self.reporter.warn(msg, "", (span.start(), span.end()))
        }

        self.table.function_data_mut(function).scopes[index].insert(name, State::Declared);
    }

    pub(crate) fn insert_function_data(&mut self, id: hir::NameId, data: FunctionData) {
        self.table.insert_function_data(id, data);
    }

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
            self.table.insert_name(id, level, exported);
            self.insert_function_data(id, FunctionData::new());
        }
    }

    pub(crate) fn resolve_statement(
        &mut self,
        function: hir::NameId,
        ast_map: &hir::FunctionAstMap,
        stmt: &hir::StmtId,
    ) {
        let stmt = ast_map.stmt(stmt);

        match stmt {
            hir::Stmt::Let { pat, initializer } => self.resolve_pattern(function, ast_map, pat),

            hir::Stmt::Expr(expr) => {}
        }
        // let stmt = self.db.lookup_intern_statement(stmt);
    }

    pub(crate) fn resolve_pattern(
        &mut self,
        function: hir::NameId,
        ast_map: &hir::FunctionAstMap,
        pat_id: &hir::PatId,
    ) {
        println!("{:?}", pat_id);
        let pat = self.db.lookup_intern_pattern(*pat_id);

        match &pat {
            hir::Pattern::Bind { name } => {
                self.declare(function, *name, ast_map.pattern_span(pat_id))
            }
            hir::Pattern::Literal(_) => {}
            hir::Pattern::Placeholder => {}
            hir::Pattern::Tuple(pats) => pats
                .iter()
                .for_each(|pat| self.resolve_pattern(function, ast_map, pat)),
        }
    }
}

pub fn resolve_imports(
    db: &impl HirDatabase,
    program: Arc<hir::Program>,
    reporter: Reporter,
) -> Arc<FileTable> {
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

    Arc::new(collector.table())
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

    // collect the top level definitions first so we can
    // use forward declarations

    for function in &program.functions {
        collector.insert_top_level(
            function.name,
            Level::Global,
            function.exported,
            function.span,
        )
    }

    for function in &program.functions {
        let data = FunctionData::new();

        let ast_map = function.map();

        if function.body().is_none() {
            continue;
        }

        for statement in function.body().as_ref().unwrap() {
            collector.resolve_statement(function.name, ast_map, statement)
        }

        // let block = db.lookup_intern_block()
    }
}
