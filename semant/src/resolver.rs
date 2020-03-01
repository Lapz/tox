use crate::db::HirDatabase;
use crate::hir;
use errors::{FileId, Reporter, WithError};

use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

#[derive(Debug)]
pub(crate) struct ResolverDataCollector<DB> {
    db: DB,
    table: FileTable,
    reporter: Reporter,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum State {
    Declared,
    Defined,
    Read,
}

#[derive(Clone, Debug, Default)]
pub(crate) struct Scopes {
    scopes: Vec<HashMap<hir::NameId, State>>,
    len: usize,
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
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
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FileTable {
    symbol_level: HashSet<hir::NameId>,
    symbol_exports: HashMap<hir::NameId, bool>,
    function_data: HashMap<hir::NameId, FunctionData>,
}

impl FileTable {
    pub fn new() -> Self {
        Self {
            symbol_exports: HashMap::default(),
            symbol_level: HashSet::default(),
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
        self.symbol_level.contains(&id)
    }

    pub(crate) fn insert_name(&mut self, id: hir::NameId, exported: bool) {
        self.symbol_level.insert(id);
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
    fn table(self) -> FileTable {
        self.table
    }

    fn reporter(self) -> Reporter {
        self.reporter
    }

    fn begin_scope(&mut self, function: hir::NameId) {
        let data = self.table.function_data_mut(function);
        data.scopes.push(HashMap::new())
    }

    fn end_scope(&mut self, function: hir::NameId) {
        let data = self.table.function_data_mut(function);

        let scopes = data.scopes.pop().unwrap();

        for (symbol, state) in &scopes {
            if state == &State::Defined {
                let _msg = format!("Unused variable `{}`", self.db.lookup_intern_name(*symbol));
                // self.reporter.warn(msg,"")
            }
        }
    }

    fn peek(&self, function_name: hir::NameId) -> usize {
        self.table.peek(function_name)
    }

    pub(crate) fn declare(&mut self, function: hir::NameId, name: hir::NameId, span: hir::Span) {
        let index = self.peek(function);

        if self.table.function_data(function).scopes[index].contains_key(&name) {
            let msg = format!(
                "The name `{}` already exists in this scope.",
                self.db.lookup_intern_name(name)
            );

            self.reporter
                .warn(msg, "", (span.start().to_usize(), span.end().to_usize()))
        }

        self.table.function_data_mut(function).scopes[index].insert(name, State::Declared);
    }

    pub(crate) fn define(&mut self, function: hir::NameId, name: hir::NameId) {
        let index = self.peek(function);

        self.table.function_data_mut(function).scopes[index].insert(name, State::Defined);
    }

    pub(crate) fn not_resolved(&mut self, function: hir::NameId, name: &hir::NameId) -> bool {
        let index = self.peek(function);
        self.table.function_data(function).scopes[index].get(name) == Some(&State::Declared)
    }

    pub(crate) fn define_pattern(
        &mut self,
        function: hir::NameId,
        ast_map: &hir::FunctionAstMap,
        pat_id: &hir::PatId,
    ) {
        let pat = ast_map.pat(pat_id);

        match &pat {
            hir::Pattern::Bind { name } => self.define(function, *name),
            hir::Pattern::Tuple(pats) => pats
                .iter()
                .for_each(|pat| self.define_pattern(function, ast_map, pat)),
            hir::Pattern::Literal(_) => {}
            hir::Pattern::Placeholder => {}
        }
    }

    pub(crate) fn insert_function_data(&mut self, id: hir::NameId, data: FunctionData) {
        self.table.insert_function_data(id, data);
    }

    pub(crate) fn insert_top_level(&mut self, id: hir::NameId, exported: bool, span: hir::Span) {
        if self.table.contains(id) {
            let name = self.db.lookup_intern_name(id);
            let message = format!("The name `{}` is defined multiple times", name);
            self.reporter.error(
                message,
                "",
                (span.start().to_usize(), span.end().to_usize()),
            );
        } else {
            self.table.insert_name(id, exported);
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
            hir::Stmt::Let { pat, initializer } => {
                self.resolve_pattern(function, ast_map, pat);

                if let Some(init) = initializer {
                    self.resolve_expression(function, ast_map, init)
                }

                self.define_pattern(function, ast_map, pat);
            }

            hir::Stmt::Expr(expr) => self.resolve_expression(function, ast_map, expr),
        }
    }

    pub(crate) fn resolve_expression(
        &mut self,
        function: hir::NameId,
        ast_map: &hir::FunctionAstMap,
        expr_id: &hir::ExprId,
    ) {
        let expr = ast_map.expr(expr_id);

        match expr {
            hir::Expr::Array(exprs) => exprs
                .iter()
                .for_each(|id| self.resolve_expression(function, ast_map, id)),
            hir::Expr::Binary {
                ref lhs, ref rhs, ..
            } => {
                self.resolve_expression(function, ast_map, lhs);
                self.resolve_expression(function, ast_map, rhs)
            }
            hir::Expr::Block(block_id) => {
                let block = ast_map.block(block_id);

                self.begin_scope(function);

                block
                    .0
                    .iter()
                    .for_each(|id| self.resolve_statement(function, ast_map, id));

                self.end_scope(function)
            }

            hir::Expr::Break | hir::Expr::Continue => {}
            hir::Expr::Call { callee, args } => {
                self.resolve_expression(function, ast_map, callee);

                args.iter()
                    .for_each(|id| self.resolve_expression(function, ast_map, id))
            }

            hir::Expr::Cast { expr, .. } => self.resolve_expression(function, ast_map, expr),
            hir::Expr::If {
                cond,
                then_branch,
                else_branch,
            } => {
                self.resolve_expression(function, ast_map, cond);
                self.resolve_expression(function, ast_map, then_branch);

                if let Some(else_branch) = else_branch {
                    self.resolve_expression(function, ast_map, else_branch);
                }
            }

            hir::Expr::Ident(name) => {
                let span = ast_map.expr_span(expr_id);
                if self.not_resolved(function, name) {
                    let msg = format!(
                        "Cannot read local name `{}` in its own initializer.",
                        self.db.lookup_intern_name(*name)
                    );

                    self.reporter
                        .error(msg, "", (span.start().to_usize(), span.end().to_usize()))
                }

                self.resolve_local(function, name, true, span);
            }
            hir::Expr::Index { base, index } => {
                self.resolve_expression(function, ast_map, base);
                self.resolve_expression(function, ast_map, index);
            }
            hir::Expr::While { cond, body } => {
                self.resolve_expression(function, ast_map, cond);

                let block = ast_map.block(body);

                self.begin_scope(function);

                block
                    .0
                    .iter()
                    .for_each(|id| self.resolve_statement(function, ast_map, id));

                self.end_scope(function);
            }

            hir::Expr::Literal(_) => {}
            hir::Expr::Paren(id) => self.resolve_expression(function, ast_map, id),
            hir::Expr::Tuple(exprs) => exprs
                .iter()
                .for_each(|id| self.resolve_expression(function, ast_map, id)),
            hir::Expr::Unary { expr, .. } => self.resolve_expression(function, ast_map, expr),
            hir::Expr::Return(expr) => {
                if let Some(expr) = expr {
                    self.resolve_expression(function, ast_map, expr)
                }
            }

            hir::Expr::Match { expr, arms } => {
                self.resolve_expression(function, ast_map, expr);
                arms.iter().for_each(|arm| {
                    arm.pats
                        .iter()
                        .for_each(|pat_id| self.resolve_pattern(function, ast_map, pat_id));
                    self.resolve_expression(function, ast_map, &arm.expr)
                })
            }
        }
    }

    pub(crate) fn resolve_local(
        &mut self,
        function: hir::NameId,
        name: &hir::NameId,
        is_read: bool,
        span: hir::Span,
    ) {
        let data = self.table.function_data_mut(function);
        let max_depth = data.scopes.len();

        for i in 0..max_depth {
            if data.scopes[max_depth - i - 1].contains_key(name) {
                if is_read {
                    if let Some(state) = data.scopes[max_depth - i - 1].get_mut(name) {
                        *state = State::Read
                    }
                }

                return;
            }
        } // check for ident name in function/local scope

        if !self.table.contains(*name) {
            //  check for external import global level
            let msg = format!(
                "Use of undefined variable `{}`",
                self.db.lookup_intern_name(*name)
            );

            self.reporter
                .error(msg, "", (span.start().to_usize(), span.end().to_usize()))
        }
    }

    pub(crate) fn resolve_pattern(
        &mut self,
        function: hir::NameId,
        ast_map: &hir::FunctionAstMap,
        pat_id: &hir::PatId,
    ) {
        let pat = ast_map.pat(pat_id);

        match &pat {
            hir::Pattern::Bind { name } => {
                self.declare(function, *name, ast_map.pattern_span(pat_id))
            }
            hir::Pattern::Tuple(pats) => pats
                .iter()
                .for_each(|pat| self.resolve_pattern(function, ast_map, pat)),
            hir::Pattern::Literal(_) => {}
            hir::Pattern::Placeholder => {}
        }
    }
}

pub fn resolve_imports_query(db: &impl HirDatabase, file: FileId) -> WithError<Arc<FileTable>> {
    let program = db.lower(file)?;
    let reporter = Reporter::new(file);
    let mut collector = ResolverDataCollector {
        db,
        reporter: reporter.clone(),
        table: FileTable::new(),
    };

    for function in &program.functions {
        collector.insert_top_level(function.name, function.exported, function.span)
    }

    if reporter.has_errors() {
        Err(reporter.finish())
    } else {
        Ok(Arc::new(collector.table()))
    }
}

pub fn resolve_program_query(db: &impl HirDatabase, file: FileId) -> WithError<()> {
    let program = db.lower(file)?;
    let reporter = Reporter::new(file);

    let mut collector = ResolverDataCollector {
        db,
        reporter,
        table: FileTable::new(),
    };

    // collect the top level definitions first so we can
    // use forward declarations

    for function in &program.functions {
        collector.insert_top_level(function.name, function.exported, function.span)
    }

    for function in &program.functions {
        let ast_map = function.map();

        if function.body().is_none() {
            continue;
        }

        for statement in function.body().as_ref().unwrap() {
            collector.resolve_statement(function.name, ast_map, statement)
        }
    }

    let reporter = collector.reporter();
    // println!("{:?}", reporter.diagnostics);

    if reporter.has_errors() {
        Err(reporter.finish())
    } else {
        Ok(())
    }
}
