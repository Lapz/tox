mod binary;
mod block;
mod call;
mod ctx;
mod enum_expr;
mod field;
mod infer;
mod pattern_matrix;
mod record;
mod stacked_map;
mod subst;
mod ty;
mod unify;

use crate::{
    hir::{ExprId, FunctionAstMap, NameId, ParamId, StmtId},
    typed, HirDatabase, Resolver, Span,
};
pub use ctx::Ctx;
use errors::{FileId, Reporter, WithError};
use indexmap::IndexMap;
pub use stacked_map::StackedMap;
use syntax::TextRange;

use std::{collections::HashMap, sync::Arc};
pub use ty::{Type, TypeCon, TypeVar, Variant};

pub type TypeMap = HashMap<NameId, InferDataMap>;

#[derive(Debug)]
pub(crate) struct InferDataCollector<DB> {
    db: DB,
    ctx: Ctx,
    resolver: Arc<Resolver>,
    reporter: Reporter,
    returns: Option<Type>,
    fn_name: Option<NameId>,
    env: StackedMap<NameId, Type>,
    file: FileId,
    type_map: TypeMap,
}

#[derive(Debug, Default, Eq, PartialEq, Clone)]
pub struct InferDataMap {
    pub expr_to_type: IndexMap<ExprId, Type>,
    pub stmt_to_type: IndexMap<StmtId, Type>,
}

impl InferDataMap {
    pub fn insert_stmt_type(&mut self, id: StmtId, ty: Type) {
        self.stmt_to_type.insert(id, ty);
    }
    pub fn insert_expr_type(&mut self, id: ExprId, ty: Type) {
        self.expr_to_type.insert(id, ty);
    }
}

pub fn infer_query(db: &impl HirDatabase, file: FileId) -> WithError<typed::Program> {
    let WithError(source, mut errors) = db.lower(file);
    let WithError(resolver, error) = db.resolve_source_file(file);
    let reporter = Reporter::new(file);
    errors.extend(error);

    let ctx = resolver.ctx.clone();

    let mut collector = InferDataCollector {
        db,
        ctx,
        resolver,
        reporter,
        file,
        returns: None,
        env: StackedMap::new(),
        fn_name: None,
        type_map: HashMap::new(),
    };

    let mut program = typed::Program { functions: vec![] };

    for function in &source.functions {
        program.functions.push(collector.infer_function(function));
    }

    let reporter = collector.finish();

    errors.extend(reporter.finish());

    WithError(program, errors)
}
