use crate::hir::{Function, FunctionAstMap, StmtId};
use crate::infer::{Type, TypeCon};
use crate::resolver::Resolver;
use crate::HirDatabase;
use errors::{FileId, WithError};
use std::sync::Arc;

#[derive(Debug)]
struct InferDataCollector<DB> {
    db: DB,
    resolver: Arc<Resolver>,
}

impl<'a, DB> InferDataCollector<&'a DB>
where
    DB: HirDatabase,
{
    fn infer_function(&mut self, function: &Function) {
        let expected = if let Some(ty) = self.resolver.get_type(&function.name.item) {
            ty
        } else {
            Type::Unknown
        };

        let body = if let Some(body) = function.body {
        } else {
            Type::Con(TypeCon::Void)
        };

        println!("{:?}", expected);
    }

    fn infer_statements(&mut self, map: &FunctionAstMap, body: &[StmtId], returns: &Type) {
        for id in body {
            let stmt = map.stmt(id);

            match stmt {
                crate::hir::Stmt::Let {
                    pat,
                    ascribed_type,
                    initializer,
                } => {}
                crate::hir::Stmt::Expr(expr) => self.infer_expr(map, expr, returns),
            }
        }
    }
}

pub fn infer_query(db: &impl HirDatabase, file: FileId) -> WithError<()> {
    let WithError(program, mut errors) = db.lower(file);
    let WithError(resolver, error) = db.resolve_source_file(file);

    errors.extend(error);

    let mut collector = InferDataCollector { db, resolver };

    for function in &program.functions {
        collector.infer_function(function);
    }

    WithError((), errors)
}
