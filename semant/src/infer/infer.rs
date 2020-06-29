use super::StackedMap;
use crate::infer::{Type, TypeCon};
use crate::resolver::Resolver;
use crate::{
    hir::{ExprId, Function, FunctionAstMap, NameId, StmtId},
    HirDatabase,
};
use errors::{FileId, Reporter, WithError};
use std::sync::Arc;

#[derive(Debug)]
struct InferDataCollector<DB> {
    db: DB,
    resolver: Arc<Resolver>,
    reporter: Reporter,
    types: StackedMap<NameId, Type>,
    return_ty: Option<Type>,
}

impl<'a, DB> InferDataCollector<&'a DB>
where
    DB: HirDatabase,
{
    fn unify(&self, lhs: &Type, rhs: &Type) {
        match (lhs, rhs) {
            _ => unimplemented!(),
        }
    }

    fn infer_expr(&mut self, expr: &ExprId, ast_map: &FunctionAstMap) -> Type {
        let expr = ast_map.expr(expr);
        match expr {
            crate::hir::Expr::Array(_) => {}
            crate::hir::Expr::Binary { lhs, op, rhs } => {}
            crate::hir::Expr::Block(_) => {}
            crate::hir::Expr::Break => {}
            crate::hir::Expr::Call {
                callee,
                args,
                type_args,
            } => {}
            crate::hir::Expr::Cast { expr, ty } => {}
            crate::hir::Expr::Continue => {}
            crate::hir::Expr::If {
                cond,
                then_branch,
                else_branch,
            } => {}
            crate::hir::Expr::Ident(_) => {}
            crate::hir::Expr::Index { base, index } => {}
            crate::hir::Expr::While { cond, body } => {}
            crate::hir::Expr::Literal(_) => {}
            crate::hir::Expr::Paren(_) => {}
            crate::hir::Expr::Tuple(_) => {}
            crate::hir::Expr::Unary { op, expr } => {}
            crate::hir::Expr::Return(_) => {}
            crate::hir::Expr::Match { expr, arms } => {}
            crate::hir::Expr::Enum { def, variant, expr } => {}
            crate::hir::Expr::RecordLiteral { def, fields } => {}
        }

        unimplemented!()
    }

    fn infer_statement(&mut self, stmt: &StmtId, ast_map: &FunctionAstMap) {
        let stmt = ast_map.stmt(stmt);

        match stmt {
            crate::hir::Stmt::Let {
                pat,
                initializer,
                ascribed_type,
            } => {
                if let (Some(expr), Some(ascribed_type)) = (initializer, ascribed_type) {
                    let lhs = self.infer_type(ascribed_type);

                    let expr_ty = self.infer_expr(expr, ast_map);

                    self.unify(lhs, &expr_ty);

                    self.add_var(pat, expr_ty);
                }
            }

            crate::hir::Stmt::Expr(expr) => self.infer_expr(expr, ast_map),
        }
    }

    fn infer_function(&mut self, function: &Function) {
        let name = function.name.item;

        let expected = self.resolver.ctx.get_type(&name).unwrap();

        self.return_ty = Some(expected.clone());

        let body_ty = if let Some(body) = &function.body {
            for stmt in body {
                self.infer_statement(stmt, &function.ast_map)
            }

            unimplemented!()
        } else {
            self.unify(&expected, &Type::Con(TypeCon::Void));
        };
    }
}

pub fn infer_query(db: &impl HirDatabase, file: FileId) -> WithError<()> {
    let program = db.lower(file)?;
    let resolver = db.resolve_source_file(file)?;

    let mut collector = InferDataCollector {
        db,
        resolver,
        reporter: Reporter::new(file),
        types: StackedMap::new(),
        return_ty: None,
    };

    for function in &program.functions {
        collector.infer_function(function);
        //
    }

    Ok(())
}
