use crate::hir::{ExprId, Function, FunctionAstMap, StmtId};
use crate::{hir::Literal, resolver::Resolver, util::Span};
use crate::{
    hir::LiteralId,
    hir::PatId,
    infer::{Type, TypeCon},
};
use crate::{Ctx, HirDatabase};
use errors::{FileId, Reporter, Span, WithError};
use std::sync::Arc;

#[derive(Debug)]
struct InferDataCollector<DB> {
    db: DB,
    ctx: Ctx,
    resolver: Arc<Resolver>,
    reporter: Reporter,
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

        self.ctx.begin_scope();

        let body = if let Some(body) = &function.body {
            Type::Unknown
        } else {
            Type::Con(TypeCon::Void)
        };

        self.ctx.end_scope();

        println!("{:?}", expected);
    }

    fn unify(
        &self,
        lhs: &Type,
        rhs: &Type,
        span: (usize, usize),
        notes: Vec<String>,
        report: bool,
    ) {
        match (lhs, rhs) {
            (Type::App(types1), Type::App(types2)) => {
                if types1.len() != types2.len() && report {
                    let msg = format!("Expected {} params, found {}", types1.len(), types2.len());
                    self.reporter.error(msg, "", span);
                    return;
                }

                if types1[types1.len() - 1] != types2[types2.len() - 1] && report {
                    let msg = format!(
                        "Expected {} found {}. The return types are different",
                        types1.len(),
                        types2.len()
                    );
                    self.reporter.error(msg, "", span);
                    return;
                }

                for (a, b) in types1.iter().zip(types2.iter()) {
                    let _ = self.unify(a, b, span, vec![], false);
                }
            }
            (Type::Tuple(types1), Type::Tuple(types2)) => {
                if types1.len() != types2.len() && report {
                    let msg = format!("Expected {} params, found {}", types1.len(), types2.len());
                    self.reporter.error(msg, "", span);
                    return;
                }

                for (a, b) in types1.iter().zip(types2.iter()) {
                    let _ = self.unify(a, b, span, vec![], false);
                }
            }
            (Type::Enum(_), Type::Enum(_)) => {}
            (Type::Class { fields, methods }, Type::Class { fields, methods }) => {}
            (Type::Var(v1), Type::Var(v2)) => {
                if v1 != v2 && report {
                    let msg = format!("Cannot unify `{:?}` vs `{:?}`", lhs, rhs);
                    self.reporter.error(msg, "", span);
                }
            }
            (Type::Poly(_, _), t) => {}
            (t, Type::Poly(_, _)) => {}
            (Type::Unknown, _) => {}
            (_, Type::Unknown) => {}
            (t1, t2) => {
                //Todo report error

                if report {
                    let msg = format!("Cannot unify `{:?}` vs `{:?}`", lhs, rhs);
                    self.reporter.error(msg, "", span);
                }
            }
        }
    }

    fn infer_statements(
        &mut self,
        map: &FunctionAstMap,
        body: &[StmtId],
        returns: &Type,
    ) -> Result<(), ()> {
        for id in body {
            let stmt = map.stmt(id);

            match stmt {
                crate::hir::Stmt::Let {
                    pat,
                    ascribed_type,
                    initializer,
                } => {
                    let (ascribed, init) = match (ascribed_type, initializer) {
                        (Some(expected), Some(init)) => {
                            let expr = self.infer_expr(map, init);

                            self.unify(&expected, expr);
                            (
                                self.resolver.lookup_intern_type(&expected.item).unwrap(),
                                expr,
                            )
                        }
                        (Some(expected), None) => (
                            self.resolver.lookup_intern_type(&expected.item).unwrap(),
                            Type::Con(TypeCon::Void),
                        ),
                        (None, Some(init)) => {
                            (Type::Con(TypeCon::Void), self.infer_expr(map, init))
                        }
                        (None, None) => (Type::Con(TypeCon::Void), Type::Con(TypeCon::Void)),
                    };
                    let pat = map.pat(&pat.item);
                    match pat {
                        crate::hir::Pattern::Bind { name } => {
                            self.ctx.insert_type(name.item, ty, kind)
                        }
                        crate::hir::Pattern::Placeholder => {}
                        crate::hir::Pattern::Tuple(idents) => {}
                        crate::hir::Pattern::Literal(literal) => {
                            let lhs = self.infer_literal(*literal);
                        }
                    }
                }
                crate::hir::Stmt::Expr(expr) => {
                    let rhs = self.infer_expr(map, expr);
                }
            }
        }

        Ok(())
    }

    fn infer_literal(&mut self, lit_id: LiteralId) -> Type {
        let lit = self.db.lookup_intern_literal(lit_id);

        match lit {
            Literal::String(_) => Type::Con(TypeCon::Str),
            Literal::Nil => Type::Con(TypeCon::Void),
            Literal::True | Literal::False => Type::Con(TypeCon::Bool),
            Literal::Int(_) => Type::Con(TypeCon::Int),
            Literal::Float(_) => Type::Con(TypeCon::Float),
        }
    }

    fn infer_expr(&mut self, map: &FunctionAstMap, id: &ExprId) -> Type {
        let expr = map.expr(id);

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
            crate::hir::Expr::Literal(literal) => return self.infer_literal(*literal),
            crate::hir::Expr::Paren(_) => {}
            crate::hir::Expr::Tuple(_) => {}
            crate::hir::Expr::Unary { op, expr } => {}
            crate::hir::Expr::Return(_) => {}
            crate::hir::Expr::Match { expr, arms } => {}
            crate::hir::Expr::Enum { def, variant, expr } => {}
            crate::hir::Expr::RecordLiteral { def, fields } => {}
        }

        Type::Unknown
    }
}

pub fn infer_query(db: &impl HirDatabase, file: FileId) -> WithError<()> {
    let WithError(program, mut errors) = db.lower(file);
    let WithError(resolver, error) = db.resolve_source_file(file);
    let reporter = Reporter::new(file);
    errors.extend(error);

    let ctx = resolver.ctx.clone();

    let mut collector = InferDataCollector {
        db,
        ctx,
        resolver,
        reporter,
    };

    for function in &program.functions {
        collector.infer_function(function);
    }

    WithError((), errors)
}
