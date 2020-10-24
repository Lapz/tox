use crate::{
    hir::{
        BinOp, BlockId, ExprId, Function, FunctionAstMap, Literal, LiteralId, PatId, StmtId,
        UnaryOp, PLACEHOLDER_NAME,
    },
    infer::{Type, TypeCon},
    resolver::Resolver,
    util, Ctx, HirDatabase,
};
use errors::{FileId, Reporter, WithError};
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

        if let Some(body) = &function.body {
            self.infer_statements(&function.ast_map, &body, &Type::Con(TypeCon::Void))
                .unwrap()
        } else {
            self.infer_statements(&function.ast_map, &[], &Type::Con(TypeCon::Void))
                .unwrap()
        };

        self.ctx.end_scope();

        println!("{:?}", expected);
    }

    fn unify(
        &mut self,
        lhs: &Type,
        rhs: &Type,
        span: (usize, usize),
        notes: Option<String>,
        report: bool,
    ) {
        match (lhs, rhs) {
            (Type::App(types1), Type::App(types2)) => {
                if types1.len() != types2.len() && report {
                    let msg = format!("Expected {} params, found {}", types1.len(), types2.len());
                    self.reporter.error(msg, notes.unwrap_or("".into()), span);
                    return;
                }

                if types1[types1.len() - 1] != types2[types2.len() - 1] && report {
                    let msg = format!(
                        "Expected {} found {}. The return types are different",
                        types1.len(),
                        types2.len()
                    );
                    self.reporter.error(msg, notes.unwrap_or("".into()), span);
                    return;
                }

                for (a, b) in types1.iter().zip(types2.iter()) {
                    let _ = self.unify(a, b, span, None, false);
                }
            }
            (Type::Tuple(types1), Type::Tuple(types2)) => {
                if types1.len() != types2.len() && report {
                    let msg = format!("Expected {} params, found {}", types1.len(), types2.len());
                    self.reporter.error(msg, notes.unwrap_or("".into()), span);
                    return;
                }

                for (a, b) in types1.iter().zip(types2.iter()) {
                    let _ = self.unify(a, b, span, None, false);
                }
            }
            (Type::Enum(_), Type::Enum(_)) => {}
            (
                Type::Class { fields, methods },
                Type::Class {
                    fields: feilds1,
                    methods: methods1,
                },
            ) => {}
            (Type::Var(v1), Type::Var(v2)) => {
                if v1 != v2 && report {
                    let msg = format!("Cannot unify `{:?}` vs `{:?}`", lhs, rhs);
                    self.reporter.error(msg, notes.unwrap_or("".into()), span);
                }
            }
            (Type::Con(TypeCon::Int), Type::Con(TypeCon::Float)) => {}
            (Type::Con(TypeCon::Float), Type::Con(TypeCon::Int)) => {}
            (Type::Con(l), Type::Con(r)) => {
                if l != r {
                    if report {
                        let msg = format!("Cannot unify `{:?}` vs `{:?}`", lhs, rhs);
                        self.reporter.error(msg, notes.unwrap_or("".into()), span);
                    }
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
                    self.reporter.error(msg, notes.unwrap_or("".into()), span);
                }
            }
        }
    }

    fn assign_pattern_type(&mut self, map: &FunctionAstMap, id: &util::Span<PatId>, ty: Type) {
        let pat = map.pat(&id.item);
        match pat {
            crate::hir::Pattern::Bind { name } => {
                self.ctx
                    .insert_type(name.item, ty, crate::resolver::TypeKind::Type)
            }
            crate::hir::Pattern::Placeholder => self.ctx.insert_type(
                self.db.intern_name(PLACEHOLDER_NAME),
                ty,
                crate::resolver::TypeKind::Type,
            ),
            crate::hir::Pattern::Tuple(idents) => match ty {
                Type::Tuple(types) => {
                    for (ident, ty) in idents.iter().zip(types.into_iter()) {
                        self.assign_pattern_type(map, ident, ty)
                    }
                }

                _ => {
                    let msg = format!("Tried assigning a non tuple type to a tuple pattern");

                    self.reporter.error(
                        msg,
                        &format!("`{:?}` is not a tuple type", ty),
                        id.as_reporter_span(),
                    );
                }
            },
            crate::hir::Pattern::Literal(_) => {}
        }
    }

    fn infer_statements(
        &mut self,
        map: &FunctionAstMap,
        body: &[util::Span<StmtId>],
        returns: &Type,
    ) -> Result<(), ()> {
        for id in body {
            let stmt = map.stmt(&id.item);
            match stmt {
                crate::hir::Stmt::Let {
                    pat,
                    ascribed_type,
                    initializer,
                } => {
                    match (ascribed_type, initializer) {
                        (Some(expected), Some(init)) => {
                            let expr = self.infer_expr(map, init);
                            let expected =
                                self.resolver.lookup_intern_type(&expected.item).unwrap();

                            self.unify(&expected, &expr, id.as_reporter_span(), None, true);
                            self.assign_pattern_type(map, pat, expected);
                        }
                        (Some(expected), None) => {
                            self.assign_pattern_type(
                                map,
                                pat,
                                self.resolver.lookup_intern_type(&expected.item).unwrap(),
                            );
                        }
                        (None, Some(init)) => {
                            let expected = self.infer_expr(map, init);
                            self.assign_pattern_type(map, pat, expected);
                        }
                        (None, None) => {
                            self.assign_pattern_type(map, pat, Type::Con(TypeCon::Void));
                        }
                    };
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

    fn infer_block(&mut self, map: &FunctionAstMap, block_id: &BlockId) -> Type {
        let block = map.block(block_id);

        unimplemented!()
    }

    fn infer_expr(&mut self, map: &FunctionAstMap, id: &util::Span<ExprId>) -> Type {
        let expr = map.expr(&id.item);

        match expr {
            crate::hir::Expr::Array(exprs) => {
                if exprs.len() == 0 {
                    return Type::Unknown;
                }

                let first = self.infer_expr(map, &exprs[0]);

                exprs.iter().skip(1).for_each(|id| {
                    let inferred = self.infer_expr(map, id);
                    self.unify(&first, &inferred, id.as_reporter_span(), None, true)
                });

                Type::Con(TypeCon::Array {
                    ty: Box::new(first),
                    size: None,
                })
            }
            crate::hir::Expr::Binary { lhs, op, rhs } => {
                let inferred_lhs = self.infer_expr(map, lhs);
                let inferred_rhs = self.infer_expr(map, rhs);

                match op {
                    BinOp::Plus | BinOp::Minus | BinOp::Mult | BinOp::Div => {
                        self.unify(
                            &inferred_lhs,
                            &inferred_rhs,
                            id.as_reporter_span(),
                            Some("`+`,`-`,`*`,`/` operators only works on i32 and f32".into()),
                            true,
                        );
                        Type::Con(TypeCon::Bool)
                    }
                    BinOp::And | BinOp::Or => {
                        self.unify(
                            &inferred_lhs,
                            &Type::Con(TypeCon::Bool),
                            lhs.as_reporter_span(),
                            Some("Expected a bool".into()),
                            true,
                        );
                        self.unify(
                            &inferred_rhs,
                            &Type::Con(TypeCon::Bool),
                            rhs.as_reporter_span(),
                            Some("Expected a bool".into()),
                            true,
                        );

                        Type::Con(TypeCon::Bool)
                    }
                    BinOp::LessThan
                    | BinOp::GreaterThan
                    | BinOp::GreaterThanEqual
                    | BinOp::LessThanEqual => {
                        self.unify(
                            &inferred_lhs,
                            &inferred_rhs,
                            id.as_reporter_span(),
                            Some("Comparison operators only work on numbers".into()),
                            true,
                        );
                        Type::Con(TypeCon::Bool)
                    }
                    BinOp::Equal => {
                        self.unify(
                            &inferred_lhs,
                            &inferred_rhs,
                            id.as_reporter_span(),
                            None,
                            true,
                        );

                        inferred_rhs
                    }
                    BinOp::EqualEqual | BinOp::NotEqual => Type::Con(TypeCon::Bool),
                    BinOp::PlusEqual | BinOp::MinusEqual | BinOp::MultEqual | BinOp::DivEqual => {
                        self.unify(
                            &inferred_lhs,
                            &inferred_rhs,
                            id.as_reporter_span(),
                            None,
                            true,
                        );

                        inferred_rhs
                    }
                }
            }

            crate::hir::Expr::Block(block) => self.infer_block(map, block),
            crate::hir::Expr::Break | crate::hir::Expr::Continue => Type::Con(TypeCon::Void),
            crate::hir::Expr::Call {
                callee,
                args,
                type_args,
            } => Type::Unknown,
            crate::hir::Expr::Cast { expr, ty } => Type::Unknown,

            crate::hir::Expr::If {
                cond,
                then_branch,
                else_branch,
            } => {
                let inferred_cond = self.infer_expr(map, cond);

                self.unify(
                    &inferred_cond,
                    &Type::Con(TypeCon::Bool),
                    cond.as_reporter_span(),
                    Some("Expected the type of the expression to be bool".into()),
                    true,
                );

                let inferred_then = self.infer_expr(map, then_branch);

                if let Some(else_b) = else_branch {
                    let inferred_else = self.infer_expr(map, else_b);

                    self.unify(
                        &inferred_then,
                        &inferred_else,
                        id.as_reporter_span(),
                        Some("One of the branches is of different type".into()),
                        true,
                    )
                }

                inferred_then
            }
            crate::hir::Expr::Ident(name) => self.ctx.get_type(&name.item).unwrap_or(Type::Unknown),
            crate::hir::Expr::Index { base, index } => {
                let inferred_base = self.infer_expr(map, base);

                let inferred_index = self.infer_expr(map, index);

                self.unify(
                    &inferred_index,
                    &Type::Con(TypeCon::Int),
                    index.as_reporter_span(),
                    Some("Array indexes can only be an integer".into()),
                    true,
                );

                match inferred_base {
                    Type::Con(TypeCon::Array { ty, .. }) => *ty,
                    _ => {
                        let msg = format!("Tried indexing a non array type");

                        self.reporter.error(
                            msg,
                            &format!("`{:?}` is not indexable", inferred_base),
                            id.as_reporter_span(),
                        );

                        Type::Unknown
                    }
                }
            }
            crate::hir::Expr::While { cond, body } => {
                let inferred_cond = self.infer_expr(map, cond);

                self.unify(
                    &inferred_cond,
                    &Type::Con(TypeCon::Bool),
                    cond.as_reporter_span(),
                    Some("Expected the type of the expression to be bool".into()),
                    true,
                );

                self.infer_block(map, body);

                Type::Con(TypeCon::Void)
            }
            crate::hir::Expr::Literal(literal) => self.infer_literal(*literal),
            crate::hir::Expr::Paren(inner) => self.infer_expr(map, inner),
            crate::hir::Expr::Tuple(exprs) => {
                let types = exprs.iter().map(|id| self.infer_expr(map, id)).collect();
                Type::Tuple(types)
            }
            crate::hir::Expr::Unary { op, expr } => {
                let inferred = self.infer_expr(map, id);
                match op {
                    UnaryOp::Minus => match inferred {
                        Type::Con(TypeCon::Int) | Type::Con(TypeCon::Float) => inferred,

                        _ => {
                            let msg = format!("Cannot use `-` operator on type `{:?}`", inferred);
                            self.reporter.error(
                                msg,
                                "`-` only works on i32,f32,",
                                expr.as_reporter_span(),
                            );

                            Type::Con(TypeCon::Int)
                        }
                    },
                    UnaryOp::Excl => {
                        self.unify(
                            &Type::Con(TypeCon::Bool),
                            &inferred,
                            expr.as_reporter_span(),
                            Some("`!` can only be used on a boolean expression".into()),
                            true,
                        );
                        Type::Con(TypeCon::Bool)
                    }
                }
            }
            crate::hir::Expr::Return(expr) => {
                if let Some(id) = expr {
                    let inferred = self.infer_expr(map, id);

                    self.unify(
                        unimplemented!(),
                        &inferred,
                        id.as_reporter_span(),
                        Some(format!(
                            "{:?}.into() is returned here but expected {:?}",
                            inferred,
                            unimplemented!()
                        )),
                        true,
                    );
                    inferred
                } else {
                    Type::Con(TypeCon::Void)
                }
            }
            crate::hir::Expr::Match { expr, arms } => Type::Unknown,
            crate::hir::Expr::Enum { def, variant, expr } => Type::Unknown,
            crate::hir::Expr::RecordLiteral { def, fields } => Type::Unknown,
        }
    }
}

pub fn infer_query(db: &impl HirDatabase, file: FileId) -> WithError<()> {
    let WithError(program, mut errors) = db.lower(file);
    let WithError(resolver, error) = db.resolve_source_file(file);
    let reporter  bb  = Reporter::new(file);
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

    errors.extend(collector.reporter.finish());

    WithError((), errors)
}
