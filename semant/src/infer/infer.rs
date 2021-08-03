use super::InferDataMap;
use crate::{
    hir::{
        self, ExprId, Function, FunctionAstMap, Literal, LiteralId, PatId, StmtId, UnaryOp,
        PLACEHOLDER_NAME,
    },
    infer::{InferDataCollector, Type, TypeCon},
    typed::{self},
    util, HirDatabase, Span,
};
use errors::Reporter;

use tracing::instrument;

impl<'a, DB> InferDataCollector<&'a DB>
where
    DB: HirDatabase,
{
    pub(crate) fn finish(self) -> Reporter {
        self.reporter
    }

    #[instrument(skip(self, function))]
    pub(crate) fn infer_function(&mut self, function: &Function) -> typed::Typed<typed::Function> {
        let map = InferDataMap::default();

        self.type_map.insert(function.name.item, map);

        let expected = self.db.resolve_named_type(self.file, function.name.item);

        self.returns = Some(expected.clone());

        self.fn_name = Some(function.name.item);

        let mut typed_params = Vec::new();

        for ast_param in &function.params {
            let param = function.ast_map.param(&ast_param.item);

            let ty = self.db.resolve_hir_type(self.file, param.ty.item);

            typed_params.push(typed::Typed::new(
                typed::Param {
                    pat: self.assign_type(param.pat, ty.clone(), &function.ast_map),
                },
                ty,
                (ast_param.start, ast_param.end),
            ));
        }

        let mut inferred_body = vec![];

        if let Some(body) = &function.body {
            body.iter()
                .for_each(|stmt| inferred_body.push(self.infer_statement(stmt, &function.ast_map)))
        }

        let map = self.type_map.remove(&function.name.item).unwrap();

        typed::Typed::new(
            typed::Function {
                exported: function.exported,
                name: function.name,
                params: typed_params,
                body: inferred_body,
            },
            expected.clone(),
            (function.span.start(), function.span.end()),
        )
    }

    #[instrument(skip(self, map))]
    pub(crate) fn assign_type(
        &mut self,
        pat: Span<PatId>,
        ty: Type,
        map: &FunctionAstMap,
    ) -> typed::Typed<typed::Pattern> {
        let span = (pat.start, pat.end);
        let pat = map.pat(&pat.item);

        match pat {
            hir::Pattern::Bind { name } => {
                self.env.insert(name.item, ty.clone());

                typed::Typed::new(typed::Pattern::Bind { name: *name }, ty, span)
            }
            hir::Pattern::Placeholder => {
                let name = self.db.intern_name(PLACEHOLDER_NAME);

                self.env.insert(name, ty.clone());
                typed::Typed::new(typed::Pattern::Placeholder, ty, span)
            }
            hir::Pattern::Tuple(pats) => match &ty {
                Type::Tuple(types) => {
                    let mut typed_pats = Vec::with_capacity(types.len());
                    for (pat, ty) in pats.iter().zip(types.iter()) {
                        typed_pats.push(self.assign_type(*pat, ty.clone(), map))
                    }

                    typed::Typed::new(typed::Pattern::Tuple(typed_pats), ty, span)
                }
                _ => {
                    let tuple = format!(
                        "({})",
                        std::iter::repeat("_,").take(pats.len()).collect::<String>()
                    );
                    let msg = format!("Found {} but expected type {:?}", tuple, ty);
                    self.reporter
                        .error(msg, "", (span.0.to_usize(), span.1.to_usize()));

                    typed::Typed::new(typed::Pattern::Placeholder, ty, span)
                }
            },
            hir::Pattern::Literal(id) => typed::Typed::new(typed::Pattern::Literal(*id), ty, span),
        }
    }

    #[instrument(skip(self,))]
    pub(crate) fn lookup_type(&mut self, name: hir::NameId) -> Type {
        if let Some(local_ty) = self.env.get(&name).cloned() {
            local_ty
        } else {
            self.db.resolve_named_type(self.file, name)
        }
    }

    #[instrument(skip(self, map,))]
    fn infer_statements(&mut self, map: &FunctionAstMap, body: &[util::Span<StmtId>]) {
        for id in body {
            let _ = self.infer_statement(id, map);
        }
    }
    #[instrument(skip(self))]
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
    #[instrument(skip(self, map))]
    pub(crate) fn infer_statement(
        &mut self,
        id: &util::Span<StmtId>,
        map: &FunctionAstMap,
    ) -> typed::Typed<typed::Stmt> {
        let stmt = map.stmt(&id.item);

        let ty = match stmt {
            hir::Stmt::Let {
                pat,
                ascribed_type,
                initializer,
            } => {
                let stmt = match (ascribed_type, initializer) {
                    (Some(expected), Some(init)) => {
                        let expr = self.infer_expr(map, init);
                        let expected = self.db.resolve_hir_type(self.file, expected.item);

                        self.unify(&expected, &expr.ty, init.as_reporter_span(), None, true);
                        let pat = self.assign_type(*pat, expr.ty.clone(), map);

                        typed::Stmt::Let {
                            pat,
                            ascribed_type: Some(expected),
                            initializer: Some(expr),
                        }
                    }
                    (Some(expected), None) => {
                        let ty = self.db.resolve_hir_type(self.file, expected.item);
                        let pat = self.assign_type(*pat, ty.clone(), map);

                        typed::Stmt::Let {
                            pat,
                            ascribed_type: Some(ty),
                            initializer: None,
                        }
                    }
                    (None, Some(init)) => {
                        let initializer = self.infer_expr(map, init);

                        let pat = self.assign_type(*pat, initializer.ty.clone(), map);

                        typed::Stmt::Let {
                            pat,
                            ascribed_type: None,
                            initializer: Some(initializer),
                        }
                    }
                    (None, None) => typed::Stmt::Let {
                        pat: self.assign_type(*pat, Type::Con(TypeCon::Void), map),
                        ascribed_type: None,
                        initializer: None,
                    },
                };

                typed::Typed::new(stmt, Type::Con(TypeCon::Void), (id.start, id.end))
            }
            hir::Stmt::Expr(expr) => {
                let expr = self.infer_expr(map, expr);
                let ty = expr.ty.clone();
                typed::Typed::new(typed::Stmt::Expr(expr), ty, (id.start, id.end))
            }
            hir::Stmt::Error => {
                typed::Typed::new(typed::Stmt::Error, Type::Unknown, (id.start, id.end))
            }
        };

        ty
    }
    #[instrument(skip(self, map))]
    pub(crate) fn infer_expr(
        &mut self,
        map: &FunctionAstMap,
        id: &util::Span<ExprId>,
    ) -> typed::Typed<typed::Expr> {
        let span = (id.start, id.end);
        let expr = map.expr(&id.item);

        match expr {
            hir::Expr::Array(exprs) => {
                if exprs.len() == 0 {
                    typed::Typed::new(
                        typed::Expr::Array(vec![]),
                        Type::Con(TypeCon::Array {
                            ty: Box::new(Type::Con(TypeCon::Void)),
                            size: None,
                        }),
                        span,
                    )
                } else {
                    let mut typed_exprs = Vec::with_capacity(exprs.len());

                    let first = self.infer_expr(map, &exprs[0]);
                    let first_ty = first.ty.clone();
                    typed_exprs.push(first);

                    exprs.iter().skip(1).for_each(|id| {
                        let inferred = self.infer_expr(map, id);
                        self.unify(&first_ty, &inferred.ty, id.as_reporter_span(), None, true);

                        typed_exprs.push(inferred)
                    });

                    typed::Typed::new(
                        typed::Expr::Array(typed_exprs),
                        Type::Con(TypeCon::Array {
                            ty: Box::new(first_ty),
                            size: None,
                        }),
                        span,
                    )
                }
            }
            hir::Expr::Binary { lhs, op, rhs } => self.infer_binary(id, lhs, op, rhs, map),

            hir::Expr::Block(block, has_value) => self.infer_block(map, block, *has_value),
            hir::Expr::Break => {
                typed::Typed::new(typed::Expr::Break, Type::Con(TypeCon::Void), span)
            }
            hir::Expr::Continue => {
                typed::Typed::new(typed::Expr::Continue, Type::Con(TypeCon::Void), span)
            }
            hir::Expr::Call {
                callee,
                args,
                type_args,
            } => self.infer_call(callee, args, type_args, span, map),
            hir::Expr::Closure { .. } => {
                typed::Typed::new(typed::Expr::Closure {}, Type::Unknown, span)
            }
            hir::Expr::Cast { expr, ty } => {
                let expr = self.infer_expr(map, expr);

                // TODO implement a well formed casting check

                let ty = self.db.resolve_hir_type(self.file, ty.item);

                typed::Typed::new(
                    typed::Expr::Cast {
                        expr: Box::new(expr),
                        ty: ty.clone(),
                    },
                    ty,
                    span,
                )
            }

            hir::Expr::If {
                cond,
                then_branch,
                else_branch,
            } => {
                let inferred_cond = self.infer_expr(map, cond);

                self.unify(
                    &inferred_cond.ty,
                    &Type::Con(TypeCon::Bool),
                    cond.as_reporter_span(),
                    Some("Expected the type of the expression to be bool".into()),
                    true,
                );

                let inferred_then = self.infer_expr(map, then_branch);
                let ty = inferred_then.ty.clone();

                if let Some(else_b) = else_branch {
                    let inferred_else = self.infer_expr(map, else_b);

                    self.unify(
                        &inferred_then.ty,
                        &inferred_else.ty,
                        id.as_reporter_span(),
                        Some("One of the branches is of different type".into()),
                        true,
                    );

                    typed::Typed::new(
                        typed::Expr::If {
                            cond: Box::new(inferred_cond),
                            then_branch: Box::new(inferred_then),
                            else_branch: Some(Box::new(inferred_else)),
                        },
                        ty,
                        span,
                    )
                } else {
                    typed::Typed::new(
                        typed::Expr::If {
                            cond: Box::new(inferred_cond),
                            then_branch: Box::new(inferred_then),
                            else_branch: None,
                        },
                        ty,
                        span,
                    )
                }
            }
            hir::Expr::Ident(name) => {
                let ty = self.lookup_type(name.item);
                typed::Typed::new(typed::Expr::Ident(*name), ty, span)
            }

            hir::Expr::Index { base, index } => {
                let inferred_base = self.infer_expr(map, base);

                let inferred_index = self.infer_expr(map, index);

                self.unify(
                    &inferred_index.ty,
                    &Type::Con(TypeCon::Int),
                    index.as_reporter_span(),
                    Some("Array indexes can only be an integer".into()),
                    true,
                );

                let ty = match &inferred_base.ty {
                    Type::Con(TypeCon::Array { ty, .. }) => *ty.clone(),
                    Type::Con(TypeCon::Str) => inferred_base.ty.clone(),
                    _ => {
                        let msg = format!("Tried indexing a non array type");

                        self.reporter.error(
                            msg,
                            &format!("`{:?}` is not indexable", inferred_base),
                            id.as_reporter_span(),
                        );

                        Type::Unknown
                    }
                };

                typed::Typed::new(
                    typed::Expr::Index {
                        base: Box::new(inferred_base),
                        index: Box::new(inferred_index),
                    },
                    ty,
                    span,
                )
            }
            hir::Expr::While { cond, body } => {
                let inferred_cond = self.infer_expr(map, cond);

                self.unify(
                    &inferred_cond.ty,
                    &Type::Con(TypeCon::Bool),
                    cond.as_reporter_span(),
                    Some("Expected the type of the expression to be bool".into()),
                    true,
                );

                let body = self.infer_block(map, body, false);

                typed::Typed::new(
                    typed::Expr::While {
                        cond: Box::new(inferred_cond),
                        body: Box::new(body),
                    },
                    Type::Con(TypeCon::Void),
                    span,
                )
            }
            hir::Expr::Literal(literal) => typed::Typed::new(
                typed::Expr::Literal(*literal),
                self.infer_literal(*literal),
                span,
            ),
            hir::Expr::Paren(inner) => {
                let expr = self.infer_expr(map, inner);
                let ty = expr.ty.clone();
                typed::Typed::new(typed::Expr::Paren(Box::new(expr)), ty, span)
            }
            hir::Expr::Tuple(exprs) => {
                let mut inferred_exprs = Vec::with_capacity(exprs.len());
                let types = exprs
                    .iter()
                    .map(|id| {
                        let expr = self.infer_expr(map, id);
                        let ty = expr.ty.clone();

                        inferred_exprs.push(expr);

                        ty
                    })
                    .collect();

                typed::Typed::new(typed::Expr::Tuple(inferred_exprs), Type::Tuple(types), span)
            }
            hir::Expr::Unary { op, expr } => {
                let inferred = self.infer_expr(map, id);
                let ty = match op {
                    UnaryOp::Minus => match inferred.ty {
                        Type::Con(TypeCon::Int) | Type::Con(TypeCon::Float) => inferred.ty.clone(),

                        _ => {
                            let msg =
                                format!("Cannot use `-` operator on type `{:?}`", inferred.ty);
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
                            &inferred.ty,
                            expr.as_reporter_span(),
                            Some("`!` can only be used on a boolean expression".into()),
                            true,
                        );
                        Type::Con(TypeCon::Bool)
                    }
                };

                typed::Typed::new(
                    typed::Expr::Unary {
                        op: *op,
                        expr: Box::new(inferred),
                    },
                    ty,
                    span,
                )
            }
            hir::Expr::Return(expr) => {
                let expected = self.returns.clone().unwrap_or(Type::Unknown);
                if let Some(id) = expr {
                    let inferred = self.infer_expr(map, id);
                    let ty = inferred.ty.clone();
                    self.unify(
                        &expected,
                        &ty,
                        id.as_reporter_span(),
                        Some(format!(
                            "{:?} is returned here but expected {:?}",
                            inferred, expected
                        )),
                        true,
                    );

                    typed::Typed::new(typed::Expr::Return(Some(Box::new(inferred))), ty, span)
                } else {
                    let inferred = Type::Con(TypeCon::Void);
                    self.unify(
                        &expected,
                        &inferred,
                        id.as_reporter_span(),
                        Some(format!(
                            "{:?} is returned here but expected {:?}",
                            inferred, expected
                        )),
                        true,
                    );
                    typed::Typed::new(typed::Expr::Return(None), inferred, span)
                }
            }
            hir::Expr::Match { expr, .. } => {
                let _inferred_expr = self.infer_expr(map, expr);

                // let mut matrix = PatternMatrix::new();

                // for match_arm in arms {
                //     let mut patterns = vec![];

                //     for pattern in &match_arm.pats {
                //         let pat = map.pat(&pattern.item);

                //         patterns.push(self.to_matrix_pattern(pat, map))
                //     }

                //     matrix.add_row(Row::new(patterns, match_arm.expr.item))
                // }
                //  Type::Unknown
                todo!()
            }
            hir::Expr::Enum { def, variant, expr } => {
                self.infer_enum_expr(def, variant, expr, span, map)
            }
            hir::Expr::RecordLiteral { def, fields } => {
                self.infer_record_expr(def, fields, span, map)
            }
            hir::Expr::Field(fields) => self.infer_field_exprs(fields, span, map),
        }
    }
}
