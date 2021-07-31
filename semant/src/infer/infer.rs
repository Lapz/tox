use super::{InferDataMap, TypeMap};
use crate::{
    hir::{
        self, ExprId, Function, FunctionAstMap, Literal, LiteralId, PatId, StmtId, UnaryOp,
        PLACEHOLDER_NAME,
    },
    infer::{self, InferDataCollector, Type, TypeCon},
    util, HirDatabase,
};
use errors::Reporter;
use indexmap::IndexMap;
use std::{collections::HashMap, hash::Hash};
use tracing::instrument;

impl<'a, DB> InferDataCollector<&'a DB>
where
    DB: HirDatabase,
{
    pub(crate) fn finish(self) -> Reporter {
        self.reporter
    }

    #[instrument(skip(self, function))]
    pub(crate) fn infer_function(&mut self, function: &Function) -> infer::Function {
        let map = InferDataMap::default();

        self.type_map.insert(function.name.item, map);

        let mut params = IndexMap::default();
        let expected = self.db.resolve_named_type(self.file, function.name.item);

        self.returns = Some(expected.clone());

        self.fn_name = Some(function.name.item);

        for ast_param in &function.params {
            let param = function.ast_map.param(&ast_param.item);

            let ty = self.db.resolve_hir_type(self.file, param.ty.item);
            params.insert(*ast_param, ty.clone());
            self.assign_type(param.pat.item, ty, &function.ast_map)
        }

        if let Some(body) = &function.body {
            body.iter().for_each(|stmt| {
                let _ = self.infer_statement(stmt, &function.ast_map);
            })
        }

        let map = self.type_map.remove(&function.name.item).unwrap();

        infer::Function {
            exported: function.exported,
            name: function.name,
            ast_map: function.ast_map.clone(),
            params,
            expr_to_type: map.expr_to_type,
            stmt_to_type: map.stmt_to_type,
            signature: expected.clone(),
            span: function.span,
            body: function.body.clone(),
        }
    }

    #[instrument(skip(self, map))]
    pub(crate) fn assign_type(&mut self, pat: PatId, ty: Type, map: &FunctionAstMap) {
        let pat = map.pat(&pat);

        match pat {
            hir::Pattern::Bind { name } => self.env.insert(name.item, ty),
            hir::Pattern::Placeholder => {
                let name = self.db.intern_name(PLACEHOLDER_NAME);

                self.env.insert(name, ty)
            }
            hir::Pattern::Tuple(pats) => match ty {
                Type::Tuple(types) => {
                    for (pat, ty) in pats.iter().zip(types.iter()) {
                        self.assign_type(pat.item, ty.clone(), map)
                    }
                }
                _ => {}
            },
            hir::Pattern::Literal(_) => {}
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
    ) -> Type {
        let stmt = map.stmt(&id.item);

        let ty = match stmt {
            hir::Stmt::Let {
                pat,
                ascribed_type,
                initializer,
            } => {
                match (ascribed_type, initializer) {
                    (Some(expected), Some(init)) => {
                        let expr = self.infer_expr(map, init);
                        let expected = self.db.resolve_hir_type(self.file, expected.item);

                        self.unify(&expected, &expr, init.as_reporter_span(), None, true);
                        self.assign_type(pat.item, expr, map);
                    }
                    (Some(expected), None) => {
                        let ty = self.db.resolve_hir_type(self.file, expected.item);
                        self.assign_type(pat.item, ty, map);
                    }
                    (None, Some(init)) => {
                        let expected = self.infer_expr(map, init);

                        self.assign_type(pat.item, expected, map);
                    }
                    (None, None) => {
                        self.assign_type(pat.item, Type::Con(TypeCon::Void), map);
                    }
                };

                Type::Con(TypeCon::Void)
            }
            hir::Stmt::Expr(expr) => self.infer_expr(map, expr),
            hir::Stmt::Error => Type::Unknown,
        };

        let map = self.type_map.get_mut(&self.fn_name.unwrap()).unwrap();
        map.insert_stmt_type(id.item, ty.clone());

        ty
    }
    #[instrument(skip(self, map))]
    pub(crate) fn infer_expr(&mut self, map: &FunctionAstMap, id: &util::Span<ExprId>) -> Type {
        let expr = map.expr(&id.item);

        let ty = match expr {
            hir::Expr::Array(exprs) => {
                if exprs.len() == 0 {
                    Type::Con(TypeCon::Array {
                        ty: Box::new(Type::Con(TypeCon::Void)),
                        size: None,
                    })
                } else {
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
            }
            hir::Expr::Binary { lhs, op, rhs } => self.infer_binary(id, lhs, op, rhs, map),

            hir::Expr::Block(block, has_value) => self.infer_block(map, block, *has_value),
            hir::Expr::Break | hir::Expr::Continue => Type::Con(TypeCon::Void),
            hir::Expr::Call {
                callee,
                args,
                type_args,
            } => self.infer_call(callee, args, type_args, map),
            hir::Expr::Closure { .. } => Type::Unknown,
            hir::Expr::Cast { expr, ty } => {
                let _ = self.infer_expr(map, expr);

                // TODO implement a well formed casting check

                self.db.resolve_hir_type(self.file, ty.item)
            }

            hir::Expr::If {
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
            hir::Expr::Ident(name) => self.lookup_type(name.item),

            hir::Expr::Index { base, index } => {
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
                    Type::Con(TypeCon::Str) => inferred_base,
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
            hir::Expr::While { cond, body } => {
                let inferred_cond = self.infer_expr(map, cond);

                self.unify(
                    &inferred_cond,
                    &Type::Con(TypeCon::Bool),
                    cond.as_reporter_span(),
                    Some("Expected the type of the expression to be bool".into()),
                    true,
                );

                self.infer_block(map, body, false);

                Type::Con(TypeCon::Void)
            }
            hir::Expr::Literal(literal) => self.infer_literal(*literal),
            hir::Expr::Paren(inner) => self.infer_expr(map, inner),
            hir::Expr::Tuple(exprs) => {
                let types = exprs.iter().map(|id| self.infer_expr(map, id)).collect();
                Type::Tuple(types)
            }
            hir::Expr::Unary { op, expr } => {
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
            hir::Expr::Return(expr) => {
                let expected = self.returns.clone().unwrap();
                if let Some(id) = expr {
                    let inferred = self.infer_expr(map, id);
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
                    inferred
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
                    inferred
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

                Type::Unknown
            }
            hir::Expr::Enum { def, variant, expr } => {
                let inferred_def = self.db.resolve_named_type(self.file, def.item);

                let mut subst = HashMap::new();

                // TODO handle matching multiple type vars aka check record literal code

                match inferred_def {
                    Type::Poly(ref vars, ref inner) => match &**inner {
                        Type::Enum(_, ref variants) => {
                            if let Some(v) = variants.get(&variant.item) {
                                match (expr, &v.ty) {
                                    (None, None) => {}
                                    (None, Some(variant_ty)) => {
                                        let msg = format!("Missing enum variant constructor",);
                                        self.reporter.error(
                                        msg,
                                        format!("Expected an enum variant constructor of type {:?} but found none",variant_ty),
                                        variant.as_reporter_span(),
                                    );
                                    }
                                    (Some(_), None) => {
                                        let msg = format!("Unexpected enum variant constructor",);
                                        let name = self.db.lookup_intern_name(variant.item);
                                        self.reporter.error(
                                            msg,
                                            format!(
                                                "enum variant `{}` does not have a constructor",
                                                name
                                            ),
                                            variant.as_reporter_span(),
                                        );
                                    }
                                    (Some(expr), Some(variant_ty)) => {
                                        let inferred_expr = self.infer_expr(map, expr);

                                        for ty_var in vars {
                                            subst.insert(*ty_var, inferred_expr.clone());
                                        }

                                        let variant_ty = self.subst(&variant_ty, &mut subst);

                                        self.unify(
                                            &inferred_expr,
                                            &variant_ty,
                                            expr.as_reporter_span(),
                                            None,
                                            true,
                                        )
                                    }
                                }
                            }

                            self.subst(&inferred_def, &mut subst)
                        }

                        _ => {
                            // Error reported in resolver
                            Type::Unknown
                        }
                    },

                    _ => {
                        // Error reported in resolver
                        Type::Unknown
                    }
                }
            }
            hir::Expr::RecordLiteral { def, fields } => {
                let inferred_def = self.db.resolve_named_type(self.file, def.item);

                let mut subst = HashMap::new();

                match inferred_def.clone() {
                    Type::Poly(vars, inner) => {
                        match &*inner {
                            Type::Class {
                                name: class_name,
                                fields: field_types,
                                methods,
                                ..
                            } => {
                                for (var, (_, expr)) in vars.iter().zip(fields.iter()) {
                                    let inferred_expr = self.infer_expr(map, expr);

                                    subst.insert(*var, inferred_expr.clone());
                                }

                                let mut instance_fields = field_types.clone();

                                for (field, expr) in fields {
                                    let inferred_expr = self.infer_expr(map, expr);

                                    if let Some(ty) = field_types.get(&field.item) {
                                        match ty {
                                            Type::Var(tv) => {
                                                subst.insert(*tv, inferred_expr.clone());
                                            }
                                            _ => {}
                                        }
                                    }

                                    let expected = self.subst(
                                        field_types.get(&field.item).unwrap_or(&Type::Unknown),
                                        &mut subst,
                                    );

                                    self.unify(
                                        &expected,
                                        &inferred_expr,
                                        field.as_reporter_span(),
                                        None,
                                        true,
                                    );

                                    instance_fields.insert(field.item, inferred_expr);
                                }

                                let instance_ty = Type::Poly(
                                    vars.clone(),
                                    Box::new(Type::Class {
                                        name: *class_name,
                                        fields: instance_fields,
                                        methods: methods.clone(),
                                    }),
                                );

                                // inferred_def

                                self.subst(&instance_ty, &mut subst)
                            }

                            _ => {
                                // Error reported in resolver
                                Type::Unknown
                            }
                        }
                    }
                    _ => {
                        // Error reported in resolver
                        Type::Unknown
                    }
                }
            }
            hir::Expr::Field(fields) => {
                let record = self.infer_expr(map, &fields[0]);

                match record {
                    Type::Poly(_, inner) => match &*inner {
                        ty @ Type::Class { .. } => {
                            // resolve method chain
                            self.infer_field_exprs(&fields[1..], ty, map)
                        }
                        Type::Unknown => Type::Unknown,
                        ty => {
                            let msg = format!(
                                "Expected expression of type `class` instead found `{:?}`",
                                ty
                            );

                            self.reporter.error(
                                msg,
                                "Field access only works on classes",
                                fields[0].as_reporter_span(),
                            );

                            Type::Unknown
                        }
                    },

                    ty @ Type::Tuple(_) => self.infer_field_exprs(&fields[1..], &ty, map),

                    Type::Unknown => Type::Unknown,
                    _ => {
                        let msg = format!(
                            "Expected expression of type `class` instead found `{:?}`",
                            record
                        );

                        self.reporter.error(
                            msg,
                            "Field access only works on classes",
                            fields[0].as_reporter_span(),
                        );

                        Type::Unknown
                    }
                }
            }
        };

        let map = self.type_map.get_mut(&self.fn_name.unwrap()).unwrap();
        map.insert_expr_type(id.item, ty.clone());

        ty
    }
}
