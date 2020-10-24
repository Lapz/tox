use crate::{
    hir::{Expr, ExprId, FunctionAstMap, NameId},
    resolver::data::ResolverDataCollector,
    util, HirDatabase,
};

impl<'a, DB> ResolverDataCollector<&'a DB>
where
    DB: HirDatabase,
{
    pub(crate) fn resolve_expression(
        &mut self,
        fn_name: &util::Span<NameId>,
        expr: &util::Span<ExprId>,
        ast_map: &FunctionAstMap,
    ) -> Result<(), ()> {
        let expr = ast_map.expr(&expr.item);

        match expr {
            Expr::Array(exprs) | Expr::Tuple(exprs) => {
                for id in exprs {
                    self.resolve_expression(fn_name, id, ast_map)?;
                }
            }
            Expr::Binary { lhs, rhs, .. } => {
                self.resolve_expression(fn_name, lhs, ast_map)?;
                self.resolve_expression(fn_name, rhs, ast_map)?;
            }
            Expr::Block(block_id) => {
                let block = ast_map.block(block_id);

                self.begin_function_scope(fn_name.item);

                for id in &block.0 {
                    self.resolve_statement(fn_name, id, ast_map)?
                }

                self.end_function_scope(fn_name.item);
            }
            Expr::Break | Expr::Continue => {}
            Expr::Call {
                callee,
                args,
                type_args,
            } => {
                self.resolve_expression(fn_name, callee, ast_map)?;

                for id in args {
                    self.resolve_expression(fn_name, id, ast_map)?;
                }

                type_args.item.iter().for_each(|ty| {
                    let _ = self.resolve_type(ty);
                })
            }
            Expr::Cast { expr, ty } => {
                self.resolve_expression(fn_name, expr, ast_map)?;
                let _ = self.resolve_type(ty);
            }

            Expr::If {
                cond,
                then_branch,
                else_branch,
            } => {
                self.resolve_expression(fn_name, cond, ast_map)?;
                self.resolve_expression(fn_name, then_branch, ast_map)?;

                if let Some(else_branch) = else_branch {
                    self.resolve_expression(fn_name, else_branch, ast_map)?;
                }
            }
            Expr::Ident(name) => {
                if self.local_is_declared(&fn_name.item, name) {
                    let msg = format!(
                        "Cannot read local name `{}` in its own initializer.",
                        self.db.lookup_intern_name(name.item)
                    );

                    self.reporter
                        .error(msg, "", (name.start().to_usize(), name.end().to_usize()))
                }

                self.resolve_local(&fn_name.item, name)
            }
            Expr::Index { base, index } => {
                self.resolve_expression(fn_name, base, ast_map)?;
                self.resolve_expression(fn_name, index, ast_map)?;
            }
            Expr::While { cond, body } => {
                self.resolve_expression(fn_name, cond, ast_map)?;

                let block = ast_map.block(body);

                self.begin_function_scope(fn_name.item);

                for id in &block.0 {
                    self.resolve_statement(fn_name, id, ast_map)?
                }

                self.end_function_scope(fn_name.item);
            }
            Expr::Literal(_) => {}
            Expr::Paren(expr) => self.resolve_expression(fn_name, expr, ast_map)?,

            Expr::Unary { expr, .. } => self.resolve_expression(fn_name, expr, ast_map)?,
            Expr::Return(expr) => {
                if let Some(expr) = expr {
                    self.resolve_expression(fn_name, expr, ast_map)?;
                }
            }
            Expr::Match { expr, arms } => {
                self.resolve_expression(fn_name, expr, ast_map)?;

                for arm in arms {
                    for pat_id in &arm.pats {
                        self.resolve_pattern(fn_name.item, pat_id, ast_map)?;
                    }

                    self.resolve_expression(fn_name, &arm.expr, ast_map)?;
                }
            }
            Expr::RecordLiteral {
                def,
                fields: record_fields,
            } => {
                if let Some(ty) = self.ctx.get_type(&def.item) {
                    match ty {
                        crate::infer::Type::Poly(_, inner) => match &*inner {
                            crate::infer::Type::Class { fields, .. } => {
                                for (field, expr) in record_fields {
                                    if !fields.contains_key(&field.item) {
                                        let msg = format!(
                                            "Unknown literal field `{}`",
                                            self.db.lookup_intern_name(field.item)
                                        );

                                        self.reporter.error(msg, "", field.as_reporter_span());
                                    }

                                    if self.resolve_expression(fn_name, expr, ast_map).is_err() {
                                        continue;
                                    };
                                }
                            }
                            _ => {
                                let msg = format!(
                                    "`{}` is not an class",
                                    self.db.lookup_intern_name(def.item)
                                );

                                self.reporter.error(msg, "", def.as_reporter_span());

                                return Err(());
                            }
                        },
                        _ => {
                            let msg = format!(
                                "`{}` is not an enum",
                                self.db.lookup_intern_name(def.item)
                            );

                            self.reporter.error(msg, "", def.as_reporter_span());

                            return Err(());
                        }
                    }
                } else {
                    let msg = format!("Unknown class `{}`", self.db.lookup_intern_name(def.item));

                    self.reporter.error(msg, "", def.as_reporter_span());

                    return Err(());
                }
            }
            Expr::Enum { def, variant, expr } => {
                if let Some(ty) = self.ctx.get_type(&def.item) {
                    match ty {
                        crate::infer::Type::Enum(variants) => {
                            if variants.get(&variant.item).is_none() {
                                let msg = format!(
                                    "Unknown enum variant `{}`",
                                    self.db.lookup_intern_name(variant.item)
                                );
                                self.reporter.error(msg, "", variant.as_reporter_span());
                                return Err(());
                            }
                        }
                        _ => {
                            let msg = format!(
                                "`{}` is not an enum",
                                self.db.lookup_intern_name(def.item)
                            );

                            self.reporter.error(msg, "", def.as_reporter_span());

                            return Err(());
                        }
                    }
                } else {
                    let msg = format!("Unknown enum `{}`", self.db.lookup_intern_name(def.item));

                    self.reporter.error(msg, "", def.as_reporter_span());

                    return Err(());
                }

                if let Some(expr) = expr {
                    self.resolve_expression(fn_name, expr, ast_map)?;
                }
            }
        }

        Ok(())
    }
}
