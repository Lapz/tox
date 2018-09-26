use ast as t;
use ctx::CompileCtx;
use infer::types::Type;
use infer::{Infer, InferResult};
use syntax::ast::{Expression, Literal, Op, UnaryOp};
use util::pos::Spanned;

impl Infer {
    pub(crate) fn infer_expr(
        &self,
        expr: Spanned<Expression>,
        ctx: &mut CompileCtx,
    ) -> InferResult<t::TypedExpression> {
        let (typed, ty) = match expr.value {
            Expression::Array { mut items } => {
                if items.is_empty() {
                    (
                        t::Expression::Array(vec![]),
                        Type::Array(Box::new(Type::Nil)),
                    )
                } else {
                    let mut nitems = vec![self.infer_expr(items.remove(0), ctx)?];

                    for item in items.into_iter().skip(1) {
                        let span = item.span;
                        let ty_expr = self.infer_expr(item, ctx)?;

                        self.unify(&nitems[0].ty, &ty_expr.ty, span, ctx)?;
                        nitems.push(ty_expr);
                    }

                    let ret_ty = nitems[0].ty.clone();

                    (t::Expression::Array(nitems), Type::Array(Box::new(ret_ty)))
                }
            }

            Expression::Assign {
                name, kind, value, ..
            } => {
                let span = name.span.to(value.span);

                let ty = self.infer_var(&name, ctx)?;
                let value_ty = self.infer_expr(*value, ctx)?;
                use syntax::ast::AssignOperator::*;
                match kind.value {
                    Equal => {
                        self.unify(&ty, &value_ty.ty, span, ctx)?;
                    }
                    MinusEqual | PlusEqual | StarEqual | SlashEqual => {
                        match self.unify(&ty, &value_ty.ty, span, ctx) {
                            Ok(()) => (),
                            Err(_) => match self.unify(&ty, &Type::Str, span, ctx) {
                                Ok(()) => (),
                                Err(_) => {
                                    ctx.remove_error();
                                    return Err(());
                                }
                            },
                        }
                    }
                }

                let ty = value_ty.ty.clone();

                (t::Expression::Assign(name.value, kind.value, value_ty), ty)
            }
            Expression::Binary { lhs, op, rhs } => {
                let span = lhs.span.to(rhs.span);

                let lhs = self.infer_expr(*lhs, ctx)?;
                let rhs = self.infer_expr(*rhs, ctx)?;

                match op.value {
                    Op::BangEqual | Op::EqualEqual => {
                        (t::Expression::Binary(lhs, op.value, rhs), Type::Bool)
                    }

                    Op::LessThan
                    | Op::LessThanEqual
                    | Op::GreaterThan
                    | Op::GreaterThanEqual
                    | Op::And
                    | Op::Or => {
                        self.unify(&lhs.ty, &rhs.ty, span, ctx)?;
                        (t::Expression::Binary(lhs, op.value, rhs), Type::Bool)
                    }

                    Op::Plus | Op::Slash | Op::Star | Op::Minus | Op::Modulo | Op::Exponential => {
                        match self.unify(&lhs.ty, &rhs.ty, span, ctx) {
                            Ok(()) => (),
                            Err(_) => match self.unify(&lhs.ty, &Type::Str, span, ctx) {
                                Ok(()) => (),
                                Err(_) => {
                                    ctx.remove_error();
                                    return Err(());
                                }
                            },
                        }

                        let ty = lhs.ty.clone();

                        (t::Expression::Binary(lhs, op.value, rhs), ty)
                    }
                }
            }
            Expression::Call { .. } => self.infer_call(expr, ctx)?,

            Expression::Closure(ref func) => unimplemented!(),

            Expression::ClassInstance { .. } => self.infer_class_instance(expr, ctx)?,

            Expression::Grouping { expr } => {
                let ty_expr = self.infer_expr(*expr, ctx)?;
                let ty = ty_expr.ty.clone();

                (t::Expression::Grouping(ty_expr), ty)
            }

            Expression::Get { .. } => self.infer_object_get(expr, ctx)?,

            Expression::SubScript { target, index } => {
                let target_span = target.span;

                match target.value {
                    Expression::Var(symbol) => {
                        let target_ty = self.infer_var(&symbol, ctx)?;

                        let span = index.span;

                        let index_ty = self.infer_expr(*index, ctx)?;

                        self.unify(&index_ty.ty, &Type::Int, span, ctx)?;

                        match target_ty {
                            Type::Array(ref ty) => {
                                (t::Expression::Index(symbol.value, index_ty), *ty.clone())
                            }
                            Type::Str => (t::Expression::Index(symbol.value, index_ty), Type::Str),

                            _ => {
                                let msg = format!(" Cannot index type `{}` ", target_ty.print(ctx));
                                ctx.error(msg, target_span);
                                return Err(());
                            }
                        }
                    }
                    _ => {
                        ctx.error("Invalid index target", target.span);
                        return Err(());
                    }
                }
            }
            Expression::Literal(literal) => {
                let ty = self.infer_literal(&literal);
                (t::Expression::Literal(literal), ty)
            }

            Expression::Set { .. } => self.infer_object_set(expr, ctx)?,

            Expression::Ternary {
                condition,
                then_branch,
                else_branch,
            } => {
                let span = condition.span;
                let cond_tyexpr = self.infer_expr(*condition, ctx)?;

                self.unify(&Type::Bool, &cond_tyexpr.ty, span, ctx)?;

                let span = then_branch.span.to(else_branch.span);
                let then_tyexpr = self.infer_expr(*then_branch, ctx)?;
                let else_tyexpr = self.infer_expr(*else_branch, ctx)?;

                self.unify(&then_tyexpr.ty, &else_tyexpr.ty, span, ctx)?;
                let ty = then_tyexpr.ty.clone();

                (
                    t::Expression::Ternary(cond_tyexpr, then_tyexpr, else_tyexpr),
                    ty,
                )
            }

            Expression::This => (t::Expression::This, self.this.clone()),
            Expression::Unary { expr, op } => {
                let span = expr.span;
                let expr = self.infer_expr(*expr, ctx)?;

                match op.value {
                    UnaryOp::Bang => (t::Expression::Unary(op.value, expr), Type::Bool),
                    UnaryOp::Minus => {
                        if !expr.ty.is_int() {
                            let msg =
                                format!("Cannot use `-` operator on type `{}`", expr.ty.print(ctx));

                            ctx.error(msg, span);
                            return Err(());
                        }

                        let ty = expr.ty.clone();
                        (t::Expression::Unary(op.value, expr), ty)
                    }
                }
            }

            Expression::Var(ref var) => {
                let ty = self.infer_var(var, ctx)?;

                (t::Expression::Var(var.value, ty.clone()), ty)
            }
        };

        Ok(t::TypedExpression {
            expr: Box::new(typed),
            ty,
        })
    }

    fn infer_call(
        &self,
        call: Spanned<Expression>,
        ctx: &mut CompileCtx,
    ) -> InferResult<(t::Expression, Type)> {
        match call.value {
            Expression::Call { callee, args } => match callee.value {
                Expression::Call { .. } => return self.infer_call(*callee, ctx),
                Expression::Var(ref sym) => {
                    if let Some(ty) = ctx.look_var(sym.value).cloned() {
                        let ty = ty.get_ty();
                        match ty {
                            Type::Fun(ref targs, ref ret) => {
                                use util::pos::Span;
                                let mut arg_tys: Vec<(Span, Type)> = Vec::with_capacity(args.len());
                                let mut callee_exprs = Vec::with_capacity(args.len());

                                for arg in args {
                                    let span = arg.span;
                                    let ty_expr = self.infer_expr(arg, ctx)?;
                                    arg_tys.push((span, ty_expr.ty.clone()));
                                    callee_exprs.push(ty_expr)
                                }

                                for (arg_ty, def_ty) in arg_tys.iter().zip(targs.iter()) {
                                    self.unify(&arg_ty.1, &def_ty, arg_ty.0, ctx)?
                                }

                                Ok((
                                    t::Expression::Call(
                                        t::TypedExpression {
                                            expr: Box::new(t::Expression::Var(
                                                sym.value,
                                                ty.clone(),
                                            )),
                                            ty: ty.clone(),
                                        },
                                        callee_exprs,
                                    ),
                                    *ret.clone(),
                                ))
                            }

                            _ => {
                                let msg = format!("`{}` is not callable", ctx.name(sym.value));

                                ctx.error(msg, callee.span);

                                return Err(());
                            }
                        }
                    } else {
                        let msg = format!("Undefined variable '{}' ", ctx.name(sym.value));
                        ctx.error(msg, sym.span);
                        return Err(());
                    }
                }

                Expression::Get { .. } => self.infer_object_get(*callee, ctx),
                _ => {
                    ctx.error(" Not callable", callee.span);
                    return Err(());
                }
            },

            _ => {
                ctx.error(" Not callable", call.span);
                Err(())
            }
        }
    }

    fn infer_literal(&self, literal: &Literal) -> Type {
        match *literal {
            Literal::Float(_) => Type::Float,

            Literal::False(_) | Literal::True(_) => Type::Bool,

            Literal::Str(_) => Type::Str,

            Literal::Nil => Type::Nil, // Nil is given the type void as only statements return Nil

            Literal::Int(_) => Type::Int,
        }
    }

    fn infer_class_instance(
        &self,
        expr: Spanned<Expression>,
        ctx: &mut CompileCtx,
    ) -> InferResult<(t::Expression, Type)> {
        match expr.value {
            Expression::ClassInstance { symbol, props } => {
                let class = if let Some(ty) = ctx.look_type(symbol.value).cloned() {
                    ty
                } else {
                    let msg = format!("Undefined class `{}` ", ctx.name(symbol.value));
                    ctx.error(msg, symbol.span);
                    return Err(());
                };

                match class {
                    Type::This { ref fields, .. } | Type::Class(_, ref fields, _, _) => {
                        let mut instance_exprs = Vec::new();
                        let mut found = false;

                        for (prop, prop_ty) in props.into_iter().zip(fields.iter()) {
                            if &prop.value.symbol.value == prop_ty.0 {
                                found = true;

                                let span = prop.span;

                                let ty = self.infer_expr(prop.value.expr, ctx)?;

                                self.unify(&prop_ty.1, &ty.ty, span, ctx)?;

                                instance_exprs.push(ty);
                            } else {
                                found = false;

                                let msg = format!(
                                    "`{}` is not a member of `{}` ",
                                    ctx.name(prop.value.symbol.value),
                                    ctx.name(*prop_ty.0)
                                );

                                ctx.error(msg, prop.span)
                            }
                        }

                        if fields.len() > instance_exprs.len() {
                            let msg =
                                format!("class `{}` is missing fields", ctx.name(symbol.value));
                            ctx.error(msg, expr.span);
                            return Err(());
                        } else if fields.len() < instance_exprs.len() {
                            let msg =
                                format!("class `{}` has too many fields", ctx.name(symbol.value));
                            ctx.error(msg, expr.span);
                            return Err(());
                        } else if !found {
                            return Err(());
                        }

                        Ok((
                            t::Expression::ClassInstance(symbol.value, instance_exprs),
                            class.clone(),
                        ))
                    }

                    _ => {
                        let msg = format!("`{}`is not a class", ctx.name(symbol.value));
                        ctx.error(msg, symbol.span);
                        Err(())
                    }
                }
            }

            _ => unreachable!(),
        }
    }

    fn infer_object_get(
        &self,
        expr: Spanned<Expression>,
        ctx: &mut CompileCtx,
    ) -> InferResult<(t::Expression, Type)> {
        match expr.value {
            Expression::Get {
                object, property, ..
            } => {
                let ob_instance = self.infer_expr(*object, ctx)?;


                match ob_instance.ty.clone() {
                    Type::This { ref name, .. } | Type::Class(ref name, _, _, _) => {
                        if let Some(ty) = ctx.look_type(*name) {
                            // Look at the conical type
                            match *ty {
                                Type::This {
                                    ref methods,
                                    ref fields,
                                    ..
                                }
                                | Type::Class(_, ref fields, ref methods, _) => {
                                    for (field_name, field_ty) in fields {
                                        if field_name == &property.value {
                                            return Ok((
                                                t::Expression::Get(property.value, ob_instance),
                                                field_ty.clone(),
                                            ));
                                        }
                                    }

                                    for (method_name, method_ty) in methods {
                                        if method_name == &property.value {
                                            let ty = method_ty.clone().get_ty();

                                            let ty = match ty {
                                                Type::Fun(_, ret) => *ret,
                                                _ => unreachable!(),
                                            };

                                            return Ok((
                                                t::Expression::Get(property.value, ob_instance),
                                                ty, // Change to return the return type
                                            ));
                                        }
                                    }
                                }

                                _ => unreachable!(),
                            }
                        }

                        let msg = format!(
                            "class `{}` doesn't have a field named `{}`",
                            ctx.name(*name),
                            ctx.name(property.value)
                        );

                        ctx.error(msg, expr.span);

                        Err(())
                    }

                    ref other_ty => {
                        let msg = format!(
                            "Type {} dosen't have the method/field {}",
                            other_ty.print(ctx),
                            ctx.name(property.value)
                        );

                        ctx.error(msg, property.span);
                        return Err(());
                    }
                }
            }
            _ => unreachable!(),
        }
    }

    fn infer_object_set(
        &self,
        expr: Spanned<Expression>,
        ctx: &mut CompileCtx,
    ) -> InferResult<(t::Expression, Type)> {
        match expr.value {
            Expression::Set {
                object,
                name: property,
                value,
                ..
            } => {
                let ob_instance = self.infer_expr(*object, ctx)?;

                match ob_instance.ty.clone() {
                    Type::This { ref name, .. } | Type::Class(ref name, _, _, _) => {
                        if let Some(ty) = ctx.look_type(*name).cloned() {
                            match ty {
                                Type::This {
                                    ref methods,
                                    ref fields,
                                    ..
                                }
                                | Type::Class(_, ref fields, ref methods, _) => {
                                    let value_span = value.span;
                                    let value_ty = self.infer_expr(*value, ctx)?;

                                    for (field_name, field_ty) in fields {
                                        if field_name == &property.value {
                                            self.unify(&value_ty.ty, field_ty, value_span, ctx)?;
                                            return Ok((
                                                t::Expression::Set(
                                                    property.value,
                                                    ob_instance,
                                                    value_ty,
                                                ),
                                                field_ty.clone(),
                                            ));
                                        }
                                    }

                                    for (method_name, method_ty) in methods {
                                        if method_name == &property.value {
                                            let ty = method_ty.clone().get_ty();

                                            self.unify(&value_ty.ty, &ty, value_span, ctx)?;
                                            return Ok((
                                                t::Expression::Set(
                                                    property.value,
                                                    ob_instance,
                                                    value_ty,
                                                ),
                                                ty,
                                            ));
                                        }
                                    }
                                }
                                _ => unreachable!(),
                            }
                        }

                        let msg = format!(
                            "class `{}` doesn't have a field named `{}`",
                            ctx.name(*name),
                            ctx.name(property.value)
                        );

                        ctx.error(msg, expr.span);

                        Err(())
                    }

                    ref other_ty => {
                        let msg = format!(
                            "Type {} dosen't have the method/field {}",
                            other_ty.print(ctx),
                            ctx.name(property.value)
                        );

                        ctx.error(msg, property.span);
                        return Err(());
                    }
                }
            }
            _ => unreachable!(),
        }
    }
}
