use ast as t;
use ctx::CompileCtx;
use infer::env::VarEntry;
use infer::types::{Type, TypeCon};
use infer::{Infer, InferResult};

use syntax::ast::{Expression, Literal, Op, UnaryOp};
use util::pos::Spanned;

impl Infer {
    pub(crate) fn infer_expr(
        &mut self,
        expr: Spanned<Expression>,
        ctx: &mut CompileCtx,
    ) -> InferResult<Spanned<t::TypedExpression>> {
        let whole_span = expr.span;
        let (typed, ty) = match expr.value {
            Expression::Array { mut items } => return self.infer_array(items, whole_span, ctx),

            Expression::Assign {
                name, kind, value, ..
            } => return self.infer_assign(name, kind, value, expr.span, ctx),

            Expression::Binary { lhs, op, rhs } => {
                return self.infer_binary(lhs, op, rhs, expr.span, ctx);
            }

            Expression::Call { .. } => self.infer_call(expr, ctx)?,

            Expression::Closure(function) => {
                let returns = if let Some(ref ty) = function.value.returns {
                    self.trans_type(&ty, ctx)?
                } else {
                    Type::Nil
                };

                let mut param_types = Vec::with_capacity(function.value.params.value.len());
                let mut env_types = Vec::with_capacity(function.value.params.value.len());

                for param in function.value.params.value.iter() {
                    let ty = self.trans_type(&param.value.ty, ctx)?;

                    env_types.push(ty.clone());
                    param_types.push(t::FunctionParam {
                        name: param.value.name.value,
                        ty,
                    })
                }

                let fn_signature = Type::Fun(env_types.clone(), Box::new(returns.clone()), true);

                ctx.add_var(
                    function.value.name.value,
                    VarEntry::Fun {
                        ty: fn_signature.clone(),
                    },
                );

                ctx.begin_scope();

                for param in param_types.iter() {
                    ctx.add_var(param.name, VarEntry::Var(param.ty.clone()))
                }

                let span = function.value.body.span;
                let name = function.value.name.value;
                let body = self.infer_statement(function.value.body, ctx)?;

                ctx.end_scope();

                (
                    Spanned::new(
                        t::Expression::Closure(Box::new(t::Function {
                            name,
                            params: param_types,
                            body: Box::new(body),
                            returns: returns.clone(),
                        })),
                        span,
                    ),
                    fn_signature,
                )
            }

            Expression::ClassInstance { .. } => self.infer_class_instance(expr, ctx)?,

            Expression::Grouping { expr } => {
                return self.infer_grouping(*expr,expr.span,ctx);
            }

            Expression::Get { .. } => self.infer_object_get(expr, ctx)?,

            Expression::SubScript { target, index } => {
                let target_span = target.span;
                let expr_span = expr.span;

                match target.value {
                    Expression::Var(symbol) => {
                        let target_ty = self.infer_var(&symbol, ctx)?;

                        let span = index.span;

                        let index_ty = self.infer_expr(*index, ctx)?;

                        self.unify(
                            &index_ty.value.ty,
                            &Type::App(TypeCon::Int, vec![]),
                            span,
                            ctx,
                        )?;

                        match target_ty {
                            Type::App(TypeCon::Array(ref ty), _) => {
                                let var = Spanned::new(
                                    t::TypedExpression {
                                        expr: Box::new(Spanned::new(
                                            t::Expression::Var(
                                                symbol.value,
                                                index_ty.value.ty.clone(),
                                            ),
                                            target_span,
                                        )),
                                        ty: index_ty.value.ty.clone(),
                                    },
                                    target_span,
                                );

                                (
                                    Spanned::new(t::Expression::Index(var, index_ty), span),
                                    *ty.clone(),
                                )
                            }
                            Type::App(TypeCon::Str, _) => {
                                let var = Spanned::new(
                                    t::TypedExpression {
                                        expr: Box::new(Spanned::new(
                                            t::Expression::Var(
                                                symbol.value,
                                                index_ty.value.ty.clone(),
                                            ),
                                            target_span,
                                        )),
                                        ty: index_ty.value.ty.clone(),
                                    },
                                    target_span,
                                );

                                (
                                    Spanned::new(t::Expression::Index(var, index_ty), span),
                                    Type::App(TypeCon::Str, vec![]),
                                )
                            }

                            _ => {
                                let msg = format!(" Cannot index type `{}` ", target_ty.print(ctx));
                                ctx.error(msg, target_span);
                                return Err(());
                            }
                        }
                    }

                    _ => {
                        //array expression or things that evalu to an array
                        let expr = self.infer_expr(*target, ctx)?;

                        match expr.value.ty.clone() {
                            Type::App(TypeCon::Array(ref ty), _) => {
                                let index_ty = self.infer_expr(*index, ctx)?;

                                (
                                    Spanned::new(t::Expression::Index(expr, index_ty), expr_span),
                                    *ty.clone(),
                                )
                            }

                            _ => {
                                ctx.error("Invalid index target", target_span);
                                return Err(());
                            }
                        }
                    }
                }
            }

            Expression::Literal(literal) => {
                let ty = self.infer_literal(&literal);
                (Spanned::new(t::Expression::Literal(literal), expr.span), ty)
            }

            Expression::Set { .. } => self.infer_object_set(expr, ctx)?,

            Expression::Ternary {
                condition,
                then_branch,
                else_branch,
            } => {
                let span = condition.span;
                let cond_tyexpr = self.infer_expr(*condition, ctx)?;

                self.unify(
                    &Type::App(TypeCon::Bool, vec![]),
                    &cond_tyexpr.value.ty,
                    span,
                    ctx,
                )?;

                let span = then_branch.span.to(else_branch.span);
                let then_tyexpr = self.infer_expr(*then_branch, ctx)?;
                let else_tyexpr = self.infer_expr(*else_branch, ctx)?;

                self.unify(&then_tyexpr.value.ty, &else_tyexpr.value.ty, span, ctx)?;
                let ty = then_tyexpr.value.ty.clone();

                (
                    Spanned::new(
                        t::Expression::Ternary(cond_tyexpr, then_tyexpr, else_tyexpr),
                        expr.span,
                    ),
                    ty,
                )
            }

            Expression::Unary { expr: operand, op } => {
                let whole_span = expr.span;
                let span = operand.span;
                let expr = operand;
                let expr = self.infer_expr(*expr, ctx)?;

                match op.value {
                    UnaryOp::Bang => (
                        Spanned::new(t::Expression::Unary(op.value, expr), span),
                        Type::App(TypeCon::Bool, vec![]),
                    ),
                    UnaryOp::Minus => {
                        if !expr.value.ty.is_int() && !expr.value.ty.is_float() {
                            let msg = format!(
                                "Cannot use `-` operator on type `{}`",
                                expr.value.ty.print(ctx)
                            );

                            ctx.error(msg, span);
                            return Err(());
                        }

                        let ty = expr.value.ty.clone();
                        (
                            Spanned::new(t::Expression::Unary(op.value, expr), whole_span),
                            ty,
                        )
                    }
                }
            }

            Expression::Var(ref var) => {
                let ty = self.infer_var(var, ctx)?;

                (
                    Spanned::new(t::Expression::Var(var.value, ty.clone()), expr.span),
                    ty,
                )
            }
        };

        Ok(Spanned::new(
            t::TypedExpression {
                expr: Box::new(typed),
                ty,
            },
            whole_span,
        ))
    }

    fn infer_call(
        &mut self,
        call: Spanned<Expression>,
        ctx: &mut CompileCtx,
    ) -> InferResult<(Spanned<t::Expression>, Type)> {
        match call.value {
            Expression::Call { callee, args } => match callee.value {
                Expression::Call { .. } => return self.infer_call(*callee, ctx),
                Expression::Var(ref sym) => {
                    if let Some(ty) = ctx.look_var(sym.value).cloned() {
                        let ty = ty.get_ty();

                        match ty {
                            Type::Fun(ref targs, ref ret, ref is_closure) => {
                                if args.len() != targs.len() {
                                    let msg = format!(
                                        "Expected `{}` args found `{}` ",
                                        targs.len(),
                                        args.len()
                                    );
                                    ctx.error(msg, call.span);
                                    return Err(());
                                }

                                let mut callee_exprs = Vec::with_capacity(args.len());

                                for (arg, param_type) in args.into_iter().zip(targs) {
                                    let span = arg.span;
                                    let ty_expr = self.infer_expr(arg, ctx)?;

                                    self.unify(param_type, &ty_expr.value.ty, span, ctx)?;

                                    callee_exprs.push(ty_expr)
                                }

                                Ok((
                                    Spanned::new(
                                        t::Expression::Call(sym.value, callee_exprs),
                                        call.span,
                                    ),
                                    if *is_closure {
                                        ty.clone()
                                    } else {
                                        *ret.clone()
                                    },
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

                Expression::Get { .. } => {
                    let (callee, ty) = self.infer_object_get(*callee, ctx)?;

                    match ty {
                        Type::Fun(ref param_types, ref returns, _) => {
                            if args.len() != param_types.len() {
                                let msg = format!(
                                    "Expected `{}` args found `{}` ",
                                    param_types.len(),
                                    args.len()
                                );
                                ctx.error(msg, call.span);
                                return Err(());
                            }

                            let mut callee_exprs = Vec::with_capacity(args.len());

                            for (arg, param_type) in args.into_iter().zip(param_types) {
                                let span = arg.span;
                                let ty_expr = self.infer_expr(arg, ctx)?;

                                self.unify(param_type, &ty_expr.value.ty, span, ctx)?;

                                callee_exprs.push(ty_expr)
                            }

                            match callee.value {
                                t::Expression::GetMethod {
                                    method_name,
                                    method,
                                } => match method.value.expr.value {
                                    t::Expression::Var(_, Type::Class(klass_name, _, _, _)) => {
                                        // type inference returns the type of the main classs
                                        Ok((
                                            Spanned::new(
                                                t::Expression::StaticMethodCall {
                                                    class_name: klass_name,
                                                    method_name,
                                                    params: callee_exprs,
                                                },
                                                call.span,
                                            ),
                                            *returns.clone(),
                                        ))
                                    }

                                    _ => Ok((
                                        Spanned::new(
                                            t::Expression::InstanceMethodCall {
                                                method_name,
                                                instance: method,
                                                params: callee_exprs,
                                            },
                                            call.span,
                                        ),
                                        *returns.clone(),
                                    )),
                                },

                                _ => unreachable!(),
                            }
                        }

                        _ => {
                            let msg = format!("Type {} is not callable", ty.print(ctx));
                            ctx.error(msg, callee.span);
                            return Err(());
                        }
                    }
                }
                Expression::Closure(function) => {
                    let returns = if let Some(ref ty) = function.value.returns {
                        self.trans_type(&ty, ctx)?
                    } else {
                        Type::Nil
                    };

                    let mut param_types = Vec::with_capacity(function.value.params.value.len());
                    let mut env_types = Vec::with_capacity(function.value.params.value.len());

                    for param in function.value.params.value.iter() {
                        let ty = self.trans_type(&param.value.ty, ctx)?;

                        env_types.push(ty.clone());
                        param_types.push(t::FunctionParam {
                            name: param.value.name.value,
                            ty,
                        })
                    }

                    let fn_signature =
                        Type::Fun(env_types.clone(), Box::new(returns.clone()), true);

                    ctx.add_var(
                        function.value.name.value,
                        VarEntry::Fun {
                            ty: fn_signature.clone(),
                        },
                    );

                    ctx.begin_scope();

                    for param in param_types.iter() {
                        ctx.add_var(param.name, VarEntry::Var(param.ty.clone()))
                    }

                    let span = function.value.body.span;
                    let name = function.value.name.value;
                    let body = self.infer_statement(function.value.body, ctx)?;

                    ctx.end_scope();

                    Ok((
                        Spanned::new(
                            t::Expression::Closure(Box::new(t::Function {
                                name,
                                params: param_types,
                                body: Box::new(body),
                                returns: returns.clone(),
                            })),
                            span,
                        ),
                        fn_signature,
                    )) // todo move to a method
                }
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

    fn infer_literal(&mut self, literal: &Literal) -> Type {
        match *literal {
            Literal::Float(_) => Type::App(TypeCon::Float, vec![]),

            Literal::False(_) | Literal::True(_) => Type::App(TypeCon::Bool, vec![]),

            Literal::Str(_) => Type::App(TypeCon::Str, vec![]),

            Literal::Nil => Type::Nil, // Nil is given the type void as only statements return Nil

            Literal::Int(_) => Type::App(TypeCon::Int, vec![]),
        }
    }

    fn infer_class_instance(
        &mut self,
        expr: Spanned<Expression>,
        ctx: &mut CompileCtx,
    ) -> InferResult<(Spanned<t::Expression>, Type)> {
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
                    Type::Class(_, ref fields, _, _) => {
                        let mut instance_exprs = Vec::new();
                        let mut unkown = false;

                        for prop in props.into_iter() {
                            if let Some(def_prop_ty) = fields.get(&prop.value.symbol.value) {
                                let span = prop.span;
                                let ident = prop.value.symbol.value;

                                let ty = self.infer_expr(prop.value.expr, ctx)?;

                                self.unify(&def_prop_ty, &ty.value.ty, span, ctx)?;

                                instance_exprs.push((ident, ty));
                            } else {
                                unkown = true;
                                let msg = format!(
                                    "`{}` is not a member of `{}` ",
                                    ctx.name(prop.value.symbol.value),
                                    ctx.name(symbol.value)
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
                        } else if unkown {
                            // encountered an unkown field
                            return Err(());
                        }

                        Ok((
                            Spanned::new(
                                t::Expression::ClassInstance(symbol.value, instance_exprs),
                                expr.span,
                            ),
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
        &mut self,
        expr: Spanned<Expression>,
        ctx: &mut CompileCtx,
    ) -> InferResult<(Spanned<t::Expression>, Type)> {
        match expr.value {
            Expression::Get { object, property } => {
                let ob_instance = self.infer_expr(*object, ctx)?;

                match ob_instance.value.ty.clone() {
                    Type::Class(ref name, _, _, _) => {
                        if let Some(ty) = ctx.look_type(*name).cloned() {
                            // Look at the conical type

                            match ty {
                                Type::Class(_, ref fields, ref methods, _) => {
                                    for (field_name, field_ty) in fields {
                                        if field_name == &property.value {
                                            return Ok((
                                                Spanned::new(
                                                    t::Expression::GetProperty {
                                                        property_name: property.value,
                                                        property: ob_instance,
                                                    },
                                                    expr.span,
                                                ),
                                                field_ty.clone(),
                                            ));
                                        }
                                    }

                                    for (method_name, method_ty) in methods {
                                        if method_name == &property.value {
                                            let ty = method_ty.clone().get_ty();

                                            return Ok((
                                                Spanned::new(
                                                    t::Expression::GetMethod {
                                                        method_name: property.value,
                                                        method: ob_instance,
                                                    },
                                                    expr.span,
                                                ),
                                                ty, // Change to return the return type
                                            ));
                                        }
                                    } // change to use a hashmap.get
                                }

                                _ => unreachable!(),
                            }
                        }

                        let msg = format!(
                            "class `{}` doesn't have a field/method named `{}`",
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
        &mut self,
        expr: Spanned<Expression>,
        ctx: &mut CompileCtx,
    ) -> InferResult<(Spanned<t::Expression>, Type)> {
        match expr.value {
            Expression::Set {
                object,
                name: property,
                value,
                ..
            } => {
                let ob_instance = self.infer_expr(*object, ctx)?;

                match ob_instance.value.ty.clone() {
                    Type::Class(ref name, _, _, _) => {
                        if let Some(ty) = ctx.look_type(*name).cloned() {
                            match ty {
                                Type::Class(_, ref fields, ref methods, _) => {
                                    let value_span = value.span;
                                    let value_ty = self.infer_expr(*value, ctx)?;

                                    for (field_name, field_ty) in fields {
                                        if field_name == &property.value {
                                            self.unify(
                                                &value_ty.value.ty,
                                                field_ty,
                                                value_span,
                                                ctx,
                                            )?;
                                            return Ok((
                                                Spanned::new(
                                                    t::Expression::Set(
                                                        property.value,
                                                        ob_instance,
                                                        value_ty,
                                                    ),
                                                    expr.span,
                                                ),
                                                field_ty.clone(),
                                            ));
                                        }
                                    }

                                    for (method_name, method_ty) in methods {
                                        if method_name == &property.value {
                                            let ty = method_ty.clone().get_ty();

                                            self.unify(&value_ty.value.ty, &ty, value_span, ctx)?;
                                            return Ok((
                                                Spanned::new(
                                                    t::Expression::Set(
                                                        property.value,
                                                        ob_instance,
                                                        value_ty,
                                                    ),
                                                    expr.span,
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
                            "class `{}` doesn't have a field/method named `{}`",
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
