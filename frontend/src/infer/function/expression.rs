use ast as t;
use ctx::CompileCtx;
use infer::env::VarEntry;
use infer::types::Type;
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
            Expression::Array { mut items } => {
                if items.is_empty() {
                    (
                        Spanned::new(t::Expression::Array(vec![]), expr.span),
                        Type::Array(Box::new(Type::Nil)),
                    )
                } else {
                    let mut nitems = vec![self.infer_expr(items.remove(0), ctx)?];

                    for item in items.into_iter().skip(1) {
                        let span = item.span;
                        let ty_expr = self.infer_expr(item, ctx)?;

                        self.unify(&nitems[0].value.ty, &ty_expr.value.ty, span, ctx)?;
                        nitems.push(ty_expr);
                    }

                    let ret_ty = nitems[0].value.ty.clone();

                    (
                        Spanned::new(t::Expression::Array(nitems), expr.span),
                        Type::Array(Box::new(ret_ty)),
                    )
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
                        self.unify(&ty, &value_ty.value.ty, span, ctx)?;
                    }
                    MinusEqual | PlusEqual | StarEqual | SlashEqual => {
                        match self.unify(&ty, &value_ty.value.ty, span, ctx) {
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

                let ty = value_ty.value.ty.clone();

                (
                    Spanned::new(
                        t::Expression::Assign(name.value, kind.value, value_ty),
                        expr.span,
                    ),
                    ty,
                )
            }

            Expression::Binary { lhs, op, rhs } => {
                let span = lhs.span.to(rhs.span);

                let lhs = self.infer_expr(*lhs, ctx)?;
                let rhs = self.infer_expr(*rhs, ctx)?;

                match op.value {
                    Op::BangEqual | Op::EqualEqual => (
                        Spanned::new(t::Expression::Binary(lhs, op.value, rhs), expr.span),
                        Type::Bool,
                    ),

                    Op::LessThan
                    | Op::LessThanEqual
                    | Op::GreaterThan
                    | Op::GreaterThanEqual
                    | Op::And
                    | Op::Or => {
                        self.unify(&lhs.value.ty, &rhs.value.ty, span, ctx)?;

                        (
                            Spanned::new(t::Expression::Binary(lhs, op.value, rhs), expr.span),
                            Type::Bool,
                        )
                    }

                    Op::Plus | Op::Slash | Op::Star | Op::Minus | Op::Modulo | Op::Exponential => {
                        match self.unify(&lhs.value.ty, &rhs.value.ty, span, ctx) {
                            Ok(()) => (),
                            Err(_) => match self.unify(&lhs.value.ty, &Type::Str, span, ctx) {
                                Ok(()) => (),
                                Err(_) => {
                                    ctx.remove_error();
                                    return Err(());
                                }
                            },
                        }

                        let ty = lhs.value.ty.clone();

                        (
                            Spanned::new(t::Expression::Binary(lhs, op.value, rhs), expr.span),
                            ty,
                        )
                    }
                }
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

                let fn_signature = Type::Fun(env_types.clone(), Box::new(returns.clone()),true);

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
                let span = expr.span;
                let ty_expr = self.infer_expr(*expr, ctx)?;
                let ty = ty_expr.value.ty.clone();

                (Spanned::new(t::Expression::Grouping(ty_expr), span), ty)
            }

            Expression::Get { .. } => self.infer_object_get(expr, ctx)?,

            Expression::SubScript { target, index } => {
                let target_span = target.span;

                match target.value {
                    Expression::Var(symbol) => {
                        let target_ty = self.infer_var(&symbol, ctx)?;

                        let span = index.span;

                        let index_ty = self.infer_expr(*index, ctx)?;

                        self.unify(&index_ty.value.ty, &Type::Int, span, ctx)?;

                        match target_ty {
                            Type::Array(ref ty) => (
                                Spanned::new(t::Expression::Index(Expression::Var(symbol,index_ty.clone()), index_ty), span),
                                *ty.clone(),
                            ),
                            Type::Str => {
                                let var = Spanned::new(t::Expression::Var(symbol.value,index_ty.value.ty.clone()), target_span);

                                 (
                                    Spanned::new(t::Expression::Index(var,index_ty), span),
                                    Type::Str,
                                )
                            },

                            _ => {
                                let msg = format!(" Cannot index type `{}` ", target_ty.print(ctx));
                                ctx.error(msg, target_span);
                                return Err(());
                            }
                        }
                    }
                    _ => {
                        let expr = self.infer_expr(*target,ctx)?;

                        match expr.value.ty {
                            Type::Array(ref ty) => {
                                unimplemented!()
                            },

                            _ => {
                                 ctx.error("Invalid index target",target_span);
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

                self.unify(&Type::Bool, &cond_tyexpr.value.ty, span, ctx)?;

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

            Expression::This => (
                Spanned::new(t::Expression::This, expr.span),
                self.this.clone(),
            ),
            Expression::Unary { expr: operand, op } => {
                let whole_span = expr.span;
                let span = operand.span;
                let expr = operand;
                let expr = self.infer_expr(*expr, ctx)?;

                match op.value {
                    UnaryOp::Bang => (
                        Spanned::new(t::Expression::Unary(op.value, expr), span),
                        Type::Bool,
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
                            Type::Fun(ref targs, ref ret,ref is_closure) => {
                                use util::pos::Span;
                                if args.len() != targs.len() {
                                    let msg = format!(
                                        "Expected `{}` args found `{}` ",
                                        targs.len(),
                                        args.len()
                                    );
                                    ctx.error(msg, call.span);
                                    return Err(());
                                }

                                let mut arg_tys: Vec<(Span, Type)> = Vec::with_capacity(args.len());
                                let mut callee_exprs = Vec::with_capacity(args.len());

                                for arg in args {
                                    let span = arg.span;
                                    let ty_expr = self.infer_expr(arg, ctx)?;
                                    arg_tys.push((span, ty_expr.value.ty.clone()));
                                    callee_exprs.push(ty_expr)
                                }

                                for (span, arg_ty) in arg_tys.iter() {
                                    for def_ty in targs.iter() {
                                        self.unify(arg_ty, def_ty, *span, ctx)?;
                                    }
                                }

                                Ok((
                                    Spanned::new(
                                        t::Expression::Call(sym.value, callee_exprs),
                                        call.span,
                                    ),
                                    if *is_closure {
                                        ty.clone()
                                    }else {
                                        *ret.clone()
                                    }
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

                    let fn_signature = Type::Fun(env_types.clone(), Box::new(returns.clone()),true);

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
            Literal::Float(_) => Type::Float,

            Literal::False(_) | Literal::True(_) => Type::Bool,

            Literal::Str(_) => Type::Str,

            Literal::Nil => Type::Nil, // Nil is given the type void as only statements return Nil

            Literal::Int(_) => Type::Int,
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
                    Type::This { ref fields, .. } | Type::Class(_, ref fields, _, _) => {
                        let mut instance_exprs = Vec::new();
                        let mut found = false;

                        for (prop, prop_ty) in props.into_iter().zip(fields.iter()) {
                            if &prop.value.symbol.value == prop_ty.0 {
                                found = true;

                                let span = prop.span;

                                let ty = self.infer_expr(prop.value.expr, ctx)?;

                                self.unify(&prop_ty.1, &ty.value.ty, span, ctx)?;

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
                                                Spanned::new(
                                                    t::Expression::Get(property.value, ob_instance),
                                                    expr.span,
                                                ),
                                                field_ty.clone(),
                                            ));
                                        }
                                    }

                                    for (method_name, method_ty) in methods {
                                        if method_name == &property.value {
                                            let ty = method_ty.clone().get_ty();
                                            let ty = match ty {
                                                Type::Fun(_, ret,_) => *ret,
                                                _ => unreachable!(),
                                            };

                                            return Ok((
                                                Spanned::new(
                                                    t::Expression::Get(property.value, ob_instance),
                                                    expr.span,
                                                ),
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
