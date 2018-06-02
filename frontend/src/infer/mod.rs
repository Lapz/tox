// mod inferexp;
// mod inferstatement;
mod types;
use super::{Infer, InferResult};
use ast as t;
use ctx::CompileCtx;
use env::{Entry, VarEntry};
// use syntax::ast::expr::Expression;
use syntax::ast::expr::{Expression, Literal, Op, UnaryOp};
use syntax::ast::statement::Statement;
use types::{Type, Unique};
use util::pos::Spanned;

impl Infer {
    pub fn infer_statement(
        &mut self,
        statement: Spanned<Statement>,
        ctx: &mut CompileCtx,
    ) -> InferResult<t::Statement> {
        match statement.value {
            Statement::Block(statements) => {
                if statements.is_empty() {
                    return Ok(t::Statement::Expr(t::TypedExpression {
                        expr: Box::new(t::Expression::Literal(Literal::Nil)),
                        ty: Type::Nil,
                    }));
                }

                ctx.begin_scope();

                let mut new_statements = Vec::with_capacity(statements.len());

                for statement in statements {
                    new_statements.push(self.infer_statement(statement, ctx)?);
                }

                ctx.end_scope();

                Ok(t::Statement::Block(new_statements))
            }

            Statement::Break => Ok(t::Statement::Break),
            Statement::Class {
                name,
                superclass,
                body,
            } => {
                use std::collections::HashMap;

                let mut field_types = HashMap::new();
                let mut methods_types = HashMap::new();
                let mut methods = Vec::new();
                let mut fields = Vec::new();

                if let Some(sclass) = superclass {
                    if let Some(mut entry) = ctx.look_type(sclass.value) {
                        println!("{:?}", entry)
                    }

                    // match sclass {
                    //     _ => ()
                    // }
                }

                for field in &body.value.1 {
                    let ty = self.trans_type(&field.value.ty, ctx)?;
                    fields.push(t::Field {
                        name: field.value.name.value,
                        ty: ty.clone(),
                    });
                    field_types.insert(field.value.name.value, ty);
                }

                self.this = Type::This {
                    name: name.value,
                    fields: field_types.clone(),
                    methods: HashMap::new(),
                };

                ctx.add_type(
                    name.value,
                    Type::Class(
                        name.value,
                        field_types.clone(),
                        HashMap::new(),
                        Unique::new(),
                    ),
                );

                for method in body.value.0 {
                    if let Statement::Function {
                        name,
                        body,
                        params,
                        returns,
                    } = method.value
                    {
                        println!("{}", self.this.print(ctx));
                        let returns = if let Some(ty) = returns {
                            self.trans_type(&ty, ctx)?
                        } else {
                            Type::Nil
                        };

                        match self.this {
                            Type::This {
                                ref mut methods, ..
                            } => {
                                methods.insert(name.value, Entry::Ty(returns.clone())); // Allows for use of methods on this
                            }
                            _ => unreachable!(),
                        }

                        let mut param_types = Vec::with_capacity(params.value.len());
                        let mut env_types = Vec::with_capacity(params.value.len());

                        for param in &params.value {
                            let ty = self.trans_type(&param.value.ty, ctx)?;

                            env_types.push(ty.clone());
                            param_types.push(t::FunctionParam {
                                name: param.value.name.value,
                                ty,
                            })
                        }

                        ctx.begin_scope();

                        for param in param_types.iter() {
                            ctx.add_var(param.name, VarEntry::Var(param.ty.clone()))
                        }

                        let span = body.span;
                        let body = self.infer_statement(*body, ctx)?;

                        ctx.end_scope();

                        self.unify(&returns, &self.body, span, ctx)?;

                        methods_types.insert(
                            name.value,
                            Entry::Fun(Type::Fun(
                                param_types
                                    .clone()
                                    .into_iter()
                                    .map(|param| param.ty)
                                    .collect(),
                                Box::new(returns.clone()),
                            )),
                        );

                        methods.push(t::Statement::Function {
                            name: name.value,
                            params: param_types,
                            body: Box::new(body),
                            returns: returns,
                        })
                    }
                }

                let ty = Type::Class(name.value, field_types, methods_types, Unique::new());

                self.this = ty.clone();

                ctx.add_type(name.value, ty);

                Ok(t::Statement::Class {
                    name: name.value,
                    methods,
                    fields,
                })
            }
            Statement::Continue => Ok(t::Statement::Continue),
            Statement::Expr(expr) => {
                let type_expr = self.infer_expr(expr, ctx)?;

                Ok(t::Statement::Expr(type_expr)) // Expressions are given the type of Nil to signify that they return nothing
            }

            Statement::Function {
                name,
                params,
                body,
                returns,
            } => {
                let returns = if let Some(ty) = returns {
                    self.trans_type(&ty, ctx)?
                } else {
                    Type::Nil
                };

                let mut param_types = Vec::with_capacity(params.value.len());
                let mut env_types = Vec::with_capacity(params.value.len());

                for param in &params.value {
                    let ty = self.trans_type(&param.value.ty, ctx)?;

                    env_types.push(ty.clone());
                    param_types.push(t::FunctionParam {
                        name: param.value.name.value,
                        ty,
                    })
                }

                ctx.add_var(
                    name.value,
                    VarEntry::Fun {
                        ty: Type::Fun(env_types.clone(), Box::new(returns.clone())),
                    },
                );

                ctx.begin_scope();

                for param in param_types.iter() {
                    ctx.add_var(param.name, VarEntry::Var(param.ty.clone()))
                }

                let span = body.span;
                let body = self.infer_statement(*body, ctx)?;

                ctx.end_scope();

                self.unify(&returns, &self.body, span, ctx)?;

                Ok(t::Statement::Function {
                    name: name.value,
                    params: param_types,
                    body: Box::new(body),
                    returns: returns,
                })
            }
            Statement::For {
                init,
                cond,
                incr,
                body,
            } => {
                if init.is_none() && cond.is_none() && incr.is_none() {
                    let body = self.infer_statement(*body, ctx)?;
                    return Ok(body);
                }

                let mut block = vec![];

                if let Some(init) = init {
                    block.push(self.infer_statement(*init, ctx)?);
                }

                let mut while_block = vec![self.infer_statement(*body, ctx)?];

                if let Some(incr) = incr {
                    let span = incr.span;
                    let ty = self.infer_expr(incr, ctx)?;
                    if !ty.ty.is_int() {
                        match ty.ty {
                            _ => {
                                let msg =
                                    format!("Increment cannot be of type `{}`", ty.ty.print(ctx));

                                ctx.error(msg, span);
                                return Err(());
                            }
                        }
                    }

                    while_block.push(t::Statement::Expr(ty))
                }

                if let Some(cond) = cond {
                    let span = cond.span;
                    let ty = self.infer_expr(cond, ctx)?;

                    self.unify(&Type::Bool, &ty.ty, span, ctx)?;

                    block.push(t::Statement::While(
                        ty,
                        Box::new(t::Statement::Block(while_block)),
                    ))
                } else {
                    block.push(t::Statement::While(
                        t::TypedExpression {
                            expr: Box::new(t::Expression::Literal(Literal::True(true))),
                            ty: Type::Bool,
                        },
                        Box::new(t::Statement::Block(while_block)),
                    ));
                }

                Ok(t::Statement::Block(block))
            }

            Statement::If {
                cond,
                then,
                otherwise,
            } => {
                let span = cond.span;
                let cond_tyexpr = self.infer_expr(cond, ctx)?;
                self.unify(&Type::Bool, &cond_tyexpr.ty, span, ctx)?;

                let then_tyexpr = Box::new(self.infer_statement(*then, ctx)?);
                let mut otherwise_tyexpr = None;

                if let Some(otherwise) = otherwise {
                    let tyexpr = Box::new(self.infer_statement(*otherwise, ctx)?);

                    otherwise_tyexpr = Some(tyexpr)
                }

                Ok(t::Statement::If {
                    cond: cond_tyexpr,
                    then: then_tyexpr,
                    otherwise: otherwise_tyexpr,
                })
            }

            Statement::Print(expr) => {
                let type_expr = self.infer_expr(expr, ctx)?;

                Ok(t::Statement::Print(type_expr)) // Expressions are given the type of Nil to signify that they return nothing
            }

            Statement::While { cond, body } => {
                let span = cond.span;
                let expr = self.infer_expr(cond, ctx)?;
                self.unify(&Type::Bool, &expr.ty, span, ctx)?;

                Ok(t::Statement::While(
                    expr,
                    Box::new(self.infer_statement(*body, ctx)?),
                ))
            }

            Statement::Var { ident, ty, expr } => {
                if let Some(expr) = expr {
                    let expr_tyexpr = self.infer_expr(expr, ctx)?;

                    if let Some(ty) = ty {
                        let t = self.trans_type(&ty, ctx)?;

                        self.unify(&expr_tyexpr.ty, &t, ty.span, ctx)?;

                        ctx.add_var(ident.value, VarEntry::Var(t.clone()));

                        return Ok(t::Statement::Var {
                            ident: ident.value,
                            ty: t,
                            expr: Some(expr_tyexpr),
                        });
                    }

                    ctx.add_var(ident.value, VarEntry::Var(expr_tyexpr.ty.clone()));

                    Ok(t::Statement::Var {
                        ident: ident.value,
                        ty: expr_tyexpr.ty.clone(),
                        expr: Some(expr_tyexpr),
                    })
                } else {
                    if let Some(ty) = ty {
                        let ty = self.trans_type(&ty, ctx)?;

                        ctx.add_var(ident.value, VarEntry::Var(ty.clone()));

                        return Ok(t::Statement::Var {
                            ident: ident.value,
                            ty,
                            expr: None,
                        });
                    }

                    ctx.add_var(ident.value, VarEntry::Var(Type::Nil));

                    Ok(t::Statement::Var {
                        ident: ident.value,
                        ty: Type::Nil,
                        expr: None,
                    })
                }
            }

            Statement::Return(expr) => {
                let type_expr = self.infer_expr(expr, ctx)?;

                self.body = type_expr.ty.clone();
                Ok(t::Statement::Return(type_expr)) // Expressions are given the type of Nil to signify that they return nothing
            }

            Statement::TypeAlias { .. } => unreachable!("Should be handled by infer_tyalias"),
        }
    }
}

impl Infer {
    fn infer_expr(
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

                println!("{}", ctx.name(name.value));

                let ty = self.infer_var(&name, ctx)?;
                let value_ty = self.infer_expr(*value, ctx)?;
                use syntax::ast::expr::AssignOperator::*;
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

            Expression::ClassInstance { .. } => self.infer_class_instance(expr, ctx)?,

            Expression::Grouping { expr } => {
                let ty_expr = self.infer_expr(*expr, ctx)?;
                let ty = ty_expr.ty.clone();

                (t::Expression::Grouping(ty_expr), ty)
            }

            Expression::Get { .. } => self.infer_object_get(expr, ctx)?,

            Expression::Index { target, index } => match target.value {
                Expression::Var(symbol, _) => {
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
                            ctx.error(msg, target.span);
                            return Err(());
                        }
                    }
                }
                _ => {
                    ctx.error("Invalid index target", target.span);
                    return Err(());
                }
            },
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

            Expression::This(_) => (t::Expression::This, self.this.clone()),
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

            Expression::Var(ref var, _) => {
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
                Expression::Var(ref sym, _) => {
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
                    Type::This {

                        ref fields,
                        ..
                    }
                    | Type::Class(_,ref fields, _, _) => {
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
                    Type::This {
                        ref name,
                        ref methods,
                        ref fields,
                    }
                    | Type::Class(ref name, ref fields, ref methods, _) => {
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
                                let ty = method_ty.clone();
                                return Ok((
                                    t::Expression::Get(property.value, ob_instance),
                                    ty.get_ty(), // Change to return the return type
                                ));
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
                    Type::This {
                        ref name,
                        ref methods,
                        ref fields,
                    }
                    | Type::Class(ref name, ref fields, ref methods, _) => {
                        let value_span = value.span;
                        let value_ty = self.infer_expr(*value, ctx)?;

                        for (field_name, field_ty) in fields {
                            if field_name == &property.value {
                                self.unify(&value_ty.ty, field_ty, value_span, ctx)?;
                                return Ok((
                                    t::Expression::Set(property.value, ob_instance, value_ty),
                                    field_ty.clone(),
                                ));
                            }
                        }

                        for (method_name, method_ty) in methods {
                            if method_name == &property.value {
                                let ty = method_ty.clone().get_ty();

                                self.unify(&value_ty.ty, &ty, value_span, ctx)?;
                                return Ok((
                                    t::Expression::Set(property.value, ob_instance, value_ty),
                                    ty,
                                ));
                            }
                        }

                        println!("{:?}", ob_instance.ty.print(ctx));

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
    // add code here
}
