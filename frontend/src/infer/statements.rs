use ctx::CompileCtx;
use syntax::ast::expr::Literal;
use syntax::ast::statement::Statement;
use util::pos::Spanned;

use ast as t;
use infer::env::{Entry, VarEntry};
use infer::types::{Type, Unique};
use infer::{Infer, InferResult};

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
                        //TODO: IMPLEMENT SUPERCLASSES
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
