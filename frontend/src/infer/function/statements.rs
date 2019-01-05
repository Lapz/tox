use ast as t;
use ctx::CompileCtx;
use infer::env::VarEntry;
use infer::{Infer, InferResult};
use ir::types::{Type, TypeCon};
use syntax::ast::{Literal, Statement};
use util::pos::Spanned;

impl Infer {
    pub fn infer_statement(
        &mut self,
        statement: Spanned<Statement>,
        ctx: &mut CompileCtx,
    ) -> InferResult<Spanned<t::Statement>> {
        match statement.value {
            Statement::Block(statements) => {
                if statements.is_empty() {
                    return Ok(Spanned::new(
                        t::Statement::Expr(Spanned::new(
                            t::TypedExpression {
                                expr: Box::new(Spanned::new(
                                    t::Expression::Literal(Literal::Nil),
                                    statement.span,
                                )),
                                ty: Type::Nil,
                            },
                            statement.span,
                        )),
                        statement.span,
                    ));
                }

                ctx.begin_scope();

                let mut new_statements = Vec::with_capacity(statements.len());

                for statement in statements {
                    new_statements.push(self.infer_statement(statement, ctx)?);
                }

                ctx.end_scope();

                Ok(Spanned::new(
                    t::Statement::Block(new_statements),
                    statement.span,
                ))
            }
            Statement::Break => Ok(Spanned::new(t::Statement::Break, statement.span)),
            Statement::Continue => Ok(Spanned::new(t::Statement::Continue, statement.span)),
            Statement::Expr(expr) => {
                let type_expr = self.infer_expr(expr, ctx)?;

                Ok(Spanned::new(t::Statement::Expr(type_expr), statement.span)) // Expressions are given the type of Nil to signify that they return nothing
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
                let mut span = body.span;

                if let Some(init) = init {
                    block.push(self.infer_statement(*init, ctx)?);
                }

                let mut while_block = vec![self.infer_statement(*body, ctx)?];

                if let Some(incr) = incr {
                    let span = incr.span;
                    let ty = self.infer_expr(incr, ctx)?;
                    if !ty.value.ty.is_int() {
                        match ty.value.ty {
                            _ => {
                                let msg = format!(
                                    "Increment cannot be of type `{}`",
                                    ty.value.ty.print(ctx.symbols())
                                );

                                ctx.error(msg, span);
                                return Err(());
                            }
                        }
                    }

                    while_block.push(Spanned::new(t::Statement::Expr(ty), span))
                }

                if let Some(cond) = cond {
                    let span = cond.span;
                    let ty = self.infer_expr(cond, ctx)?;

                    self.unify(&Type::App(TypeCon::Bool, vec![]), &ty.value.ty, span, ctx)?;

                    block.push(Spanned::new(
                        t::Statement::While(
                            ty,
                            Box::new(Spanned::new(t::Statement::Block(while_block), span)),
                        ),
                        span,
                    ))
                } else {
                    block.push(Spanned::new(
                        t::Statement::While(
                            Spanned::new(
                                t::TypedExpression {
                                    expr: Box::new(Spanned::new(
                                        t::Expression::Literal(Literal::True(true)),
                                        span,
                                    )),
                                    ty: Type::App(TypeCon::Bool, vec![]),
                                },
                                span,
                            ),
                            Box::new(Spanned::new(t::Statement::Block(while_block), span)),
                        ),
                        span,
                    ));
                }

                Ok(Spanned::new(t::Statement::Block(block), span))
            }

            Statement::If {
                cond,
                then,
                otherwise,
            } => {
                let span = cond.span;
                let cond_tyexpr = self.infer_expr(cond, ctx)?;
                self.unify(
                    &Type::App(TypeCon::Bool, vec![]),
                    &cond_tyexpr.value.ty,
                    span,
                    ctx,
                )?;

                let then_tyexpr = Box::new(self.infer_statement(*then, ctx)?);
                let mut otherwise_tyexpr = None;

                if let Some(otherwise) = otherwise {
                    let tyexpr = Box::new(self.infer_statement(*otherwise, ctx)?);

                    otherwise_tyexpr = Some(tyexpr)
                }

                Ok(Spanned::new(
                    t::Statement::If {
                        cond: cond_tyexpr,
                        then: then_tyexpr,
                        otherwise: otherwise_tyexpr,
                    },
                    statement.span,
                ))
            }

            Statement::Print(expr) => {
                let mut type_expr = self.infer_expr(expr, ctx)?;

                type_expr.value.ty = Type::Nil; // print expressions are given the type of Nil to signify that they return nothing

                Ok(Spanned::new(t::Statement::Print(type_expr), statement.span))
            }

            Statement::While { cond, body } => {
                let span = cond.span;
                let expr = self.infer_expr(cond, ctx)?;
                self.unify(&Type::App(TypeCon::Bool, vec![]), &expr.value.ty, span, ctx)?;

                Ok(Spanned::new(
                    t::Statement::While(expr, Box::new(self.infer_statement(*body, ctx)?)),
                    statement.span,
                ))
            }

            Statement::VarDeclaration { ident, ty, expr } => {
                if let Some(expr) = expr {
                    let expr_tyexpr = self.infer_expr(expr, ctx)?;

                    if let Some(ty) = ty {
                        let t = self.trans_type(&ty, ctx)?;

                        self.unify(&expr_tyexpr.value.ty, &t, ty.span, ctx)?;

                        ctx.add_var(ident.value, VarEntry::Var(t.clone()));

                        return Ok(Spanned::new(
                            t::Statement::Var {
                                ident: ident.value,
                                ty: t,
                                expr: Some(expr_tyexpr),
                            },
                            statement.span,
                        ));
                    }

                    ctx.add_var(ident.value, VarEntry::Var(expr_tyexpr.value.ty.clone()));

                    Ok(Spanned::new(
                        t::Statement::Var {
                            ident: ident.value,
                            ty: expr_tyexpr.value.ty.clone(),
                            expr: Some(expr_tyexpr),
                        },
                        statement.span,
                    ))
                } else {
                    if let Some(ty) = ty {
                        let ty = self.trans_type(&ty, ctx)?;

                        ctx.add_var(ident.value, VarEntry::Var(ty.clone()));

                        return Ok(Spanned::new(
                            t::Statement::Var {
                                ident: ident.value,
                                ty,
                                expr: None,
                            },
                            statement.span,
                        ));
                    }

                    ctx.add_var(ident.value, VarEntry::Var(Type::Nil));

                    Ok(Spanned::new(
                        t::Statement::Var {
                            ident: ident.value,
                            ty: Type::Nil,
                            expr: None,
                        },
                        statement.span,
                    ))
                }
            }

            Statement::Return(expr) => {
                let type_expr = self.infer_expr(expr, ctx)?;

                self.body = type_expr.value.ty.clone();
                Ok(Spanned::new(
                    t::Statement::Return(type_expr),
                    statement.span,
                )) // Expressions are given the type of Nil to signify that they return nothing
            }
        }
    }
}
