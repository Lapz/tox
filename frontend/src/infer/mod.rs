// mod inferexp;
// mod inferstatement;
mod types;
use super::{Infer, InferResult};
use ast as t;
use ctx::CompileCtx;
use env::VarEntry;
// use syntax::ast::expr::Expression;
use syntax::ast::expr::{Expression, Literal, UnaryOp};
use syntax::ast::statement::Statement;
use types::Type;
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
            Statement::Class { .. } => unreachable!("Should be handled by infer_class"),
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
                let mut trans_types = Vec::with_capacity(params.value.len());

                for param in &params.value {
                    trans_types.push(t::FunctionParam {
                        name: param.value.name.value,
                        ty: self.trans_type(&param.value.ty, ctx)?,
                    })
                }

                let body = self.infer_statement(*body, ctx)?;

                let returns = if let Some(ty) = returns {
                    self.trans_type(&ty, ctx)?
                } else {
                    Type::Nil
                };

                Ok(t::Statement::Function {
                    name: name.value,
                    params: trans_types,
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

                        unimplemented!()
                    }
                }

                let ty = value_ty.ty.clone();

                (t::Expression::Assign(name.value, kind.value, value_ty), ty)
            }

            Expression::Grouping { expr } => {
                let ty_expr = self.infer_expr(*expr, ctx)?;
                let ty = ty_expr.ty.clone();

                (t::Expression::Grouping(ty_expr), ty)
            },
            Expression::Literal(literal) => {
                let ty = self.infer_literal(&literal);
                (t::Expression::Literal(literal), ty)
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
            
            _ => unimplemented!(),
        };

        Ok(t::TypedExpression {
            expr: Box::new(typed),
            ty,
        })
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
    // add code here
}
