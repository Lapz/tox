mod expression;
mod expressions;
mod statements;

use ast as t;
use ctx::CompileCtx;
use infer::env::VarEntry;
use infer::types::{Type, TypeCon, TypeVar};
use infer::{Infer, InferResult};
use syntax::ast::Function;
use util::pos::Spanned;

impl Infer {
    pub fn infer_function(
        &mut self,
        function: Spanned<Function>,
        ctx: &mut CompileCtx,
    ) -> InferResult<t::Function> {
        let mut poly_tvs = Vec::with_capacity(function.value.name.value.type_params.len()); // All type parameters

        for ident in &function.value.name.value.type_params {
            let tv = TypeVar::new();

            ctx.add_type(ident.value, Type::Var(tv));
            poly_tvs.push(tv);
        }

        let returns = if let Some(ref ty) = function.value.returns {
            self.trans_type(&ty, ctx)?
        } else {
            Type::Nil
        };

        let mut param_types = Vec::with_capacity(function.value.params.value.len());
        let mut env_types = Vec::with_capacity(function.value.params.value.len()); // types stored in token

        for param in function.value.params.value.iter() {
            let ty = self.trans_type(&param.value.ty, ctx)?;

            env_types.push(ty.clone());
            param_types.push(t::FunctionParam {
                name: param.value.name.value,
                ty,
            })
        }

        env_types.push(returns.clone()); // Return is the last value

        ctx.add_var(
            function.value.name.value.name.value,
            VarEntry::Fun {
                ty: Type::Generic(
                    poly_tvs.clone(),
                    Box::new(Type::App(TypeCon::Arrow, env_types.clone())),
                ),
            },
        );

        ctx.begin_scope();

        for param in param_types.iter() {
            ctx.add_var(param.name, VarEntry::Var(param.ty.clone()))
        }

        let mut span = function.value.body.span;
        let mut body = self.infer_statement(function.value.body, ctx)?;

        ctx.end_scope();

        self.unify(&returns, &self.body, span, ctx)?;

        if &ctx.name(function.value.name.value.name.value) == "main" {
            self.set_main(function.value.name.value.name.value)
        }

        if Type::Nil == returns {
            match &mut body.value.statement.value {
                t::Statement::Block(ref mut statements) => {
                    let mut add_return = false;

                    if let Some(statement) = statements.last() {
                        match statement.value.statement.value {
                            t::Statement::Return(_) => (),
                            _ => {
                                add_return = true;
                                span = statement.span;
                            }
                        }
                    }

                    if add_return {
                        statements.push(Spanned::new(
                            t::TypedStatement {
                                statement: Box::new(Spanned::new(
                                    t::Statement::Return(Spanned::new(
                                        t::TypedExpression {
                                            expr: Box::new(Spanned::new(
                                                t::Expression::Literal(t::Literal::Nil),
                                                span,
                                            )),
                                            ty: Type::Nil,
                                        },
                                        span,
                                    )),
                                    span,
                                )),
                                ty: Type::Nil,
                            },
                            span,
                        ))
                    }
                }
                _ => (),
            }
        } // AUTO INSERT RETURN

        self.body = Type::Nil; // fixes a bug that i dont what caueses

        Ok(t::Function {
            name: function.value.name.value.name.value,
            params: param_types,
            body: Box::new(body),
            returns: returns,
        })
    }
}
