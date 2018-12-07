mod expression;
mod statements;

use ast as t;
use ctx::CompileCtx;
use infer::env::VarEntry;
use infer::types::Type;
use infer::{Infer, InferResult};
use syntax::ast::Function;
use util::pos::Spanned;

impl Infer {
    pub fn infer_function(
        &mut self,
        function: Spanned<Function>,
        ctx: &mut CompileCtx,
    ) -> InferResult<t::Function> {
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

        ctx.add_var(
            function.value.name.value,
            VarEntry::Fun {
                ty: Type::Fun(env_types.clone(), Box::new(returns.clone()), false),
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

        if &ctx.name(function.value.name.value) == "main" {
            self.set_main(function.value.name.value)
        }

        if Type::Nil == returns {
            match &mut body.value {
                t::Statement::Block(ref mut statements) => {
                    let mut add_return = false;

                    if let Some(statement) = statements.last() {
                        match statement.value {
                            t::Statement::Return(_) => (),
                            _ => {
                                add_return = true;
                                span = statement.span;
                            }
                        }
                    }

                    if add_return {
                        statements.push(Spanned::new(
                            t::Statement::Return(Spanned::new(
                                t::TypedExpression {
                                    expr: Box::new(Spanned::new(t::Expression::Literal(t::Literal::Nil), span)),
                                    ty: Type::Nil,
                                },
                                span,
                            )),
                            span,
                        ))
                    }
                }
                _ => (),
            }
        } // AUTO INSERT RETURN

        Ok(t::Function {
            name: function.value.name.value,
            params: param_types,
            body: Box::new(body),
            returns: returns,
        })
    }
}
