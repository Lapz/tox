use ast as t;
use ctx::CompileCtx;

use infer::types::{Type, TypeCon};
use infer::{Infer, InferResult};
use syntax::ast::Expression;
use util::pos::{Span, Spanned};

impl Infer {
    pub(crate) fn infer_subscript(
        &mut self,
        target: Spanned<Expression>,
        index: Spanned<Expression>,
        whole_span: Span,
        ctx: &mut CompileCtx,
    ) -> InferResult<Spanned<t::TypedExpression>> {
        let target_span = target.span;
        let index_span = index.span;

        let index_ty = self.infer_expr(index, ctx)?;

        // we can index string i.e "abc"[0]
        // or we can index var i.e a[10];
        // thats why we check for vars
        let (expr,ty) = match target.value {
            Expression::Var(symbol) => {
                let target_ty = self.infer_symbol_type(&symbol, ctx)?;

                self.unify(
                    &index_ty.value.ty,
                    &Type::App(TypeCon::Int, vec![]), // what ever is in the brace has to be an int
                    index_span,
                    ctx,
                )?;

                match target_ty {
                    Type::App(TypeCon::Array(ref ty), _) => {
                        let var = Spanned::new(
                            t::TypedExpression {
                                expr: Box::new(Spanned::new(
                                    t::Expression::Var(symbol.value, target_ty.clone()),
                                    target_span,
                                )),
                                ty: target_ty.clone(),
                            },
                            target_span,
                        ); // the type of the target manually construted to be a type Expression

                        (
                            Spanned::new(t::Expression::Index(var, index_ty), whole_span),
                            *ty.clone(),
                        )
                    }
                    Type::App(TypeCon::Str, _) => {
                        let var = Spanned::new(
                            t::TypedExpression {
                                expr: Box::new(Spanned::new(
                                    t::Expression::Var(symbol.value, index_ty.value.ty.clone()),
                                    target_span,
                                )),
                                ty: index_ty.value.ty.clone(),
                            },
                            target_span,
                        );

                        (
                            Spanned::new(t::Expression::Index(var, index_ty), whole_span),
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
                // array expression or things that evalu to an array

                let expr = self.infer_expr(target, ctx)?;

                match expr.value.ty.clone() {
                    Type::App(TypeCon::Array(ref ty), _) => (
                        Spanned::new(t::Expression::Index(expr, index_ty), whole_span),
                        *ty.clone(),
                    ),

                    _ => {
                        ctx.error("Invalid index target", target_span);
                        return Err(());
                    }
                }
            }
        };
         
        Ok(Spanned {
            value:t::TypedExpression {
                expr:Box::new(expr),
                ty
            },
            span:whole_span
        })
        
    }
}
