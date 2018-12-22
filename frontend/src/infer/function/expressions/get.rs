use ast as t;
use ctx::CompileCtx;
use infer::env::VarEntry;
use infer::types::{Type, TypeCon, TypeVar};
use infer::{Infer, InferResult};
use syntax::ast::{AssignOperator, Expression, Function, Literal, Op, UnaryOp};
use util::pos::{Span, Spanned};
use util::symbol::Symbol;

impl Infer {
    pub(crate) fn infer_get(
        &mut self,
        object: Spanned<Expression>,
        property: Spanned<Symbol>,
        whole_span: Span,
        ctx: &mut CompileCtx,
    ) -> InferResult<Spanned<t::TypedExpression>> {
        let class_instance = self.infer_expr(object, ctx)?;

        match class_instance.value.ty.clone() {
            Type::Class(ref class_name, ref propertys, ref methods, _) => {
                for property_type in propertys {
                    if property_type.name == property.value {
                        return Ok(Spanned::new(
                            t::TypedExpression {
                                expr: Box::new(Spanned::new(
                                    t::Expression::GetProperty {
                                        property_name: property.value,
                                        property: class_instance,
                                    },
                                    whole_span,
                                )),
                                ty: property_type.ty.clone(),
                            },
                            whole_span,
                        ));
                    }
                }

                for method_type in methods {
                    if method_type.name == property.value {
                        return Ok(Spanned::new(
                            t::TypedExpression {
                                expr: Box::new(Spanned::new(
                                    t::Expression::GetMethod {
                                        method_name: property.value,
                                        method: class_instance,
                                    },
                                    whole_span,
                                )),
                                ty: method_type.ty.clone(),
                            },
                            whole_span,
                        ));
                    }
                }

                let msg = format!(
                    "class `{}` doesn't have a field/method named `{}`",
                    ctx.name(*class_name),
                    ctx.name(property.value)
                );

                ctx.error(msg, whole_span);
                Err(())
            }

            ref err_type => {
                let msg = format!(
                    "Type {} dosen't have the method/field {}",
                    err_type.print(ctx),
                    ctx.name(property.value)
                );

                ctx.error(msg, whole_span);
                Err(())
            }
        }
    }
}
