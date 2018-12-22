use ast as t;
use ctx::CompileCtx;
use infer::env::VarEntry;
use infer::types::{Type, TypeCon, TypeVar};
use infer::{Infer, InferResult};
use syntax::ast::{AssignOperator, Expression, Function, Literal, Op, UnaryOp};
use util::pos::{Span, Spanned};
use util::symbol::Symbol;

impl Infer {
    pub(crate) fn infer_set(
        &mut self,
        object: Spanned<Expression>,
        property: Spanned<Symbol>,
        value: Spanned<Expression>,
        whole_span: Span,
        ctx: &mut CompileCtx,
    ) -> InferResult<Spanned<t::TypedExpression>> {
        let class_instance = self.infer_expr(object, ctx)?;

        match class_instance.value.ty.clone() {
            Type::Class(ref name, _, _, _) => {
                if let Some(ty) = ctx.look_type(*name).cloned() {
                    match ty {
                        Type::Class(_, ref propertys, ref methods, _) => {
                            let value_span = value.span;
                            let value_ty = self.infer_expr(value, ctx)?;

                            for property_type in propertys {
                                if property_type.name == property.value {
                                    self.unify(
                                        &value_ty.value.ty,
                                        &property_type.ty,
                                        value_span,
                                        ctx,
                                    )?;
                                    return Ok(Spanned::new(
                                        t::TypedExpression {
                                            expr: Box::new(Spanned::new(
                                                t::Expression::Set(
                                                    property.value,
                                                    class_instance,
                                                    value_ty,
                                                ),
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
                                    self.unify(
                                        &value_ty.value.ty,
                                        &method_type.ty,
                                        value_span,
                                        ctx,
                                    )?;
                                    return Ok(Spanned::new(
                                        t::TypedExpression {
                                            expr: Box::new(Spanned::new(
                                                t::Expression::Set(
                                                    property.value,
                                                    class_instance,
                                                    value_ty,
                                                ),
                                                whole_span,
                                            )),
                                            ty:method_type.ty.clone(),
                                        },
                                        whole_span,
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

                ctx.error(msg, whole_span);

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
}
