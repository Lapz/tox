use super::{Infer, InferResult};
use ctx::CompileCtx;
use types::Type;
use util::pos::Span;

impl Infer {
    pub fn unify(
        &self,
        lhs: &Type,
        rhs: &Type,
        span: Span,
        ctx: &mut CompileCtx,
    ) -> InferResult<()> {
        match (lhs, rhs) {
            (
                &Type::Class(ref name1, ref m1, ref f1, ref unique1),
                &Type::Class(ref name2, ref m2, ref f2, ref unique2),
            ) => {
                if unique1 != unique2 {
                    let msg = format!(
                        "Class `{}` != Class `{}`",
                        ctx.name(*name1),
                        ctx.name(*name2)
                    );

                    ctx.error(msg, span);
                    return Err(());
                }

                for (method1, method2) in m1.iter().zip(m2) {
                    self.unify(&method1.1.ty(), &method2.1.ty(), span, ctx)?;
                }

                for (field1, field2) in f1.iter().zip(f2) {
                    self.unify(&field1.1, &field2.1, span, ctx)?;
                }

                Ok(())
            }
            (&Type::Fun(_,ref ret),ref o) => {
                self.unify(ret,o,span,ctx)?;
                Ok(())
            }

            (&Type::Int, &Type::Int) => Ok(()),
            (&Type::Str, &Type::Str) => Ok(()),
            (&Type::Bool, &Type::Bool) => Ok(()),
            (&Type::Float, &Type::Float) => Ok(()),
            (&Type::Nil, &Type::Nil) => Ok(()),
            (&Type::Nil, &Type::Class(_, _, _, _)) => Ok(()),
            (&Type::Array(_), &Type::Nil) => Ok(()),
            (&Type::Dict(_, _), &Type::Nil) => Ok(()),
            (t1, t2) => {
                let msg = format!("Cannot unify `{}` vs `{}`", t1.print(ctx), t2.print(ctx));
                ctx.error(msg, span);
                Err(())
            }
        }
    }
}
