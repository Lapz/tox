
use super::{Infer, InferResult};
use crate::ctx::CompileCtx;
use crate::infer::types::{Type, TypeCon};
use std::collections::HashMap;
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
                &Type::Class(ref name1, ref p1, ref m1, ref unique1),
                &Type::Class(ref name2, ref p2, ref m2, ref unique2),
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
                    self.unify(&method1.ty, &method2.ty, span, ctx)?;
                }

                for (property1, property2) in p1.iter().zip(p2) {
                    self.unify(&property1.ty, &property2.ty, span, ctx)?;
                }

                Ok(())
            }

            (&Type::App(TypeCon::Void, _), &Type::Class(_, _, _, _)) => Ok(()),
            (&Type::Class(_, _, _, _), &Type::App(TypeCon::Void, _)) => Ok(()),

            (
                &Type::Enum {
                    name: ref name1, ..
                },
                &Type::Enum {
                    name: ref name2, ..
                },
            ) => {
                if name1 != name2 {
                    let msg = format!("Enum `{}` != Enum `{}`", ctx.name(*name1), ctx.name(*name2));
                    ctx.error(msg, span);
                }

                // for(v1,v2) in variants1.iter().zip(variants2.iter()) {
                //     self.unify(v)
                // }

                //TODO unify the variants maybe ?

                Ok(())
            }

            (&Type::App(ref tycon1, ref types1), &Type::App(ref tycon2, ref types2)) => {
                if tycon1 != tycon2 {
                    let msg = format!(
                        "Cannot unify `{}` vs `{}`",
                        lhs.print(ctx.symbols()),
                        rhs.print(ctx.symbols())
                    );
                    ctx.error(msg, span);
                    return Err(());
                }

                for (a, b) in types1.iter().zip(types2.iter()) {
                    self.unify(a, b, span, ctx)?
                }
                Ok(())
            }

            (&Type::Generic(ref vars1, ref ret1), &Type::Generic(ref vars2, ref ret2)) => {
                let mut mappings = HashMap::new();

                for var in vars1 {
                    mappings.insert(*var, Type::Var(*var));
                }

                for var in vars2 {
                    mappings.insert(*var, Type::Var(*var));
                }

                self.unify(ret1, &self.subst(ret2, &mut mappings), span, ctx)
            }

            (&Type::Var(ref v1), &Type::Var(ref v2)) => {
                if v1 == v2 {
                    Ok(())
                } else {
                    let msg = format!(
                        "Cannot unify `{}` vs `{}`",
                        lhs.print(ctx.symbols()),
                        rhs.print(ctx.symbols())
                    );
                    ctx.error(msg, span);
                    Err(())
                }
            }

            (&Type::Generic(_, ref ret1), ref t) => self.unify(ret1, t, span, ctx),

            (ref t, &Type::Generic(_, ref ret1)) => self.unify(ret1, t, span, ctx),

            (&Type::Nil, &Type::Nil) => Ok(()),
            (&Type::Nil, &Type::App(TypeCon::Void, _)) => Ok(()),

            (t1, t2) => {
                let msg = format!(
                    "Cannot unify `{}` vs `{}`",
                    t1.print(ctx.symbols()),
                    t2.print(ctx.symbols())
                );
                ctx.error(msg, span);
                Err(())
            }
        }
    }
}
