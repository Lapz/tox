use super::infer::InferDataCollector;
use crate::infer::{Subst, Type, TypeCon, Variant};
use crate::util::Span;
use crate::HirDatabase;
use std::collections::HashMap;
use syntax::TextRange;

impl<'a, DB> InferDataCollector<&'a DB>
where
    DB: HirDatabase,
{
    pub(crate) fn unify(&mut self, lhs: &Type, rhs: &Type, span: TextRange) -> Result<(), ()> {
        match (lhs, rhs) {
            (
                Type::Class {
                    name: n1,
                    methods: m1,
                    fields: f1,
                },
                Type::Class {
                    name: n2,
                    methods: m2,
                    fields: f2,
                },
            ) => {
                for ((_, t1), (_, t2)) in m1.iter().zip(m2) {
                    self.unify(t1, t2, span)?;
                }

                for ((_, t1), (_, t2)) in f1.iter().zip(f2) {
                    self.unify(t1, t2, span)?;
                }

                Ok(())
            }
            (Type::Enum(name1, variants1), Type::Enum(name2, variants2)) => {
                // for (v1, v2) in variants1.iter().zip(variants2.iter()) {
                //     self.unify(&v1.1.ty, &v2.1.ty, span);
                // }

                //TODO unify the variants maybe ?

                Ok(())
            }
            (Type::App(types1), Type::App(types2)) => {
                if let (Some(ret1), Some(ret2)) = (types1.last(), types2.last()) {
                    if ret1 != ret2 {
                        let msg = format!(
                            "Cannot unify `{}` vs `{}`",
                            ret1.format(self.db),
                            ret2.format(self.db)
                        );

                        let additional = format!(
                            "The return type of `{}` is different ",
                            ret2.format(self.db)
                        );

                        self.reporter.error(
                            msg,
                            additional,
                            (span.start().to_usize(), span.end().to_usize()),
                        );

                        return Err(());
                    }
                }

                for (a, b) in types1.iter().zip(types2.iter()) {
                    self.unify(a, b, span)?
                }
                Ok(())
            }
            (Type::Poly(ref vars1, ref ret1), Type::Poly(ref vars2, ref ret2)) => {
                let mut mappings = HashMap::new();

                for var in vars1 {
                    mappings.insert(*var, Type::Var(*var));
                }

                for var in vars2 {
                    mappings.insert(*var, Type::Var(*var));
                }

                let subst = self.subst(ret2, &mut mappings);

                self.unify(ret1, &subst, span)
            }
            (Type::Var(ref v1), Type::Var(ref v2)) => {
                if v1 == v2 {
                    Ok(())
                } else {
                    let msg = format!(
                        "Cannot unify `{}` vs `{}`",
                        lhs.format(self.db),
                        rhs.format(self.db)
                    );
                    self.reporter.error(
                        msg,
                        "additional_info",
                        (span.start().to_usize(), span.end().to_usize()),
                    );
                    Err(())
                }
            }
            (Type::Poly(_, ref ret1), ref t) => self.unify(ret1, t, span),
            (ref t, &Type::Poly(_, ref ret1)) => self.unify(ret1, t, span),
            (t1, t2) => {
                let msg = format!(
                    "Cannot unify `{}` vs `{}`",
                    t1.format(self.db),
                    t2.format(self.db)
                );
                self.reporter
                    .error(msg, "", (span.start().to_usize(), span.end().to_usize()));
                Err(())
            }
        }
    }
}
