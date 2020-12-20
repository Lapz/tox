use crate::{
    infer::{InferDataCollector, Type, TypeCon},
    HirDatabase,
};
use std::collections::HashMap;
use tracing::{debug, instrument};

impl<'a, DB> InferDataCollector<&'a DB>
where
    DB: HirDatabase,
{
    #[instrument(skip(self))]
    pub(crate) fn unify(
        &mut self,
        lhs: &Type,
        rhs: &Type,
        span: (usize, usize),
        notes: Option<String>,
        report: bool,
    ) {
        debug!("Unifying ${:?} ${:?}", lhs, rhs);
        match (lhs, rhs) {
            (Type::App(types1), Type::App(types2)) => {
                if types1.len() != types2.len() && report {
                    let msg = format!("Expected {} params, found {}", types1.len(), types2.len());
                    self.reporter.error(msg, notes.unwrap_or("".into()), span);
                    return;
                }

                if types1[types1.len() - 1] != types2[types2.len() - 1] && report {
                    let msg = format!(
                        "Expected {} found {}. The return types are different",
                        types1.len(),
                        types2.len()
                    );
                    self.reporter.error(msg, notes.unwrap_or("".into()), span);
                    return;
                }

                for (a, b) in types1.iter().zip(types2.iter()) {
                    let _ = self.unify(a, b, span, None, false);
                }
            }
            (Type::App(signature), ret) => {
                let last = signature.last().unwrap_or(&Type::Con(TypeCon::Void));

                self.unify(last, ret, span, notes, report)
            }
            (Type::Tuple(types1), Type::Tuple(types2)) => {
                if types1.len() != types2.len() && report {
                    let msg = format!("Expected {} params, found {}", types1.len(), types2.len());
                    self.reporter.error(msg, notes.unwrap_or("".into()), span);
                    return;
                }

                for (a, b) in types1.iter().zip(types2.iter()) {
                    let _ = self.unify(a, b, span, None, false);
                }
            }
            (Type::Enum(name1, variants1), Type::Enum(name2, variants2)) => {
                if variants1 != variants2 {
                    if report {
                        let msg = format!(
                            "Expected enum `{}` but found enum `{}`",
                            self.db.lookup_intern_name(*name1),
                            self.db.lookup_intern_name(*name2)
                        );
                        self.reporter.error(msg, notes.unwrap_or("".into()), span);
                    }
                }
            }
            (
                Type::Class {
                    name,
                    fields,
                    methods,
                },
                Type::Class {
                    name: name1,
                    fields: fields1,
                    methods: methods1,
                },
            ) => {
                if fields != fields1 && methods != methods1 {
                    if report {
                        let msg = format!(
                            "Expected class `{}` but found class `{}`",
                            self.db.lookup_intern_name(*name),
                            self.db.lookup_intern_name(*name1)
                        );
                        self.reporter.error(msg, notes.unwrap_or("".into()), span);
                    }
                }
            }
            (Type::Var(v1), Type::Var(v2)) => {
                if v1 != v2 && report {
                    let msg = format!("Mismatching types, expected `{:?}` found `{:?}`", lhs, rhs);
                    self.reporter.error(msg, notes.unwrap_or("".into()), span);
                }
            }
            (Type::Con(TypeCon::Int), Type::Con(TypeCon::Float)) => {}
            (Type::Con(TypeCon::Float), Type::Con(TypeCon::Int)) => {}
            (Type::Con(l), Type::Con(r)) => {
                if l != r {
                    if report {
                        let msg =
                            format!("Mismatching types, expected `{:?}` found `{:?}`", lhs, rhs);
                        self.reporter.error(msg, notes.unwrap_or("".into()), span);
                    }
                }
            }

            (Type::Poly(vars1, t1), Type::Poly(vars2, t2)) => {
                let mut mappings = HashMap::new();

                for var in vars1 {
                    mappings.insert(*var, Type::Var(*var));
                }

                for var in vars2 {
                    mappings.insert(*var, Type::Var(*var));
                }

                let subst = self.subst(t2, &mut mappings);

                self.unify(t1, &subst, span, notes, true)
            }
            (Type::Poly(_, ret), t) => self.unify(ret, t, span, notes, true),
            (t, Type::Poly(_, ret)) => self.unify(t, ret, span, notes, true),
            (Type::Unknown, _) => {}
            (_, Type::Unknown) => {}
            (_, _) => {
                //Todo report error

                if report {
                    let msg = format!("Mismatching types, expected `{:?}` found `{:?}`", lhs, rhs);
                    self.reporter.error(msg, notes.unwrap_or("".into()), span);
                }
            }
        }
    }
}
