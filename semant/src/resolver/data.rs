use crate::{
    hir::{self, NameId, TypeId},
    infer::{Type, TypeCon},
    util, Ctx, HirDatabase,
};
use errors::Reporter;
use hir::PatId;
use std::collections::{HashMap, HashSet};

#[derive(Debug)]
pub(crate) struct ResolverDataCollector<DB> {
    pub(crate) db: DB,
    pub(crate) ctx: Ctx,
    pub(crate) reporter: Reporter,
    pub(crate) items: HashSet<hir::NameId>,
    pub(crate) exported_items: HashSet<hir::NameId>,
    pub(crate) function_data: HashMap<hir::NameId, FunctionData>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Resolver {
    pub(crate) ctx: Ctx,
    pub(crate) items: HashSet<hir::NameId>,
    pub(crate) exported_items: HashSet<hir::NameId>,
    pub(crate) function_data: HashMap<hir::NameId, FunctionData>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum State {
    Declared,
    Defined,
    Read,
}

#[derive(Clone, Debug, Default)]
pub(crate) struct Scopes {
    scopes: Vec<HashMap<hir::NameId, State>>,
    len: usize,
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub(crate) struct FunctionData {
    pub(crate) scopes: Vec<HashMap<hir::NameId, util::Span<State>>>,
    locals: HashMap<util::Span<hir::NameId>, usize>,
}

impl FunctionData {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
            ..Self::default()
        }
    }

    pub(crate) fn peek(&self) -> usize {
        self.scopes.len() - 1
    }
}

impl Resolver {
    pub fn has_export(&self, id: &hir::NameId) -> bool {
        self.exported_items.get(id).is_some()
    }
}

impl<'a, DB> ResolverDataCollector<&'a DB>
where
    DB: HirDatabase,
{
    pub fn finish(self) -> (Resolver, Reporter) {
        (
            Resolver {
                ctx: self.ctx,
                items: self.items,
                exported_items: self.exported_items,
                function_data: self.function_data,
            },
            self.reporter,
        )
    }
    pub(crate) fn begin_scope(&mut self) {
        self.ctx.begin_scope();
    }

    pub(crate) fn end_scope(&mut self) {
        self.ctx.end_scope();
    }

    pub(crate) fn insert_type(&mut self, name_id: &util::Span<NameId>, ty: Type) -> Result<(), ()> {
        if self.ctx.get_type(&name_id.item).is_some() {
            let name = self.db.lookup_intern_name(name_id.item);

            self.reporter.error(
                format!("Type `{}` is defined multiple times", name),
                "",
                (name_id.start().to_usize(), name_id.end().to_usize()),
            );

            Err(())
        } else {
            self.ctx.insert_type(name_id.item, ty);
            Ok(())
        }
    }

    pub(crate) fn add_function(&mut self, name_id: util::Span<NameId>, exported: bool) {
        if self.items.contains(&name_id.item) {
            let name = self.db.lookup_intern_name(name_id.item);

            self.reporter.error(
                format!("The name `{}` is defined multiple times", name),
                "",
                (name_id.start().to_usize(), name_id.end().to_usize()),
            )
        } else {
            if exported {
                self.exported_items.insert(name_id.item);
            }

            self.items.insert(name_id.item);

            self.function_data.insert(name_id.item, FunctionData::new());
        }
    }

    pub(crate) fn resolve_function_scope(&self, name: NameId) -> usize {
        self.function_data[&name].peek()
    }

    pub(crate) fn resolve_local(&mut self, fn_name: &NameId, name: &util::Span<NameId>) {
        let data = self.function_data.get_mut(fn_name).unwrap();

        let max_scopes = data.scopes.len();

        for i in 0..max_scopes {
            if data.scopes[max_scopes - i - 1].contains_key(&name.item) {
                if let Some(state) = data.scopes[max_scopes - i - 1].get_mut(&name.item) {
                    *state = util::Span::new(State::Read, name.start(), name.end())
                }

                return; // reduce work done
            }
        } // check for ident name in function/local scope

        //  check for external import global level
        // function names when called are stored as
        // and IdentExpr followed by the args
        // so to resolve them we need to look at the file ctx
        if !self.items.contains(&name.item) {
            let msg = format!(
                "Use of undefined variable `{}`",
                self.db.lookup_intern_name(name.item)
            );

            self.reporter
                .error(msg, "", (name.start().to_usize(), name.end().to_usize()))
        }
    }

    pub(crate) fn add_local(&mut self, fn_name: NameId, param: util::Span<NameId>) {
        let scope = self.resolve_function_scope(fn_name);

        if self.function_data[&fn_name].scopes[scope].contains_key(&param.item) {
            let msg = format!(
                "The identifier `{}` has already been declared.",
                self.db.lookup_intern_name(param.item)
            );

            self.reporter
                .warn(msg, "", (param.start().to_usize(), param.end().to_usize()))
        }

        let function_data = self.function_data.get_mut(&fn_name).unwrap();
        function_data.scopes[scope].insert(
            param.item,
            util::Span::new(State::Declared, param.start(), param.end()),
        );
    }

    pub(crate) fn local_is_declared(&self, fn_name: &NameId, name: &util::Span<NameId>) -> bool {
        let scope = self.resolve_function_scope(*fn_name);

        if let Some(state) = self.function_data[fn_name].scopes[scope].get(&name.item) {
            return state.item == State::Declared;
        } else {
            false
        }
    }

    pub(crate) fn define_local(&mut self, fn_name: &NameId, name: &util::Span<NameId>) {
        let scope = self.resolve_function_scope(*fn_name);

        let function_data = self.function_data.get_mut(&fn_name).unwrap();
        function_data.scopes[scope].insert(
            name.item,
            util::Span::new(State::Defined, name.start(), name.end()),
        );
    }

    pub(crate) fn begin_function_scope(&mut self, fn_name: NameId) {
        let function_data = self.function_data.get_mut(&fn_name).unwrap();

        function_data.scopes.push(HashMap::new())
    }

    pub(crate) fn end_function_scope(&mut self, fn_name: NameId) {
        let function_data = self.function_data.get_mut(&fn_name).unwrap();

        let scope = function_data.scopes.pop().unwrap();

        for (name, state) in &scope {
            if state.item == State::Declared {
                let msg = format!("Unused variable `{}`", self.db.lookup_intern_name(*name));
                self.reporter
                    .warn(msg, "", (state.start().to_usize(), state.end().to_usize()))
            }
        }
    }

    /// Resolve a  pattern
    /// A pattern can occur in a fn param def
    /// or in a let statement
    pub(crate) fn resolve_pattern(
        &mut self,
        fn_name: NameId,
        pat_id: &util::Span<PatId>,
        ast_map: &hir::FunctionAstMap,
    ) {
        let pat = ast_map.pat(&pat_id.item);

        match pat {
            hir::Pattern::Bind { name } => {
                self.add_local(fn_name, *name);
                self.define_local(&fn_name, name);
            }
            hir::Pattern::Tuple(patterns) => {
                for pat in patterns {
                    self.resolve_pattern(fn_name, pat, ast_map)
                }
            }
            hir::Pattern::Placeholder | hir::Pattern::Literal(_) => {}
        }
    }

    pub(crate) fn resolve_type(&mut self, id: &util::Span<TypeId>) -> Result<Type, ()> {
        let ty = self.db.lookup_intern_type(id.item);

        match ty {
            hir::Type::ParenType(types) => {
                let mut signature = vec![];
                for id in &types {
                    signature.push(self.resolve_type(id)?)
                }

                Ok(Type::Tuple(signature))
            }

            hir::Type::ArrayType { ty, size } => Ok(Type::Con(TypeCon::Array {
                ty: Box::new(self.resolve_type(&ty)?),
                size,
            })),
            hir::Type::FnType { params, ret } => {
                let mut signature = vec![];

                for id in &params {
                    signature.push(self.resolve_type(id)?)
                }

                if let Some(returns) = &ret {
                    signature.push(self.resolve_type(returns)?)
                } else {
                    signature.push(Type::Con(TypeCon::Void))
                }

                Ok(Type::App(signature))
            }
            hir::Type::Poly { name, type_args } => {
                let ty = match self.ctx.get_type(&name) {
                    Some(ty) => ty,
                    None => {
                        let span = (id.start().to_usize(), id.end().to_usize());
                        self.reporter.error(
                            format!(
                                "Use of undefined type `{}`",
                                self.db.lookup_intern_name(name)
                            ),
                            "",
                            span,
                        );

                        return Err(());
                    }
                };

                for arg in &type_args {
                    if let Err(()) = self.resolve_type(arg) {
                        continue;
                    }
                }

                Ok(ty)
            }
            hir::Type::Ident(name) => {
                if let Some(ty) = self.ctx.get_type(&name) {
                    return Ok(ty);
                }

                let span = (id.start().to_usize(), id.end().to_usize());
                self.reporter.error(
                    format!(
                        "Use of undefined type `{}`",
                        self.db.lookup_intern_name(name)
                    ),
                    "",
                    span,
                );

                Err(())
            }
        }
    }
}
