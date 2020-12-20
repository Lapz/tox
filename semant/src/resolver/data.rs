use crate::{
    hir::{self, Name, NameId, TypeId},
    infer::{StackedMap, Type, TypeCon},
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
    pub(crate) binding_error: bool,
    pub(crate) function_data: HashMap<hir::NameId, FunctionData>,
    pub(crate) interned_types: StackedMap<hir::TypeId, Type>,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum TypeKind {
    Function,
    Alias,
    Enum,
    Class,
    Type,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Resolver {
    pub(crate) ctx: Ctx,
    pub(crate) items: HashSet<hir::NameId>,
    pub(crate) exported_items: HashSet<hir::NameId>,
    pub(crate) function_data: HashMap<hir::NameId, FunctionData>,
    pub(crate) interned_types: StackedMap<hir::TypeId, Type>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum State {
    Declared,
    Defined,
    Read,
}

pub enum ItemKind {
    Class,
    Function,
    Enum,
}
/// Information at a local variable declared in a block
#[derive(Copy, Debug, Clone, PartialEq, Eq)]
pub struct LocalData {
    state: util::Span<State>,
    reads: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct FunctionData {
    pub(crate) type_params: HashMap<NameId, Type>,
    pub(crate) params: HashMap<NameId, Type>,
    pub(crate) scopes: StackedMap<hir::NameId, LocalData>,
}

impl FunctionData {
    pub fn new() -> Self {
        Self {
            type_params: HashMap::new(),
            params: HashMap::new(),
            scopes: StackedMap::new(),
        }
    }
}

impl Resolver {
    pub fn get_type(&self, id: &hir::NameId) -> Option<Type> {
        self.ctx.get_type(id)
    }

    pub fn lookup_intern_type(&self, id: &hir::TypeId) -> Option<Type> {
        self.interned_types.get(id).map(Clone::clone)
    }

    pub fn has_export(&self, id: &hir::NameId) -> bool {
        self.exported_items.get(id).is_some()
    }

    pub fn get_param(&self, fn_name: &hir::NameId, param: &hir::NameId) -> Option<Type> {
        self.function_data[fn_name]
            .params
            .get(param)
            .map(Clone::clone)
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
                interned_types: self.interned_types,
            },
            self.reporter,
        )
    }

    pub(crate) fn intern_type(&mut self, id: TypeId, ty: Type) {
        self.interned_types.insert(id, ty);
    }

    pub(crate) fn lookup_type(&mut self, id: &TypeId) -> Option<Type> {
        self.interned_types.get(id).map(Clone::clone)
    }

    pub(crate) fn begin_scope(&mut self) {
        self.ctx.begin_scope();
        // self.interned_types.begin_scope();
    }

    pub(crate) fn end_scope(&mut self) {
        self.ctx.end_scope();
        // self.interned_types.end_scope();
    }

    pub(crate) fn insert_type_param(
        &mut self,
        fn_name: &NameId,
        param_id: &util::Span<NameId>,
        ty: Type,
    ) {
        let function_data = self.function_data.get_mut(&fn_name).unwrap();
        function_data.type_params.insert(param_id.item, ty);
    }

    pub(crate) fn insert_param(
        &mut self,
        fn_name: &NameId,
        param: &util::Span<PatId>,
        ast_map: &hir::FunctionAstMap,
        ty: Type,
    ) {
        let pat = ast_map.pat(&param.item);
        let function_data = self.function_data.get_mut(&fn_name).unwrap();

        match pat {
            hir::Pattern::Bind { name } => {
                function_data.params.insert(name.item, ty);
            }
            hir::Pattern::Placeholder => {
                function_data
                    .params
                    .insert(self.db.intern_name(Name::new("_")), ty);
            }
            hir::Pattern::Tuple(pats) => match ty {
                Type::Tuple(types) => {
                    for (pat, ty) in pats.iter().zip(types.iter()) {
                        self.insert_param(fn_name, pat, ast_map, ty.clone())
                    }
                }

                Type::Poly(_, inner) => match &*inner {
                    Type::Tuple(types) => {
                        for (pat, ty) in pats.iter().zip(types.iter()) {
                            self.insert_param(fn_name, pat, ast_map, ty.clone())
                        }
                    }
                    _ => {}
                },

                _ => {}
            },
            hir::Pattern::Literal(_) => {
                // TODO error in resolving
            }
        }
    }

    pub(crate) fn insert_type(&mut self, name_id: &util::Span<NameId>, ty: Type, kind: TypeKind) {
        if self.ctx.get_type(&name_id.item).is_some() {
            match kind {
                TypeKind::Function | TypeKind::Enum => (), // Error already reported
                _ => {
                    let name = self.db.lookup_intern_name(name_id.item);

                    self.reporter.error(
                        format!("`{}` is defined multiple times", name),
                        "",
                        (name_id.start().to_usize(), name_id.end().to_usize()),
                    );
                }
            }
        } else {
            self.ctx.insert_type(name_id.item, ty, kind);
        }
    }

    pub(crate) fn add_item(&mut self, name_id: util::Span<NameId>, kind: ItemKind, exported: bool) {
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

            match kind {
                ItemKind::Function => {
                    self.function_data.insert(name_id.item, FunctionData::new());
                }
                ItemKind::Class | ItemKind::Enum => {}
            }
        }
    }

    pub(crate) fn resolve_local(&mut self, fn_name: &NameId, name: &util::Span<NameId>) {
        let data = self.function_data.get_mut(fn_name).unwrap();

        if let Some(state) = data.scopes.get_mut(&name.item) {
            state.state = util::Span::new(State::Read, name.start(), name.end());
            state.reads += 1;
            return;
        } //check for ident name in function/local scope

        //  check for external import global level
        // function names when called are stored as
        // and IdentExpr followed by the args
        // so to resolve them we need to look at the file ctx
        if !self.items.contains(&name.item) {
            let msg = format!(
                "Undefined identifier `{}`",
                self.db.lookup_intern_name(name.item)
            );

            self.reporter
                .error(msg, "", (name.start().to_usize(), name.end().to_usize()))
        }
    }

    pub(crate) fn add_local(
        &mut self,
        fn_name: NameId,
        param: util::Span<NameId>,
    ) -> Result<(), ()> {
        if self.function_data[&fn_name].scopes.is_in_scope(&param.item) {
            let msg = if self.binding_error {
                format!(
                    "Duplicate binding `{}`",
                    self.db.lookup_intern_name(param.item)
                )
            } else {
                format!(
                    "The identifier `{}` has already been declared.",
                    self.db.lookup_intern_name(param.item)
                )
            };

            if self.binding_error {
                self.reporter
                    .error(msg, "", (param.start().to_usize(), param.end().to_usize()));
                return Err(());
            } else {
                self.reporter
                    .warn(msg, "", (param.start().to_usize(), param.end().to_usize()));
            }
        }

        let function_data = self.function_data.get_mut(&fn_name).unwrap();
        function_data.scopes.insert(
            param.item,
            LocalData {
                state: util::Span::new(State::Declared, param.start(), param.end()),
                reads: 0,
            },
        );

        Ok(())
    }

    pub(crate) fn local_is_declared(&self, fn_name: &NameId, name: &util::Span<NameId>) -> bool {
        if let Some(state) = self.function_data[fn_name].scopes.get(&name.item) {
            state.state.item == State::Declared
        } else {
            false
        }
    }

    pub(crate) fn define_local(&mut self, fn_name: &NameId, name: &util::Span<NameId>) {
        let function_data = self.function_data.get_mut(&fn_name).unwrap();

        function_data.scopes.update(
            name.item,
            LocalData {
                state: util::Span::new(State::Defined, name.start(), name.end()),
                reads: 0,
            },
        );
    }

    pub(crate) fn begin_function_scope(&mut self, fn_name: NameId) {
        let function_data = self.function_data.get_mut(&fn_name).unwrap();

        function_data.scopes.begin_scope();
    }

    pub(crate) fn end_function_scope(&mut self, fn_name: NameId) {
        let function_data = self.function_data.get_mut(&fn_name).unwrap();

        for (name, state) in function_data.scopes.end_scope_iter() {
            let LocalData { reads, state } = state;

            if reads == 0 || state.item == State::Declared {
                let msg = format!("Unused variable `{}`", self.db.lookup_intern_name(name));
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
    ) -> Result<(), ()> {
        let pat = ast_map.pat(&pat_id.item);

        match pat {
            hir::Pattern::Bind { name } => {
                self.add_local(fn_name, *name)?;
                self.define_local(&fn_name, name);
            }
            hir::Pattern::Tuple(patterns) => {
                // When resolving a tuple pat
                // Multiple of the same binding is a hard error

                self.binding_error = true;

                let mut error_occurred = false;

                for pat in patterns {
                    if let Err(()) = self.resolve_pattern(fn_name, pat, ast_map) {
                        error_occurred = true;
                        continue;
                    }
                }

                self.binding_error = false;

                if error_occurred {
                    return Err(());
                }
            }
            hir::Pattern::Placeholder | hir::Pattern::Literal(_) => {}
        }

        Ok(())
    }

    pub(crate) fn resolve_type(&mut self, id: &util::Span<TypeId>) -> Type {
        let ty = self.db.lookup_intern_type(id.item);

        if let Some(interned_ty) = self.lookup_type(&id.item) {
            return interned_ty;
        }

        let ty = match ty {
            hir::Type::ParenType(types) => {
                let mut signature = vec![];
                for id in &types {
                    signature.push(self.resolve_type(id))
                }

                Type::Tuple(signature)
            }

            hir::Type::ArrayType { ty, size } => Type::Con(TypeCon::Array {
                ty: Box::new(self.resolve_type(&ty)),
                size,
            }),
            hir::Type::FnType { params, ret } => {
                let mut signature = vec![];

                for id in &params {
                    signature.push(self.resolve_type(id))
                }

                if let Some(returns) = &ret {
                    signature.push(self.resolve_type(returns))
                } else {
                    signature.push(Type::Con(TypeCon::Void))
                }

                Type::App(signature)
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

                        Type::Unknown
                    }
                };
                println!("Here: NameId:{:?} \n {:#?}", name, ty);
                if self.ctx.get_kind(&name) == TypeKind::Function {
                    let span = (id.start().to_usize(), id.end().to_usize());
                    self.reporter.error(
                        format!(
                            "Expected a type found `{}`",
                            self.db.lookup_intern_name(name)
                        ),
                        "A function cannot be used as a type",
                        span,
                    );

                    return Type::Unknown;
                }

                for arg in &type_args {
                    let _ = self.resolve_type(arg);
                }

                ty
            }
            hir::Type::Ident(name) => {
                if let Some(ty) = self.ctx.get_type(&name) {
                    if self.ctx.get_kind(&name) == TypeKind::Function {
                        let span = (id.start().to_usize(), id.end().to_usize());
                        self.reporter.error(
                            format!(
                                "Expected a type found `{}`",
                                self.db.lookup_intern_name(name)
                            ),
                            "A function cannot be used as a type",
                            span,
                        );

                        return Type::Unknown;
                    }

                    self.intern_type(id.item, ty.clone());
                    return ty;
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

                Type::Unknown
            }
        };

        self.intern_type(id.item, ty.clone());

        ty
    }
}
