use super::stacked_map::StackedMap;
use crate::{
    db::HirDatabase,
    hir::{Name, NameId},
    infer::ty::{EnumVariant, Type, TypeCon, TypeVar},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ctx {
    types: StackedMap<NameId, Type>,
    tvar_count: u32,
}

impl Ctx {
    pub fn new(db: &impl HirDatabase) -> Self {
        let mut types = StackedMap::new();

        types.insert(db.intern_name(Name::new("i32")), Type::Con(TypeCon::Int));
        types.insert(db.intern_name(Name::new("f32")), Type::Con(TypeCon::Float));
        types.insert(db.intern_name(Name::new("bool")), Type::Con(TypeCon::Bool));
        types.insert(db.intern_name(Name::new("void")), Type::Con(TypeCon::Void));
        types.insert(db.intern_name(Name::new("string")), Type::Con(TypeCon::Str));

        let result_name = db.intern_name(Name::new("Result"));
        types.insert(
            result_name,
            Type::Poly(
                vec![TypeVar::from(0), TypeVar::from(1)],
                Box::new(Type::Enum(
                    result_name,
                    vec![
                        EnumVariant {
                            tag: 0,
                            inner: Some(Type::Var(TypeVar::from(0))), // Ok(T)
                        },
                        EnumVariant {
                            tag: 1,
                            inner: Some(Type::Var(TypeVar::from(1))), // Err(U)
                        },
                    ],
                )),
            ),
        );

        Self {
            types,
            tvar_count: 2,
        }
    }

    pub(crate) fn type_var(&mut self) -> TypeVar {
        let tv = TypeVar::from(self.tvar_count);
        self.tvar_count += 1;

        tv
    }

    pub(crate) fn begin_scope(&mut self) {
        self.types.begin_scope();
    }

    pub(crate) fn end_scope(&mut self) {
        self.types.end_scope();
    }

    pub(crate) fn get_type(&self, name: &NameId) -> Option<Type> {
        self.types.get(name).map(Clone::clone)
    }

    pub(crate) fn insert_type(&mut self, name: NameId, ty: Type) {
        self.types.insert(name, ty)
    }
}
