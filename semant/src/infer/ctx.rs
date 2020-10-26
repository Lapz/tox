use super::stacked_map::StackedMap;
use crate::resolver::TypeKind;
use crate::{
    db::HirDatabase,
    hir::{Name, NameId},
    infer::ty::{Type, TypeCon, TypeVar, Variant},
};
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ctx {
    pub types: StackedMap<NameId, Type>,
    kind: HashMap<NameId, TypeKind>,
    tvar_count: u32,
}

impl Ctx {
    pub fn new(db: &impl HirDatabase) -> Self {
        let mut types = StackedMap::new();
        let mut kind = HashMap::new();

        kind.insert(db.intern_name(Name::new("i32")), TypeKind::Type);
        types.insert(db.intern_name(Name::new("i32")), Type::Con(TypeCon::Int));

        types.insert(db.intern_name(Name::new("f32")), Type::Con(TypeCon::Float));
        kind.insert(db.intern_name(Name::new("f32")), TypeKind::Type);

        types.insert(db.intern_name(Name::new("bool")), Type::Con(TypeCon::Bool));
        kind.insert(db.intern_name(Name::new("bool")), TypeKind::Type);

        types.insert(db.intern_name(Name::new("void")), Type::Con(TypeCon::Void));
        kind.insert(db.intern_name(Name::new("void")), TypeKind::Type);

        types.insert(db.intern_name(Name::new("string")), Type::Con(TypeCon::Str));
        kind.insert(db.intern_name(Name::new("string")), TypeKind::Type);

        let result_name = db.intern_name(Name::new("Result"));
        kind.insert(db.intern_name(Name::new("Result")), TypeKind::Enum);

        let mut result_variants = HashMap::new();

        result_variants.insert(
            db.intern_name(Name::new("Ok")),
            Variant {
                tag: 0,
                ty: Some(Type::Var(TypeVar::from(0))), // Ok(T)
            },
        );

        result_variants.insert(
            db.intern_name(Name::new("Err")),
            Variant {
                tag: 1,
                ty: Some(Type::Var(TypeVar::from(1))), // Err(U)
            },
        );
        types.insert(
            result_name,
            Type::Poly(
                vec![TypeVar::from(0), TypeVar::from(1)],
                Box::new(Type::Enum(result_variants)),
            ),
        );

        Self {
            types,
            tvar_count: 2,
            kind,
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

    pub(crate) fn get_kind(&self, name: &NameId) -> TypeKind {
        self.kind[name]
    }

    pub(crate) fn insert_type(&mut self, name: NameId, ty: Type, kind: TypeKind) {
        self.types.insert(name, ty);
        self.kind.insert(name, kind);
    }
}
