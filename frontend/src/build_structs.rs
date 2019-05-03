use crate::ast as t;
use crate::infer::types;
use ir::instructions::{Layout, StructLayout};
use ir::{FUNCTION_SIZE, POINTER_SIZE};
use std::collections::HashMap;
use util::symbol::{Symbol, Symbols};
#[derive(Debug)]
struct Builder<'a> {
    symbols: &'a Symbols<()>,
    current_layout: HashMap<Symbol, usize>,
}

impl<'a> Builder<'a> {
    pub fn new(symbols: &'a Symbols<()>) -> Self {
        Builder {
            symbols,
            current_layout: HashMap::new(),
        }
    }

    pub fn layout(self) -> HashMap<Symbol, usize> {
        self.current_layout
    }

    pub fn size_of(&self, ty: types::Type) -> Layout {
        match ty {
            types::Type::App(types::TypeCon::Array(inner), _) => self.size_of(*inner),
            types::Type::App(types::TypeCon::Arrow, _) => Layout::new(FUNCTION_SIZE, FUNCTION_SIZE),
            types::Type::App(types::TypeCon::Bool, _) => Layout::new(1, 1),
            types::Type::App(types::TypeCon::Float, _) => Layout::new(8, 4),
            types::Type::App(types::TypeCon::Str, _) => Layout::new(POINTER_SIZE, POINTER_SIZE),
            types::Type::App(types::TypeCon::Int, _) => Layout::new(8, 4),
            types::Type::App(types::TypeCon::Void, _) => Layout::new(0, 1),

            _ => unimplemented!(),
        }
    }

    pub fn add_field() {}
}

pub(crate) fn build_struct(class: t::Class) -> StructLayout {
    unimplemented!()
}
