use std::collections::HashMap;

#[derive(Debug, Default)]
pub struct SymbolTable {
    mappings: HashMap<Symbol, String>,
    next: usize,
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Hash, Copy)]
pub struct Symbol {
    offset: usize,
    ty: SymbolType,
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Hash, Copy)]
pub enum SymbolType {
    Label,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add(&mut self, name: String, offset: usize, ty: SymbolType) {
        for (key, value) in self.mappings.iter() {
            if value == &name {
                return;
            }
        }

        let symbol = Symbol { offset, ty };

        self.mappings.insert(symbol, name);
        self.next += 1;
    }

    pub fn offset(&self, name: &str) -> Option<usize> {
        for (key, value) in self.mappings.iter() {
            if value == name {
                return Some(key.offset);
            }
        }

        None
    }
}
