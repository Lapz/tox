//! This module provides a Table which keeps a track of the mappings between a
//! `Symbol` and a `String`

use std::fmt::{Display, Formatter, Result};
use std::collections::{HashMap};
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Hash, Copy)]
pub struct Symbol(pub u64);
impl Display for Symbol {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "Symbol {}", self.0)
    }
}
#[derive(Debug, Clone)]
pub struct SymbolFactory {
    next: RefCell<u64>,
    mappings: RefCell<HashMap<Symbol, String>>,
}

#[derive(Debug, Clone)]
pub struct Table<T: Clone> {
    strings: Rc<SymbolFactory>,
    table: HashMap<Symbol, Vec<T>>,
    scopes: Vec<Option<Symbol>>,
}

impl<T: Clone> Table<T> {
    /// A new Table Instance
    pub fn new(strings: Rc<SymbolFactory>) -> Self {
        Table {
            strings,
            table: HashMap::new(),
            scopes: vec![],
        }
    }

    /// Adds a new scope to the table
    pub fn begin_scope(&mut self) {
        self.scopes.push(None);
    }
    /// Recursivly destorys the scopes
    pub fn end_scope(&mut self) {
        while let Some(Some(symbol)) = self.scopes.pop() {
            let mapping = self.table.get_mut(&symbol).expect("Symbol not in table");
            mapping.pop();
        }
    }

    /// Enters a peice of data into the current scope
    pub fn enter(&mut self, symbol: Symbol, data: T) {
        let mapping = self.table.entry(symbol).or_insert_with(Vec::new);
        mapping.push(data);
        
        self.scopes.push(Some(symbol));
    }

    /// Looks in the table for the `Symbol` and if found returns the top element in
    /// the stack of Vec<T>
    pub fn look(&self, symbol: Symbol) -> Option<&T> {
        self.table.get(&symbol).and_then(|vec| vec.last())
    }

    /// Finds the name given to a `Symbol`
    pub fn name(&self, symbol: Symbol) -> String {
        self.strings.mappings.borrow()[&symbol].to_owned()
    }

    /// Checks if the given name allready exists within the table
    /// a new `Symbol` is returned else the previous `Symbol`
    pub fn symbol(&mut self, name: &str) -> Symbol {
        for (key, value) in self.strings.mappings.borrow().iter() {
            if value == name {
                return *key;
            }
        }
        let symbol = Symbol(*self.strings.next.borrow());
        self.strings
            .mappings
            .borrow_mut()
            .insert(symbol, name.to_owned());
        *self.strings.next.borrow_mut() += 1;
        symbol
    }
    /// Inserts the `Symbol` into the table
    pub fn replace(&mut self, symbol: Symbol, data: T) {
        let bindings = self.table.entry(symbol).or_insert_with(Vec::new);
        bindings.pop().expect("Call enter() before replace()");
        bindings.push(data);
    }
}

impl SymbolFactory {
    pub fn new() -> Self {
        let mut map = HashMap::new();
        map.insert(Symbol(0), "this".into());
        map.insert(Symbol(1), "super".into());

        SymbolFactory {
            next: RefCell::new(2),
            mappings: RefCell::new(map),
        }
    }
}

#[cfg(test)]
mod test {
    use symbol::{Symbol, SymbolFactory, Table};
    use std::rc::Rc;

    #[test]
    fn test() {
        let strings = Rc::new(SymbolFactory::new());
        let mut map: Table<String> = Table::new(strings);
        map.enter(Symbol(0), "a".into());
        map.enter(Symbol(1), "b".into());
        map.begin_scope();
        assert_eq!(Some(&"a".into()), map.look(Symbol(0)));
        assert_eq!(Some(&"b".into()), map.look(Symbol(1)));
        assert_eq!(None, map.look(Symbol(2)));
        map.enter(Symbol(1), "a".into());
        map.enter(Symbol(2), "c".into());
        assert_eq!(Some(&"a".into()), map.look(Symbol(1)));
        assert_eq!(Some(&"c".into()), map.look(Symbol(2)));
        map.end_scope();
        assert_eq!(Some(&"a".into()), map.look(Symbol(0)));
        assert_eq!(map.symbol("c".into()), Symbol(2));
    }
}
