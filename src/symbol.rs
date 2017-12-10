use std::fmt;
use std::fmt::Formatter;
use std::fmt::Display;
use std::collections::HashMap;

#[derive(Debug,Clone,Eq,PartialEq,PartialOrd,Hash,Copy)]
pub struct Symbol(u64);

#[derive(Debug)]
struct SymbolFactory<'a>{
    next:u64,
    mappings:HashMap<Symbol,&'a str>,
}

#[derive(Debug)]
pub struct Symbols<'a,T> {
    strings: SymbolFactory<'a>,
    table:HashMap<Symbol,Vec<T>>,
    scopes:Vec<Option<Symbol>>,
}


impl <'a,T> Symbols<'a,T> {
    pub fn new() -> Self {
        Symbols {
            strings:SymbolFactory::new(),
            table:HashMap::new(),
            scopes:vec![],
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

    pub fn enter(&mut self, symbol:Symbol,data:T) {
        let mapping = self.table.entry(symbol).or_insert(vec![]);

        mapping.push(data);
        self.scopes.push(Some(symbol));
    }

    /// Looks in the table for the symbol and if found returns the top element in
    /// the stack of Vec<T>
    pub fn look(&mut self,symbol:Symbol) -> Option<&T> {
        self.table.get(&symbol).and_then(|vec| vec.last())
    }

      pub fn name(&self,symbol:Symbol) -> Option<&&'a str> {
        self.strings.mappings.get(&symbol)
    }

    pub fn symbol(&mut self, name:&'a str) -> Symbol {
        for (key,value) in self.strings.mappings.iter() {
            if value == &name {
                return *key;
            }
        }
        let symbol = Symbol(self.strings.next);
        self.strings.next +=1;
        symbol
    }


}

impl <'a>  SymbolFactory<'a> {
    pub fn new() -> Self {
        let mut map = HashMap::new();
        map.insert(Symbol(0),"this",);
        map.insert(Symbol(1),"init",);

        SymbolFactory{
            next:2,
            mappings:map
        }
    }
}

#[cfg(test)]
mod test {
    use symbol::{Symbol,Symbols};
    
    #[test]
    fn test() {
        let mut map = Symbols::new();
        map.enter(Symbol(0),"a");
        map.enter(Symbol(1), "b");
        map.begin_scope();
        assert_eq!(Some(&"a"),map.look(Symbol(0)));
        assert_eq!(Some(&"b"),map.look(Symbol(1)));
        assert_eq!(None,map.look(Symbol(2)));
        map.enter(Symbol(1),"a");
        map.enter(Symbol(2), "c");
        assert_eq!(Some(&"a"),map.look(Symbol(1)));
        assert_eq!(Some(&"c"),map.look(Symbol(2)));
        map.end_scope();
        assert_eq!(Some(&"a"),map.look(Symbol(0)));
        assert_eq!(map.symbol("c"),Symbol(2));
        
    }
}