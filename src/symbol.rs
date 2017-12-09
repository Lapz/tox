use std::fmt;
use std::fmt::Formatter;
use std::fmt::Display;
use std::collections::HashMap;

pub type Symbol = u64;

pub struct Strings<'a> {
    next:Symbol,
    map:HashMap<Symbol,&'a str>
}

impl <'a> Strings<'a> {
    fn new() -> Self {
        Symbol {
            next:0,
            map:HashMap::new(),
        }
    }
}

pub struct Symbols<'a,T> {
    strings:Strings<'a>,
    table:HashMap<Symbol,Vec<T>>
}

impl <'a,T> Symbol<T> {
    fn symbol(&mut self, symbol:&'a str) -> Symbol {
        unimplemented!()
    }

    fn name(&mut self, name:Symbol) -> Option<&'a str> {
        unimplemented!()
    }

    fn empty(&mut self) {
        unimplemented!()
    }

    fn enter(&mut self,symbol:Symbol) {
unimplemented!()
    }

    fn look(&self,symbol:Symbol) {
unimplemented!()
    }
}