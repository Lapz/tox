use std::fmt;
use std::fmt::Formatter;
use std::fmt::Display;
use std::collections::HashMap;

#[derive(Debug,Clone,Copy)]
pub struct Variable(u64);

pub struct VariableMap<'a> {
    next:u64,
    map:HashMap<&'a str,Variable>
}

impl <'a> VariableMap<'a> {
    pub fn new() -> Self {
        let mut map = HashMap::new();
        map.insert("this",Variable(0));
        map.insert("init",Variable(1));

        VariableMap {
            next:2,
            map
        }
    }

    pub fn from_name(&mut self,name:&'a str) -> Variable {
        if let Some(variable) = self.map.get(&name)  {
            return *variable;
        }

        let variable = Variable(self.next);

        self.map.insert(name,variable);

        self.next += 1;

        variable
    } 

    pub fn find(&self, variable:&Variable) -> Option<&'a str> {
        self.map.iter().find((|&(_,v)| v == variable)).map(|(k,_)| k)
    }
}