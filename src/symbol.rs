use std::fmt;
use std::fmt::Formatter;
use std::fmt::Display;
use std::collections::HashMap;

#[derive(Debug,Clone,Eq,PartialEq,Hash,Copy)]
pub struct Variable(u64);

pub struct VariableMap<'a> {
    next:u64,
    map:HashMap<Variable,&'a str>,
}

pub struct Symbols<'a,T> {
    variables:VariableMap<'a>,
    scopes:Vec<Vec<T>>,
}


impl <'a,T> Symbols<'a,T> {
    pub fn new() -> Self {
        Symbols {
            variables:VariableMap::new(),
            scopes:vec![],
        }
    }

    pub fn begin_scope(&mut self) {
        self.scopes.push(vec![]);
    }

    pub fn end_scop(&mut self) {
        for 
    }
}

impl <'a> VariableMap<'a> {
    pub fn new() -> Self {
        let mut map = HashMap::new();
        map.insert(Variable(0),"this",);
        map.insert(Variable(1),"init",);

        VariableMap {
            next:2,
            map
        }
    }

    pub fn name(&self,var:Variable) -> Option<&&'a str> {
        self.map.get(&var)
    }

    pub fn symbol(&mut self, name:&'a str) -> Variable {
        for (key,value) in self.map.iter() {
            if value == &name {
                return *key;
            }
        }
        let variable = Variable(self.next);
        self.next +=1;

        variable
    }

}