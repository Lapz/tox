use std::collections::HashMap;
use ast::statement::*;
use ast::expr::*;
use std::fmt::{Display, Formatter};
use std::fmt;

#[derive(Debug)]
pub struct Resolver<'a> {
    scopes:Vec<HashMap<Variable<'a>,bool>>,
    pub current_function: FunctionType,
    pub current_class: ClassType,
    locals: HashMap<VariableUseHandle, usize>,
}

#[derive(Debug)]
pub enum ResolverError {
        ReadInInit(String),
        AllReadyDecleared(String),
        TopLevelReturn,
        This,
        Init,
}

impl Display for ResolverError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
           ResolverError::ReadInInit(ref v) => {
                    write!(f, "Cannot read local variable '{}' in its own initializer.", v)
            },
            
            ResolverError::Init => write!(f, "Cannot return a value from an initializer."),
            ResolverError::AllReadyDecleared(ref v) => {
                    write!(f, "Variable with name '{}', already declared in this scope.", v)
            },
            ResolverError::This => write!(f, "Cannot use 'this' outside of a class."),
            ResolverError::TopLevelReturn => write!(f, "Cannot return from top-level code."),
            }
        }
}

#[derive(Debug,Clone,Copy,PartialEq)]
pub enum FunctionType {
     Function,
    None,
    Method,
    Init,
}

#[derive(Clone,Copy,Debug,PartialEq)]
pub enum ClassType {
    None,
    Class, 
}

pub trait Resolve {
    fn resolve(&self, resolver: &mut Resolver) -> Result<(), ResolverError>;
}

impl <'a> Default for Resolver<'a> {
    fn default() -> Self {
        Resolver {
            scopes: vec![],
            current_function: FunctionType::None,
            current_class: ClassType::None,
            locals: HashMap::new(),
        }
    }
}


impl <'a> Resolver <'a> {
    pub fn new() -> Self {
        Self::default()
    }


    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new())
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn peek(&self) -> usize {
        self.scopes.len() - 1
    }


    fn not_resolved(&self, name: &Variable<'a>) -> bool {
        !self.scopes.is_empty() && self.scopes[self.peek()].get(name) == Some(&false)
    }


    pub fn declare(&mut self, name: Variable<'a>) -> Result<(), ResolverError> {
        if self.scopes.is_empty() {
            return Ok(());
        }

        let index = self.peek();

        if self.scopes[index].contains_key(&name) {
            return Err(ResolverError::AllReadyDecleared(name.0.to_owned()));
        }

        self.scopes[index].insert(name, false);

        Ok(())
    }

    pub fn insert(&mut self, name: Variable<'a>, state: bool) {
        let top = self.peek();
        self.scopes[top].insert(name, state);
    }

    pub fn define(&mut self, name: Variable<'a>) {
        if self.scopes.is_empty() {
            return;
        };

        let index = self.peek();
        self.scopes[index].insert(name, true);
    }

    pub fn resolve_local(&mut self, name: &Variable<'a>, handle: VariableUseHandle) {
        let max_depth = self.scopes.len();

        for i in 0..max_depth {
            if self.scopes[max_depth - i - 1].contains_key(name) {
                self.locals.insert(handle, i);
                return;
            }
        }

        self.locals.insert(handle, max_depth); // Globals
    }


    // fn resolve_func(
    //     &mut self,
    //     function: Expression<'a>,
    //     kind: FunctionType,
    // ) -> Result<(), ResolverError> {
    //     let enclosing_function = self.current_function;

    //     self.current_function = kind;

    //     let function = match function {
    //         Expression::Func(ref func) => func,
    //         _ => unreachable!(),
    //     };

    //     self.begin_scope();

    //     for param in &function.parameters {
    //         self.declare(param.clone())?;
    //         self.define(param.clone());
    //     }

    //     function.body.resolve(self)?;

    //     self.end_scope();

    //     self.current_function = enclosing_function;

    //     Ok(())
    // }
}