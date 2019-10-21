use crate::SyntaxNode;
use std::collections::HashMap;
use syntax::{ast, text_of_first_token, AstNode, AstPtr, SmolStr};

pub struct Ctx {
    functions: FunctionMap,
}

impl Ctx {
    pub fn new() -> Self {
        Ctx {
            functions: FunctionMap::new(),
        }
    }

    pub(crate) fn add_function(&mut self, fn_def: ast::FnDef) -> FunctionId {
        unimplemented!()
        // self.functions.add_function(fn_def)
    }
}
macro_rules! create_intern_key {
    ($name:ident) => {
        #[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
        pub struct $name(salsa::InternId);
        impl salsa::InternKey for $name {
            fn from_intern_id(v: salsa::InternId) -> Self {
                $name(v)
            }
            fn as_intern_id(&self) -> salsa::InternId {
                self.0
            }
        }
    };
}
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Name(SmolStr);

impl Name {
    pub fn missing() -> Self {
        Name(SmolStr::new("missing name"))
    }
}

impl From<ast::Name> for Name {
    fn from(name: ast::Name) -> Name {
        Name(text_of_first_token(name.syntax()).clone())
    }
}

create_intern_key!(FunctionId);
#[derive(Debug)]
pub struct Function {
    id: FunctionId,
}

pub(crate) struct FunctionMap {
    functions: HashMap<FunctionId, Function>,
    nodes: HashMap<FunctionId, AstPtr<ast::FnDef>>,
    id_counter: u32,
}

impl std::ops::Index<FunctionId> for FunctionMap {
    type Output = Function;

    fn index(&self, id: FunctionId) -> &Self::Output {
        &self.functions[&id]
    }
}

impl FunctionMap {
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
            nodes: HashMap::new(),
            id_counter: 0,
        }
    }

    // pub fn add_function(&mut self, fn_def: ast::FnDef) -> FunctionId {
    //     let id = FunctionId(Id::new(self.id_counter));

    //     self.id_counter += 1;

    //     let function = Function::new(id);

    //     self.functions.insert(id, function);

    //     self.nodes.insert(id, AstPtr::new(&fn_def));

    //     id
    // }
}

impl Function {
    fn new(id: FunctionId) -> Self {
        Function { id }
    }
    fn name(&self, ctx: &Ctx) -> ast::Name {
        unimplemented!()
    }

    fn body() {}

    fn ty() {}

    fn infer() {}
}

create_intern_key!(ClassId);
create_intern_key!(EnumId);
create_intern_key!(TypeAliasId);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExprId(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PatId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NameId(u32);

#[derive(Debug, Clone)]
pub enum Pattern {
    Bind { name: Name },
    Placeholder,
    Tuple(Vec<PatId>),
    Literal(ExprId),
}

pub enum Literal {}
