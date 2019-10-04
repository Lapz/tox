use crate::SyntaxNode;
use std::collections::HashMap;
use syntax::{ast, AstNode, AstPtr};

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
        self.functions.add_function(fn_def)
    }
}
#[derive(PartialEq, Eq, Clone, Copy, Hash)]
pub struct Id(u32);

impl Id {
    pub fn new(id: u32) -> Self {
        Id(id)
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Hash)]
pub struct FunctionId(pub Id);

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

    pub fn add_function(&mut self, fn_def: ast::FnDef) -> FunctionId {
        let id = FunctionId(Id::new(self.id_counter));

        self.id_counter += 1;

        let function = Function::new(id);

        self.functions.insert(id, function);

        self.nodes.insert(id, AstPtr::new(&fn_def));

        id
    }
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
