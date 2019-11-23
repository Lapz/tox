use crate::db;
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

impl From<ast::IdentType> for Name {
    fn from(name: ast::IdentType) -> Name {
        Name(text_of_first_token(name.syntax()).clone())
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
    name: NameId,
    params: Vec<Param>,
}

#[derive(Debug)]
pub struct Param {
    pub(crate) pat: PatId,
    pub(crate) ty: TypeId,
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
    fn new(id: FunctionId, name: NameId) -> Self {
        Function {
            id,
            name,
            params: Vec::new(),
        }
    }

    fn name(&self, db: &impl db::HirDatabase) -> Name {
        db.lookup_intern_name(db.function_data(self.id).name)
    }

    fn body() {}

    fn ty(&self, db: &impl db::HirDatabase) {
        // db.look_up()
    }

    fn infer() {}
}

create_intern_key!(ClassId);
create_intern_key!(EnumId);
create_intern_key!(TypeAliasId);
create_intern_key!(NameId);
create_intern_key!(TypeId);
create_intern_key!(PatId);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExprId(u32);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Pattern {
    Bind { name: Name },
    Placeholder,
    Tuple(Vec<PatId>),
    Literal(ExprId),
}

#[derive(Debug)]
pub enum Literal {}

#[derive(Debug, Hash, Clone, PartialEq, Eq)]
pub enum Type {
    ParenType(Vec<TypeId>),
    /// An array type with no supplied size is assumed to be dynamic in growth
    /// If the size is present the array has a static size
    ArrayType {
        ty: TypeId,
        size: Option<usize>,
    },
    FnType {
        params: Vec<TypeId>,
        ret: Option<TypeId>,
    },
    Ident(NameId),
}
