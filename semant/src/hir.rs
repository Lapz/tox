pub(crate) mod function;

use crate::db;

pub(crate) use function::{Function, FunctionAstMap};
use std::collections::HashMap;
use syntax::{ast, text_of_first_token, AstNode, AstPtr, SmolStr, SyntaxKind, TextRange, T};
pub type Span = TextRange;
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeParamId(pub(crate) u64);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ParamId(pub(crate) u64);
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]

pub struct StmtId(pub(crate) u64);
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]

pub struct BodyId(pub(crate) u64);

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

#[derive(Debug, Eq, PartialEq)]
pub struct Param {
    pub(crate) pat: PatId,
    pub(crate) ty: TypeId,
}

#[derive(Debug, Eq, PartialEq)]
pub struct TypeParam {
    pub(crate) name: NameId,
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

create_intern_key!(ClassId);
create_intern_key!(EnumId);
create_intern_key!(TypeAliasId);
create_intern_key!(NameId);
create_intern_key!(TypeId);
create_intern_key!(PatId);

create_intern_key!(LiteralId);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExprId(pub(crate) u64);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Pattern {
    Bind { name: Name },
    Placeholder,
    Tuple(Vec<PatId>),
    Literal(LiteralId),
}

#[derive(Debug, Hash, Clone, PartialEq, Eq)]
pub enum Literal {
    String(SmolStr),
    Nil,
    True,
    False,
    Int(SmolStr),
    Float(SmolStr),
}

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

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Stmt {
    Let {
        pat: PatId,
        initializer: Option<ExprId>,
    },
    Expr(ExprId),
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Array(Vec<ExprId>),
    Binary { lhs: ExprId, op: BinOp, rhs: ExprId },
    Block(Vec<StmtId>),
    Break,
    Call { callee: ExprId, args: Vec<ExprId> },
    Cast { expr: ExprId, ty: TypeId },
    Continue,
    If { cond: ExprId },
    Ident(NameId),
    Index { base: ExprId, index: ExprId },
    While { cond: ExprId, body: Vec<StmtId> },
    Literal(LiteralId),
    Paren(ExprId),
    Unary { op: UnaryOp, expr: ExprId },
    Return(Option<ExprId>),
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum BinOp {
    Plus,
    Minus,
    Mult,
    Div,
    And,
    Or,
    LessThan,
    GreaterThan,
    EqualEqual,
    Excl,
    NotEqual,
    LessThanEqual,
    GreaterThanEqual,
    PlusEqual,
    MinusEqual,
    MultEqual,
    DivEqual,
}
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum UnaryOp {
    Minus,
    Excl,
}

impl UnaryOp {
    pub(crate) fn from_kind(kind: SyntaxKind) -> Option<UnaryOp> {
        let op = match kind {
            T![-] => UnaryOp::Minus,
            T![!] => UnaryOp::Excl,
            _ => return None,
        };

        Some(op)
    }
}

impl BinOp {
    pub(crate) fn from_kind(kind: SyntaxKind) -> Option<BinOp> {
        let op = match kind {
            T![-] => BinOp::Minus,
            T![+] => BinOp::Plus,
            T![*] => BinOp::Mult,
            T![/] => BinOp::Div,
            T![&&] => BinOp::And,
            T![||] => BinOp::Or,
            T![<] => BinOp::LessThan,
            T![>] => BinOp::GreaterThan,
            T![==] => BinOp::EqualEqual,
            T![!] => BinOp::Excl,
            T![!=] => BinOp::NotEqual,
            T![<=] => BinOp::LessThanEqual,
            T![>=] => BinOp::GreaterThanEqual,
            T![+=] => BinOp::PlusEqual,
            T![-=] => BinOp::MinusEqual,
            T![*=] => BinOp::MultEqual,
            T![/=] => BinOp::DivEqual,
            _ => return None,
        };
        Some(op)
    }
}

impl Literal {
    pub(crate) fn from_token(token: syntax::SyntaxToken) -> Literal {
        use syntax::SyntaxKind::*;
        let text = token.text().clone();
        let kind = token.kind();
        match kind {
            INT_NUMBER => Literal::Int(text),
            STRING => Literal::String(text),
            FLOAT_NUMBER => Literal::Float(text),
            T![true] => Literal::True,
            T![false] => Literal::False,
            T![nil] => Literal::Nil,
            _ => unreachable!(),
        }
    }
}
