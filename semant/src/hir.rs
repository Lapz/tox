use crate::{infer, util};
use errors::FileId;
use indexmap::IndexMap;

use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
    path::Path,
    sync::Arc,
};
use syntax::{ast, text_of_first_token, AstNode, SmolStr, SyntaxKind, TextRange, T};
#[derive(Debug, Default, Eq, PartialEq, Clone, Hash)]
pub struct SourceFile {
    pub imports: Vec<Arc<Import>>,
    pub modules: Vec<Arc<Module>>,
    pub functions: Vec<Arc<Function>>,
    pub type_alias: Vec<Arc<TypeAlias>>,
    pub classes: Vec<Arc<Class>>,
    pub enums: Vec<Arc<Enum>>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct PatId(pub(crate) u64);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeParamId(pub(crate) u64);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ParamId(pub(crate) u64);
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]

pub struct StmtId(pub(crate) u64);
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]

pub struct BodyId(pub(crate) u64);

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Name(SmolStr);

pub const PLACEHOLDER_NAME: Name = Name(SmolStr::new_inline_from_ascii(1, b"_"));

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Import {
    pub(crate) id: ImportId,
    pub(crate) segments: Vec<Segment>,
    pub(crate) file: FileId,
    pub(crate) span: TextRange,
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Segment {
    pub(crate) name: util::Span<NameId>,
    pub(crate) nested_imports: Vec<util::Span<NameId>>,
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct Function {
    pub exported: bool,
    pub name: util::Span<NameId>,
    pub ast_map: Arc<FunctionAstMap>,
    pub params: Vec<util::Span<ParamId>>,
    pub type_params: Vec<util::Span<TypeParamId>>,
    pub body: Option<Vec<util::Span<StmtId>>>,
    pub returns: Option<util::Span<TypeId>>,
    pub span: TextRange,
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct Class {
    pub(crate) exported: bool,
    pub name: util::Span<NameId>,
    pub(crate) ast_map: FunctionAstMap,
    pub(crate) type_params: Vec<util::Span<TypeParamId>>,
    pub(crate) fields: Vec<util::Span<Field>>,
    pub methods: Vec<Arc<Function>>,
    pub(crate) span: TextRange,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Enum {
    pub(crate) exported: bool,
    pub(crate) name: util::Span<NameId>,
    pub(crate) ast_map: FunctionAstMap,
    pub(crate) type_params: Vec<util::Span<TypeParamId>>,
    pub(crate) variants: Vec<util::Span<EnumVariant>>,
    pub(crate) span: TextRange,
}

#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone)]
pub struct Field {
    pub(crate) property: util::Span<NameId>,
    pub(crate) ty: util::Span<TypeId>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumVariant {
    pub(crate) ty: Option<util::Span<TypeId>>,
    pub(crate) name: NameId,
}
/// A symbol is composed of a name and the file it belongs to
/// Symbols with the same name but from different files are not the sames
/// i.e
/// export foo {
///
/// };
/// export foo {
///
/// };
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol(pub(crate) FunctionId, pub(crate) FileId);

impl Name {
    pub fn missing() -> Self {
        Name(SmolStr::new("missing name"))
    }

    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }

    pub fn new<T: AsRef<str> + Into<String>>(s: T) -> Self {
        Name(SmolStr::new(s))
    }
}

impl std::fmt::Display for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)
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

impl AsRef<str> for Name {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl AsRef<Path> for Name {
    fn as_ref(&self) -> &Path {
        &Path::new(self.0.as_str())
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct Param {
    pub pat: util::Span<PatId>,
    pub(crate) ty: util::Span<TypeId>,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct TypeParam {
    pub(crate) name: util::Span<NameId>,
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct TypeAlias {
    pub(crate) name: util::Span<NameId>,
    pub(crate) exported: bool,
    pub(crate) type_params: Vec<util::Span<TypeParamId>>,
    pub(crate) ty: util::Span<TypeId>,
    pub(crate) ast_map: FunctionAstMap,
    pub(crate) span: TextRange,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Module {
    pub(crate) id: ModuleId,
    pub(crate) name: util::Span<NameId>,
    pub(crate) file: FileId,
    pub(crate) span: TextRange,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExprId(pub(crate) u64);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BlockId(pub(crate) u64);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Pattern {
    Bind { name: util::Span<NameId> },
    Placeholder,
    Tuple(Vec<util::Span<PatId>>),
    Literal(LiteralId),
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct MatchArm {
    pub(crate) pats: Vec<util::Span<PatId>>,
    pub(crate) expr: util::Span<ExprId>,
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
    ParenType(Vec<util::Span<TypeId>>),
    /// An array type with no supplied size is assumed to be dynamic in growth
    /// If the size is present the array has a static size
    ArrayType {
        ty: util::Span<TypeId>,
        size: Option<usize>,
    },
    FnType {
        params: Vec<util::Span<TypeId>>,
        ret: Option<util::Span<TypeId>>,
    },
    Poly {
        name: NameId,
        type_args: Vec<util::Span<TypeId>>,
    },
    Ident(NameId),
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Stmt {
    Let {
        pat: util::Span<PatId>,
        ascribed_type: Option<util::Span<TypeId>>,
        initializer: Option<util::Span<ExprId>>,
    },
    Expr(util::Span<ExprId>),
    Error,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Block(pub Vec<util::Span<StmtId>>);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    Array(Vec<util::Span<ExprId>>),
    Binary {
        lhs: util::Span<ExprId>,
        op: BinOp,
        rhs: util::Span<ExprId>,
    },
    Block(BlockId, bool),
    Break,
    Call {
        callee: util::Span<ExprId>,
        args: Vec<util::Span<ExprId>>,
        type_args: util::Span<Vec<util::Span<TypeId>>>,
    },
    Cast {
        expr: util::Span<ExprId>,
        ty: util::Span<TypeId>,
    },
    Closure {
        params: Vec<util::Span<ParamId>>,
        body: util::Span<BlockId>,
        returns: Option<util::Span<TypeId>>,
    },
    Continue,
    If {
        cond: util::Span<ExprId>,
        then_branch: util::Span<ExprId>,
        else_branch: Option<util::Span<ExprId>>,
    },
    Ident(util::Span<NameId>),
    Index {
        base: util::Span<ExprId>,
        index: util::Span<ExprId>,
    },
    While {
        cond: util::Span<ExprId>,
        body: BlockId,
    },
    Literal(LiteralId),
    Paren(util::Span<ExprId>),
    Tuple(Vec<util::Span<ExprId>>),
    Unary {
        op: UnaryOp,
        expr: util::Span<ExprId>,
    },
    Return(Option<util::Span<ExprId>>),
    Match {
        expr: util::Span<ExprId>,
        arms: Vec<MatchArm>,
    },
    Enum {
        def: util::Span<NameId>,
        variant: util::Span<NameId>,
        expr: Option<util::Span<ExprId>>,
    },
    RecordLiteral {
        def: util::Span<NameId>,
        fields: Vec<(util::Span<NameId>, util::Span<ExprId>)>,
    },
    Field(Vec<util::Span<ExprId>>),
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum BinOp {
    Plus,
    Minus,
    Mult,
    Div,
    And,
    Or,
    LessThan,
    GreaterThan,
    Equal,
    EqualEqual,
    NotEqual,
    LessThanEqual,
    GreaterThanEqual,
    PlusEqual,
    MinusEqual,
    MultEqual,
    DivEqual,
}
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum UnaryOp {
    Minus,
    Excl,
}

macro_rules! create_intern_key {
    ($name:ident) => {
        #[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
        pub struct $name(pub salsa::InternId);
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

/// A map of id's that are a local to an item
/// Allows one to go from id => hir::item
///  and from id => astPtr
#[derive(Debug, Default, Eq, PartialEq, Clone)]
pub struct FunctionAstMap {
    hir_to_pattern: IndexMap<PatId, Pattern>,
    hir_to_params: IndexMap<ParamId, Param>,
    hir_to_type_params: IndexMap<TypeParamId, TypeParam>,
    hir_to_block: IndexMap<BlockId, Block>,
    hir_to_stmt: IndexMap<StmtId, Stmt>,
    hir_to_expr: IndexMap<ExprId, Expr>,
    expr_to_type: IndexMap<ExprId, infer::Type>,
    // ast_to_expr: IndexMap<ExprId, AstPtr<ast::Expr>>,
}

impl FunctionAstMap {
    pub fn insert_param(&mut self, id: ParamId, param: Param) {
        self.hir_to_params.insert(id, param);
    }

    pub fn insert_type_param(&mut self, id: TypeParamId, param: TypeParam) {
        self.hir_to_type_params.insert(id, param);
    }

    pub fn insert_stmt(&mut self, id: StmtId, stmt: Stmt) {
        self.hir_to_stmt.insert(id, stmt);
    }

    pub fn insert_expr(&mut self, id: ExprId, expr: Expr) {
        self.hir_to_expr.insert(id, expr);
    }

    pub fn insert_block(&mut self, id: BlockId, block: Block) {
        self.hir_to_block.insert(id, block);
    }

    pub fn insert_pat(&mut self, id: PatId, pat: Pattern) {
        self.hir_to_pattern.insert(id, pat);
    }

    pub fn stmt(&self, id: &StmtId) -> &Stmt {
        &self.hir_to_stmt[id]
    }

    pub fn expr(&self, id: &ExprId) -> &Expr {
        &self.hir_to_expr[id]
    }

    pub fn block(&self, id: &BlockId) -> &Block {
        &self.hir_to_block[id]
    }

    pub fn pat(&self, id: &PatId) -> &Pattern {
        &self.hir_to_pattern[id]
    }

    pub fn type_param(&self, id: &TypeParamId) -> &TypeParam {
        &self.hir_to_type_params[id]
    }

    pub fn param(&self, id: &ParamId) -> &Param {
        &self.hir_to_params[id]
    }
}

macro_rules! hash {
    ($state:expr => $( $field:expr ),*) => {
        {
            $(
            $state.write_u64(
            $field
                .values()
                .map(|kv| {
                    let mut h = DefaultHasher::new();
                    kv.hash(&mut h);
                    h.finish()
                })
                .fold(0, u64::wrapping_add),
            );
        )*
        }
    };

}

impl Hash for FunctionAstMap {
    fn hash<H: Hasher>(&self, state: &mut H) {
        hash!(state => self.hir_to_params,

            self.hir_to_stmt,
            self.hir_to_expr,
            self.hir_to_block,
            self.hir_to_pattern,
            self.hir_to_type_params,
            self.hir_to_params
        )
    }
}

create_intern_key!(ClassId);
create_intern_key!(EnumId);
create_intern_key!(TypeAliasId);
create_intern_key!(NameId);
create_intern_key!(FunctionId);
create_intern_key!(TypeId);
create_intern_key!(LiteralId);
create_intern_key!(ModuleId);
create_intern_key!(ImportId);

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
            T![=] => BinOp::Equal,
            T![&&] => BinOp::And,
            T![||] => BinOp::Or,
            T![<] => BinOp::LessThan,
            T![>] => BinOp::GreaterThan,
            T![==] => BinOp::EqualEqual,
            T![!=] => BinOp::NotEqual,
            T![<=] => BinOp::LessThanEqual,
            T![>=] => BinOp::GreaterThanEqual,
            T![+=] => BinOp::PlusEqual,
            T![-=] => BinOp::MinusEqual,
            T![*=] => BinOp::MultEqual,
            T![/=] => BinOp::DivEqual,
            _ => {
                return None;
            }
        };
        Some(op)
    }
}

impl Literal {
    pub(crate) fn from_token(token: syntax::SyntaxToken) -> Literal {
        use syntax::SyntaxKind::*;
        let kind = token.kind();
        let text = if kind == STRING {
            let text = token.text();
            SmolStr::new(&text[1..text.len() - 1]) // our lexer includes \" so we need to strip it out of strings
        } else {
            token.text().clone()
        };

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
