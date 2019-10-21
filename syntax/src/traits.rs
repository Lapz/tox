use crate::ast::{self, SyntaxKind};

use crate::{SyntaxNode, SyntaxNodeChildren, SyntaxToken};
use rowan::{SmolStr, TextRange};
use std::iter::successors;
use std::marker::PhantomData;
/// The main trait to go from untyped `SyntaxNode`  to a typed ast. The
/// conversion itself has zero runtime cost: ast and syntax nodes have exactly
/// the same representation: a pointer to the tree root and a pointer to the
/// node itself.
pub trait AstNode: Clone {
    fn can_cast(kind: SyntaxKind) -> bool;

    fn cast(syntax: SyntaxNode) -> Option<Self>
    where
        Self: Sized;
    fn syntax(&self) -> &SyntaxNode;
}

/// Like `AstNode`, but wraps tokens rather than interior nodes.
pub trait AstToken {
    fn cast(token: SyntaxToken) -> Option<Self>
    where
        Self: Sized;
    fn syntax(&self) -> &SyntaxToken;
    fn text(&self) -> &SmolStr {
        self.syntax().text()
    }
}

/// A pointer to a syntax node inside a file. It can be used to remember a
/// specific node across reparses of the same file.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SyntaxNodePtr {
    pub(crate) range: TextRange,
    kind: SyntaxKind,
}

impl SyntaxNodePtr {
    pub fn new(node: &SyntaxNode) -> SyntaxNodePtr {
        SyntaxNodePtr {
            range: node.text_range(),
            kind: node.kind(),
        }
    }

    pub fn to_node(self, root: &SyntaxNode) -> SyntaxNode {
        assert!(root.parent().is_none());
        successors(Some(root.clone()), |node| {
            node.children()
                .find(|it| self.range.is_subrange(&it.text_range()))
        })
        .find(|it| it.text_range() == self.range && it.kind() == self.kind)
        .unwrap_or_else(|| panic!("can't resolve local ptr to SyntaxNode: {:?}", self))
    }

    pub fn range(self) -> TextRange {
        self.range
    }

    pub fn kind(self) -> SyntaxKind {
        self.kind
    }

    pub fn cast<N: AstNode>(self) -> Option<AstPtr<N>> {
        if !N::can_cast(self.kind()) {
            return None;
        }
        Some(AstPtr {
            raw: self,
            _ty: PhantomData,
        })
    }
}

/// Like `SyntaxNodePtr`, but remembers the type of node
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct AstPtr<N: AstNode> {
    raw: SyntaxNodePtr,
    _ty: PhantomData<fn() -> N>,
}

impl<N: AstNode> Copy for AstPtr<N> {}
impl<N: AstNode> Clone for AstPtr<N> {
    fn clone(&self) -> AstPtr<N> {
        *self
    }
}

impl<N: AstNode> AstPtr<N> {
    pub fn new(node: &N) -> AstPtr<N> {
        AstPtr {
            raw: SyntaxNodePtr::new(node.syntax()),
            _ty: PhantomData,
        }
    }

    pub fn to_node(self, root: &SyntaxNode) -> N {
        let syntax_node = self.raw.to_node(root);
        N::cast(syntax_node).unwrap()
    }

    pub fn syntax_node_ptr(self) -> SyntaxNodePtr {
        self.raw
    }

    pub fn cast<U: AstNode>(self) -> Option<AstPtr<U>> {
        if !U::can_cast(self.raw.kind()) {
            return None;
        }
        Some(AstPtr {
            raw: self.raw,
            _ty: PhantomData,
        })
    }
}

impl<N: AstNode> From<AstPtr<N>> for SyntaxNodePtr {
    fn from(ptr: AstPtr<N>) -> SyntaxNodePtr {
        ptr.raw
    }
}

/// An iterator over `SyntaxNode` children of a particular AST type.
#[derive(Debug)]
pub struct AstChildren<N> {
    inner: SyntaxNodeChildren,
    ph: PhantomData<N>,
}

impl<N> AstChildren<N> {
    fn new(parent: &SyntaxNode) -> Self {
        AstChildren {
            inner: parent.children(),
            ph: PhantomData,
        }
    }
}

impl<N: AstNode> Iterator for AstChildren<N> {
    type Item = N;
    fn next(&mut self) -> Option<N> {
        self.inner.by_ref().find_map(N::cast)
    }
}

pub fn child_opt<P: AstNode + ?Sized, C: AstNode>(parent: &P) -> Option<C> {
    children(parent).next()
}

pub fn children<P: AstNode + ?Sized, C: AstNode>(parent: &P) -> AstChildren<C> {
    AstChildren::new(parent.syntax())
}

pub fn text_of_first_token(node: &SyntaxNode) -> &SmolStr {
    node.green()
        .children()
        .first()
        .and_then(|it| it.as_token())
        .unwrap()
        .text()
}

pub trait TypeAscriptionOwner: AstNode {
    fn ascribed_type(&self) -> Option<ast::TypeRef> {
        child_opt(self)
    }
}

pub trait NameOwner: AstNode {
    fn name(&self) -> Option<ast::Name> {
        child_opt(self)
    }
}

pub trait VisibilityOwner: AstNode {
    fn visibility(&self) -> Option<ast::Visibility> {
        child_opt(self)
    }
}

pub trait LoopBodyOwner: AstNode {
    fn loop_body(&self) -> Option<ast::Block> {
        child_opt(self)
    }
}

pub trait ArgListOwner: AstNode {
    fn arg_list(&self) -> Option<ast::ArgList> {
        child_opt(self)
    }
}

pub trait NamedFieldsOwner: AstNode {
    fn fields(&self) -> AstChildren<ast::NamedFieldDef> {
        children(self)
    }
}

pub trait FnDefOwner: AstNode {
    fn functions(&self) -> AstChildren<ast::FnDef> {
        children(self)
    }
}

pub trait TypeAliasDefOwner: AstNode {
    fn type_alias(&self) -> AstChildren<ast::TypeAliasDef> {
        children(self)
    }
}

pub trait EnumDefOwner: AstNode {
    fn enums(&self) -> AstChildren<ast::EnumDef> {
        children(self)
    }
}

pub trait ClassDefOwner: AstNode {
    fn classes(&self) -> AstChildren<ast::ClassDef> {
        children(self)
    }
}

pub trait ExternImportDefOwner: AstNode {
    fn imports(&self) -> AstChildren<ast::ExternImportDef> {
        children(self)
    }
}
pub trait TypeParamsOwner: AstNode {
    fn type_param_list(&self) -> Option<ast::TypeParamList> {
        child_opt(self)
    }
}
