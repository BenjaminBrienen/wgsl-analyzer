//! In wgsl-analyzer, syntax trees are transient objects.
//!
//! That means that we create trees when we need them, and tear them down to
//! save memory. In this architecture, hanging on to a particular syntax node
//! for a long time is ill-advisable, as that keeps the whole tree resident.
//!
//! Instead, we provide a [`SyntaxNodePointer`] type, which stores information about
//! *location* of a particular syntax node in a tree. Its a small type which can
//! be cheaply stored, and which can be resolved to a real [`SyntaxNode`] when
//! necessary.

use std::{
    hash::{Hash, Hasher},
    marker::PhantomData,
};

use crate::{AstNode, SyntaxNode};
use parser::WgslLanguage;
use rowan::TextRange;

/// A "pointer" to a [`SyntaxNode`], via location in the source code.
pub type SyntaxNodePointer = rowan::ast::SyntaxNodePtr<WgslLanguage>;

/// Like `SyntaxNodePointer`, but remembers the type of node.
pub struct AstPointer<N: AstNode> {
    raw: SyntaxNodePointer,
    _ty: PhantomData<fn() -> N>,
}

impl<N: AstNode> std::fmt::Debug for AstPointer<N> {
    fn fmt(
        &self,
        #[expect(clippy::min_ident_chars, reason = "trait impl")] f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        f.debug_tuple("AstPtr").field(&self.raw).finish()
    }
}

impl<N: AstNode> Copy for AstPointer<N> {}
impl<N: AstNode> Clone for AstPointer<N> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<N: AstNode> Eq for AstPointer<N> {}

impl<N: AstNode> PartialEq for AstPointer<N> {
    fn eq(
        &self,
        other: &Self,
    ) -> bool {
        self.raw == other.raw
    }
}

impl<N: AstNode> Hash for AstPointer<N> {
    fn hash<H: Hasher>(
        &self,
        state: &mut H,
    ) {
        self.raw.hash(state);
    }
}

impl<N: AstNode> AstPointer<N> {
    pub fn new(node: &N) -> Self {
        Self {
            raw: SyntaxNodePointer::new(node.syntax()),
            _ty: PhantomData,
        }
    }

    /// # Panics
    ///
    /// Panics if the cast failed.
    #[must_use]
    pub fn to_node(
        &self,
        root: &SyntaxNode,
    ) -> N {
        let syntax_node = self.raw.to_node(root);
        N::cast(syntax_node).unwrap()
    }

    #[must_use]
    pub const fn syntax_node_pointer(&self) -> SyntaxNodePointer {
        self.raw
    }

    #[must_use]
    pub fn text_range(&self) -> TextRange {
        self.raw.text_range()
    }

    #[must_use]
    pub fn cast<U: AstNode>(self) -> Option<AstPointer<U>> {
        if !U::can_cast(self.raw.kind()) {
            return None;
        }
        Some(AstPointer {
            raw: self.raw,
            _ty: PhantomData,
        })
    }

    #[must_use]
    pub fn kind(&self) -> parser::SyntaxKind {
        self.raw.kind()
    }

    #[must_use]
    pub fn upcast<M: AstNode>(self) -> AstPointer<M>
    where
        N: Into<M>,
    {
        AstPointer {
            raw: self.raw,
            _ty: PhantomData,
        }
    }

    /// Like `SyntaxNodePointer::cast` but the trait bounds work out.
    #[must_use]
    pub fn try_from_raw(raw: SyntaxNodePointer) -> Option<Self> {
        N::can_cast(raw.kind()).then_some(Self {
            raw,
            _ty: PhantomData,
        })
    }

    #[must_use]
    pub fn wrap_left<R>(self) -> AstPointer<either::Either<N, R>>
    where
        either::Either<N, R>: AstNode,
    {
        AstPointer {
            raw: self.raw,
            _ty: PhantomData,
        }
    }

    #[must_use]
    pub fn wrap_right<L>(self) -> AstPointer<either::Either<L, N>>
    where
        either::Either<L, N>: AstNode,
    {
        AstPointer {
            raw: self.raw,
            _ty: PhantomData,
        }
    }
}

impl<N: AstNode> From<AstPointer<N>> for SyntaxNodePointer {
    fn from(ptr: AstPointer<N>) -> Self {
        ptr.raw
    }
}
