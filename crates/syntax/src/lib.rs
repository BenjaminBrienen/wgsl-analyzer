pub mod algorithms;
pub mod ast;
pub mod pointer;
pub mod syntax_error;
pub mod syntax_node;

use std::{marker::PhantomData, ops::Deref};
use triomphe::Arc;

use either::Either;
pub use parser::{
    Edition, ParseEntryPoint, ParseError, SyntaxElement, SyntaxKind, SyntaxNode,
    SyntaxNodeChildren, SyntaxToken,
};
pub use pointer::SyntaxNodePointer;
pub use rowan::{
    Direction, GreenNode, NodeOrToken, SyntaxText, TextRange, TextSize, TokenAtOffset, WalkEvent,
    api::Preorder,
};
use smol_str::SmolStr;

use crate::syntax_error::SyntaxError;

/// `Parse` is the result of the parsing: a syntax tree and a collection of
/// errors.
///
/// Note that we always produce a syntax tree, even for completely invalid
/// files.
#[derive(Debug, PartialEq, Eq)]
pub struct Parse<T> {
    green: GreenNode,
    errors: Option<Arc<[ParseError]>>,
    _ty: PhantomData<fn() -> T>,
}

impl<T> Clone for Parse<T> {
    fn clone(&self) -> Self {
        Self {
            green: self.green.clone(),
            errors: self.errors.clone(),
            _ty: PhantomData,
        }
    }
}

impl<T> Parse<T> {
    fn new(
        green: GreenNode,
        errors: Vec<ParseError>,
    ) -> Self {
        Self {
            green,
            errors: if errors.is_empty() {
                None
            } else {
                Some(errors.into())
            },
            _ty: PhantomData,
        }
    }

    #[must_use]
    pub fn syntax(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.green.clone())
    }

    #[must_use]
    pub fn errors(&self) -> Vec<ParseError> {
        if let Some(error) = self.errors.as_deref() {
            error.to_vec()
        } else {
            vec![]
        }
        // validation::validate(&self.syntax(), &mut errors);
    }

    /// Returns the syntax tree as a file.
    ///
    /// # Panics
    ///
    /// Panics if the cast fails.
    #[must_use]
    pub fn tree(&self) -> ast::SourceFile {
        ast::SourceFile::cast(self.syntax()).unwrap()
    }
}

#[must_use]
pub fn parse(input: &str) -> Parse<SyntaxNode> {
    parse_entrypoint(input, ParseEntryPoint::File)
}

#[must_use]
pub fn parse_entrypoint(
    input: &str,
    parse_entrypoint: ParseEntryPoint,
) -> Parse<SyntaxNode> {
    let (green, errors) = parser::parse_entrypoint(input, parse_entrypoint).into_parts();
    Parse::new(green, errors)
}

/// Conversion from `SyntaxNode` to typed AST
pub trait AstNode {
    /// This panics if the `SyntaxKind` is not statically known.
    #[must_use]
    fn kind() -> SyntaxKind
    where
        Self: Sized,
    {
        panic!("dynamic `SyntaxKind` for `AstNode::kind()`")
    }

    fn can_cast(kind: SyntaxKind) -> bool
    where
        Self: Sized;

    fn cast(syntax: SyntaxNode) -> Option<Self>
    where
        Self: Sized;

    fn syntax(&self) -> &SyntaxNode;

    /// # Panics
    ///
    /// Panics if the cast fails.
    #[must_use]
    fn clone_for_update(&self) -> Self
    where
        Self: Sized,
    {
        Self::cast(self.syntax().clone_for_update()).unwrap()
    }

    /// # Panics
    ///
    /// Panics if the cast fails.
    #[must_use]
    fn clone_subtree(&self) -> Self
    where
        Self: Sized,
    {
        Self::cast(self.syntax().clone_subtree()).unwrap()
    }
}

pub trait AstToken {
    fn can_cast(kind: SyntaxToken) -> bool
    where
        Self: Sized;

    fn cast(syntax: SyntaxToken) -> Option<Self>
    where
        Self: Sized;

    fn syntax(&self) -> &SyntaxToken;

    fn text(&self) -> &str {
        self.syntax().text()
    }
}

impl AstNode for SyntaxNode {
    fn can_cast(_: SyntaxKind) -> bool {
        true
    }

    fn cast(syntax: SyntaxNode) -> Option<Self> {
        Some(syntax)
    }

    fn syntax(&self) -> &SyntaxNode {
        self
    }
}

/// An iterator over `SyntaxNode` children of a particular AST type.
#[derive(Debug, Clone)]
pub struct AstChildren<N> {
    inner: SyntaxNodeChildren,
    ph: PhantomData<N>,
}

impl<N> AstChildren<N> {
    #[must_use]
    pub fn new(parent: &SyntaxNode) -> Self {
        Self {
            inner: parent.children(),
            ph: PhantomData,
        }
    }
}

impl<N: AstNode> Iterator for AstChildren<N> {
    type Item = N;

    fn next(&mut self) -> Option<N> {
        self.inner.find_map(N::cast)
    }
}

pub enum TokenText<'borrow> {
    Borrowed(&'borrow str),
    Owned(rowan::GreenToken),
}

impl<'borrow> TokenText<'borrow> {
    #[must_use]
    pub fn as_str(&'borrow self) -> &'borrow str {
        match self {
            TokenText::Borrowed(string) => string,
            TokenText::Owned(green) => green.text(),
        }
    }
}

impl From<TokenText<'_>> for String {
    fn from(token_text: TokenText<'_>) -> Self {
        token_text.as_str().into()
    }
}

impl From<TokenText<'_>> for SmolStr {
    fn from(token_text: TokenText<'_>) -> Self {
        Self::new(token_text.as_str())
    }
}

impl Deref for TokenText<'_> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.as_str()
    }
}

mod support {
    use std::borrow::Cow;

    use super::{AstChildren, AstNode, SyntaxKind, SyntaxNode, SyntaxToken, TokenText};
    use crate::AstToken;

    pub(crate) fn child<N: AstNode>(parent: &SyntaxNode) -> Option<N> {
        parent.children().find_map(N::cast)
    }

    pub(crate) fn children<N: AstNode>(parent: &SyntaxNode) -> AstChildren<N> {
        AstChildren::new(parent)
    }

    pub(crate) fn child_syntax(
        parent: &SyntaxNode,
        kind: SyntaxKind,
    ) -> Option<SyntaxNode> {
        parent.children().find(|n| n.kind() == kind)
    }

    pub(crate) fn child_token<N: AstToken>(parent: &SyntaxNode) -> Option<N> {
        parent
            .children_with_tokens()
            .filter_map(rowan::NodeOrToken::into_token)
            .find_map(N::cast)
    }

    pub(crate) fn token(
        parent: &SyntaxNode,
        kind: SyntaxKind,
    ) -> Option<SyntaxToken> {
        parent
            .children_with_tokens()
            .filter_map(rowan::NodeOrToken::into_token)
            .find(|token| token.kind() == kind)
    }

    pub(crate) fn text_of_first_token(node: &SyntaxNode) -> TokenText<'_> {
        fn first_token(green_ref: &rowan::GreenNodeData) -> Option<&rowan::GreenTokenData> {
            green_ref
                .children()
                .next()
                .and_then(rowan::NodeOrToken::into_token)
        }

        match node.green() {
            Cow::Borrowed(green_ref) => {
                TokenText::Borrowed(first_token(green_ref).map_or("", rowan::GreenTokenData::text))
            },
            Cow::Owned(green) => first_token(&green)
                .map(ToOwned::to_owned)
                .map_or(TokenText::Borrowed(""), TokenText::Owned),
        }
    }
}

pub trait HasName: AstNode {
    fn name(&self) -> Option<ast::Name> {
        support::child(self.syntax())
    }
}

pub trait HasGenerics: AstNode {
    fn generic_arg_list(&self) -> Option<ast::GenericArgumentList> {
        support::child(self.syntax())
    }
}

pub trait HasAttributes: AstNode {
    fn attribute_list(&self) -> Option<ast::AttributeList> {
        support::child(self.syntax())
    }
    fn attributes(&self) -> Either<AstChildren<ast::Attribute>, std::iter::Empty<ast::Attribute>> {
        match self.attribute_list() {
            Some(list) => Either::Left(list.attributes()),
            None => Either::Right(std::iter::empty()),
        }
    }
}

#[macro_export]
macro_rules! match_ast {
    (match $node:ident { $($tt:tt)* }) => { match_ast!(match ($node) { $($tt)* }) };

    (match ($node:expr) {
        $( ast::$ast:ident($it:ident) => $result:expr, )*
        _ => $catch_all:expr $(,)?
    }) => {{
        $( if let Some($it) = <ast::$ast as $crate::AstNode>::cast($node.clone()) { $result } else )*
        { $catch_all }
    }};
}

impl<A: AstNode, B: AstNode> AstNode for Either<A, B> {
    fn can_cast(kind: SyntaxKind) -> bool
    where
        Self: Sized,
    {
        A::can_cast(kind) || B::can_cast(kind)
    }

    fn cast(syntax: SyntaxNode) -> Option<Self>
    where
        Self: Sized,
    {
        if let Some(a_node) = A::cast(syntax.clone()) {
            return Some(Self::Left(a_node));
        } else if let Some(b_node) = B::cast(syntax) {
            return Some(Self::Right(b_node));
        }
        None
    }

    fn syntax(&self) -> &SyntaxNode {
        match self {
            Self::Left(left) => left.syntax(),
            Self::Right(right) => right.syntax(),
        }
    }
}

#[must_use]
pub fn format(file: &ast::SourceFile) -> SyntaxNode {
    file.syntax().clone_for_update()
}
