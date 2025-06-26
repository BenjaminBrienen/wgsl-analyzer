//! This module defines Concrete Syntax Tree (CST), used by wgsl-analyzer.
//!
//! The CST includes comments and whitespace, provides a single node type,
//! `SyntaxNode`, and a basic traversal API (parent, children, siblings).
//!
//! The *real* implementation is in the (language-agnostic) `rowan` crate, this
//! module just wraps its API.

use parser::{SyntaxNode, WgslLanguage};
use rowan::{GreenNodeBuilder, Language as _};

use crate::{Parse, ParseError, SyntaxKind, TextSize};

pub(crate) use rowan::{GreenNode, GreenToken, NodeOrToken};

// #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
// pub enum WgslLanguage {}
// impl Language for WgslLanguage {
//     type Kind = SyntaxKind;

//     fn kind_from_raw(raw: rowan::SyntaxKind) -> SyntaxKind {
//         SyntaxKind::from(raw.0)
//     }

//     fn kind_to_raw(kind: SyntaxKind) -> rowan::SyntaxKind {
//         rowan::SyntaxKind(kind.into())
//     }
// }

// pub type SyntaxNode = rowan::SyntaxNode<WgslLanguage>;
// pub type SyntaxToken = rowan::SyntaxToken<WgslLanguage>;
// pub type SyntaxElement = rowan::SyntaxElement<WgslLanguage>;
// pub type SyntaxNodeChildren = rowan::SyntaxNodeChildren<WgslLanguage>;
// pub type SyntaxElementChildren = rowan::SyntaxElementChildren<WgslLanguage>;
// pub type PreorderWithTokens = rowan::api::PreorderWithTokens<WgslLanguage>;

#[derive(Default)]
pub struct SyntaxTreeBuilder {
    errors: Vec<ParseError>,
    inner: GreenNodeBuilder<'static>,
}

impl SyntaxTreeBuilder {
    pub(crate) fn finish_raw(self) -> (GreenNode, Vec<ParseError>) {
        let green = self.inner.finish();
        (green, self.errors)
    }

    #[must_use]
    pub fn finish(self) -> Parse<SyntaxNode> {
        let (green, errors) = self.finish_raw();
        // Disable block validation, see https://github.com/rust-lang/rust-analyzer/pull/10357
        #[expect(clippy::overly_complex_bool_expr, reason = "TODO")]
        if cfg!(debug_assertions) && false {
            let node = SyntaxNode::new_root(green.clone());
            // crate::validation::validate_block_structure(&node);
        }
        Parse::new(green, errors)
    }

    pub fn token(
        &mut self,
        kind: SyntaxKind,
        text: &str,
    ) {
        let kind = WgslLanguage::kind_to_raw(kind);
        self.inner.token(kind, text);
    }

    pub fn start_node(
        &mut self,
        kind: SyntaxKind,
    ) {
        let kind = WgslLanguage::kind_to_raw(kind);
        self.inner.start_node(kind);
    }

    pub fn finish_node(&mut self) {
        self.inner.finish_node();
    }
}
