//! Machinery for hygienic macros.
//!
//! Inspired by Matthew Flatt et al., “Macros That Work Together: Compile-Time Bindings, Partial
//! Expansion, and Definition Contexts,” *Journal of Functional Programming* 22, no. 2
//! (March 1, 2012): 181–216, <https://doi.org/10.1017/S0956796812000093>.
//!
//! Also see <https://rustc-dev-guide.rust-lang.org/macro-expansion.html#hygiene-and-hierarchies>
//!
//! # The Expansion Order Hierarchy
//!
//! `ExpnData` in rustc, rust-analyzer's version is [`MacroCallLoc`]. Traversing the hierarchy
//! upwards can be achieved by walking up [`MacroCallLoc::kind`]'s contained file id, as
//! [`MacroFile`]s are interned [`MacroCallLoc`]s.
//!
//! # The Macro Definition Hierarchy
//!
//! `SyntaxContextData` in rustc and rust-analyzer. Basically the same in both.
//!
//! # The Call-site Hierarchy
//!
//! `ExpnData::call_site` in rustc, [`MacroCallLoc::call_site`] in rust-analyzer.
use std::fmt;

use salsa::InternId;

/// Interned [`SyntaxContextData`].
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SyntaxContextId(InternId);

impl salsa::InternKey for SyntaxContextId {
    fn from_intern_id(
        #[expect(clippy::min_ident_chars, reason = "trait impl")] v: salsa::InternId
    ) -> Self {
        Self(v)
    }
    fn as_intern_id(&self) -> salsa::InternId {
        self.0
    }
}

impl fmt::Display for SyntaxContextId {
    fn fmt(
        &self,
        #[expect(clippy::min_ident_chars, reason = "trait impl")] f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        write!(f, "{}", self.0.as_u32())
    }
}

impl SyntaxContextId {
    #[must_use]
    pub fn is_root(self) -> bool {
        self == Self::root()
    }

    /// Deconstruct a `SyntaxContextId` into a raw `u32`.
    /// This should only be used for deserialization purposes for the proc-macro server.
    #[must_use]
    pub fn into_u32(self) -> u32 {
        self.0.as_u32()
    }

    /// Constructs a `SyntaxContextId` from a raw `u32`.
    /// This should only be used for serialization purposes for the proc-macro server.
    #[must_use]
    pub fn from_u32(u32: u32) -> Self {
        Self(InternId::from(u32))
    }

    /// The root context, which is the parent of all other contexts. All [`FileId`]s have this context.
    #[must_use]
    pub fn root() -> Self {
        Self(InternId::from(0_u32))
    }
}

/// A syntax context describes a hierarchy tracking order of macro definitions.
#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub struct SyntaxContextData {
    pub outer_transparency: Transparency,
    pub parent: SyntaxContextId,
    /// This context, but with all transparent and semi-transparent expansions filtered away.
    pub opaque: SyntaxContextId,
    /// This context, but with all transparent expansions filtered away.
    pub opaque_and_semitransparent: SyntaxContextId,
}

/// A property of a macro expansion that determines how identifiers
/// produced by that expansion are resolved.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Hash, Debug)]
pub enum Transparency {
    /// Identifier produced by a transparent expansion is always resolved at call-site.
    /// Call-site spans in procedural macros, hygiene opt-out in `macro` should use this.
    Transparent,
    /// Identifier produced by a semi-transparent expansion may be resolved
    /// either at call-site or at definition-site.
    /// If it's a local variable, label or `$crate` then it's resolved at def-site.
    /// Otherwise it's resolved at call-site.
    /// `macro_rules` macros behave like this, built-in macros currently behave like this too,
    /// but that's an implementation detail.
    SemiTransparent,
    /// Identifier produced by an opaque expansion is always resolved at definition-site.
    /// Def-site spans in procedural macros, identifiers from `macro` by default use this.
    Opaque,
}
