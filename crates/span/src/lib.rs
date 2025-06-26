//! File and span related types.
use std::fmt::{self, Write as _};

use salsa::InternId;

mod ast_id;
mod hygiene;
mod map;

pub use self::{
    ast_id::{AstIdMap, AstIdNode, ErasedFileAstId, FileAstId},
    hygiene::{SyntaxContextData, SyntaxContextId, Transparency},
    map::{RealSpanMap, SpanMap},
};

pub use syntax::{TextRange, TextSize};
pub use vfs::FileId;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct FilePosition {
    pub file_id: FileId,
    pub offset: TextSize,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub struct FileRange {
    pub file_id: FileId,
    pub range: TextRange,
}

// The first index is always the root node's AstId
/// The root ast id always points to the encompassing file.
///
/// Using this in spans is discouraged because any range relative to it will
/// be effectively absolute, ruining the entire point of anchored relative text ranges.
pub const ROOT_ERASED_FILE_AST_ID: ErasedFileAstId =
    la_arena::Idx::from_raw(la_arena::RawIdx::from_u32(0));

/// `FileId` used as the span for syntax node fixups. Any Span containing this file id is to be
/// considered fake.
pub const FIXUP_ERASED_FILE_AST_ID_MARKER: ErasedFileAstId =
    // we pick the second to last for this in case we every consider making this a NonMaxU32, this
    // is required to be stable for the proc-macro-server
    la_arena::Idx::from_raw(la_arena::RawIdx::from_u32(!0 - 1));

pub type Span = SpanData<SyntaxContextId>;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct SpanData<Ctx> {
    /// The text range of this span, relative to the anchor.
    /// We need the anchor for incrementality, as storing absolute ranges will require
    /// recomputation on every change in a file at all times.
    pub range: TextRange,
    /// The anchor this span is relative to.
    pub anchor: SpanAnchor,
    /// The syntax context of the span.
    pub ctx: Ctx,
}

impl fmt::Display for Span {
    fn fmt(
        &self,
        #[expect(clippy::min_ident_chars, reason = "trait impl")] f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        fmt::Debug::fmt(&self.anchor.file_id.index(), f)?;
        f.write_char(':')?;
        fmt::Debug::fmt(&self.anchor.ast_id.into_raw(), f)?;
        f.write_char('@')?;
        fmt::Debug::fmt(&self.range, f)?;
        f.write_char('#')?;
        self.ctx.fmt(f)
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct SpanAnchor {
    pub file_id: FileId,
    pub ast_id: ErasedFileAstId,
}

impl fmt::Debug for SpanAnchor {
    fn fmt(
        &self,
        #[expect(clippy::min_ident_chars, reason = "trait impl")] f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        f.debug_tuple("SpanAnchor")
            .field(&self.file_id)
            .field(&self.ast_id.into_raw())
            .finish()
    }
}

/// The high-level file id.
///
/// Input to the analyzer is a set of files, where each file is identified by
/// `FileId` and contains source code. However, another source of source code in
/// Rust are macros: each macro can be thought of as producing a "temporary
/// file". To assign an id to such a file, we use the id of the macro call that
/// produced the file. So, a `HirFileId` is either a `FileId` (source code
/// written by user), or a `MacroCallId` (source code produced by macro).
///
/// What is a `MacroCallId`? Simplifying, it's a `HirFileId` of a file
/// containing the call plus the offset of the macro call in the file. Note that
/// this is a recursive definition! However, the `size_of` of `HirFileId` is
/// finite (because everything bottoms out at the real `FileId`) and small
/// (`MacroCallId` uses the location interning. You can check details here:
/// <https://en.wikipedia.org/wiki/String_interning>).
///
/// The two variants are encoded in a single u32 which are differentiated by the MSB.
/// If the MSB is 0, the value represents a `FileId`, otherwise the remaining 31 bits represent a
/// `MacroCallId`.
// FIXME: Give this a better fitting name
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct HirFileId(u32);

impl From<HirFileId> for u32 {
    fn from(value: HirFileId) -> Self {
        value.0
    }
}

impl fmt::Debug for HirFileId {
    fn fmt(
        &self,
        #[expect(clippy::min_ident_chars, reason = "trait impl")] f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        self.repr().fmt(f)
    }
}

/// `MacroCallId` identifies a particular macro invocation, like
/// `println!("Hello, {}", world)`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MacroCallId(salsa::InternId);

impl salsa::InternKey for MacroCallId {
    fn from_intern_id(
        #[expect(clippy::min_ident_chars, reason = "trait impl")] v: salsa::InternId
    ) -> Self {
        Self(v)
    }
    fn as_intern_id(&self) -> salsa::InternId {
        self.0
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum HirFileIdRepr {
    FileId(FileId),
}

impl fmt::Debug for HirFileIdRepr {
    fn fmt(
        &self,
        #[expect(clippy::min_ident_chars, reason = "trait impl")] f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        match self {
            Self::FileId(arg0) => f.debug_tuple("FileId").field(&arg0.index()).finish(),
        }
    }
}

impl From<FileId> for HirFileId {
    fn from(id: FileId) -> Self {
        Self(id.index())
    }
}

impl HirFileId {
    const MAX_HIR_FILE_ID: u32 = u32::MAX;

    #[inline]
    #[must_use]
    pub const fn file_id(self) -> Option<FileId> {
        match self.0 {
            0 => Some(FileId::from_raw(self.0)),
            _ => None,
        }
    }

    #[inline]
    #[must_use]
    #[expect(clippy::unimplemented, reason = "no such thing as macros here")]
    pub fn repr(self) -> HirFileIdRepr {
        match self.0 {
            0 => HirFileIdRepr::FileId(FileId::from_raw(self.0)),
            _ => unimplemented!(),
        }
    }
}
