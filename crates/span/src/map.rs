//! A map that maps a span to every position in a file. Usually maps a span to some range of positions.
//! Allows bidirectional lookup.

use std::hash::Hash;

use stdx::{always, itertools::Itertools as _};
use syntax::{TextRange, TextSize};
use vfs::FileId;

use crate::{ErasedFileAstId, ROOT_ERASED_FILE_AST_ID, Span, SpanAnchor, SyntaxContextId};

/// Maps absolute text ranges for the corresponding file to the relevant span data.
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct SpanMap<S> {
    spans: Vec<(TextSize, S)>,
    // FIXME: Should be
    // spans: Vec<(TextSize, crate::SyntaxContextId)>,
}

impl<Spanny: Copy> SpanMap<Spanny> {
    /// Creates a new empty [`SpanMap`].
    #[must_use]
    pub const fn empty() -> Self {
        Self { spans: Vec::new() }
    }

    /// Finalizes the [`SpanMap`], shrinking its backing storage and validating that the offsets are
    /// in order.
    pub fn finish(&mut self) {
        always!(
            self.spans
                .iter()
                .tuple_windows()
                .all(|(first, second)| first.0 < second.0),
            "spans are not in order"
        );
        self.spans.shrink_to_fit();
    }

    /// Pushes a new span onto the [`SpanMap`].
    ///
    /// # Panics
    ///
    /// Panics if the last offset of the span is greater than `offset`.
    pub fn push(
        &mut self,
        offset: TextSize,
        span: Spanny,
    ) {
        if cfg!(debug_assertions) {
            if let Some(&(last_offset, _)) = self.spans.last() {
                assert!(
                    last_offset < offset,
                    "last_offset({last_offset:?}) must be smaller than offset({offset:?})"
                );
            }
        }
        self.spans.push((offset, span));
    }

    /// Returns all [`TextRange`]s that correspond to the given span.
    ///
    /// Note this does a linear search through the entire backing vector.
    pub fn ranges_with_span(
        &self,
        span: Spanny,
    ) -> impl Iterator<Item = TextRange> + '_
    where
        Spanny: Eq,
    {
        // FIXME: This should ignore the syntax context!
        self.spans
            .iter()
            .enumerate()
            .filter_map(move |(index, &(end, spanny))| {
                if spanny != span {
                    return None;
                }
                let start = index
                    .checked_sub(1)
                    .map_or(TextSize::new(0), |previous| self.spans[previous].0);
                Some(TextRange::new(start, end))
            })
    }

    /// Returns the span at the given position.
    #[must_use]
    pub fn span_at(
        &self,
        offset: TextSize,
    ) -> Spanny {
        let entry = self.spans.partition_point(|&(item, _)| item <= offset);
        self.spans[entry].1
    }

    /// Returns the spans associated with the given range.
    /// In other words, this will return all spans that correspond to all offsets within the given range.
    pub fn spans_for_range(
        &self,
        range: TextRange,
    ) -> impl Iterator<Item = Spanny> + '_ {
        let (start, end) = (range.start(), range.end());
        let start_entry = self.spans.partition_point(|&(item, _)| item <= start);
        let end_entry = self.spans[start_entry..].partition_point(|&(item, _)| item <= end); // FIXME: this might be wrong?
        self.spans[start_entry..][..end_entry]
            .iter()
            .map(|&(_, spanny)| spanny)
    }

    pub fn iter(&self) -> impl Iterator<Item = (TextSize, Spanny)> + '_ {
        self.spans.iter().copied()
    }
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub struct RealSpanMap {
    file_id: FileId,
    /// Invariant: Sorted vec over `TextSize`
    // FIXME: SortedVec<(TextSize, ErasedFileAstId)>?
    pairs: Box<[(TextSize, ErasedFileAstId)]>,
    end: TextSize,
}

impl RealSpanMap {
    /// Creates a real file span map that returns absolute ranges (relative ranges to the root ast id).
    #[must_use]
    pub fn absolute(file_id: FileId) -> Self {
        Self {
            file_id,
            pairs: Box::from([(TextSize::new(0), ROOT_ERASED_FILE_AST_ID)]),
            end: TextSize::new(!0),
        }
    }

    #[must_use]
    pub const fn from_file(
        file_id: FileId,
        pairs: Box<[(TextSize, ErasedFileAstId)]>,
        end: TextSize,
    ) -> Self {
        Self {
            file_id,
            pairs,
            end,
        }
    }

    /// # Panics
    ///
    /// Panics if the range goes beyond the file.
    #[must_use]
    pub fn span_for_range(
        &self,
        range: TextRange,
    ) -> Span {
        assert!(
            range.end() <= self.end,
            "range {range:?} goes beyond the end of the file {:?}",
            self.end
        );
        let start = range.start();
        let index = self
            .pairs
            .binary_search_by(|&(item, _)| item.cmp(&start).then(std::cmp::Ordering::Less))
            .unwrap_err();
        let (offset, ast_id) = self.pairs[index - 1];
        Span {
            range: range - offset,
            anchor: SpanAnchor {
                file_id: self.file_id,
                ast_id,
            },
            ctx: SyntaxContextId::root(),
        }
    }
}
