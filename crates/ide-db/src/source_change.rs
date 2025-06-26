//! This modules defines type to represent changes to the source code, that flow
//! from the server to the client.
//!
//! It can be viewed as a dual for `Change`.

use std::{collections::hash_map::Entry, fmt, iter, mem};

// use crate::imports::insert_use::{ImportScope, ImportScopeKind};
use crate::text_edit::{TextEdit, TextEditBuilder};
use crate::{
    SnippetCap,
    assists::Command,
    // syntax_helpers::tree_diff::diff
};
use base_db::AnchoredPathBuf;
use itertools::Itertools as _;
use nohash_hasher::IntMap;
use rustc_hash::FxHashMap;
use span::{AstIdNode, FileId};
use stdx::never;
use syntax::{
    AstNode as _,
    SyntaxElement,
    SyntaxNode,
    SyntaxNodePointer,
    SyntaxToken,
    TextRange,
    TextSize,
    // syntax_editor::{SyntaxAnnotation, SyntaxEditor},
};

/// An annotation ID associated with an indel, to describe changes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ChangeAnnotationId(u32);

impl fmt::Display for ChangeAnnotationId {
    fn fmt(
        &self,
        #[expect(clippy::min_ident_chars, reason = "trait impl")] f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        fmt::Display::fmt(&self.0, f)
    }
}

#[derive(Debug, Clone)]
pub struct ChangeAnnotation {
    pub label: String,
    pub needs_confirmation: bool,
    pub description: Option<String>,
}

#[derive(Default, Debug, Clone)]
pub struct SourceChange {
    pub(crate) source_file_edits: IntMap<FileId, (TextEdit, Option<SnippetEdit>)>,
    pub(crate) file_system_edits: Vec<FileSystemEdit>,
    pub(crate) is_snippet: bool,
    pub(crate) annotations: FxHashMap<ChangeAnnotationId, ChangeAnnotation>,
    next_annotation_id: u32,
}

impl SourceChange {
    pub fn from_text_edit<FileIdish: Into<FileId>>(
        file_id: FileIdish,
        edit: TextEdit,
    ) -> Self {
        Self {
            source_file_edits: iter::once((file_id.into(), (edit, None))).collect(),
            ..Default::default()
        }
    }

    pub fn insert_annotation(
        &mut self,
        annotation: ChangeAnnotation,
    ) -> ChangeAnnotationId {
        let id = ChangeAnnotationId(self.next_annotation_id);
        self.next_annotation_id += 1;
        self.annotations.insert(id, annotation);
        id
    }

    /// Inserts a [`TextEdit`] for the given [`FileId`]. This properly handles merging existing
    /// edits for a file if some already exist.
    pub fn insert_source_edit<FieldIdish: Into<FileId>>(
        &mut self,
        file_id: FieldIdish,
        edit: TextEdit,
    ) {
        self.insert_source_and_snippet_edit(file_id.into(), edit, None);
    }

    /// Inserts a [`TextEdit`] and potentially a [`SnippetEdit`] for the given [`FileId`].
    /// This properly handles merging existing edits for a file if some already exist.
    pub fn insert_source_and_snippet_edit<FieldIdish: Into<FileId>>(
        &mut self,
        file_id: FieldIdish,
        edit: TextEdit,
        snippet_edit: Option<SnippetEdit>,
    ) {
        match self.source_file_edits.entry(file_id.into()) {
            Entry::Occupied(mut entry) => {
                let value = entry.get_mut();
                never!(
                    value.0.union(edit).is_err(),
                    "overlapping edits for same file"
                );
                never!(
                    value.1.is_some() && snippet_edit.is_some(),
                    "overlapping snippet edits for same file"
                );
                if value.1.is_none() {
                    value.1 = snippet_edit;
                }
            },
            Entry::Vacant(entry) => {
                entry.insert((edit, snippet_edit));
            },
        }
    }

    pub fn push_file_system_edit(
        &mut self,
        edit: FileSystemEdit,
    ) {
        self.file_system_edits.push(edit);
    }

    #[must_use]
    pub fn get_source_and_snippet_edit(
        &self,
        file_id: FileId,
    ) -> Option<&(TextEdit, Option<SnippetEdit>)> {
        self.source_file_edits.get(&file_id)
    }

    #[must_use]
    pub fn merge(
        mut self,
        other: Self,
    ) -> Self {
        self.extend(other.source_file_edits);
        self.extend(other.file_system_edits);
        self.is_snippet |= other.is_snippet;
        self
    }
}

impl Extend<(FileId, TextEdit)> for SourceChange {
    fn extend<T: IntoIterator<Item = (FileId, TextEdit)>>(
        &mut self,
        iter: T,
    ) {
        self.extend(
            iter.into_iter()
                .map(|(file_id, edit)| (file_id, (edit, None))),
        );
    }
}

impl Extend<(FileId, (TextEdit, Option<SnippetEdit>))> for SourceChange {
    fn extend<T: IntoIterator<Item = (FileId, (TextEdit, Option<SnippetEdit>))>>(
        &mut self,
        iter: T,
    ) {
        iter.into_iter()
            .for_each(|(file_id, (edit, snippet_edit))| {
                self.insert_source_and_snippet_edit(file_id, edit, snippet_edit);
            });
    }
}

impl Extend<FileSystemEdit> for SourceChange {
    fn extend<T: IntoIterator<Item = FileSystemEdit>>(
        &mut self,
        iter: T,
    ) {
        iter.into_iter()
            .for_each(|edit| self.push_file_system_edit(edit));
    }
}

impl From<IntMap<FileId, TextEdit>> for SourceChange {
    fn from(source_file_edits: IntMap<FileId, TextEdit>) -> Self {
        let source_file_edits = source_file_edits
            .into_iter()
            .map(|(file_id, edit)| (file_id, (edit, None)))
            .collect();
        Self {
            source_file_edits,
            file_system_edits: Vec::new(),
            is_snippet: false,
            ..Self::default()
        }
    }
}

impl FromIterator<(FileId, TextEdit)> for SourceChange {
    fn from_iter<T: IntoIterator<Item = (FileId, TextEdit)>>(iter: T) -> Self {
        let mut this = Self::default();
        this.extend(iter);
        this
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SnippetEdit(Vec<(u32, TextRange)>);

impl SnippetEdit {
    pub fn new(snippets: Vec<Snippet>) -> Self {
        let mut snippet_ranges = snippets
            .into_iter()
            .zip(1..)
            .with_position()
            .flat_map(|pos| {
                let (snippet, index) = match pos {
                    (itertools::Position::First | itertools::Position::Middle, item) => item,
                    // last/only snippet gets index 0
                    (itertools::Position::Last | itertools::Position::Only, (snippet, _)) => {
                        (snippet, 0)
                    },
                };

                match snippet {
                    Snippet::Tabstop(pos) => vec![(index, TextRange::empty(pos))],
                    Snippet::Placeholder(range) => vec![(index, range)],
                    Snippet::PlaceholderGroup(ranges) => {
                        ranges.into_iter().map(|range| (index, range)).collect()
                    },
                }
            })
            .collect_vec();

        snippet_ranges.sort_by_key(|(_, range)| range.start());

        // Ensure that none of the ranges overlap
        #[expect(clippy::suspicious_operation_groupings, reason = "intentional")]
        let disjoint_ranges = snippet_ranges
            .iter()
            .zip(snippet_ranges.iter().skip(1))
            .all(|((_, left), (_, right))| left.end() <= right.start() || left == right);
        stdx::always!(disjoint_ranges);

        Self(snippet_ranges)
    }

    /// Inserts all of the snippets into the given text.
    pub fn apply(
        &self,
        text: &mut String,
    ) {
        // Start from the back so that we don't have to adjust ranges
        for (index, range) in self.0.iter().rev() {
            if range.is_empty() {
                // is a tabstop
                text.insert_str(range.start().into(), &format!("${index}"));
            } else {
                // is a placeholder
                text.insert(range.end().into(), '}');
                text.insert_str(range.start().into(), &format!("${{{index}:"));
            }
        }
    }

    /// Gets the underlying snippet index + text range
    /// Tabstops are represented by an empty range, and placeholders use the range that they were given
    #[must_use]
    pub fn into_edit_ranges(self) -> Vec<(u32, TextRange)> {
        self.0
    }
}

pub struct SourceChangeBuilder {
    pub edit: TextEditBuilder,
    pub file_id: FileId,
    pub source_change: SourceChange,
    pub command: Option<Command>,

    // /// Keeps track of all edits performed on each file
    // pub file_editors: FxHashMap<FileId, SyntaxEditor>,
    // /// Keeps track of which annotations correspond to which snippets
    // pub snippet_annotations: Vec<(AnnotationSnippet, SyntaxAnnotation)>,
    /// Maps the original, immutable `SyntaxNode` to a `clone_for_update` twin.
    pub mutated_tree: Option<TreeMutator>,
    /// Keeps track of where to place snippets
    pub snippet_builder: Option<SnippetBuilder>,
}

pub struct TreeMutator {
    immutable: SyntaxNode,
    mutable_clone: SyntaxNode,
}

#[derive(Default)]
pub struct SnippetBuilder {
    /// Where to place snippets at
    places: Vec<PlaceSnippet>,
}

impl TreeMutator {
    /// Creates a new [`TreeMutator`].
    ///
    /// # Panics
    ///
    /// Panics if `immutable` has no ancestors.
    #[must_use]
    pub fn new(immutable: &SyntaxNode) -> Self {
        let immutable = immutable.ancestors().last().unwrap();
        let mutable_clone = immutable.clone_for_update();
        Self {
            immutable,
            mutable_clone,
        }
    }

    /// # Panics
    ///
    /// Panics if the cast failed.
    pub fn make_mut<N: AstIdNode>(
        &self,
        node: &N,
    ) -> N {
        N::cast(self.make_syntax_mut(node.syntax())).unwrap()
    }

    #[must_use]
    pub fn make_syntax_mut(
        &self,
        node: &SyntaxNode,
    ) -> SyntaxNode {
        let ptr = SyntaxNodePointer::new(node);
        ptr.to_node(&self.mutable_clone)
    }
}

impl SourceChangeBuilder {
    pub fn new<FileIdish: Into<FileId>>(file_id: FileIdish) -> Self {
        Self {
            edit: TextEdit::builder(),
            file_id: file_id.into(),
            source_change: SourceChange::default(),
            command: None,
            // file_editors: FxHashMap::default(),
            // snippet_annotations: vec![],
            mutated_tree: None,
            snippet_builder: None,
        }
    }

    // pub fn edit_file(
    //     &mut self,
    //     file_id: impl Into<FileId>,
    // ) {
    //     self.commit();
    //     self.file_id = file_id.into();
    // }

    // pub fn make_editor(
    //     &self,
    //     node: &SyntaxNode,
    // ) -> SyntaxEditor {
    //     SyntaxEditor::new(node.ancestors().last().unwrap_or_else(|| node.clone()))
    // }

    // pub fn add_file_edits(
    //     &mut self,
    //     file_id: impl Into<FileId>,
    //     edit: SyntaxEditor,
    // ) {
    //     match self.file_editors.entry(file_id.into()) {
    //         Entry::Occupied(mut entry) => entry.get_mut().merge(edit),
    //         Entry::Vacant(entry) => {
    //             entry.insert(edit);
    //         },
    //     }
    // }

    // pub fn make_placeholder_snippet(
    //     &mut self,
    //     _cap: SnippetCap,
    // ) -> SyntaxAnnotation {
    //     self.add_snippet_annotation(AnnotationSnippet::Over)
    // }

    // pub fn make_tabstop_before(
    //     &mut self,
    //     _cap: SnippetCap,
    // ) -> SyntaxAnnotation {
    //     self.add_snippet_annotation(AnnotationSnippet::Before)
    // }

    // pub fn make_tabstop_after(
    //     &mut self,
    //     _cap: SnippetCap,
    // ) -> SyntaxAnnotation {
    //     self.add_snippet_annotation(AnnotationSnippet::After)
    // }

    // fn commit(&mut self) {
    //     // Apply syntax editor edits
    //     for (file_id, editor) in mem::take(&mut self.file_editors) {
    //         let edit_result = editor.finish();
    //         let mut snippet_edit = vec![];

    //         // Find snippet edits
    //         for (kind, annotation) in &self.snippet_annotations {
    //             let elements = edit_result.find_annotation(*annotation);

    //             let snippet = match (kind, elements) {
    //                 (AnnotationSnippet::Before, [element]) => {
    //                     Snippet::Tabstop(element.text_range().start())
    //                 },
    //                 (AnnotationSnippet::After, [element]) => {
    //                     Snippet::Tabstop(element.text_range().end())
    //                 },
    //                 (AnnotationSnippet::Over, [element]) => {
    //                     Snippet::Placeholder(element.text_range())
    //                 },
    //                 (AnnotationSnippet::Over, elements) if !elements.is_empty() => {
    //                     Snippet::PlaceholderGroup(
    //                         elements.iter().map(|item| item.text_range()).collect(),
    //                     )
    //                 },
    //                 _ => continue,
    //             };

    //             snippet_edit.push(snippet);
    //         }

    //         let mut edit = TextEdit::builder();
    //         diff(edit_result.old_root(), edit_result.new_root()).into_text_edit(&mut edit);
    //         let edit = edit.finish();

    //         let snippet_edit = if !snippet_edit.is_empty() {
    //             Some(SnippetEdit::new(snippet_edit))
    //         } else {
    //             None
    //         };

    //         if !edit.is_empty() || snippet_edit.is_some() {
    //             self.source_change
    //                 .insert_source_and_snippet_edit(file_id, edit, snippet_edit);
    //         }
    //     }

    //     // Apply mutable edits
    //     let snippet_edit = self.snippet_builder.take().map(|builder| {
    //         SnippetEdit::new(
    //             builder
    //                 .places
    //                 .into_iter()
    //                 .flat_map(PlaceSnippet::finalize_position)
    //                 .collect(),
    //         )
    //     });

    //     if let Some(tm) = self.mutated_tree.take() {
    //         diff(&tm.immutable, &tm.mutable_clone).into_text_edit(&mut self.edit);
    //     }

    //     let edit = mem::take(&mut self.edit).finish();
    //     if !edit.is_empty() || snippet_edit.is_some() {
    //         self.source_change
    //             .insert_source_and_snippet_edit(self.file_id, edit, snippet_edit);
    //     }
    // }

    pub fn make_mut<N: AstIdNode>(
        &mut self,
        node: &N,
    ) -> N {
        self.mutated_tree
            .get_or_insert_with(|| TreeMutator::new(node.syntax()))
            .make_mut(node)
    }

    // pub fn make_import_scope_mut(
    //     &mut self,
    //     scope: ImportScope,
    // ) -> ImportScope {
    //     ImportScope {
    //         kind: match scope.kind.clone() {
    //             ImportScopeKind::File(item) => ImportScopeKind::File(self.make_mut(item)),
    //             ImportScopeKind::Module(item) => ImportScopeKind::Module(self.make_mut(item)),
    //             ImportScopeKind::Block(item) => ImportScopeKind::Block(self.make_mut(item)),
    //         },
    //         required_cfgs: scope
    //             .required_cfgs
    //             .iter()
    //             .map(|item| self.make_mut(item.clone()))
    //             .collect(),
    //     }
    // }

    /// Returns a copy of the `node`, suitable for mutation.
    ///
    /// Syntax trees in rust-analyzer are typically immutable, and mutating
    /// operations panic at runtime. However, item is possible to make a copy of
    /// the tree and mutate the copy freely. Mutation is based on interior
    /// mutability, and different nodes in the same tree see the same mutations.
    ///
    /// The typical pattern for an assist is to find specific nodes in the read
    /// phase, and then get their mutable counterparts using `make_mut` in the
    /// mutable state.
    pub fn make_syntax_mut(
        &mut self,
        node: &SyntaxNode,
    ) -> SyntaxNode {
        self.mutated_tree
            .get_or_insert_with(|| TreeMutator::new(node))
            .make_syntax_mut(node)
    }

    /// Remove specified `range` of text.
    pub fn delete(
        &mut self,
        range: TextRange,
    ) {
        self.edit.delete(range);
    }

    /// Append specified `text` at the given `offset`
    pub fn insert<Stringy: Into<String>>(
        &mut self,
        offset: TextSize,
        text: Stringy,
    ) {
        self.edit.insert(offset, text.into());
    }

    /// Replaces specified `range` of text with a given string.
    pub fn replace<Stringy: Into<String>>(
        &mut self,
        range: TextRange,
        replace_with: Stringy,
    ) {
        self.edit.replace(range, replace_with.into());
    }

    // pub fn replace_ast<N: AstIdNode>(
    //     &mut self,
    //     old: N,
    //     new: N,
    // ) {
    //     diff(old.syntax(), new.syntax()).into_text_edit(&mut self.edit)
    // }

    pub fn create_file<Stringy: Into<String>>(
        &mut self,
        destination: AnchoredPathBuf,
        content: Stringy,
    ) {
        let file_system_edit = FileSystemEdit::CreateFile {
            destination,
            initial_contents: content.into(),
        };
        self.source_change.push_file_system_edit(file_system_edit);
    }

    pub fn move_file<FieldIdish: Into<FileId>>(
        &mut self,
        source: FieldIdish,
        destination: AnchoredPathBuf,
    ) {
        let file_system_edit = FileSystemEdit::MoveFile {
            src: source.into(),
            destination,
        };
        self.source_change.push_file_system_edit(file_system_edit);
    }

    /// Triggers the parameter hint popup after the assist is applied
    pub const fn trigger_parameter_hints(&mut self) {
        self.command = Some(Command::TriggerParameterHints);
    }

    /// Renames the item at the cursor position after the assist is applied
    pub const fn rename(&mut self) {
        self.command = Some(Command::Rename);
    }

    /// Adds a tabstop snippet to place the cursor before `node`.
    ///
    /// # Panics
    ///
    /// Panics if `node` has no parent.
    pub fn add_tabstop_before<N: AstIdNode>(
        &mut self,
        _cap: SnippetCap,
        node: &N,
    ) {
        assert!(node.syntax().parent().is_some());
        self.add_snippet(PlaceSnippet::Before(node.syntax().clone().into()));
    }

    /// Adds a tabstop snippet to place the cursor after `node`.
    ///
    /// # Panics
    ///
    /// Panics if `node` has no parent.
    pub fn add_tabstop_after<N: AstIdNode>(
        &mut self,
        _cap: SnippetCap,
        node: &N,
    ) {
        assert!(node.syntax().parent().is_some());
        self.add_snippet(PlaceSnippet::After(node.syntax().clone().into()));
    }

    /// Adds a tabstop snippet to place the cursor before `token`.
    ///
    /// # Panics
    ///
    /// Panics if `token` has no parent.
    pub fn add_tabstop_before_token(
        &mut self,
        _cap: SnippetCap,
        token: SyntaxToken,
    ) {
        assert!(token.parent().is_some());
        self.add_snippet(PlaceSnippet::Before(token.into()));
    }

    /// Adds a tabstop snippet to place the cursor after `token`.
    ///
    /// # Panics
    ///
    /// Panics if `token` has no parent.
    pub fn add_tabstop_after_token(
        &mut self,
        _cap: SnippetCap,
        token: SyntaxToken,
    ) {
        assert!(token.parent().is_some());
        self.add_snippet(PlaceSnippet::After(token.into()));
    }

    /// Adds a snippet to move the cursor selected over `node`.
    ///
    /// # Panics
    ///
    /// Panics if `token` has no parent.
    pub fn add_placeholder_snippet<N: AstIdNode>(
        &mut self,
        _cap: SnippetCap,
        node: &N,
    ) {
        assert!(node.syntax().parent().is_some());
        self.add_snippet(PlaceSnippet::Over(node.syntax().clone().into()));
    }

    /// Adds a snippet to move the cursor selected over `token`.
    ///
    /// # Panics
    ///
    /// Panics if `token` has no parent.
    pub fn add_placeholder_snippet_token(
        &mut self,
        _cap: SnippetCap,
        token: SyntaxToken,
    ) {
        assert!(token.parent().is_some());
        self.add_snippet(PlaceSnippet::Over(token.into()));
    }

    /// Adds a snippet to move the cursor selected over `nodes`
    ///
    /// This allows for renaming newly generated items without having to go
    /// through a separate rename step..
    ///
    /// # Panics
    ///
    /// Panics if there is a node in `nodes` with no parent.
    pub fn add_placeholder_snippet_group(
        &mut self,
        _cap: SnippetCap,
        nodes: Vec<SyntaxNode>,
    ) {
        assert!(nodes.iter().all(|node| node.parent().is_some()));
        self.add_snippet(PlaceSnippet::OverGroup(
            nodes.into_iter().map(std::convert::Into::into).collect(),
        ));
    }

    fn add_snippet(
        &mut self,
        snippet: PlaceSnippet,
    ) {
        let snippet_builder = self
            .snippet_builder
            .get_or_insert(SnippetBuilder { places: vec![] });
        snippet_builder.places.push(snippet);
        self.source_change.is_snippet = true;
    }

    // fn add_snippet_annotation(
    //     &mut self,
    //     kind: AnnotationSnippet,
    // ) -> SyntaxAnnotation {
    //     let annotation = SyntaxAnnotation::default();
    //     self.snippet_annotations.push((kind, annotation));
    //     self.source_change.is_snippet = true;
    //     annotation
    // }

    // pub fn finish(mut self) -> SourceChange {
    //     self.commit();

    //     // Only one file can have snippet edits
    //     stdx::never!(
    //         self.source_change
    //             .source_file_edits
    //             .iter()
    //             .filter(|(_, (_, snippet_edit))| snippet_edit.is_some())
    //             .at_most_one()
    //             .is_err()
    //     );

    //     mem::take(&mut self.source_change)
    // }
}

#[derive(Debug, Clone)]
pub enum FileSystemEdit {
    CreateFile {
        destination: AnchoredPathBuf,
        initial_contents: String,
    },
    MoveFile {
        src: FileId,
        destination: AnchoredPathBuf,
    },
    MoveDir {
        src: AnchoredPathBuf,
        src_id: FileId,
        destination: AnchoredPathBuf,
    },
}

impl From<FileSystemEdit> for SourceChange {
    fn from(edit: FileSystemEdit) -> Self {
        Self {
            source_file_edits: IntMap::default(),
            file_system_edits: vec![edit],
            is_snippet: false,
            ..Self::default()
        }
    }
}

pub enum Snippet {
    /// A tabstop snippet (e.g. `$0`).
    Tabstop(TextSize),
    /// A placeholder snippet (e.g. `${0:placeholder}`).
    Placeholder(TextRange),
    /// A group of placeholder snippets, e.g.
    ///
    /// ```ignore
    /// let ${0:new_var} = 4;
    /// fun(1, 2, 3, ${0:new_var});
    /// ```
    PlaceholderGroup(Vec<TextRange>),
}

pub enum AnnotationSnippet {
    /// Place a tabstop before an element
    Before,
    /// Place a tabstop before an element
    After,
    /// Place a placeholder snippet in place of the element(s)
    Over,
}

enum PlaceSnippet {
    /// Place a tabstop before an element
    Before(SyntaxElement),
    /// Place a tabstop before an element
    After(SyntaxElement),
    /// Place a placeholder snippet in place of the element
    Over(SyntaxElement),
    /// Place a group of placeholder snippets which are linked together
    /// in place of the elements
    OverGroup(Vec<SyntaxElement>),
}

impl PlaceSnippet {
    fn finalize_position(self) -> Vec<Snippet> {
        match self {
            Self::Before(item) => vec![Snippet::Tabstop(item.text_range().start())],
            Self::After(item) => vec![Snippet::Tabstop(item.text_range().end())],
            Self::Over(item) => vec![Snippet::Placeholder(item.text_range())],
            Self::OverGroup(item) => {
                vec![Snippet::PlaceholderGroup(
                    item.into_iter().map(|item| item.text_range()).collect(),
                )]
            },
        }
    }
}
