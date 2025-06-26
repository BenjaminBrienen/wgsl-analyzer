//! `AstIdMap` allows to create stable IDs for "large" syntax nodes like items
//! and macro calls.
//!
//! Specifically, it enumerates all items in a file and uses position of a an
//! item as an ID. That way, id's don't change unless the set of items itself
//! changes.

use std::{
    any::type_name,
    fmt,
    hash::{BuildHasher as _, BuildHasherDefault, Hash, Hasher},
    marker::PhantomData,
};

use la_arena::{Arena, Idx, RawIdx};
use rustc_hash::FxHasher;
use syntax::{AstNode, SyntaxNode, ast, pointer::AstPointer, pointer::SyntaxNodePointer};

use rustc_hash::{FxBuildHasher, FxHashMap};
use syntax::ast::SourceFile;

/// See crates\hir-expand\src\ast_id_map.rs
/// This is a type erased `FileAstId`.
pub type ErasedFileAstId = la_arena::Idx<SyntaxNodePointer>;

// First hash, then index, then kind.
const HASH_BITS: u32 = 16;
const INDEX_BITS: u32 = 11;
const KIND_BITS: u32 = 5;
const _: () = assert!(HASH_BITS + INDEX_BITS + KIND_BITS == u32::BITS);

#[inline]
#[expect(clippy::as_conversions, reason = "intentional")]
const fn u16_hash(hash: u64) -> u16 {
    // We do basically the same as `FxHasher`. We don't use rustc-hash and truncate because the
    // higher bits have more entropy, but unlike rustc-hash we don't rotate because it rotates
    // for hashmaps that just use the low bits, but we compare all bits.
    const MAGIC_K: u16 = 0xecc5;
    let (part1, part2, part3, part4) = (
        hash as u16,
        (hash >> 16) as u16,
        (hash >> 32) as u16,
        (hash >> 48) as u16,
    );
    part1
        .wrapping_add(part2)
        .wrapping_mul(MAGIC_K)
        .wrapping_add(part3)
        .wrapping_mul(MAGIC_K)
        .wrapping_add(part4)
        .wrapping_mul(MAGIC_K)
}

#[inline]
fn pack_hash_index_and_kind(
    hash: u16,
    index: u32,
    kind: u32,
) -> u32 {
    (u32::from(hash)) | (index << HASH_BITS) | (kind << (HASH_BITS + INDEX_BITS))
}

/// Maps items' `SyntaxNode`s to `ErasedFileAstId`s and back.
#[derive(Default)]
pub struct AstIdMap {
    /// Maps stable id to unstable ptr.
    arena: Arena<SyntaxNodePointer>,
    /// Reverse: map ptr to id.
    map: hashbrown::HashMap<Idx<SyntaxNodePointer>, (), ()>,
}

type ArenaId = Idx<(SyntaxNodePointer, ErasedFileAstId)>;

impl fmt::Debug for AstIdMap {
    fn fmt(
        &self,
        #[expect(clippy::min_ident_chars, reason = "trait impl")] f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        f.debug_struct("AstIdMap")
            .field("arena", &self.arena)
            .finish_non_exhaustive()
    }
}

impl PartialEq for AstIdMap {
    fn eq(
        &self,
        other: &Self,
    ) -> bool {
        self.arena == other.arena
    }
}
impl Eq for AstIdMap {}

impl AstIdMap {
    pub fn from_source_old(source: &SourceFile) -> Self {
        let mut map = Self::default();

        source
            .syntax()
            .children()
            .filter_map(ast::Item::cast)
            .for_each(|item| {
                map.alloc(item.syntax());
                if let ast::Item::Function(function) = item {
                    if let Some(parameters) = function.parameter_list() {
                        for import in parameters
                            .parameters()
                            .filter_map(|parameter| parameter.import())
                        {
                            map.alloc(import.syntax());
                        }
                    }
                }
            });
        map
    }

    /// # Panics
    ///
    /// Panics if .
    #[must_use]
    pub fn from_source(node: &SyntaxNode) -> Self {
        assert!(node.parent().is_none());
        let mut result = Self::default();

        // make sure to allocate the root node
        if !should_alloc_id(node.kind()) {
            result.alloc(node);
        }
        // By walking the tree in breadth-first order we make sure that parents
        // get lower ids then children. That is, adding a new child does not
        // change parent's id. This means that, say, adding a new function to a
        // trait does not change ids of top-level items, which helps caching.
        bdfs(node, |item| {
            if should_alloc_id(item.kind()) {
                result.alloc(&item);
                TreeOrder::BreadthFirst
            } else {
                TreeOrder::DepthFirst
            }
        });
        result.map = hashbrown::HashMap::with_capacity_and_hasher(result.arena.len(), ());
        for (index, pointer) in result.arena.iter() {
            let hash = hash_ptr(pointer);
            #[expect(clippy::unreachable, reason = "TODO")]
            match result
                .map
                .raw_entry_mut()
                .from_hash(hash, |idx2| *idx2 == index)
            {
                hashbrown::hash_map::RawEntryMut::Occupied(_) => unreachable!(),
                hashbrown::hash_map::RawEntryMut::Vacant(entry) => {
                    entry.insert_with_hasher(hash, index, (), |&index| {
                        hash_ptr(&result.arena[index])
                    });
                },
            }
        }
        result.arena.shrink_to_fit();
        result
    }

    /// The [`AstId`] of the root node
    #[must_use]
    pub fn root(&self) -> SyntaxNodePointer {
        self.arena[Idx::from_raw(RawIdx::from_u32(0))]
    }

    pub fn ast_id<N: AstIdNode>(
        &self,
        item: &N,
    ) -> FileAstId<N> {
        self.ast_id_for_pointer(AstPointer::new(item))
    }

    #[must_use]
    pub fn ast_id_for_pointer<N: AstIdNode>(
        &self,
        pointer: AstPointer<N>,
    ) -> FileAstId<N> {
        let pointer = pointer.syntax_node_pointer();
        FileAstId {
            raw: self.erased_ast_id(&pointer),
            covariant: PhantomData,
        }
    }

    /// # Panics
    ///
    /// Panics if The item id was not found.
    #[must_use]
    pub fn get<N: AstIdNode>(
        &self,
        id: FileAstId<N>,
    ) -> AstPointer<N> {
        AstPointer::try_from_raw(self.arena[id.raw]).unwrap()
    }

    #[must_use]
    pub fn get_erased(
        &self,
        id: ErasedFileAstId,
    ) -> SyntaxNodePointer {
        self.arena[id]
    }

    fn erased_ast_id(
        &self,
        pointer: &SyntaxNodePointer,
    ) -> ErasedFileAstId {
        let hash = hash_ptr(pointer);
        match self
            .map
            .raw_entry()
            .from_hash(hash, |&index| &self.arena[index] == pointer)
        {
            Some((&index, ())) => index,
            None => {
                panic!(
                    "Can't find {:?} in AstIdMap:\n{:?}",
                    pointer,
                    self.arena.iter().map(|(_id, i)| i).collect::<Vec<_>>(),
                )
            },
        }
    }

    fn alloc(
        &mut self,
        item: &SyntaxNode,
    ) -> ErasedFileAstId {
        self.arena.alloc(SyntaxNodePointer::new(item))
    }
}

/// `AstId` points to an AST node in a specific file.
pub struct FileAstId<N> {
    raw: ErasedFileAstId,
    covariant: PhantomData<fn() -> N>,
}

impl<N> PartialEq for FileAstId<N> {
    fn eq(
        &self,
        other: &Self,
    ) -> bool {
        self.raw == other.raw
    }
}

impl<N> Eq for FileAstId<N> {}

impl<N> Clone for FileAstId<N> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<N> Copy for FileAstId<N> {}

impl<N> fmt::Debug for FileAstId<N> {
    fn fmt(
        &self,
        #[expect(clippy::min_ident_chars, reason = "trait impl")] f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        write!(
            f,
            "FileAstId::<{}>({})",
            type_name::<N>(),
            self.raw.into_raw()
        )
    }
}

#[inline]
fn hash_ptr(ptr: &SyntaxNodePointer) -> u64 {
    FxBuildHasher.hash_one(ptr)
}

#[inline]
fn hash_ast_id(ptr: ErasedFileAstId) -> u64 {
    FxBuildHasher.hash_one(ptr)
}

impl<N> Hash for FileAstId<N> {
    fn hash<H: Hasher>(
        &self,
        state: &mut H,
    ) {
        self.raw.hash(state);
    }
}

impl<N> FileAstId<N> {
    // Can't make this a From implementation because of coherence
    #[must_use]
    pub fn upcast<M: AstIdNode>(self) -> FileAstId<M>
    where
        N: Into<M>,
    {
        FileAstId {
            raw: self.raw,
            covariant: PhantomData,
        }
    }

    #[must_use]
    pub const fn erase(self) -> ErasedFileAstId {
        self.raw
    }
}

pub trait AstIdNode: AstNode {}

macro_rules! register_ast_id_node {
    (impl AstIdNode for $($ident:ident),+ ) => {
        $(
            impl AstIdNode for ast::$ident {}
        )+
        fn should_alloc_id(kind: syntax::SyntaxKind) -> bool {
            $(
                ast::$ident::can_cast(kind)
            )||+
        }
    };
}

register_ast_id_node! {
    impl AstIdNode for
    Item, StructDeclaration
}

#[derive(Copy, Clone, PartialEq, Eq)]
enum TreeOrder {
    BreadthFirst,
    DepthFirst,
}

/// Walks the subtree in bdfs order, calling `f` for each node. What is bdfs
/// order? It is a mix of breadth-first and depth first orders. Nodes for which
/// `f` returns [`TreeOrder::BreadthFirst`] are visited breadth-first, all the other nodes are explored
/// [`TreeOrder::DepthFirst`].
///
/// In other words, the size of the bfs queue is bound by the number of "true"
/// nodes.
fn bdfs(
    node: &SyntaxNode,
    mut function: impl FnMut(SyntaxNode) -> TreeOrder,
) {
    let mut curr_layer = vec![node.clone()];
    let mut next_layer = vec![];
    while !curr_layer.is_empty() {
        #[allow(
            clippy::iter_with_drain,
            reason = "clippy bug https://github.com/rust-lang/rust-clippy/issues/15119"
        )]
        curr_layer.drain(..).for_each(|node| {
            let mut preorder = node.preorder();
            while let Some(event) = preorder.next() {
                match event {
                    syntax::WalkEvent::Enter(node) => {
                        if function(node.clone()) == TreeOrder::BreadthFirst {
                            next_layer.extend(node.children());
                            preorder.skip_subtree();
                        }
                    },
                    syntax::WalkEvent::Leave(_) => {},
                }
            }
        });
        std::mem::swap(&mut curr_layer, &mut next_layer);
    }
}

impl AstIdNode for ast::Import {}
impl AstIdNode for ast::SourceFile {}
impl AstIdNode for ast::Function {}
impl AstIdNode for ast::GlobalVariableDeclaration {}
impl AstIdNode for ast::GlobalConstantDeclaration {}
impl AstIdNode for ast::OverrideDeclaration {}
impl AstIdNode for ast::TypeAliasDeclaration {}
impl AstIdNode for ast::Name {}
