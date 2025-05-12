pub mod input;
mod shader_processor;

pub mod change;

mod util_types;
use input::{SourceRoot, SourceRootId};
use line_index::LineIndex;
pub use query_group::{self};
pub use util_types::*;
use vfs::{AnchoredPath, VfsPath};

use std::sync::Arc;

use rustc_hash::{FxHashMap, FxHashSet};
use syntax::{Parse, ParseEntryPoint};
pub use vfs::FileId;

pub trait Upcast<T: ?Sized> {
    fn upcast(&self) -> &T;
}

pub trait FileLoader {
    fn resolve_path(
        &self,
        path: AnchoredPath<'_>,
    ) -> Option<FileId>;
}

#[salsa::db]
pub trait SourceDatabase: salsa::Database {
    fn file_text(
        &self,
        file_id: FileId,
    ) -> Arc<String>;

    fn file_path(
        &self,
        file_id: FileId,
    ) -> VfsPath;

    fn file_id(
        &self,
        path: VfsPath,
    ) -> FileId;

    fn custom_imports(&self) -> Arc<FxHashMap<String, String>>;

    fn shader_defs(&self) -> Arc<FxHashSet<String>>;

    /// Path to a file, relative to the root of its source root.
    /// Source root of the file.
    fn file_source_root(
        &self,
        file_id: FileId,
    ) -> SourceRootId;

    /// Contents of the source root.
    fn source_root(
        &self,
        id: SourceRootId,
    ) -> Arc<SourceRoot>;

    fn parse_no_preprocessor(
        &self,
        file_id: FileId,
    ) -> syntax::Parse;

    fn parse_with_unconfigured(
        &self,
        file_id: FileId,
    ) -> (Parse, Arc<Vec<UnconfiguredCode>>);

    fn parse(
        &self,
        file_id: FileId,
    ) -> Parse;

    fn parse_import_no_preprocessor(
        &self,
        key: String,
    ) -> Result<syntax::Parse, ()>;

    fn parse_import(
        &self,
        key: String,
        parse_entrypoint: ParseEntryPoint,
    ) -> Result<Parse, ()>;

    fn line_index(
        &self,
        file_id: FileId,
    ) -> Arc<LineIndex>;
}

fn line_index(
    db: &dyn SourceDatabase,
    file_id: FileId,
) -> Arc<LineIndex> {
    let text = db.file_text(file_id);
    Arc::new(LineIndex::new(&text))
}

fn parse_no_preprocessor_query(
    db: &dyn SourceDatabase,
    file_id: FileId,
) -> syntax::Parse {
    let source = db.file_text(file_id);
    syntax::parse(&source)
}

fn parse_import_no_preprocessor_query(
    db: &dyn SourceDatabase,
    key: String,
) -> Result<syntax::Parse, ()> {
    let imports = db.custom_imports();
    let source = imports.get(&key).ok_or(())?;
    Ok(syntax::parse(source))
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnconfiguredCode {
    pub range: TextRange,
    pub def: String,
}

fn parse_with_unconfigured_query(
    db: &dyn SourceDatabase,
    file_id: FileId,
) -> (Parse, Arc<Vec<UnconfiguredCode>>) {
    let shader_defs = db.shader_defs();
    let source = db.file_text(file_id);

    let mut unconfigured = Vec::new();

    let processed_source =
        shader_processor::get_shader_processor().process(&source, &shader_defs, |range, def| {
            let range = TextRange::new(
                TextSize::from(range.start as u32),
                TextSize::from(range.end as u32),
            );
            unconfigured.push(UnconfiguredCode {
                range,
                def: def.to_string(),
            })
        });
    let parse = syntax::parse(&processed_source);
    (parse, Arc::new(unconfigured))
}

fn parse_query(
    db: &dyn SourceDatabase,
    file_id: FileId,
) -> Parse {
    db.parse_with_unconfigured(file_id).0
}

fn parse_import_query(
    db: &dyn SourceDatabase,
    key: String,
    parse_entrypoint: ParseEntryPoint,
) -> Result<Parse, ()> {
    let imports = db.custom_imports();
    let shader_defs = db.shader_defs();
    let source = imports.get(&key).ok_or(())?;

    let processed_source =
        shader_processor::get_shader_processor().process(source, &shader_defs, |_, _| {});
    Ok(syntax::parse_entrypoint(
        &processed_source,
        parse_entrypoint,
    ))
}

/// Silly workaround for cyclic deps between the traits
pub struct FileLoaderDelegate<T>(pub T);

impl<T: SourceDatabase> FileLoader for FileLoaderDelegate<&'_ T> {
    fn resolve_path(
        &self,
        path: AnchoredPath<'_>,
    ) -> Option<FileId> {
        // FIXME: this *somehow* should be platform agnostic...
        let source_root = self.0.file_source_root(path.anchor);
        let source_root = self.0.source_root(source_root);
        source_root.resolve_path(path)
    }
}

/// Database which stores all significant input facts: source code and project
/// model. Everything else in rust-analyzer is derived from these queries.
#[query_group::query_group]
pub trait RootQueryDb: SourceDatabase + salsa::Database {
    /// Parses the file into the syntax tree.
    #[salsa::invoke(parse)]
    #[salsa::lru(128)]
    fn parse(
        &self,
        file_id: EditionedFileId,
    ) -> Parse<ast::SourceFile>;

    /// Returns the set of errors obtained from parsing the file including validation errors.
    #[salsa::transparent]
    fn parse_errors(
        &self,
        file_id: EditionedFileId,
    ) -> Option<&[SyntaxError]>;

    #[salsa::transparent]
    fn toolchain_channel(
        &self,
        krate: Package,
    ) -> Option<ReleaseChannel>;

    /// Packages whose root file is in `id`.
    #[salsa::invoke_interned(source_root_packages)]
    fn source_root_packages(
        &self,
        id: SourceRootId,
    ) -> Arc<[Package]>;

    #[salsa::transparent]
    fn relevant_packages(
        &self,
        file_id: FileId,
    ) -> Arc<[Package]>;

    /// Returns the packages in topological order.
    ///
    /// **Warning**: do not use this query in `hir-*` packages! It kills incrementality across package metadata modifications.
    #[salsa::input]
    fn all_packages(&self) -> Arc<Box<[Package]>>;

    /// Returns an iterator over all transitive dependencies of the given package,
    /// including the package itself.
    ///
    /// **Warning**: do not use this query in `hir-*` packages! It kills incrementality across package metadata modifications.
    #[salsa::transparent]
    fn transitive_dependencies(
        &self,
        package_id: Package,
    ) -> FxHashSet<Package>;

    /// Returns all transitive reverse dependencies of the given package,
    /// including the package itself.
    ///
    /// **Warning**: do not use this query in `hir-*` packages! It kills incrementality across package metadata modifications.
    #[salsa::invoke(input::transitive_rev_dependencies)]
    #[salsa::transparent]
    fn transitive_rev_dependencies(
        &self,
        of: Package,
    ) -> FxHashSet<Package>;
}
