use std::ops::{Deref, DerefMut, Index};

use base_db::{EditionedFileId, FileExtension, Package, SourceDatabase};

use crate::{FxIndexMap, item_scope::ItemScope, mod_path::AbsoluteModPath, name_resolution::collector};

/// A map of all modules and their children in a package.
///
/// Used for name resolution.
/// Can also be used to iterate over all modules in a package to discover all symbols or all unit tests.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ModulesMap {
    /// All folders and modules in the project.
    /// Invariant: If a module path exists, then the parent module path exists.
    pub inner: FxIndexMap<AbsoluteModPath, ModuleData>,
    // TODO: refactor into DefMap
    pub root: AbsoluteModPath,
    // TODO: refactor into DefMap
    pub package_id: Package,
}

impl ModulesMap {
    fn new(root: AbsoluteModPath, package_id: Package) -> Self {
        Self { inner: FxIndexMap::default(), root, package_id }
    }

    fn iter(&self) -> impl Iterator<Item = (&AbsoluteModPath, &ModuleData)> + '_ {
        self.inner.iter()
    }

    fn iter_mut(&mut self) -> impl Iterator<Item = (&AbsoluteModPath, &mut ModuleData)> + '_ {
        self.inner.iter_mut()
    }
}

impl Deref for ModulesMap {
    type Target = FxIndexMap<AbsoluteModPath, ModuleData>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl DerefMut for ModulesMap {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl Index<AbsoluteModPath> for ModulesMap {
    type Output = ModuleData;

    fn index(&self, index: AbsoluteModPath) -> &ModuleData {
        self.inner
            .get(&index)
            .unwrap_or_else(|| panic!("AbsoluteModPath not found in ModulesMap: {index:#?}"))
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ModuleData {
    /// The file of the module.
    pub file: Option<EditionedFileId>,
    pub scope: ItemScope,
}

#[inline]
pub fn package_modules_map(db: &dyn SourceDatabase, package_id: Package) -> ModulesMap {
    let package = package_id.data(db);
    let root_file_id = package_id.root_file_id(db);
    let mut module_data = ModuleData::new(
        Some(root_file_id),
    );
    let root = AbsoluteModPath::new_root();
    let mut modules = ModulesMap::new(root.clone(), package_id);
    module_data.scope = collector::collect_module(db, module_data.file.unwrap());
    modules.insert(root, module_data);
    // let (def_map, local_def_map) =
    //     collector::collect_defs(db, modules, TreeId::new(root_file_id.into(), None), None);
    modules
}

#[salsa::tracked]
impl ModulesMap {
    #[salsa::tracked(returns(ref))]
    pub fn of(
        db: &dyn SourceDatabase,
        package: Package,
    ) -> Self {
        modules_map_query(db, package)
    }
}

impl ModuleData {
    pub(crate) fn new(
        file: Option<EditionedFileId>,
    ) -> Self {
        Self {
            file,
            scope: ItemScope::default(),
        }
    }

}

fn modules_map_query(
    db: &dyn SourceDatabase,
    package: Package,
) -> ModulesMap {
    let package_data = package.data(db);
    let source_root = package_data.source_root(db);

    let base_modules: Vec<_> = source_root
        .iter()
        .filter_map(|file_id| {
            let extension = FileExtension::from_file(&source_root, file_id).ok()?;
            let file_id = EditionedFileId::from_file_with_extension(db, file_id, extension);
            let mod_path = AbsoluteModPath::for_file(db, package, file_id)?;
            Some((
                mod_path,
                ModuleData::new(Some(file_id)),
                extension,
            ))
        })
        .collect();

    // Invariant: If a module path exists, then the parent module path exists.
    let root = AbsoluteModPath::new_root();
    let mut modules = ModulesMap::new(root.clone(), package);
    modules.insert(root, ModuleData::new(None));

    for (module_path, module, extension) in base_modules {
        // Insert modules, making sure to shadow WGSL files
        let is_empty = modules.inner
            .get(&module_path)
            .is_none_or(|module| module.file.is_none());
        if is_empty || extension == FileExtension::Wesl {
            modules.insert(module_path.clone(), module);
        }

        let mut parent_path = module_path;
        while let Some(_) = parent_path.pop_segment()
            && !modules.contains_key(&parent_path)
        {
            modules.insert(parent_path.clone(), ModuleData::new(None));
        }
    }
    modules
}
