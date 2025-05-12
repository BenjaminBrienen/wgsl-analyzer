use std::sync::Arc;

use salsa::Durability;
use vfs::{FileId, VfsPath};

use crate::{
    RootQueryDb, SourceDatabase,
    input::{SourceRoot, SourceRootId},
};

#[derive(Default)]
pub struct Change {
    pub roots: Option<Vec<SourceRoot>>,
    pub files_changed: Vec<(FileId, Option<Arc<String>>, VfsPath)>,
}

impl Change {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn set_roots(
        &mut self,
        roots: Vec<SourceRoot>,
    ) {
        self.roots = Some(roots);
    }

    pub fn change_file(
        &mut self,
        file_id: FileId,
        new_text: Option<Arc<String>>,
        new_path: VfsPath,
    ) {
        self.files_changed.push((file_id, new_text, new_path));
    }

    pub fn apply(
        self,
        db: &mut dyn RootQueryDb,
    ) -> Option<PackagesIdMap> {
        let _p = tracing::info_span!("FileChange::apply").entered();
        if let Some(roots) = self.roots {
            for (idx, root) in roots.into_iter().enumerate() {
                let root_id = SourceRootId(idx as u32);
                let durability = source_root_durability(&root);
                for file_id in root.iter() {
                    db.set_file_source_root_with_durability(file_id, root_id, durability);
                }

                db.set_source_root_with_durability(root_id, Arc::new(root), durability);
            }
        }

        for (file_id, text) in self.files_changed {
            let source_root_id = db.file_source_root(file_id);
            let source_root = db.source_root(source_root_id.source_root_id(db));

            let durability = file_text_durability(&source_root.source_root(db));
            // XXX: can't actually remove the file, just reset the text
            let text = text.unwrap_or_default();
            db.set_file_text_with_durability(file_id, &text, durability)
        }

        if let Some(package_graph) = self.package_graph {
            return Some(package_graph.set_in_db(db));
        }
        None
    }
}

fn source_root_durability(source_root: &SourceRoot) -> Durability {
    if source_root.is_library {
        Durability::MEDIUM
    } else {
        Durability::LOW
    }
}

fn file_text_durability(source_root: &SourceRoot) -> Durability {
    if source_root.is_library {
        Durability::HIGH
    } else {
        Durability::LOW
    }
}
