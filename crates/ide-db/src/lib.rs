// mod assists;
mod label;
mod source_change;
pub mod text_edit;

pub mod syntax_helpers {
    // pub mod format_string;
    // pub mod format_string_exprs;
    // pub mod tree_diff;
    // pub use hir::prettify_macro_expansion;
    // pub mod node_ext;
    // pub mod suggest_name;

    // pub use parser::LexedStr;
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SnippetCap {
    _private: (),
}

impl SnippetCap {
    pub const fn new(allow_snippets: bool) -> Option<SnippetCap> {
        if allow_snippets {
            Some(SnippetCap { _private: () })
        } else {
            None
        }
    }
}
