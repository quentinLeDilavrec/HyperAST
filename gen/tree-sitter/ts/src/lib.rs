//! HyperAST generator for the TypeScript language using TreeSitter.
//!
//! wraps tree-sitter-typescript

#[cfg(feature = "impl")]
pub mod legion;

#[cfg(feature = "types")]
pub(crate) mod types;

#[cfg(feature = "types")]
pub use types::Type;
#[cfg(feature = "types")]
pub use types::{Lang, Role, TIdN, TStore};

#[cfg(feature = "legion")]
pub use hyperast::tree_gen::utils_ts::TNode;

#[cfg(feature = "ts")]
pub fn language() -> tree_sitter::Language {
    tree_sitter::Language::new(tree_sitter_typescript::LANGUAGE_TYPESCRIPT)
}

#[cfg(feature = "ts")]
pub fn node_types() -> &'static str {
    tree_sitter_typescript::TYPESCRIPT_NODE_TYPES
}
