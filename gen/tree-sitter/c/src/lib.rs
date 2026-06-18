//! HyperAST generator for the C programming language using TreeSitter.
//!
//! wraps tree-sitter-c

cfg_if::cfg_if! { if #[cfg(feature = "types")] {
    pub(crate) mod types;

    pub use types::Type;
    pub use types::{Lang, Role, TIdN, TStore};

    #[doc(hidden)]
    pub use types::TType;

}}

#[cfg(all(feature = "impl", feature = "legion"))]
pub mod legion;

#[cfg(all(feature = "impl", feature = "legion"))]
pub mod legion_ts_simp;

#[cfg(all(test, feature = "impl"))]
mod tests;

#[cfg(feature = "legion")]
pub use hyperast::tree_gen::utils_ts::TNode;

#[cfg(feature = "ts")]
pub fn language() -> tree_sitter::Language {
    tree_sitter::Language::new(tree_sitter_c::LANGUAGE)
}

#[cfg(feature = "ts")]
pub fn node_types() -> &'static str {
    tree_sitter_c::NODE_TYPES
}
