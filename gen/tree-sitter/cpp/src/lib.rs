//! HyperAST generator for the C++ programming language using TreeSitter.
//!
//! wraps tree-sitter-cpp

cfg_if::cfg_if! { if #[cfg(all(feature = "alt_grammar", feature = "types"))] {
    pub(crate) mod types_alt;
    pub(crate) use types_alt as types;
} else if #[cfg(feature = "types")] {
    pub(crate) mod types;
}}

#[cfg(feature = "types")]
pub use types::{Lang, Role, TIdN, TStore, Type};

#[cfg(feature = "types")]
#[doc(hidden)]
pub use types::TType;

cfg_if::cfg_if! { if #[cfg(feature = "legion")] {
    pub mod legion;

    pub mod legion_ts_simp;

    pub use hyperast::tree_gen::utils_ts::TNode;

    pub mod iter;
}}

#[cfg(feature = "ts")]
pub fn language() -> tree_sitter::Language {
    cfg_if::cfg_if! { if #[cfg(feature = "alt_grammar")] {
        tree_sitter::Language::new(tree_sitter_cpp_alt::LANGUAGE)
    } else {
        tree_sitter::Language::new(tree_sitter_cpp::LANGUAGE)
    }}
}

#[cfg(feature = "ts")]
pub fn node_types() -> &'static str {
    cfg_if::cfg_if! { if #[cfg(feature = "alt_grammar")] {
        tree_sitter_cpp_alt::NODE_TYPES
    } else {
        tree_sitter_cpp::NODE_TYPES
    }}
}

#[cfg(all(test, feature = "impl_intern"))]
mod tests;
