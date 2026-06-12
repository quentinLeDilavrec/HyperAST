//! HyperAST generator for the C programming language using TreeSitter.
//!
//! wraps tree-sitter-c

#[cfg(all(feature = "impl", feature = "legion"))]
pub mod legion;

#[cfg(test)]
#[cfg(all(feature = "impl", feature = "legion"))]
mod legion_ts_simp;

#[cfg(test)]
mod tests;

pub(crate) mod types;

pub use types::Type;
pub use types::{Lang, Role, TIdN, TStore};

#[doc(hidden)]
pub use types::TType;

#[cfg(feature = "legion")]
mod tnode {
    pub use hyperast::tree_gen::utils_ts::TNode;
}

#[cfg(feature = "legion")]
pub use tnode::TNode;

#[cfg(feature = "impl")]
pub fn language() -> tree_sitter::Language {
    tree_sitter::Language::new(tree_sitter_c::LANGUAGE)
}

#[cfg(feature = "impl")]
pub fn node_types() -> &'static str {
    tree_sitter_c::NODE_TYPES
}
