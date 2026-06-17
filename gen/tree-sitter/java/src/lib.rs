//! HyperAST generator for the Java programming language using TreeSitter.
//!
//! wraps tree-sitter-java

#[cfg(all(feature = "impl", feature = "bevy"))]
pub mod bevy;
#[cfg(feature = "impl")]
pub mod legion_with_refs; // TODO rename and move to a module for construction

cfg_if::cfg_if! { if #[cfg(feature = "types")] {
    pub(crate) mod types;
    pub use types::Type;
    pub use types::{Lang, Role, TIdN, TStore};

    #[doc(hidden)]
    pub use types::TType;
}}

#[cfg(all(feature = "impl", feature = "impact"))]
pub mod impact;
#[cfg(all(feature = "impl", feature = "tsg"))]
pub mod tsg;
#[cfg(feature = "impl")]
pub mod usage;

#[cfg(all(test, feature = "impl"))]
mod tests;

pub use hyperast::utils;

#[cfg(feature = "legion")]
pub use hyperast::tree_gen::utils_ts::TNode;

#[cfg(feature = "legion")]
pub mod iter;

#[cfg(feature = "legion")]
mod legion_ts_simp;

#[cfg(feature = "ts")]
pub fn language() -> tree_sitter::Language {
    tree_sitter::Language::new(tree_sitter_java::LANGUAGE)
}

#[cfg(feature = "ts")]
pub fn node_types() -> &'static str {
    tree_sitter_java::NODE_TYPES
}
