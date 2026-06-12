//! HyperAST generator for the Java programming language using TreeSitter.
//!
//! wraps tree-sitter-java

#[cfg(all(feature = "impl", feature = "bevy"))]
pub mod bevy;
#[cfg(feature = "impl")]
pub mod compat;
#[cfg(feature = "impl")]
pub mod legion_with_refs; // TODO rename and move to a module for construction

pub(crate) mod types;

#[cfg(not(feature = "alt_grammar"))]
pub use types::Type;
#[cfg(not(feature = "alt_grammar"))]
pub use types::{Lang, Role, TIdN, TStore};

#[cfg(not(feature = "alt_grammar"))]
#[doc(hidden)]
pub use types::TType;

#[cfg(all(feature = "impl", feature = "impact"))]
pub mod impact;
#[cfg(all(feature = "impl", feature = "tsg"))]
// #[cfg(test)]
pub mod tsg;
#[cfg(feature = "impl")]
pub mod usage;

#[cfg(feature = "impl")]
#[cfg(test)]
mod tests;

pub use hyperast::utils;

#[cfg(feature = "legion")]
mod tnode {
    pub use hyperast::tree_gen::utils_ts::TNode;
}

#[cfg(feature = "legion")]
pub use tnode::TNode;

#[cfg(feature = "legion")]
pub mod iter;

#[cfg(feature = "impl")]
pub fn language() -> tree_sitter::Language {
    tree_sitter::Language::new(tree_sitter_java::LANGUAGE)
}

#[cfg(feature = "impl")]
pub fn node_types() -> &'static str {
    tree_sitter_java::NODE_TYPES
}
