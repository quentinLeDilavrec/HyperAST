#[cfg(feature = "impl")]
pub mod legion;

pub(crate) mod types;

pub use types::Type;
pub use types::{Lang, TIdN, TStore};

#[doc(hidden)]
pub use types::TType;

#[cfg(all(test, feature = "impl"))]
mod tests;

#[cfg(feature = "legion")]
pub use hyperast::tree_gen::utils_ts::TNode;

#[cfg(feature = "legion")]
pub mod iter;

#[cfg(feature = "impl")]
pub fn language() -> tree_sitter::Language {
    tree_sitter::Language::new(tree_sitter_xml::LANGUAGE_XML)
}

#[cfg(feature = "impl")]
pub fn node_types() -> &'static str {
    tree_sitter_xml::XML_NODE_TYPES
}
