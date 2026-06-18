//! Fully compress all subtrees from a C syntax tree produced with tree-sitter.
//!
//! Check the `hyperast_gen_ts_cpp` crate for usage examples.

use hyperast::tree_gen::zipped_ts_extra::TsTreeGen;

pub type CTreeGen<'store, 'b, Extra> = TsTreeGen<'store, 'b, crate::TStore, Extra, true>;
