//! Fully compress all subtrees from a Java syntax tree produced with tree-sitter.
//!
//! Check the `hyperast_gen_ts_cpp` crate for usage examples.

use hyperast::tree_gen::zipped_ts_extra::TsTreeGen;

use crate::TStore;

pub type JavaTreeGen<'store, 'b, Extra> = TsTreeGen<'store, 'b, TStore, Extra, true>;
