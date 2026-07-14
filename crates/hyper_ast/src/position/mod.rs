//! # Positioning elements in HyperAST
//!
//! Because the HyperAST is a Direct Acyclic Graph (DAG),
//! any given sub-tree possibly has multiple global positions.
//! The global position, path, or offset of a subtree is global/contextual information, thus it cannot be stored efficiently on subtrees of a DAG.
//!
//! You can look at this module as an example of computing global metrics out of local ones.
//!
//! This module specifically helps with tasks related to positioning nodes globally,
//! - it helps maintain positional states during traversals
//! - it helps convert between positional representations
//!     - topological
//!     - path (list of offsets)
//!     - a file path, an offset and a length
//!     - with/out hidden nodes
//!
//! ## Incremental position storing
//! [structural_pos]
//!
//! ## topological index
//! [topological_offset]
//!     - post-order
//! ##  path
//! [offsets_and_nodes]
//!     - list of offsets
//! ## collection of path
//!     Optimization related, sometimes necessary to have acceptable perfs.
//!     - list of paths
//!     - it of paths
//!     - topo ordered list of paths
//!       incremental compute
//!     - reversed dag of paths
//!       mem optimization,
//! ## with hidden nodes (spaces, abstract nodes,....)

use crate::store::defaults::NodeIdentifier;
use crate::types::{HyperAST, NodeId, TypedNodeId};

pub trait TreePath<IdN = NodeIdentifier, Idx = u16> {
    fn node(&self) -> Option<&IdN>;
    fn offset(&self) -> Option<&Idx>;
    fn check<HAST>(&self, stores: &HAST) -> Result<(), ()>
    where
        HAST: HyperAST<IdN = IdN::IdN>,
        HAST::IdN: Eq,
        IdN: NodeId,
        IdN::IdN: NodeId<IdN = IdN::IdN>;
}

pub trait TreePathMut<IdN, Idx>: TreePath<IdN, Idx> {
    fn pop(&mut self) -> Option<(IdN, Idx)>;
    fn goto(&mut self, node: IdN, i: Idx);
    fn inc(&mut self, node: IdN);
    fn dec(&mut self, node: IdN);
}

pub trait TypedTreePath<TIdN: TypedNodeId, Idx>: TreePath<TIdN::IdN, Idx> {
    fn node_typed(&self) -> Option<&TIdN>;
    fn pop_typed(&mut self) -> Option<(TIdN, Idx)>;
    fn goto_typed(&mut self, node: TIdN, i: Idx);
}

pub mod position_accessors;

pub struct PositionConverter<'src, SrcPos> {
    src: &'src SrcPos,
}

impl<'src, SrcPos> PositionConverter<'src, SrcPos> {
    pub fn new(src: &'src SrcPos) -> Self {
        Self { src }
    }
    pub fn with_stores<'store, HAST>(
        self,
        stores: &'store HAST,
    ) -> WithHyperAstPositionConverter<'store, 'src, SrcPos, HAST> {
        WithHyperAstPositionConverter {
            src: self.src,
            stores,
        }
    }
}

pub struct WithHyperAstPositionConverter<'store, 'src, SrcPos, HAST> {
    src: &'src SrcPos,
    stores: &'store HAST,
}

pub mod building;

/// different layouts for path and positions
///
/// these serves mostly as implementation markers,
/// from a user perspective they should probably not show up much.
///
/// For example, with top down offsets,
/// the traversal should be implemented differently if it is pre- or post- order,
/// as to choose if we iterate the list of offsets in order or in reverse order
pub mod tags {
    // TODO maybe make a NoSpace generic struct, it would help factorize some implementations
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub struct TopDownNoSpace;
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub struct TopDownFull;
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub struct BottomUpNoSpace;
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub struct BottomUpFull;
}

pub mod node_filter_traits {
    pub trait NoSpace {}
    pub trait Full {}
}

pub use building::CompoundPositionPreparer;

pub mod offsets;
pub use offsets::*;

pub mod file_and_offset;

pub type Position = file_and_offset::Position<std::path::PathBuf, usize>;
pub type RootedPosition<IdN> =
    rooted_wrapper::RootedWrapper<IdN, file_and_offset::Position<std::path::PathBuf, usize>>;

pub mod offsets_and_nodes;
pub use offsets_and_nodes::*;

pub mod topological_offset;

pub mod row_col;

pub mod file_and_range;

#[allow(unused)] // TODO remove all not working function and test the remaining ones
mod spaces_related;
pub use spaces_related::{
    compute_position_and_nodes_with_no_spaces, compute_position_with_no_spaces, path_with_spaces,
};

pub mod computing_offset_bottom_up;
//pub use computing_offset_bottom_up::{extract_file_postion, extract_position};

mod computing_offset_top_down;
pub use computing_offset_top_down::{compute_position, compute_position_and_nodes, compute_range};

pub mod computing_path;
pub use computing_path::resolve_range;

// advanced optimization, uses a dag StructuralPositionStore to share parent paths

pub mod structural_pos;
pub use structural_pos::{
    ExploreStructuralPositions, Scout, SpHandle, StructuralPositionStore, TypedScout,
};
pub type StructuralPosition<IdN = NodeIdentifier, Idx = u16> =
    structural_pos::StructuralPosition<IdN, Idx>;

pub mod conversions_impls;

pub mod conversion_observer;
pub mod rooted_wrapper;

#[cfg(test)]
mod tests;
