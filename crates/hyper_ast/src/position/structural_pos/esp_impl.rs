use crate::PrimInt;
use crate::position::position_accessors::{
    RootedPosition, SolvedPosition, WithFullPostOrderPath, WithOffsets, WithPath,
    WithPostOrderOffsets, WithPostOrderPath,
};
use crate::types::NodeId;

use super::tags;
use super::{ExploreStructuralPositions, SpHandle};

impl<IdN, Idx> crate::position::node_filter_traits::Full
    for ExploreStructuralPositions<'_, IdN, Idx>
{
}
impl<IdN, Idx> crate::position::node_filter_traits::NoSpace
    for ExploreStructuralPositions<'_, IdN, Idx, tags::BottomUpNoSpace>
{
}

impl<IdN: NodeId + Eq + Copy, Idx: PrimInt> SolvedPosition<IdN>
    for ExploreStructuralPositions<'_, IdN, Idx>
{
    fn node(&self) -> IdN {
        self.sps.nodes[self.i]
    }
}
impl<IdN: NodeId + Eq + Copy, Idx: PrimInt> RootedPosition<IdN>
    for ExploreStructuralPositions<'_, IdN, Idx>
{
    fn root(&self) -> IdN {
        todo!("value must be computed")
    }
}
impl<IdN: NodeId + Eq + Copy, Idx: PrimInt> WithOffsets
    for ExploreStructuralPositions<'_, IdN, Idx>
{
    type Idx = Idx;
}
impl<IdN: NodeId + Eq + Copy, Idx: PrimInt> WithPostOrderOffsets
    for ExploreStructuralPositions<'_, IdN, Idx>
{
    fn iter(&self) -> impl Iterator<Item = Self::Idx> {
        IterOffsets(self.clone())
    }
}

pub struct IterOffsets<'a, IdN, Idx = usize>(ExploreStructuralPositions<'a, IdN, Idx>);

impl<IdN, Idx: PrimInt> Iterator for IterOffsets<'_, IdN, Idx> {
    type Item = Idx;

    fn next(&mut self) -> Option<Self::Item> {
        let o = self.0.sps.offsets[self.0.i];
        self.0.try_go_up().map(|_| o - num::one())
    }
}

impl<IdN: NodeId + Eq + Copy, Idx: PrimInt> WithPath<IdN>
    for ExploreStructuralPositions<'_, IdN, Idx>
{
}
impl<IdN: NodeId + Eq + Copy, Idx: PrimInt> WithPostOrderPath<IdN>
    for ExploreStructuralPositions<'_, IdN, Idx>
{
    fn iter_offsets_and_parents(&self) -> impl Iterator<Item = (Self::Idx, IdN)> {
        IterOffsetsNodes(self.clone())
    }
}
impl<IdN: NodeId + Eq + Copy, Idx: PrimInt> WithFullPostOrderPath<IdN>
    for ExploreStructuralPositions<'_, IdN, Idx>
{
    fn iter_with_nodes(&self) -> (IdN, impl Iterator<Item = (Self::Idx, IdN)>) {
        (self.node(), IterOffsetsNodes(self.clone()))
    }
}

pub struct IterOffsetsNodes<'a, IdN, Idx = usize>(ExploreStructuralPositions<'a, IdN, Idx>);

impl<IdN: Copy, Idx: PrimInt> Iterator for IterOffsetsNodes<'_, IdN, Idx> {
    type Item = (Idx, IdN);

    fn next(&mut self) -> Option<Self::Item> {
        let o = self.0.sps.offsets[self.0.i];
        self.0
            .try_go_up()
            .map(|h| (o - num::one(), self.0.sps.nodes[h.0]))
    }
}

impl<IdN: NodeId + Eq + Copy, Idx: PrimInt> ExploreStructuralPositions<'_, IdN, Idx> {
    pub(in crate::position) fn peek_parent_node(&self) -> Option<IdN> {
        if self.i == 0 {
            return None;
        }
        let i = self.i - 1;
        let r = self.sps.nodes[self.sps.parents[i]];
        Some(r)
    }
    pub(in crate::position) fn peek_offset(&self) -> Option<Idx> {
        if self.i == 0 {
            return None;
        }
        let i = self.i - 1;
        let r = self.sps.offsets[i] - num::one();
        Some(r)
    }
    pub(in crate::position) fn peek_node(&self) -> Option<IdN> {
        if self.i == 0 {
            return None;
        }
        let i = self.i - 1;
        let r = self.sps.nodes[i];
        Some(r)
    }
}

impl<IdN: Copy, Idx> Iterator for ExploreStructuralPositions<'_, IdN, Idx> {
    type Item = IdN;

    fn next(&mut self) -> Option<Self::Item> {
        self.try_go_up().map(|i| self.sps.nodes[i.0])
    }
}
impl<IdN, Idx> ExploreStructuralPositions<'_, IdN, Idx> {
    /// return previous index
    #[inline]
    fn try_go_up(&mut self) -> Option<SpHandle> {
        if self.i == 0 {
            return None;
        }
        let i = self.i - 1;
        let r = i;
        if i > 0 {
            self.i = self.sps.parents[i] + 1;
        } else {
            self.i = i;
        }
        Some(SpHandle(r))
    }
}
