//! Greedy bottom-up matcher
//!
//! introduced by:
//! Jean-Rémy Falleri, Floréal Morandat, Xavier Blanc, Matias Martinez, Martin Monperrus
//! in "Fine-grained and accurate source code differencing", 2014
use num_traits::{cast, one};
use std::fmt::Debug;

use hyperast::PrimInt;
use hyperast::types::{HyperAST, LendT, WithHashs};

use crate::decompressed_tree_store::SimpleZsTree as ZsTree;
use crate::decompressed_tree_store::{POBorrowSlice, Shallow};
use crate::mappings::MonoMappingStore;
use crate::matchers::optimal::zs::ZsMatcher;
use crate::matchers::{Decompressible, Mapper};
use crate::similarity_metrics::SimilarityMeasure;

use super::factorized_bounds::DecompTreeBounds;

/// TODO wait for `#![feature(adt_const_params)]` #95174 to be improved
///
/// it will allow to make use complex types as const generics
/// ie. make the different threshold neater
pub struct GreedyBottomUpMatcher<
    Mpr,
    const SIZE_THRESHOLD: usize = 1000,
    const SIM_THRESHOLD_NUM: u64 = 1,
    const SIM_THRESHOLD_DEN: u64 = 2,
> {
    _phantom: std::marker::PhantomData<*const Mpr>,
}

/// TODO PostOrder might not be necessary
impl<
    Dsrc: DecompTreeBounds<HAST, M::Src> + POBorrowSlice<HAST, M::Src>,
    Ddst: DecompTreeBounds<HAST, M::Dst> + POBorrowSlice<HAST, M::Dst>,
    HAST: HyperAST + Copy,
    M: MonoMappingStore + Default,
    const SIZE_THRESHOLD: usize,
    const SIM_THRESHOLD_NUM: u64,
    const SIM_THRESHOLD_DEN: u64,
>
    GreedyBottomUpMatcher<
        Mapper<Dsrc, Ddst, HAST, M>,
        SIZE_THRESHOLD,
        SIM_THRESHOLD_NUM,
        SIM_THRESHOLD_DEN,
    >
where
    for<'t> LendT<'t, HAST>: WithHashs,
    M::Src: PrimInt + Shallow<M::Src>,
    M::Dst: PrimInt + Shallow<M::Dst>,
    HAST::Label: Eq,
    HAST::IdN: Debug,
{
    pub fn match_it(mut mapper: Mapper<HAST, Dsrc, Ddst, M>) -> Mapper<HAST, Dsrc, Ddst, M> {
        mapper.reserve_mappings();
        Self::execute(&mut mapper);
        mapper
    }

    pub fn execute(mapper: &mut Mapper<HAST, Dsrc, Ddst, M>) {
        Self::with_recovery(mapper, Mapper::last_chance_match_zs::<M, SIZE_THRESHOLD>);
    }

    pub fn with_recovery(
        mapper: &mut Mapper<HAST, Dsrc, Ddst, M>,
        recovery: impl Fn(&mut Mapper<HAST, Dsrc, Ddst, M>, M::Src, M::Dst),
    ) {
        mapper.bottom_up_with_similarity_threshold_and_recovery(
            |_, _, _| SIM_THRESHOLD_NUM as f64 / SIM_THRESHOLD_DEN as f64,
            SimilarityMeasure::dice,
            recovery,
        );
    }
}

impl<
    HAST: HyperAST + Copy,
    Dsrc: DecompTreeBounds<HAST, M::Src> + POBorrowSlice<HAST, M::Src>,
    Ddst: DecompTreeBounds<HAST, M::Dst> + POBorrowSlice<HAST, M::Dst>,
    M: MonoMappingStore,
> crate::matchers::Mapper<HAST, Dsrc, Ddst, M>
where
    M::Src: PrimInt,
    M::Dst: PrimInt,
{
    pub fn bottom_up_with_similarity_threshold_and_recovery(
        &mut self,
        threshold: impl Fn(&Mapper<HAST, Dsrc, Ddst, M>, M::Src, M::Dst) -> f64,
        similarity: impl Fn(&SimilarityMeasure) -> f64,
        recovery: impl Fn(&mut Self, M::Src, M::Dst),
    ) {
        assert_eq!(
            // TODO move it inside the arena ...
            self.src_arena.root(),
            cast::<_, M::Src>(self.src_arena.len()).unwrap() - one()
        );
        assert!(self.src_arena.len() > 0);
        for src in self.src_arena.iter_df_post::<false>() {
            if self.mappings.is_src(&src) || !self.src_has_children(src) {
                continue;
            }
            if let Some(dst) = self.best_dst_candidate(&threshold, &similarity, src) {
                recovery(self, src, dst);
                self.mappings.link(src, dst);
            }
        }

        // for root
        let src = self.mapping.src_arena.root();
        let dst = self.mapping.dst_arena.root();
        self.mapping.mappings.link(src, dst);
        recovery(self, src, dst);
    }

    pub(crate) fn best_dst_candidate(
        &mut self,
        threshold: &impl Fn(&Self, M::Src, M::Dst) -> f64,
        similarity: &impl Fn(&SimilarityMeasure) -> f64,
        src: M::Src,
    ) -> Option<M::Dst> {
        let candidates = self.get_dst_candidates(&src);
        let mut best = None;
        let mut max: f64 = -1.;
        for cand in candidates {
            let sim = SimilarityMeasure::range(
                &self.src_arena.descendants_range(&src),
                &self.dst_arena.descendants_range(&cand),
                &self.mappings,
            );
            let sim = similarity(&sim);
            if sim > max && sim >= threshold(self, src, cand) {
                max = sim;
                best = Some(cand);
            }
        }
        best
    }
}

impl<
    HAST: HyperAST + Copy,
    Dsrc: DecompTreeBounds<HAST, M::Src> + POBorrowSlice<HAST, M::Src>,
    Ddst: DecompTreeBounds<HAST, M::Dst> + POBorrowSlice<HAST, M::Dst>,
    M: MonoMappingStore,
> crate::matchers::Mapper<HAST, Dsrc, Ddst, M>
where
    M::Src: PrimInt,
    M::Dst: PrimInt,
    HAST::Label: Eq,
    for<'t> LendT<'t, HAST>: WithHashs,
{
    pub(crate) fn last_chance_match_zs<
        MZs: MonoMappingStore<Src = M::Src, Dst = M::Dst> + Default,
        const SIZE: usize,
    >(
        &mut self,
        src: M::Src,
        dst: M::Dst,
    ) where
        M::Src: Shallow<M::Src>,
        M::Dst: Shallow<M::Dst>,
    {
        let src_s = self.src_arena.descendants_count(&src);
        let dst_s = self.dst_arena.descendants_count(&dst);
        if !(src_s < SIZE || dst_s < SIZE) {
            return;
        }
        self.match_subtree_zs::<MZs>(src, dst);
    }

    /// matching taking place during the recovery phase, using the (optimal) ZS algorithm [`crate::matchers::optimal::zs`]
    pub(crate) fn match_subtree_zs<MZs>(&mut self, src: M::Src, dst: M::Dst)
    where
        MZs: MonoMappingStore<Src = M::Src, Dst = M::Dst> + Default,
        M::Src: Shallow<M::Src>,
        M::Dst: Shallow<M::Dst>,
    {
        use crate::decompressed_tree_store::ShallowDecompressedTreeStore;
        use hyperast::types::DecompressedFrom as _;

        let stores = self.hyperast;
        let o_src = self.mapping.src_arena.original(&src);
        let o_dst = self.mapping.dst_arena.original(&dst);
        let src_arena = ZsTree::<HAST::IdN, Dsrc::IdD>::decompress(stores, &o_src);
        let src_arena = Decompressible {
            hyperast: stores,
            decomp: src_arena,
        };
        let src_offset = src - src_arena.root();
        let dst_arena = ZsTree::<HAST::IdN, M::Dst>::decompress(stores, &o_dst);
        let dst_arena = Decompressible {
            hyperast: stores,
            decomp: dst_arena,
        };
        let mappings: MZs = ZsMatcher::match_with(self.hyperast, src_arena, dst_arena);
        let dst_offset = self.dst_arena.first_descendant(&dst);
        assert_eq!(self.src_arena.first_descendant(&src), src_offset);
        self.apply_mappings(src_offset, dst_offset, mappings);
    }

    /// Considers a contiguous slice for each already allocated post order tree,
    /// while `match_subtree_zs` has to allocate new trees.
    pub(crate) fn match_subtree_zs_slice<MZs>(&mut self, src: M::Src, dst: M::Dst)
    where
        Dsrc: POBorrowSlice<HAST, M::Src, IdD = M::Src>,
        Ddst: POBorrowSlice<HAST, M::Dst, IdD = M::Dst>,
        MZs: MonoMappingStore<Src = M::Src, Dst = M::Dst> + Default,
    {
        use crate::decompressed_tree_store::ShallowDecompressedTreeStore;
        let src_arena = self.mapping.src_arena.slice_po(&src);
        let src_offset = src - src_arena.root();
        let dst_arena = self.mapping.dst_arena.slice_po(&dst);
        let mappings: MZs = ZsMatcher::match_with(self.hyperast, src_arena, dst_arena);
        let dst_offset = self.dst_arena.first_descendant(&dst);
        assert_eq!(self.src_arena.first_descendant(&src), src_offset);
        self.apply_mappings(src_offset, dst_offset, mappings);
    }
}
