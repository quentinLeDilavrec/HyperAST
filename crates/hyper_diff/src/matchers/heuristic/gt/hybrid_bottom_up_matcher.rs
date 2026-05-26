use std::fmt::Debug;

use hyperast::PrimInt;
use hyperast::types::{HyperAST, LendT, WithHashs};

use crate::decompressed_tree_store::ContiguousDescendants;
use crate::decompressed_tree_store::POBorrowSlice;
use crate::mappings::MonoMappingStore;
use crate::matchers::Mapper;
use crate::similarity_metrics::SimilarityMeasure;

use super::factorized_bounds::DecompTreeBounds;

/// TODO wait for `#![feature(adt_const_params)]` #95174 to be improved
///
/// it will allow to make use complex types as const generics
/// ie. make the different threshold neater
pub struct HybridBottomUpMatcher<
    Mpr: crate::matchers::WithMappings,
    MZs = <Mpr as crate::matchers::WithMappings>::M,
    const SIZE_THRESHOLD: usize = 100,
    const SIM_THRESHOLD_NUM: u64 = 1,
    const SIM_THRESHOLD_DEN: u64 = 2,
> {
    _phantom: std::marker::PhantomData<*const (Mpr, MZs)>,
}

impl<
    Dsrc: DecompTreeBounds<HAST, M::Src> + POBorrowSlice<HAST, M::Src>,
    Ddst: DecompTreeBounds<HAST, M::Dst> + POBorrowSlice<HAST, M::Dst>,
    HAST: HyperAST + Copy,
    M: MonoMappingStore + Default,
    MZs: MonoMappingStore<Src = M::Src, Dst = M::Dst> + Default,
    const SIZE_THRESHOLD: usize,
    const SIM_THRESHOLD_NUM: u64,
    const SIM_THRESHOLD_DEN: u64,
>
    HybridBottomUpMatcher<
        Mapper<HAST, Dsrc, Ddst, M>,
        MZs,
        SIZE_THRESHOLD,
        SIM_THRESHOLD_NUM,
        SIM_THRESHOLD_DEN,
    >
where
    for<'t> LendT<'t, HAST>: WithHashs,
    M::Src: PrimInt,
    M::Dst: PrimInt,
    HAST::Label: Eq,
    HAST::IdN: Debug,
{
    pub fn match_it(
        mut mapper: crate::matchers::Mapper<HAST, Dsrc, Ddst, M>,
    ) -> crate::matchers::Mapper<HAST, Dsrc, Ddst, M> {
        mapper.mapping.mappings.topit(
            mapper.mapping.src_arena.len(),
            mapper.mapping.dst_arena.len(),
        );
        Self::execute(&mut mapper);
        mapper
    }

    pub fn execute(mapper: &mut Mapper<HAST, Dsrc, Ddst, M>) {
        mapper.bottom_up_with_similarity_threshold_and_recovery(
            Mapper::adaptive_threshold,
            SimilarityMeasure::chawathe,
            Mapper::last_chance_match_hybrid::<MZs, SIZE_THRESHOLD>,
        );
    }
}

impl<HAST: HyperAST + Copy, M, Dsrc, Ddst> Mapper<HAST, Dsrc, Ddst, M> {
    /// Hybrid recovery, leveraging advantages of different techniques.
    pub fn last_chance_match_hybrid<MZs, const SIZE_THRESHOLD: usize>(
        &mut self,
        src: M::Src,
        dst: M::Dst,
    ) where
        M: MonoMappingStore + Default,
        MZs: MonoMappingStore<Src = M::Src, Dst = M::Dst> + Default,
        Dsrc: DecompTreeBounds<HAST, M::Src> + POBorrowSlice<HAST, M::Src>,
        Ddst: DecompTreeBounds<HAST, M::Dst> + POBorrowSlice<HAST, M::Dst>,
        for<'t> LendT<'t, HAST>: WithHashs,
        M::Src: PrimInt,
        M::Dst: PrimInt,
        HAST::Label: Eq,
    {
        if self.src_arena.descendants_count(&src) < SIZE_THRESHOLD
            && self.dst_arena.descendants_count(&dst) < SIZE_THRESHOLD
        {
            self.last_chance_match_zs_slice::<MZs>(src, dst);
        } else {
            self.last_chance_match_histogram(src, dst);
        }
    }
    pub fn adaptive_threshold<SrcIdS, DstIdS, SrcIdD, DstIdD>(&self, a: SrcIdD, cand: DstIdD) -> f64
    where
        Dsrc: ContiguousDescendants<HAST, SrcIdD, SrcIdS>,
        Ddst: ContiguousDescendants<HAST, DstIdD, DstIdS>,
    {
        let a = self.mapping.src_arena.descendants_count(&a) as f64;
        let cand = self.mapping.dst_arena.descendants_count(&cand) as f64;
        1f64 / (1f64 + (a + cand).ln())
    }
}
