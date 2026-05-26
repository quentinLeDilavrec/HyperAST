use std::fmt::Debug;
use std::marker::PhantomData;

use hyperast::PrimInt;
use hyperast::types::{HyperAST, LendT, WithHashs};

use crate::mappings::{MappingStore, MonoMappingStore};
use crate::matchers::Mapper;
use crate::similarity_metrics::SimilarityMeasure;

use super::factorized_bounds::LazyDecompTreeBorrowBounds;

/// TODO wait for `#![feature(adt_const_params)]` #95174 to be improved
///
/// it will allow to make use complex types as const generics
/// ie. make the different threshold neater
pub struct LazyHybridBottomUpMatcher<
    Mpr: crate::matchers::WithMappings,
    MZs = <Mpr as crate::matchers::WithMappings>::M,
    const SIZE_THRESHOLD: usize = 1000,
> {
    _phantom: PhantomData<*const (Mpr, MZs)>,
}

impl<
    Dsrc: LazyDecompTreeBorrowBounds<HAST, M::Src>,
    Ddst: LazyDecompTreeBorrowBounds<HAST, M::Dst>,
    MZs: MonoMappingStore<Src = Dsrc::IdD, Dst = Ddst::IdD> + Default,
    HAST: HyperAST + Copy,
    M: MonoMappingStore,
    const SIZE_THRESHOLD: usize,
> LazyHybridBottomUpMatcher<Mapper<HAST, Dsrc, Ddst, M>, MZs, SIZE_THRESHOLD>
where
    for<'t> LendT<'t, HAST>: WithHashs,
    M::Src: PrimInt,
    M::Dst: PrimInt,
    Dsrc::IdD: PrimInt,
    Ddst::IdD: PrimInt,
    HAST::Label: Eq,
    HAST::IdN: Debug,
{
    pub fn match_it(mut mapper: Mapper<HAST, Dsrc, Ddst, M>) -> Mapper<HAST, Dsrc, Ddst, M> {
        (mapper.mapping.mappings).topit(mapper.src_arena.len(), mapper.dst_arena.len());
        Self::execute(&mut mapper);
        mapper
    }

    pub fn execute(mapper: &mut Mapper<HAST, Dsrc, Ddst, M>) {
        mapper.bottom_up_lazy_with_similarity_threshold_and_recovery(
            Mapper::adaptive_threshold,
            SimilarityMeasure::chawathe,
            Mapper::last_chance_match_hybrid_lazy::<MZs, SIZE_THRESHOLD>,
        );
    }
}

impl<
    HAST: HyperAST + Copy,
    M: MonoMappingStore,
    Dsrc: LazyDecompTreeBorrowBounds<HAST, M::Src>,
    Ddst: LazyDecompTreeBorrowBounds<HAST, M::Dst>,
> Mapper<HAST, Dsrc, Ddst, M>
where
    for<'t> LendT<'t, HAST>: WithHashs,
    M::Src: PrimInt,
    M::Dst: PrimInt,
    Dsrc::IdD: PrimInt,
    Ddst::IdD: PrimInt,
    HAST::Label: Eq,
    HAST::IdN: Debug,
{
    /// Hybrid recovery, leveraging advantages of different techniques.
    ///
    /// Uses ZS (optimal) (from Greedy) if the number of descendants is below SIZE_THRESHOLD,
    /// uses histogram matching (from Simple) otherwise.
    pub fn last_chance_match_hybrid_lazy<MZs, const SIZE: usize>(
        &mut self,
        src: Dsrc::IdD,
        dst: Ddst::IdD,
    ) where
        MZs: MonoMappingStore<Src = Dsrc::IdD, Dst = Ddst::IdD> + Default,
    {
        self.match_subtree_hybrid_lazy::<MZs>(src, dst, SIZE);
    }

    /// see [`Mapper::last_chance_match_hybrid_lazy`]
    pub fn match_subtree_hybrid_lazy<MZs>(&mut self, src: Dsrc::IdD, dst: Ddst::IdD, size: usize)
    where
        MZs: MonoMappingStore<Src = Dsrc::IdD, Dst = Ddst::IdD> + Default,
    {
        let src_s = self.src_arena.descendants_count(&src);
        let dst_s = self.dst_arena.descendants_count(&dst);
        if src_s < size || dst_s < size {
            self.match_subtree_zs_lazy_slice::<MZs>(src, dst);
        } else {
            self.match_subtree_histogram_lazy(src, dst);
        }
    }
}
