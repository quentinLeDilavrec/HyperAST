use std::{fmt::Debug, marker::PhantomData};

use hyperast::PrimInt;
use hyperast::types::{HyperAST, LendT, Tree};
use hyperast::types::{WithHashs, WithStats};

use crate::mappings::MonoMappingStore;
use crate::matchers::Mapper;
use crate::similarity_metrics::SimilarityMeasure;

use super::factorized_bounds::LazyDecompTreeBorrowBounds;

pub struct LazyHybridMarriageBottomUpMatcher<
    Mpr: crate::matchers::WithMappings,
    MZs = <Mpr as crate::matchers::WithMappings>::M,
    const SIZE_THRESHOLD: usize = 1000,
> {
    _phantom: PhantomData<*const (Mpr, MZs)>,
}

impl<
    Dsrc: LazyDecompTreeBorrowBounds<HAST, M::Src>,
    Ddst: LazyDecompTreeBorrowBounds<HAST, M::Dst>,
    HAST,
    M,
    MZs,
    const SIZE_THRESHOLD: usize,
> LazyHybridMarriageBottomUpMatcher<Mapper<HAST, Dsrc, Ddst, M>, MZs, SIZE_THRESHOLD>
where
    for<'t> LendT<'t, HAST>: Tree + WithHashs + WithStats,
    HAST::IdN: Clone + Eq + Debug,
    Dsrc::IdD: PrimInt,
    Ddst::IdD: PrimInt,
    M::Src: PrimInt,
    M::Dst: PrimInt,
    MZs: MonoMappingStore<Src = Dsrc::IdD, Dst = Ddst::IdD> + Default,
    HAST: HyperAST + Copy,
    M: MonoMappingStore,
    HAST::Label: Eq,
    HAST::IdN: Debug,
{
    pub fn match_it(
        mut mapper: crate::matchers::Mapper<HAST, Dsrc, Ddst, M>,
    ) -> crate::matchers::Mapper<HAST, Dsrc, Ddst, M> {
        (mapper.mapping.mappings).topit(mapper.src_arena.len(), mapper.dst_arena.len());
        Self::execute(&mut mapper);
        mapper
    }

    pub fn execute(mapper: &mut Mapper<HAST, Dsrc, Ddst, M>) {
        mapper.bottom_up_stable_lazy_with_similarity_threshold_and_recovery(
            Mapper::adaptive_threshold,
            SimilarityMeasure::chawathe,
            super::lazy_hybrid_bottom_up_matcher::LazyHybridBottomUpMatcher::<
                _,
                MZs,
                SIZE_THRESHOLD,
            >::last_chance_match_hybrid,
        );
    }
}
