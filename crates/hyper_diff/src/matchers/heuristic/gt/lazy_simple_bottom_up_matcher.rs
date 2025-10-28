use crate::matchers::Mapper;
use crate::matchers::mapping_store::MonoMappingStore;
use crate::matchers::similarity_metrics::SimilarityMeasure;
use hyperast::PrimInt;
use hyperast::types::{HyperAST, LendT, NodeId, WithHashs};
use std::fmt::Debug;

use super::factorized_bounds::LazyDecompTreeBorrowBounds;

pub struct LazySimpleBottomUpMatcher<
    Mpr,
    const SIMILARITY_THRESHOLD_NUM: u64 = 1,
    const SIMILARITY_THRESHOLD_DEN: u64 = 2,
> {
    _phantom: std::marker::PhantomData<*const Mpr>,
}

impl<
    'a,
    Dsrc: LazyDecompTreeBorrowBounds<HAST, M::Src>,
    Ddst: LazyDecompTreeBorrowBounds<HAST, M::Dst>,
    HAST: HyperAST + Copy,
    M: MonoMappingStore,
    const SIM_THRESHOLD_NUM: u64, // 1
    const SIM_THRESHOLD_DEN: u64, // 2
> LazySimpleBottomUpMatcher<Mapper<HAST, Dsrc, Ddst, M>, SIM_THRESHOLD_NUM, SIM_THRESHOLD_DEN>
where
    for<'t> LendT<'t, HAST>: WithHashs,
    M::Src: PrimInt,
    M::Dst: PrimInt,
    Dsrc::IdD: PrimInt,
    Ddst::IdD: PrimInt,
    HAST::Label: Eq,
    HAST::IdN: Debug,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
{
    pub fn match_it(
        mut mapper: crate::matchers::Mapper<HAST, Dsrc, Ddst, M>,
    ) -> crate::matchers::Mapper<HAST, Dsrc, Ddst, M> {
        (mapper.mapping.mappings).topit(mapper.src_arena.len(), mapper.dst_arena.len());
        Self::execute(&mut mapper);
        mapper
    }

    pub fn execute(mapper: &mut Mapper<HAST, Dsrc, Ddst, M>) {
        mapper.bottom_up_with_similarity_threshold_and_recovery(
            |_, _, _| SIM_THRESHOLD_NUM as f64 / SIM_THRESHOLD_DEN as f64,
            SimilarityMeasure::chawathe,
            Mapper::last_chance_match_histogram_lazy,
        );
    }
}
