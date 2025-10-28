use crate::matchers::Mapper;
use crate::matchers::mapping_store::MonoMappingStore;
use crate::matchers::similarity_metrics::SimilarityMeasure;
use hyperast::PrimInt;
use hyperast::types::{HyperAST, LendT, NodeId, Tree, WithHashs, WithStats};
use std::{fmt::Debug, marker::PhantomData};

use super::factorized_bounds::LazyDecompTreeBorrowBounds;

pub struct LazySimpleMarriageBottomUpMatcher<
    Mpr: crate::matchers::WithMappings,
    MZs = <Mpr as crate::matchers::WithMappings>::M,
    const SIM_THRESHOLD_NUM: u64 = 1,
    const SIM_THRESHOLD_DEN: u64 = 2,
> {
    _phantom: PhantomData<*const (Mpr, MZs)>,
}

impl<
    Dsrc: LazyDecompTreeBorrowBounds<HAST, M::Src>,
    Ddst: LazyDecompTreeBorrowBounds<HAST, M::Dst>,
    HAST,
    M,
    MZs,
    const SIM_THRESHOLD_NUM: u64,
    const SIM_THRESHOLD_DEN: u64,
>
    LazySimpleMarriageBottomUpMatcher<
        Mapper<HAST, Dsrc, Ddst, M>,
        MZs,
        SIM_THRESHOLD_NUM,
        SIM_THRESHOLD_DEN,
    >
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
        mapper.bottom_up_stable_with_similarity_threshold_and_recovery(
            |_, _, _| SIM_THRESHOLD_NUM as f64 / SIM_THRESHOLD_DEN as f64,
            SimilarityMeasure::chawathe,
            Mapper::last_chance_match_histogram_lazy,
        );
    }
}
