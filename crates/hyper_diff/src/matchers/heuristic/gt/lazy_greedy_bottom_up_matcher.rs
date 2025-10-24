//! makes greedy_bottom_up_matcher lazy
//! - [ ] first make post order iterator lazy
//!
use crate::matchers::Mapper;
use crate::matchers::mapping_store::MonoMappingStore;
use crate::matchers::similarity_metrics;
use hyperast::PrimInt;
use hyperast::types::{HyperAST, LendT, NodeId, Tree, WithHashs, WithStats};
use std::{fmt::Debug, marker::PhantomData};

use super::factorized_bounds::LazyDecompTreeBorrowBounds;

pub struct LazyGreedyBottomUpMatcher<
    Dsrc,
    Ddst,
    HAST: HyperAST + Copy,
    M: MonoMappingStore,
    MZs: MonoMappingStore = M,
    const SIZE_THRESHOLD: usize = 1000,
    const SIM_THRESHOLD_NUM: u64 = 1,
    const SIM_THRESHOLD_DEN: u64 = 2,
> {
    mapper: Mapper<HAST, Dsrc, Ddst, M>,
    _phantom: PhantomData<*const MZs>,
}

impl<
    Dsrc: LazyDecompTreeBorrowBounds<HAST, M::Src>,
    Ddst: LazyDecompTreeBorrowBounds<HAST, M::Dst>,
    HAST,
    M,
    MZs,
    const SIZE_THRESHOLD: usize,
    const SIM_THRESHOLD_NUM: u64,
    const SIM_THRESHOLD_DEN: u64,
>
    LazyGreedyBottomUpMatcher<
        Dsrc,
        Ddst,
        HAST,
        M,
        MZs,
        SIZE_THRESHOLD,
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
        mapper.mapping.mappings.topit(
            mapper.mapping.src_arena.len(),
            mapper.mapping.dst_arena.len(),
        );
        Self::execute(&mut mapper);
        mapper
    }

    pub fn execute(mapper: &mut Mapper<HAST, Dsrc, Ddst, M>) {
        let recovery = |mapper: &mut Mapper<HAST, Dsrc, Ddst, M>, src, dst| {
            let src_s = mapper.src_arena.descendants_count(&src);
            let dst_s = mapper.dst_arena.descendants_count(&dst);
            if src_s < SIZE_THRESHOLD || dst_s < SIZE_THRESHOLD {
                mapper.last_chance_match_zs_lazy_slice::<MZs>(src, dst);
            }
        };
        mapper.bottom_up_with_similarity_threshold_and_recovery(
            |_, _, _| SIM_THRESHOLD_NUM as f64 / SIM_THRESHOLD_DEN as f64,
            similarity_metrics::SimilarityMeasure::dice,
            recovery,
        );
    }
}
