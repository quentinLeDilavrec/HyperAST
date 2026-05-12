use std::fmt::Debug;

use hyperast::PrimInt;
use hyperast::types::NodeId;
use hyperast::types::{HyperAST, LendT, WithHashs};

use crate::decompressed_tree_store::Shallow;
use crate::mappings::MonoMappingStore;
use crate::matchers::Mapper;
use crate::similarity_metrics;

use super::factorized_bounds::LazyDecompTreeBorrowBounds;

pub struct LazyXYBottomUpMatcher<
    Mpr,
    const SIM_THRESHOLD_NUM: u64 = 1,
    const SIM_THRESHOLD_DEN: u64 = 2,
> {
    _phantom: std::marker::PhantomData<*const Mpr>,
}

impl<
    Dsrc: LazyDecompTreeBorrowBounds<HAST, M::Src>,
    Ddst: LazyDecompTreeBorrowBounds<HAST, M::Dst>,
    HAST: HyperAST + Copy,
    M: MonoMappingStore,
    const SIM_THRESHOLD_NUM: u64,
    const SIM_THRESHOLD_DEN: u64,
> LazyXYBottomUpMatcher<Mapper<HAST, Dsrc, Ddst, M>, SIM_THRESHOLD_NUM, SIM_THRESHOLD_DEN>
where
    for<'t> LendT<'t, HAST>: WithHashs,
    Dsrc::IdD: PrimInt,
    Ddst::IdD: PrimInt,
    M::Src: PrimInt,
    M::Dst: PrimInt,
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
        mapper.bottom_up_with_similarity_threshold_and_recovery(
            |_, _, _| SIM_THRESHOLD_NUM as f64 / SIM_THRESHOLD_DEN as f64,
            similarity_metrics::SimilarityMeasure::jaccard,
            Self::last_chance_match,
        );
    }

    fn last_chance_match(mapper: &mut Mapper<HAST, Dsrc, Ddst, M>, src: Dsrc::IdD, dst: Ddst::IdD) {
        let it = mapper.prep_histogram_matching_lazy(src, dst);
        for (_t, (src_histogram, dst_histogram)) in it {
            if src_histogram.len() == 1 && dst_histogram.len() == 1 {
                let src = src_histogram[0].to_shallow();
                let dst = dst_histogram[0].to_shallow();
                mapper.mappings.link_if_both_unmapped(src, dst);
            }
        }
    }
}
