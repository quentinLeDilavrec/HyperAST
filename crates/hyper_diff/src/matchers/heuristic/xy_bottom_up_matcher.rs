use std::fmt::Debug;

use hyperast::PrimInt;
use hyperast::types::NodeId;
use hyperast::types::{HyperAST, LendT, WithHashs};

use crate::mappings::MonoMappingStore;
use crate::matchers::Mapper;
use crate::similarity_metrics;

use super::factorized_bounds::DecompTreeBounds;

pub struct XYBottomUpMatcher<
    Mpr,
    const SIM_THRESHOLD_NUM: u64 = 1,
    const SIM_THRESHOLD_DEN: u64 = 2,
> {
    _phantom: std::marker::PhantomData<*const Mpr>,
}

impl<
    Dsrc: DecompTreeBounds<HAST, M::Src>,
    Ddst: DecompTreeBounds<HAST, M::Dst>,
    HAST: HyperAST + Copy,
    M: MonoMappingStore,
    const SIM_THRESHOLD_NUM: u64,
    const SIM_THRESHOLD_DEN: u64,
> XYBottomUpMatcher<Mapper<HAST, Dsrc, Ddst, M>, SIM_THRESHOLD_NUM, SIM_THRESHOLD_DEN>
where
    for<'t> LendT<'t, HAST>: WithHashs,
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
        for src in mapper.src_arena.iter_df_post::<false>() {
            if mapper.mappings.is_src(&src) || !mapper.src_has_children(src) {
                continue;
            }
            let candidates = mapper.get_dst_candidates(&src);
            let mut best = None;
            let mut max: f64 = -1.;
            for cand in candidates {
                let sim = similarity_metrics::SimilarityMeasure::range(
                    &mapper.src_arena.descendants_range(&src),
                    &mapper.dst_arena.descendants_range(&cand),
                    &mapper.mappings,
                )
                .jaccard();
                if sim > max && sim >= SIM_THRESHOLD_NUM as f64 / SIM_THRESHOLD_DEN as f64 {
                    max = sim;
                    best = Some(cand);
                }
            }

            if let Some(dst) = best {
                Self::last_chance_match(mapper, src, dst);
                mapper.mappings.link(src, dst);
            }
        }
        // for root
        let src = mapper.mapping.src_arena.root();
        let dst = mapper.mapping.dst_arena.root();
        mapper.mapping.mappings.link(src, dst);
        Self::last_chance_match(mapper, src, dst);
    }

    fn last_chance_match(mapper: &mut Mapper<HAST, Dsrc, Ddst, M>, src: M::Src, dst: M::Dst) {
        let it = mapper.prep_histogram_matching(&src, &dst);
        for (_t, (src_histogram, dst_histogram)) in it {
            if src_histogram.len() == 1 && dst_histogram.len() == 1 {
                let src = src_histogram[0];
                let dst = dst_histogram[0];
                mapper.mappings.link_if_both_unmapped(src, dst);
            }
        }
    }
}
