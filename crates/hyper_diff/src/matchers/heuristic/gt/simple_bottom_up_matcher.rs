use std::fmt::Debug;

use hyperast::PrimInt;
use hyperast::types::{HyperAST, LendT, WithHashs};

use crate::mappings::MonoMappingStore;
use crate::matchers::Mapper;
use crate::similarity_metrics;

use super::factorized_bounds::DecompTreeBounds;

pub struct SimpleBottomUpMatcher<
    Mpr,
    const SIMILARITY_THRESHOLD_NUM: u64 = 1,
    const SIMILARITY_THRESHOLD_DEN: u64 = 2,
> {
    _phantom: std::marker::PhantomData<*const Mpr>,
}

impl<
    Dsrc: DecompTreeBounds<HAST, M::Src>,
    Ddst: DecompTreeBounds<HAST, M::Dst>,
    HAST: HyperAST + Copy,
    M: MonoMappingStore + Default,
    const SIMILARITY_THRESHOLD_NUM: u64, // 1
    const SIMILARITY_THRESHOLD_DEN: u64, // 2
>
    SimpleBottomUpMatcher<
        Mapper<HAST, Dsrc, Ddst, M>,
        SIMILARITY_THRESHOLD_NUM,
        SIMILARITY_THRESHOLD_DEN,
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
        assert!(mapper.src_arena.len() > 0);
        let similarity_threshold: f64 =
            SIMILARITY_THRESHOLD_NUM as f64 / SIMILARITY_THRESHOLD_DEN as f64;

        for src in mapper.src_arena.iter_df_post::<false>() {
            if !mapper.mappings.is_src(&src) && mapper.src_has_children(src) {
                let candidates = mapper.get_dst_candidates(&src);
                let mut best = None;
                let mut max_similarity: f64 = -1.;

                // Can be used to calculate an appropriate threshold. In Gumtree this is done when no threshold is provided.
                // let tree_size = mapper.src_arena.descendants_count(&tree);

                for candidate in candidates {
                    // In gumtree implementation they check if Simliarity_Threshold is set, otherwise they compute a fitting value
                    // But here we assume threshold is always set.
                    let similarity = similarity_metrics::chawathe_similarity(
                        &mapper.src_arena.descendants(&src),
                        &mapper.dst_arena.descendants(&candidate),
                        &mapper.mappings,
                    );

                    if similarity > max_similarity && similarity >= similarity_threshold {
                        max_similarity = similarity;
                        best = Some(candidate);
                    }
                }

                if let Some(dst) = best {
                    mapper.last_chance_match_histogram(src, dst);
                    mapper.mappings.link(src, dst);
                }
            } else if mapper.mappings.is_src(&src)
                && mapper.has_unmapped_src_children(&src)
                && let Some(dst) = mapper.mappings.get_dst(&src)
                && mapper.has_unmapped_dst_children(&dst)
            {
                mapper.last_chance_match_histogram(src, dst);
            }
        }

        // for root
        let src = mapper.mapping.src_arena.root();
        let dst = mapper.mapping.dst_arena.root();
        mapper.mapping.mappings.link(src, dst);
        mapper.last_chance_match_histogram(src, dst);
    }
}
