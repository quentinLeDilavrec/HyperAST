use crate::decompressed_tree_store::{DecompressedTreeStore, PostOrderIterable};
use crate::matchers::mapping_store::MonoMappingStore;
use crate::matchers::{Mapper, similarity_metrics};
use hyperast::PrimInt;
use hyperast::types::{HyperAST, LendT, NodeId, WithHashs};
use std::fmt::Debug;

use super::factorized_bounds::DecompTreeBounds;

pub struct SimpleBottomUpMatcher<
    Dsrc,
    Ddst,
    HAST,
    M: MonoMappingStore,
    const SIMILARITY_THRESHOLD_NUM: u64 = 1,
    const SIMILARITY_THRESHOLD_DEN: u64 = 2,
> {
    mapper: Mapper<HAST, Dsrc, Ddst, M>,
}

impl<
    'a,
    Dsrc: DecompTreeBounds<HAST, M::Src>,
    Ddst: DecompTreeBounds<HAST, M::Dst>,
    HAST: HyperAST + Copy,
    M: MonoMappingStore + Default,
    const SIMILARITY_THRESHOLD_NUM: u64, // 1
    const SIMILARITY_THRESHOLD_DEN: u64, // 2
> SimpleBottomUpMatcher<Dsrc, Ddst, HAST, M, SIMILARITY_THRESHOLD_NUM, SIMILARITY_THRESHOLD_DEN>
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

    pub fn execute<'b>(mapper: &mut Mapper<HAST, Dsrc, Ddst, M>) {
        assert!(mapper.src_arena.len() > 0);
        let similarity_threshold: f64 =
            SIMILARITY_THRESHOLD_NUM as f64 / SIMILARITY_THRESHOLD_DEN as f64;

        for node in mapper.src_arena.iter_df_post::<false>() {
            if !mapper.mappings.is_src(&node) && mapper.src_has_children(node) {
                let candidates = mapper.get_dst_candidates(&node);
                let mut best = None;
                let mut max_similarity: f64 = -1.;

                // Can be used to calculate an appropriate threshold. In Gumtree this is done when no threshold is provided.
                // let tree_size = mapper.src_arena.descendants_count(&tree);

                for candidate in candidates {
                    // In gumtree implementation they check if Simliarity_Threshold is set, otherwise they compute a fitting value
                    // But here we assume threshold is always set.
                    let similarity = similarity_metrics::chawathe_similarity(
                        &mapper.src_arena.descendants(&node),
                        &mapper.dst_arena.descendants(&candidate),
                        &mapper.mappings,
                    );

                    if similarity > max_similarity && similarity >= similarity_threshold {
                        max_similarity = similarity;
                        best = Some(candidate);
                    }
                }

                if let Some(best_candidate) = best {
                    mapper.last_chance_match_histogram(&node, &best_candidate);
                    mapper.mappings.link(node, best_candidate);
                }
            } else if mapper.mappings.is_src(&node) && mapper.has_unmapped_src_children(&node) {
                if let Some(dst) = mapper.mappings.get_dst(&node) {
                    if mapper.has_unmapped_dst_children(&dst) {
                        mapper.last_chance_match_histogram(&node, &dst);
                    }
                }
            }
        }

        mapper.mapping.mappings.link(
            mapper.mapping.src_arena.root(),
            mapper.mapping.dst_arena.root(),
        );
        mapper.last_chance_match_histogram(&mapper.src_arena.root(), &mapper.dst_arena.root());
    }
}
