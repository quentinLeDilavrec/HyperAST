use crate::decompressed_tree_store::{
    ContiguousDescendants, DecompressedTreeStore, DecompressedWithParent, LazyDecompressed,
    LazyDecompressedTreeStore, PostOrder, PostOrderIterable, Shallow, ShallowDecompressedTreeStore,
};
use crate::matchers::Mapper;
use crate::matchers::{mapping_store::MonoMappingStore, similarity_metrics};
use hyperast::PrimInt;
use hyperast::types::{HyperAST, LendT, NodeId, WithHashs};
use std::fmt::Debug;

pub struct LazySimpleBottomUpMatcher<
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
    Dsrc: DecompressedTreeStore<HAST, Dsrc::IdD, M::Src>
        + DecompressedWithParent<HAST, Dsrc::IdD>
        + PostOrder<HAST, Dsrc::IdD, M::Src>
        + PostOrderIterable<HAST, Dsrc::IdD, M::Src>
        + ContiguousDescendants<HAST, Dsrc::IdD, M::Src>
        + ShallowDecompressedTreeStore<HAST, Dsrc::IdD, M::Src>
        + LazyDecompressedTreeStore<HAST, M::Src>
        + LazyDecompressed<M::Src>,
    Ddst: DecompressedTreeStore<HAST, Ddst::IdD, M::Dst>
        + DecompressedWithParent<HAST, Ddst::IdD>
        + PostOrder<HAST, Ddst::IdD, M::Dst>
        + PostOrderIterable<HAST, Ddst::IdD, M::Dst>
        + ContiguousDescendants<HAST, Ddst::IdD, M::Dst>
        + ShallowDecompressedTreeStore<HAST, Ddst::IdD, M::Dst>
        + LazyDecompressedTreeStore<HAST, M::Dst>
        + LazyDecompressed<M::Dst>,
    HAST: HyperAST + Copy,
    M: MonoMappingStore + Default,
    const SIMILARITY_THRESHOLD_NUM: u64, // 1
    const SIMILARITY_THRESHOLD_DEN: u64, // 2
> LazySimpleBottomUpMatcher<Dsrc, Ddst, HAST, M, SIMILARITY_THRESHOLD_NUM, SIMILARITY_THRESHOLD_DEN>
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

    pub fn execute(mapper: &mut crate::matchers::Mapper<HAST, Dsrc, Ddst, M>) {
        assert!(mapper.mapping.src_arena.len() > 0);
        for node in mapper.mapping.src_arena.iter_df_post::<false>() {
            let decomp = mapper.mapping.src_arena.decompress_to(&node);
            if !mapper.mapping.mappings.is_src(&node) && mapper.src_has_children_lazy(decomp) {
                let candidates = mapper.get_dst_candidates_lazily(&decomp);
                let mut best_candidate = None;
                let mut max_similarity: f64 = -1.;

                for candidate in candidates {
                    let t_descendents = (mapper.mapping.src_arena).descendants_range(&decomp);
                    let candidate_descendents =
                        mapper.mapping.dst_arena.descendants_range(&candidate);
                    let similarity = similarity_metrics::SimilarityMeasure::range(
                        &t_descendents,
                        &candidate_descendents,
                        &mapper.mapping.mappings,
                    )
                    .chawathe();

                    if similarity
                        >= SIMILARITY_THRESHOLD_NUM as f64 / SIMILARITY_THRESHOLD_DEN as f64
                        && similarity > max_similarity
                    {
                        max_similarity = similarity;
                        best_candidate = Some(candidate);
                    }
                }

                if let Some(best_candidate) = best_candidate {
                    mapper.last_chance_match_histogram_lazy(decomp, best_candidate);
                    mapper
                        .mapping
                        .mappings
                        .link(*decomp.shallow(), *best_candidate.shallow());
                }
            } else if mapper.mapping.mappings.is_src(&node)
                && mapper.has_unmapped_src_children_lazy(&decomp)
            {
                if let Some(dst) = mapper.mapping.mappings.get_dst(&node) {
                    let dst = mapper.mapping.dst_arena.decompress_to(&dst);
                    if mapper.has_unmapped_dst_children_lazy(&dst) {
                        mapper.last_chance_match_histogram_lazy(decomp, dst);
                    }
                }
            }
        }

        mapper.mapping.mappings.link(
            mapper.mapping.src_arena.root(),
            mapper.mapping.dst_arena.root(),
        );
        mapper.last_chance_match_histogram_lazy(
            mapper.mapping.src_arena.starter(),
            mapper.mapping.dst_arena.starter(),
        );
    }
}
