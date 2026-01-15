use crate::decompressed_tree_store::POBorrowSlice;
use crate::matchers::Mapper;
use crate::matchers::mapping_store::MonoMappingStore;
use crate::matchers::similarity_metrics;
use hyperast::PrimInt;
use hyperast::types::{HyperAST, LendT, NodeId, NodeStore, Tree, WithHashs};
use std::fmt::Debug;

use super::factorized_bounds::DecompTreeBounds;

pub struct MarriageBottomUpMatcher<
    Mpr: crate::matchers::WithMappings,
    MZs = <Mpr as crate::matchers::WithMappings>::M,
    const SIZE_THRESHOLD: usize = 1000,
    const SIM_THRESHOLD_NUM: u64 = 1,
    const SIM_THRESHOLD_DEN: u64 = 2,
> {
    _phantom: std::marker::PhantomData<*const (Mpr, MZs)>,
}

impl<
    Dsrc: DecompTreeBounds<HAST, M::Src> + POBorrowSlice<HAST, M::Src>,
    Ddst: DecompTreeBounds<HAST, M::Dst> + POBorrowSlice<HAST, M::Dst>,
    HAST: HyperAST + Copy,
    M: MonoMappingStore + Default,
    MZs: MonoMappingStore<Src = M::Src, Dst = M::Dst> + Default,
    const SIZE_THRESHOLD: usize,
    const SIM_THRESHOLD_NUM: u64,
    const SIM_THRESHOLD_DEN: u64,
>
    MarriageBottomUpMatcher<
        Mapper<HAST, Dsrc, Ddst, M>,
        MZs,
        SIZE_THRESHOLD,
        SIM_THRESHOLD_NUM,
        SIM_THRESHOLD_DEN,
    >
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
        assert!(mapper.src_arena.len() > 0);
        for a in mapper.src_arena.iter_df_post::<false>() {
            if !mapper.mappings.is_src(&a) && Self::src_has_children(mapper, a) {
                if let Some(best_dst) = Self::best_dst_candidate(mapper, &a)
                    && Self::best_src_candidate(mapper, &best_dst) == Some(a)
                {
                    Self::last_chance_match_zs(mapper, a, best_dst);
                    mapper.mappings.link(a, best_dst);
                }
            } else if let Some(dst) = mapper.mappings.get_dst(&a)
                && mapper.mappings.is_src(&a)
                && Self::has_unmapped_src_children(mapper, &a)
                && Self::has_unmapped_dst_children(
                    mapper,
                    &mapper.mappings.get_dst(&a).expect("No dst found for src"),
                )
            {
                Self::last_chance_match_zs(mapper, a, dst);
            }
        }
        // for root
        mapper.mapping.mappings.link(
            mapper.mapping.src_arena.root(),
            mapper.mapping.dst_arena.root(),
        );
        Self::last_chance_match_zs(mapper, mapper.src_arena.root(), mapper.dst_arena.root());
    }

    fn src_has_children(mapper: &mut Mapper<HAST, Dsrc, Ddst, M>, src: M::Src) -> bool {
        use num_traits::ToPrimitive;
        let r = mapper
            .hyperast
            .node_store()
            .resolve(&mapper.src_arena.original(&src))
            .has_children();
        assert_eq!(
            r,
            mapper.src_arena.lld(&src) < src,
            "{:?} {:?}",
            mapper.src_arena.lld(&src),
            src.to_usize()
        );
        r
    }

    fn has_unmapped_src_children(mapper: &mut Mapper<HAST, Dsrc, Ddst, M>, src: &M::Src) -> bool {
        for a in mapper.src_arena.descendants(src) {
            if !mapper.mappings.is_src(&a) {
                return true;
            }
        }
        false
    }

    fn has_unmapped_dst_children(mapper: &mut Mapper<HAST, Dsrc, Ddst, M>, dst: &M::Dst) -> bool {
        for a in mapper.dst_arena.descendants(dst) {
            if !mapper.mappings.is_dst(&a) {
                return true;
            }
        }
        false
    }

    fn best_dst_candidate(
        mapper: &mut Mapper<HAST, Dsrc, Ddst, M>,
        src: &M::Src,
    ) -> Option<M::Dst> {
        let candidates = mapper.get_dst_candidates(src);
        let mut best = None;
        let mut max: f64 = -1.;
        for cand in candidates {
            let sim = similarity_metrics::SimilarityMeasure::range(
                &mapper.src_arena.descendants_range(src),
                &mapper.dst_arena.descendants_range(&cand),
                &mapper.mappings,
            )
            .chawathe();
            if sim > max && sim >= SIM_THRESHOLD_NUM as f64 / SIM_THRESHOLD_DEN as f64 {
                max = sim;
                best = Some(cand);
            }
        }
        best
    }

    fn best_src_candidate(
        mapper: &mut Mapper<HAST, Dsrc, Ddst, M>,
        dst: &M::Dst,
    ) -> Option<M::Src> {
        let candidates = mapper.get_src_candidates(dst);
        let mut best = None;
        let mut max: f64 = -1.;
        for cand in candidates {
            let sim = similarity_metrics::SimilarityMeasure::range(
                &mapper.src_arena.descendants_range(&cand),
                &mapper.dst_arena.descendants_range(dst),
                &mapper.mappings,
            )
            .chawathe();
            if sim > max && sim >= SIM_THRESHOLD_NUM as f64 / SIM_THRESHOLD_DEN as f64 {
                max = sim;
                best = Some(cand);
            }
        }
        best
    }

    pub(crate) fn last_chance_match_zs(
        mapper: &mut Mapper<HAST, Dsrc, Ddst, M>,
        src: M::Src,
        dst: M::Dst,
    ) {
        let src_s = mapper.src_arena.descendants_count(&src);
        let dst_s = mapper.dst_arena.descendants_count(&dst);
        if !(src_s < SIZE_THRESHOLD || dst_s < SIZE_THRESHOLD) {
            return;
        }
        mapper.last_chance_match_zs::<M>(src, dst);
    }
}
