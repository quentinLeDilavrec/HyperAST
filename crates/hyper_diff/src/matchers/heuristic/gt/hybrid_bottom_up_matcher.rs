use crate::decompressed_tree_store::POBorrowSlice;
use crate::matchers::Mapper;
use crate::matchers::mapping_store::MonoMappingStore;
use crate::matchers::similarity_metrics;
use hyperast::PrimInt;
use hyperast::types::{HyperAST, LendT, NodeId, NodeStore, Tree, WithHashs};
use std::fmt::Debug;

use super::factorized_bounds::DecompTreeBounds;

/// TODO wait for `#![feature(adt_const_params)]` #95174 to be improved
///
/// it will allow to make use complex types as const generics
/// ie. make the different threshold neater
pub struct HybridBottomUpMatcher<
    Dsrc,
    Ddst,
    HAST,
    M: MonoMappingStore,
    MZs: MonoMappingStore = M,
    const SIZE_THRESHOLD: usize = 100,
    const SIM_THRESHOLD_NUM: u64 = 1,
    const SIM_THRESHOLD_DEN: u64 = 2,
> {
    _phantom: std::marker::PhantomData<*const (Mapper<HAST, Dsrc, Ddst, M>, MZs)>,
}

impl<
    'a,
    Dsrc: DecompTreeBounds<HAST, M::Src> + POBorrowSlice<HAST, M::Src>,
    Ddst: DecompTreeBounds<HAST, M::Dst> + POBorrowSlice<HAST, M::Dst>,
    HAST: HyperAST + Copy,
    M: MonoMappingStore + Default,
    MZs: MonoMappingStore<Src = M::Src, Dst = M::Dst> + Default,
    const SIZE_THRESHOLD: usize,
    const SIM_THRESHOLD_NUM: u64,
    const SIM_THRESHOLD_DEN: u64,
>
    HybridBottomUpMatcher<
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
        Self::with_recovery(mapper, Self::last_chance_match_hybrid);
    }

    pub fn with_recovery(
        mapper: &mut Mapper<HAST, Dsrc, Ddst, M>,
        recovery: impl Fn(&mut Mapper<HAST, Dsrc, Ddst, M>, M::Src, M::Dst),
    ) {
        for t in mapper.mapping.src_arena.iter_df_post::<true>() {
            if mapper.mapping.src_arena.parent(&t).is_none() {
                mapper.mapping.mappings.link(
                    mapper.mapping.src_arena.root(),
                    mapper.mapping.dst_arena.root(),
                );
                Self::last_chance_match_hybrid(
                    mapper,
                    mapper.mapping.src_arena.root(),
                    mapper.mapping.dst_arena.root(),
                );
                break;
            } else if !(mapper.mappings.is_src(&t) || !Self::src_has_children(mapper, t)) {
                let candidates = mapper.get_dst_candidates(&t);
                let mut best = None;
                let mut max_sim = -1f64;
                for candidate in candidates {
                    let sim = similarity_metrics::SimilarityMeasure::range(
                        &mapper.src_arena.descendants_range(&t),
                        &mapper.dst_arena.descendants_range(&candidate),
                        &mapper.mappings,
                    )
                    .chawathe();
                    let threshold = Mapper::adaptive_threshold(mapper, t, candidate);
                    // SIM_THRESHOLD_NUM as f64 / SIM_THRESHOLD_DEN as f64;
                    if sim > max_sim && sim >= threshold {
                        max_sim = sim;
                        best = Some(candidate);
                    }
                }
                if let Some(best) = best {
                    recovery(mapper, t, best);
                    mapper.mappings.link(t, best);
                }
            } else if mapper.mappings.is_src(&t) && mapper.has_unmapped_src_children(&t) {
                if let Some(dst) = mapper.mappings.get_dst(&t) {
                    if mapper.has_unmapped_dst_children(&dst) {
                        recovery(mapper, t, dst);
                    }
                }
            }
        }
    }

    fn last_chance_match_hybrid(
        mapper: &mut Mapper<HAST, Dsrc, Ddst, M>,
        src: M::Src,
        dst: M::Dst,
    ) {
        if mapper.src_arena.descendants_count(&src) < SIZE_THRESHOLD
            && mapper.dst_arena.descendants_count(&dst) < SIZE_THRESHOLD
        {
            mapper.last_chance_match_zs_slice::<MZs>(src, dst);
        } else {
            mapper.last_chance_match_histogram(&src, &dst);
        }
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
}

impl<HAST: HyperAST + Copy, M, Dsrc, Ddst> Mapper<HAST, Dsrc, Ddst, M> {
    pub fn adaptive_threshold<SrcIdS, DstIdS, SrcIdD, DstIdD>(
        mapper: &Mapper<HAST, Dsrc, Ddst, M>,
        a: SrcIdD,
        cand: DstIdD,
    ) -> f64
    where
        Dsrc: crate::decompressed_tree_store::ContiguousDescendants<HAST, SrcIdD, SrcIdS>,
        Ddst: crate::decompressed_tree_store::ContiguousDescendants<HAST, DstIdD, DstIdS>,
    {
        let a = mapper.mapping.src_arena.descendants_count(&a) as f64;
        let cand = mapper.mapping.dst_arena.descendants_count(&cand) as f64;
        1f64 / (1f64 + (a + cand).ln())
    }
}
