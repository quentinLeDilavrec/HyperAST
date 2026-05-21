use std::fmt::Debug;

use hyperast::PrimInt;
use hyperast::types::{HyperAST, LendT, WithHashs};

use crate::decompressed_tree_store::POBorrowSlice;
use crate::mappings::MonoMappingStore;
use crate::matchers::Mapper;
use crate::similarity_metrics;

use super::factorized_bounds::DecompTreeBounds;

/// TODO wait for `#![feature(adt_const_params)]` #95174 to be improved
///
/// it will allow to make use complex types as const generics
/// ie. make the different threshold neater
pub struct HybridBottomUpMatcher<
    Mpr: crate::matchers::WithMappings,
    MZs = <Mpr as crate::matchers::WithMappings>::M,
    const SIZE_THRESHOLD: usize = 100,
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
    HybridBottomUpMatcher<
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
        for src in mapper.mapping.src_arena.iter_df_post::<false>() {
            if !mapper.mappings.is_src(&src) && mapper.src_has_children(src) {
                let candidates = mapper.get_dst_candidates(&src);
                let mut best = None;
                let mut max_sim = -1f64;
                for cand in candidates {
                    let sim = similarity_metrics::SimilarityMeasure::range(
                        &mapper.src_arena.descendants_range(&src),
                        &mapper.dst_arena.descendants_range(&cand),
                        &mapper.mappings,
                    )
                    .chawathe();
                    let threshold = Mapper::adaptive_threshold(mapper, src, cand);
                    if sim > max_sim && sim >= threshold {
                        max_sim = sim;
                        best = Some(cand);
                    }
                }
                if let Some(dst) = best {
                    recovery(mapper, src, dst);
                    mapper.mappings.link(src, dst);
                }
            } else if mapper.mappings.is_src(&src)
                && mapper.has_unmapped_src_children(&src)
                && let Some(dst) = mapper.mappings.get_dst(&src)
                && mapper.has_unmapped_dst_children(&dst)
            {
                recovery(mapper, src, dst);
            }
        }
        // for root
        let src = mapper.mapping.src_arena.root();
        let dst = mapper.mapping.dst_arena.root();
        mapper.mapping.mappings.link(src, dst);
        Self::last_chance_match_hybrid(mapper, src, dst);
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
            mapper.last_chance_match_histogram(src, dst);
        }
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
