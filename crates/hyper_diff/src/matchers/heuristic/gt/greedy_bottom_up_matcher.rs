use num_traits::{cast, one};
use std::fmt::Debug;

use hyperast::PrimInt;
use hyperast::types::UniformNodeId;
use hyperast::types::{HyperAST, LendT, WithHashs};

use crate::decompressed_tree_store::POBorrowSlice;
use crate::decompressed_tree_store::SimpleZsTree as ZsTree;
use crate::mappings::MonoMappingStore;
use crate::matchers::optimal::zs::ZsMatcher;
use crate::matchers::{Decompressible, Mapper};
use crate::similarity_metrics;

use super::factorized_bounds::DecompTreeBounds;

/// TODO wait for `#![feature(adt_const_params)]` #95174 to be improved
///
/// it will allow to make use complex types as const generics
/// ie. make the different threshold neater
pub struct GreedyBottomUpMatcher<
    Mpr,
    const SIZE_THRESHOLD: usize = 1000,
    const SIM_THRESHOLD_NUM: u64 = 1,
    const SIM_THRESHOLD_DEN: u64 = 2,
> {
    _phantom: std::marker::PhantomData<*const Mpr>,
}

/// TODO PostOrder might not be necessary
impl<
    Dsrc: DecompTreeBounds<HAST, M::Src> + POBorrowSlice<HAST, M::Src>,
    Ddst: DecompTreeBounds<HAST, M::Dst> + POBorrowSlice<HAST, M::Dst>,
    HAST: HyperAST + Copy,
    M: MonoMappingStore + Default,
    const SIZE_THRESHOLD: usize,
    const SIM_THRESHOLD_NUM: u64,
    const SIM_THRESHOLD_DEN: u64,
>
    GreedyBottomUpMatcher<
        Mapper<Dsrc, Ddst, HAST, M>,
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
    HAST::IdN: UniformNodeId,
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
                mapper.last_chance_match_zs::<M>(src, dst);
            }
        };
        Self::with_recovery(mapper, recovery);
    }

    pub fn with_recovery(
        mapper: &mut Mapper<HAST, Dsrc, Ddst, M>,
        recovery: impl Fn(&mut Mapper<HAST, Dsrc, Ddst, M>, M::Src, M::Dst),
    ) {
        assert_eq!(
            // TODO move it inside the arena ...
            mapper.src_arena.root(),
            cast::<_, M::Src>(mapper.src_arena.len()).unwrap() - one()
        );
        assert!(mapper.src_arena.len() > 0);
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
                .dice();
                if sim > max && sim >= SIM_THRESHOLD_NUM as f64 / SIM_THRESHOLD_DEN as f64 {
                    max = sim;
                    best = Some(cand);
                }
            }

            if let Some(dst) = best {
                recovery(mapper, src, dst);
                mapper.mappings.link(src, dst);
            }
        }

        // for root
        let src = mapper.mapping.src_arena.root();
        let dst = mapper.mapping.dst_arena.root();
        mapper.mapping.mappings.link(src, dst);
        recovery(mapper, src, dst);
    }
}

impl<
    HAST: HyperAST + Copy,
    Dsrc: DecompTreeBounds<HAST, M::Src> + POBorrowSlice<HAST, M::Src>,
    Ddst: DecompTreeBounds<HAST, M::Dst> + POBorrowSlice<HAST, M::Dst>,
    M: MonoMappingStore,
> crate::matchers::Mapper<HAST, Dsrc, Ddst, M>
where
    M::Src: PrimInt,
    M::Dst: PrimInt,
    HAST::Label: Eq,
    HAST::IdN: UniformNodeId,
    for<'t> LendT<'t, HAST>: WithHashs,
{
    pub(crate) fn last_chance_match_zs<
        MZs: MonoMappingStore<Src = M::Src, Dst = M::Dst> + Default,
    >(
        &mut self,
        src: M::Src,
        dst: M::Dst,
    ) {
        use crate::decompressed_tree_store::ShallowDecompressedTreeStore;
        use hyperast::types::DecompressedFrom as _;

        let stores = self.hyperast;
        let o_src = self.mapping.src_arena.original(&src);
        let o_dst = self.mapping.dst_arena.original(&dst);
        let src_arena = ZsTree::<HAST::IdN, M::Src>::decompress(stores, &o_src);
        let src_arena = Decompressible {
            hyperast: stores,
            decomp: src_arena,
        };
        let src_offset = src - src_arena.root();
        let dst_arena = ZsTree::<HAST::IdN, M::Dst>::decompress(stores, &o_dst);
        let dst_arena = Decompressible {
            hyperast: stores,
            decomp: dst_arena,
        };
        let mappings: MZs = ZsMatcher::match_with(self.hyperast, src_arena, dst_arena);
        let dst_offset = self.dst_arena.first_descendant(&dst);
        assert_eq!(self.src_arena.first_descendant(&src), src_offset);
        self.apply_mappings(src_offset, dst_offset, mappings);
    }

    pub(crate) fn last_chance_match_zs_slice<
        MZs: MonoMappingStore<Src = M::Src, Dst = M::Dst> + Default,
    >(
        &mut self,
        src: M::Src,
        dst: M::Dst,
    ) where
        Dsrc: crate::decompressed_tree_store::POBorrowSlice<HAST, M::Src>,
        Ddst: crate::decompressed_tree_store::POBorrowSlice<HAST, M::Dst>,
    {
        use crate::decompressed_tree_store::ShallowDecompressedTreeStore;
        let src_arena = self.mapping.src_arena.slice_po(&src);
        let src_offset = src - src_arena.root();
        let dst_arena = self.mapping.dst_arena.slice_po(&dst);
        let mappings: MZs = ZsMatcher::match_with(self.hyperast, src_arena, dst_arena);
        let dst_offset = self.dst_arena.first_descendant(&dst);
        assert_eq!(self.src_arena.first_descendant(&src), src_offset);
        self.apply_mappings(src_offset, dst_offset, mappings);
    }
}
