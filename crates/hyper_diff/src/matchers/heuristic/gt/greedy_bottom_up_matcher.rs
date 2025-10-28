use crate::decompressed_tree_store::POBorrowSlice;
use crate::decompressed_tree_store::SimpleZsTree as ZsTree;
use crate::matchers::mapping_store::MonoMappingStore;
use crate::matchers::{Decompressible, Mapper};
use crate::matchers::{optimal::zs::ZsMatcher, similarity_metrics};
use hyperast::PrimInt;
use hyperast::types::{DecompressedFrom, HyperAST, LendT, NodeId, NodeStore, Tree, WithHashs};
use num_traits::{cast, one};
use std::fmt::Debug;

use super::factorized_bounds::DecompTreeBounds;

/// TODO wait for `#![feature(adt_const_params)]` #95174 to be improved
///
/// it will allow to make use complex types as const generics
/// ie. make the different threshold neater
pub struct GreedyBottomUpMatcher<
    Dsrc,
    Ddst,
    HAST,
    M: MonoMappingStore,
    const SIZE_THRESHOLD: usize = 1000,
    const SIM_THRESHOLD_NUM: u64 = 1,
    const SIM_THRESHOLD_DEN: u64 = 2,
> {
    _phantom: std::marker::PhantomData<*const Mapper<HAST, Dsrc, Ddst, M>>,
}

/// TODO PostOrder might not be necessary
impl<
    'a,
    Dsrc: DecompTreeBounds<HAST, M::Src> + POBorrowSlice<HAST, M::Src>,
    Ddst: DecompTreeBounds<HAST, M::Dst> + POBorrowSlice<HAST, M::Dst>,
    HAST: HyperAST + Copy,
    M: MonoMappingStore + Default,
    const SIZE_THRESHOLD: usize,
    const SIM_THRESHOLD_NUM: u64,
    const SIM_THRESHOLD_DEN: u64,
> GreedyBottomUpMatcher<Dsrc, Ddst, HAST, M, SIZE_THRESHOLD, SIM_THRESHOLD_NUM, SIM_THRESHOLD_DEN>
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
        // // WARN it is in postorder and it depends on decomp store
        // // -1 as root is handled after forloop
        for a in mapper.src_arena.iter_df_post::<true>() {
            if mapper.src_arena.parent(&a).is_none() {
                // TODO remove and flip const param of iter_df_post
                break;
            }
            if !(mapper.mappings.is_src(&a) || !Self::src_has_children(mapper, a)) {
                let candidates = mapper.get_dst_candidates(&a);
                let mut best = None;
                let mut max: f64 = -1.;
                for cand in candidates {
                    let sim = similarity_metrics::SimilarityMeasure::range(
                        &mapper.src_arena.descendants_range(&a),
                        &mapper.dst_arena.descendants_range(&cand),
                        &mapper.mappings,
                    )
                    .dice();
                    if sim > max && sim >= SIM_THRESHOLD_NUM as f64 / SIM_THRESHOLD_DEN as f64 {
                        max = sim;
                        best = Some(cand);
                    }
                }

                if let Some(best) = best {
                    recovery(mapper, a, best);
                    mapper.mappings.link(a, best);
                }
            }
        }
        // for root
        mapper.mapping.mappings.link(
            mapper.mapping.src_arena.root(),
            mapper.mapping.dst_arena.root(),
        );

        recovery(
            mapper,
            mapper.mapping.src_arena.root(),
            mapper.mapping.dst_arena.root(),
        );
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
    HAST::IdN: NodeId<IdN = HAST::IdN>,
    for<'t> LendT<'t, HAST>: WithHashs,
{
    pub(crate) fn last_chance_match_zs<
        MZs: MonoMappingStore<Src = M::Src, Dst = M::Dst> + Default,
    >(
        &mut self,
        src: M::Src,
        dst: M::Dst,
    ) {
        // let src_s = mapper.src_arena.descendants_count(&src);
        // let dst_s = mapper.dst_arena.descendants_count(&dst);
        // if !(src_s < cast(SIZE_THRESHOLD).unwrap() || dst_s < cast(SIZE_THRESHOLD).unwrap()) {
        //     return;
        // }
        let stores = self.hyperast;
        let src_offset;
        use crate::decompressed_tree_store::ShallowDecompressedTreeStore;
        let o_src = self.mapping.src_arena.original(&src);
        let o_dst = self.mapping.dst_arena.original(&dst);
        let src_arena = ZsTree::<HAST::IdN, M::Src>::decompress(stores, &o_src);
        let src_arena = Decompressible {
            hyperast: stores,
            decomp: src_arena,
        };
        src_offset = src - src_arena.root();
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
        let src_offset;
        use crate::decompressed_tree_store::ShallowDecompressedTreeStore;
        let src_arena = self.mapping.src_arena.slice_po(&src);
        src_offset = src - src_arena.root();
        let dst_arena = self.mapping.dst_arena.slice_po(&dst);
        let mappings: MZs = ZsMatcher::match_with(self.hyperast, src_arena, dst_arena);
        let dst_offset = self.dst_arena.first_descendant(&dst);
        assert_eq!(self.src_arena.first_descendant(&src), src_offset);
        self.apply_mappings(src_offset, dst_offset, mappings);
    }
}
