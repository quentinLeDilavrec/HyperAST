use crate::decompressed_tree_store::ShallowDecompressedTreeStore;
use crate::decompressed_tree_store::{
    ContiguousDescendants, DecompressedTreeStore, DecompressedWithParent, POBorrowSlice, PostOrder,
    PostOrderIterable,
};
use crate::matchers::Mapper;
use crate::matchers::mapping_store::MonoMappingStore;
use crate::matchers::{optimal::zs::ZsMatcher, similarity_metrics};
use hyperast::PrimInt;
use hyperast::types::{DecompressedFrom, HyperAST, LendT, NodeId, NodeStore, Tree, WithHashs};
use num_traits::cast;
use std::fmt::Debug;

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
    mapper: Mapper<HAST, Dsrc, Ddst, M>,
    _phantom: std::marker::PhantomData<*const MZs>,
}

impl<
    'a,
    Dsrc: DecompressedTreeStore<HAST, M::Src>
        + DecompressedWithParent<HAST, M::Src>
        + PostOrder<HAST, M::Src>
        + PostOrderIterable<HAST, M::Src>
        + DecompressedFrom<HAST, Out = Dsrc>
        + ContiguousDescendants<HAST, M::Src>
        + POBorrowSlice<HAST, M::Src>,
    Ddst: DecompressedTreeStore<HAST, M::Dst>
        + DecompressedWithParent<HAST, M::Dst>
        + PostOrder<HAST, M::Dst>
        + PostOrderIterable<HAST, M::Dst>
        + DecompressedFrom<HAST, Out = Ddst>
        + ContiguousDescendants<HAST, M::Dst>
        + POBorrowSlice<HAST, M::Dst>,
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
        for t in mapper.mapping.src_arena.iter_df_post::<true>() {
            if mapper.mapping.src_arena.parent(&t).is_none() {
                mapper.mapping.mappings.link(
                    mapper.mapping.src_arena.root(),
                    mapper.mapping.dst_arena.root(),
                );
                Self::last_chance_match_hybrid(
                    mapper,
                    &mapper.mapping.src_arena.root(),
                    &mapper.mapping.dst_arena.root(),
                );
                break;
            } else if !(mapper.mappings.is_src(&t) || !Self::src_has_children(mapper, t)) {
                let candidates = mapper.get_dst_candidates(&t);
                let mut best = None;
                let mut max_sim = -1f64;
                for candidate in candidates {
                    let t_descendents = &mapper.src_arena.descendants(&t);
                    let candidate_descendents = &mapper.dst_arena.descendants(&candidate);
                    let sim = similarity_metrics::chawathe_similarity(
                        t_descendents,
                        candidate_descendents,
                        &mapper.mappings,
                    );
                    let threshold = 1f64
                        / (1f64
                            + ((candidate_descendents.len() + t_descendents.len()) as f64).ln());
                    // SIM_THRESHOLD_NUM as f64 / SIM_THRESHOLD_DEN as f64;
                    if sim > max_sim && sim >= threshold {
                        max_sim = sim;
                        best = Some(candidate);
                    }
                }
                if let Some(best) = best {
                    Self::last_chance_match_hybrid(mapper, &t, &best);
                    mapper.mappings.link(t, best);
                }
            } else if mapper.mappings.is_src(&t) && mapper.has_unmapped_src_children(&t) {
                if let Some(dst) = mapper.mappings.get_dst(&t) {
                    if mapper.has_unmapped_dst_children(&dst) {
                        Self::last_chance_match_hybrid(mapper, &t, &dst);
                    }
                }
            }
        }
    }

    fn last_chance_match_hybrid(
        mapper: &mut Mapper<HAST, Dsrc, Ddst, M>,
        src: &M::Src,
        dst: &M::Dst,
    ) {
        if mapper.src_arena.descendants_count(src) < SIZE_THRESHOLD
            && mapper.dst_arena.descendants_count(dst) < SIZE_THRESHOLD
        {
            Self::last_chance_match_optimal(mapper, src, dst);
        } else {
            mapper.last_chance_match_histogram(src, dst);
        }
    }

    fn last_chance_match_optimal(
        mapper: &mut Mapper<HAST, Dsrc, Ddst, M>,
        src: &M::Src,
        dst: &M::Dst,
    ) {
        let src_arena = mapper.src_arena.slice_po(src);
        let dst_arena = mapper.dst_arena.slice_po(dst);

        let src_offset: M::Src = *src - src_arena.root();
        let dst_offset: M::Dst = mapper.dst_arena.first_descendant(dst);

        let mappings: MZs = ZsMatcher::match_with(mapper.hyperast, src_arena, dst_arena);

        for (i, t) in mappings.iter() {
            //remapping
            let src: M::Src = src_offset + cast(i).unwrap();
            let dst: M::Dst = dst_offset + cast(t).unwrap();
            // use it
            if !mapper.mappings.is_src(&src) && !mapper.mappings.is_dst(&dst) {
                let tsrc = mapper
                    .hyperast
                    .resolve_type(&mapper.src_arena.original(&src));
                let tdst = mapper
                    .hyperast
                    .resolve_type(&mapper.dst_arena.original(&dst));
                if tsrc == tdst {
                    mapper.mappings.link(src, dst);
                }
            }
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
