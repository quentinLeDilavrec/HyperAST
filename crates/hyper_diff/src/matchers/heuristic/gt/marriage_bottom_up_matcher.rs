use crate::decompressed_tree_store::{
    ContiguousDescendants, DecompressedTreeStore, DecompressedWithParent, POBorrowSlice, PostOrder,
    PostOrderIterable,
};
use crate::matchers::mapping_store::MonoMappingStore;
use crate::matchers::{Mapper, Mapping};
use crate::matchers::{optimal::zs::ZsMatcher, similarity_metrics};
use hyperast::PrimInt;
use hyperast::types::{DecompressedFrom, HyperAST, LendT, NodeId, NodeStore, Tree, WithHashs};
use num_traits::cast;
use std::fmt::Debug;

pub struct MarriageBottomUpMatcher<
    Dsrc,
    Ddst,
    HAST,
    M: MonoMappingStore,
    MZs: MonoMappingStore = M,
    const SIZE_THRESHOLD: usize = 1000,
    const SIM_THRESHOLD_NUM: u64 = 1,
    const SIM_THRESHOLD_DEN: u64 = 2,
> {
    pub(crate) mapper: Mapper<HAST, Dsrc, Ddst, M>,
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
    MarriageBottomUpMatcher<
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

    pub fn execute<'b>(mapper: &mut Mapper<HAST, Dsrc, Ddst, M>) {
        assert!(mapper.src_arena.len() > 0);
        for a in mapper.src_arena.iter_df_post::<false>() {
            if !(mapper.mappings.is_src(&a) || !Self::src_has_children(mapper, a)) {
                if let Some(best_dst) = Self::best_dst_candidate(mapper, &a) {
                    if Self::best_src_candidate(mapper, &best_dst) == Some(a) {
                        Self::last_chance_match_zs(mapper, a, best_dst);
                        mapper.mappings.link(a, best_dst);
                    }
                }
            } else if mapper.mappings.is_src(&a)
                && Self::has_unmapped_src_children(mapper, &a)
                && Self::has_unmapped_dst_children(
                    mapper,
                    &mapper.mappings.get_dst(&a).expect("No dst found for src"),
                )
            {
                if let Some(dst) = mapper.mappings.get_dst(&a) {
                    Self::last_chance_match_zs(mapper, a, dst);
                }
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
        // WIP https://blog.rust-lang.org/2022/10/28/gats-stabilization.html#implied-static-requirement-from-higher-ranked-trait-bounds
        let src_s = mapper.src_arena.descendants_count(&src);
        let dst_s = mapper.dst_arena.descendants_count(&dst);
        if !(src_s < cast(SIZE_THRESHOLD).unwrap() || dst_s < cast(SIZE_THRESHOLD).unwrap()) {
            return;
        }
        let src_offset;
        use crate::decompressed_tree_store::ShallowDecompressedTreeStore;
        let mappings: MZs = {
            let src_arena = mapper.mapping.src_arena.slice_po(&src);
            src_offset = src - src_arena.root();
            let dst_arena = mapper.mapping.dst_arena.slice_po(&dst);
            ZsMatcher::match_with(mapper.hyperast, src_arena, dst_arena)
        };
        let dst_offset = mapper.dst_arena.first_descendant(&dst);
        assert_eq!(mapper.src_arena.first_descendant(&src), src_offset);
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
}
