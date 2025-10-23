use crate::decompressed_tree_store::{
    ContiguousDescendants, DecompressedTreeStore, DecompressedWithParent, LazyDecompressed,
    LazyDecompressedTreeStore, LazyPOBorrowSlice, PostOrder, PostOrderIterable, Shallow,
    ShallowDecompressedTreeStore,
};
use crate::matchers::Mapper;
use crate::matchers::mapping_store::MonoMappingStore;
use crate::matchers::similarity_metrics;
use hyperast::PrimInt;
use hyperast::types::{HyperAST, LendT, NodeId, NodeStore, Tree, WithHashs, WithStats};
use num_traits::{cast, one};
use std::{fmt::Debug, marker::PhantomData};

pub struct LazyMarriageBottomUpMatcher<
    Dsrc,
    Ddst,
    HAST: HyperAST + Copy,
    M: MonoMappingStore,
    MZs: MonoMappingStore = M,
    const SIZE_THRESHOLD: usize = 1000,
    const SIM_THRESHOLD_NUM: u64 = 1,
    const SIM_THRESHOLD_DEN: u64 = 2,
> {
    mapper: Mapper<HAST, Dsrc, Ddst, M>,
    _phantom: PhantomData<*const MZs>,
}

impl<
    Dsrc: LazyDecompressed<M::Src>,
    Ddst: LazyDecompressed<M::Dst>,
    HAST,
    M,
    MZs,
    const SIZE_THRESHOLD: usize,
    const SIM_THRESHOLD_NUM: u64,
    const SIM_THRESHOLD_DEN: u64,
>
    LazyMarriageBottomUpMatcher<
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
    for<'t> LendT<'t, HAST>: Tree + WithHashs + WithStats,
    HAST::IdN: Clone + Eq + Debug,
    Dsrc::IdD: PrimInt,
    Ddst::IdD: PrimInt,
    M::Src: PrimInt,
    M::Dst: PrimInt,
    MZs: MonoMappingStore<Src = Dsrc::IdD, Dst = Ddst::IdD> + Default,
    HAST: HyperAST + Copy,
    M: MonoMappingStore,
    Dsrc: DecompressedTreeStore<HAST, Dsrc::IdD, M::Src>
        + DecompressedWithParent<HAST, Dsrc::IdD>
        + PostOrder<HAST, Dsrc::IdD, M::Src>
        + PostOrderIterable<HAST, Dsrc::IdD, M::Src>
        + ContiguousDescendants<HAST, Dsrc::IdD, M::Src>
        + LazyPOBorrowSlice<HAST, Dsrc::IdD, M::Src>
        + ShallowDecompressedTreeStore<HAST, Dsrc::IdD, M::Src>
        + LazyDecompressedTreeStore<HAST, M::Src>,
    Ddst: DecompressedTreeStore<HAST, Ddst::IdD, M::Dst>
        + DecompressedWithParent<HAST, Ddst::IdD>
        + PostOrder<HAST, Ddst::IdD, M::Dst>
        + PostOrderIterable<HAST, Ddst::IdD, M::Dst>
        + ContiguousDescendants<HAST, Ddst::IdD, M::Dst>
        + LazyPOBorrowSlice<HAST, Ddst::IdD, M::Dst>
        + ShallowDecompressedTreeStore<HAST, Ddst::IdD, M::Dst>
        + LazyDecompressedTreeStore<HAST, M::Dst>
        + LazyDecompressed<M::Dst>,
    HAST::Label: Eq,
    HAST::IdN: Debug,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
{
    pub fn match_it(
        mapper: crate::matchers::Mapper<HAST, Dsrc, Ddst, M>,
    ) -> crate::matchers::Mapper<HAST, Dsrc, Ddst, M> {
        let mut matcher = Self {
            mapper,
            _phantom: PhantomData,
        };
        matcher.mapper.mapping.mappings.topit(
            matcher.mapper.mapping.src_arena.len(),
            matcher.mapper.mapping.dst_arena.len(),
        );
        Self::execute(&mut matcher.mapper);
        matcher.mapper
    }

    pub fn execute(mapper: &mut Mapper<HAST, Dsrc, Ddst, M>) {
        use super::lazy2_greedy_bottom_up_matcher::LazyGreedyBottomUpMatcher as Matcher;
        #[rustfmt::skip]
        let last_chance_match = Matcher::<
            _, _, _, _, MZs,
            SIZE_THRESHOLD,
            SIM_THRESHOLD_NUM,
            SIM_THRESHOLD_DEN,
        >::last_chance_match_zs;
        Self::_execute(mapper, last_chance_match);
    }

    pub(super) fn _execute(
        mapper: &mut Mapper<HAST, Dsrc, Ddst, M>,
        last_chance_match: impl Fn(&mut Mapper<HAST, Dsrc, Ddst, M>, Dsrc::IdD, Ddst::IdD),
    ) {
        assert_eq!(
            // TODO move it inside the arena ...
            mapper.src_arena.root(),
            cast::<_, M::Src>(mapper.src_arena.len()).unwrap() - one()
        );
        assert!(mapper.src_arena.len() > 0);
        for a in mapper.src_arena.iter_df_post::<false>() {
            let is_mapped = mapper.mappings.is_src(&a);
            let a = mapper.mapping.src_arena.decompress_to(&a);
            if !(is_mapped || !Self::src_has_children(mapper, a)) {
                if let Some(best_dst) = Self::best_dst_candidate_lazy(mapper, &a) {
                    if Self::best_src_candidate_lazy(mapper, &best_dst) == Some(a) {
                        last_chance_match(mapper, a, best_dst);
                        mapper.mappings.link(*a.shallow(), *best_dst.shallow());
                    }
                }
            }
        }
        // for root
        mapper.mapping.mappings.link(
            mapper.mapping.src_arena.root(),
            mapper.mapping.dst_arena.root(),
        );
        let src = mapper.src_arena.starter();
        let dst = mapper.dst_arena.starter();
        last_chance_match(mapper, src, dst);
    }

    fn src_has_children(mapper: &Mapper<HAST, Dsrc, Ddst, M>, src: Dsrc::IdD) -> bool {
        let o = mapper.src_arena.original(&src);
        let r = mapper.hyperast.node_store().resolve(&o).has_children();

        // TODO put it back
        // debug_assert_eq!(
        //     r,
        //     internal.src_arena.lld(&src) < *src.shallow(),
        //     "{:?} {:?}",
        //     internal.src_arena.lld(&src),
        //     src.to_usize()
        // );
        r
    }

    fn best_dst_candidate_lazy(
        mapper: &mut Mapper<HAST, Dsrc, Ddst, M>,
        src: &Dsrc::IdD,
    ) -> Option<Ddst::IdD> {
        let candidates = mapper.get_dst_candidates_lazily(src);
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

    fn best_src_candidate_lazy(
        mapper: &mut Mapper<HAST, Dsrc, Ddst, M>,
        dst: &Ddst::IdD,
    ) -> Option<Dsrc::IdD> {
        let candidates = mapper.get_src_candidates_lazily(dst);
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
}
