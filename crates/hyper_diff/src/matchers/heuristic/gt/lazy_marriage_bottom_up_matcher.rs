use crate::decompressed_tree_store::Shallow;
use crate::matchers::Mapper;
use crate::matchers::mapping_store::MonoMappingStore;
use crate::matchers::{optimal::zs::ZsMatcher, similarity_metrics};
use hyperast::PrimInt;
use hyperast::types::{HyperAST, NodeId, NodeStore, Tree, WithHashs, WithStats};
use num_traits::{cast, one};
use std::{fmt::Debug, marker::PhantomData};

use super::factorized_bounds::LazyDecompTreeBorrowBounds;

pub struct LazyMarriageBottomUpMatcher<
    Dsrc,
    Ddst,
    HAST,
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
    Dsrc: LazyDecompTreeBorrowBounds<HAST, M::Src>,
    Ddst: LazyDecompTreeBorrowBounds<HAST, M::Dst>,
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
    for<'t> <HAST as hyperast::types::AstLending<'t>>::RT: Tree + WithHashs + WithStats,
    for<'t> <HAST as hyperast::types::AstLending<'t>>::RT: WithStats,
    HAST::IdN: Clone + Eq + Debug,
    Dsrc::IdD: PrimInt,
    Ddst::IdD: PrimInt,
    M::Src: PrimInt,
    M::Dst: PrimInt,
    MZs: MonoMappingStore<Src = Dsrc::IdD, Dst = Ddst::IdD> + Default,
    HAST: HyperAST + Copy,
    M: MonoMappingStore,
    HAST::Label: Eq,
    HAST::IdN: Debug,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
{
    pub fn match_it(
        mapping: crate::matchers::Mapper<HAST, Dsrc, Ddst, M>,
    ) -> crate::matchers::Mapper<HAST, Dsrc, Ddst, M> {
        let mut matcher = Self {
            mapper: mapping,
            _phantom: PhantomData,
        };
        matcher.mapper.mapping.mappings.topit(
            matcher.mapper.mapping.src_arena.len(),
            matcher.mapper.mapping.dst_arena.len(),
        );
        Self::execute(&mut matcher.mapper);
        matcher.mapper
    }

    pub fn execute(internal: &mut Mapper<HAST, Dsrc, Ddst, M>) {
        assert_eq!(
            // TODO move it inside the arena ...
            internal.src_arena.root(),
            cast::<_, M::Src>(internal.src_arena.len()).unwrap() - one()
        );
        assert!(internal.src_arena.len() > 0);
        // // WARN it is in postorder and it depends on decomp store
        // // -1 as root is handled after forloop
        for a in internal.src_arena.iter_df_post::<false>() {
            let is_mapped = internal.mappings.is_src(&a);
            let a = internal.mapping.src_arena.decompress_to(&a);
            if !(is_mapped || !Self::src_has_children(internal, a)) {
                if let Some(best_dst) = Self::best_dst_candidate_lazy(internal, &a) {
                    if Self::best_src_candidate_lazy(internal, &best_dst) == Some(a) {
                        Self::last_chance_match(internal, a, best_dst);
                        internal.mappings.link(*a.shallow(), *best_dst.shallow());
                    }
                }
            }
        }
        // for root
        internal.mapping.mappings.link(
            internal.mapping.src_arena.root(),
            internal.mapping.dst_arena.root(),
        );
        let src = internal.src_arena.starter();
        let dst = internal.dst_arena.starter();
        Self::last_chance_match(internal, src, dst);
    }

    fn src_has_children(internal: &Mapper<HAST, Dsrc, Ddst, M>, src: Dsrc::IdD) -> bool {
        let o = internal.src_arena.original(&src);
        let r = internal.hyperast.node_store().resolve(&o).has_children();

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
        internal: &mut Mapper<HAST, Dsrc, Ddst, M>,
        src: &Dsrc::IdD,
    ) -> Option<Ddst::IdD> {
        let candidates = internal.get_dst_candidates_lazily(src);
        let mut best = None;
        let mut max: f64 = -1.;
        for cand in candidates {
            let sim = similarity_metrics::SimilarityMeasure::range(
                &internal.src_arena.descendants_range(src),
                &internal.dst_arena.descendants_range(&cand),
                &internal.mappings,
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

    pub(crate) fn last_chance_match(
        mapper: &mut Mapper<HAST, Dsrc, Ddst, M>,
        src: Dsrc::IdD,
        dst: Ddst::IdD,
    ) {
        Self::last_chance_match_zs(mapper, src, dst);
        // mapper.last_chance_match_histogram_lazy(src, dst);
    }

    pub(crate) fn last_chance_match_zs(
        mapper: &mut Mapper<HAST, Dsrc, Ddst, M>,
        src: Dsrc::IdD,
        dst: Ddst::IdD,
    ) {
        let mapping = &mut mapper.mapping;
        let src_arena = &mut mapping.src_arena;
        let dst_arena = &mut mapping.dst_arena;
        let src_s = src_arena.descendants_count(&src);
        let dst_s = dst_arena.descendants_count(&dst);
        if !(src_s < cast(SIZE_THRESHOLD).unwrap() || dst_s < cast(SIZE_THRESHOLD).unwrap()) {
            return;
        }
        let src_offset;
        let dst_offset;
        let zs_mappings: MZs = {
            use crate::decompressed_tree_store::ShallowDecompressedTreeStore;
            let src_arena = src_arena.slice_po(&src);
            src_offset = src - src_arena.root();
            let dst_arena = dst_arena.slice_po(&dst);
            dst_offset = dst - dst_arena.root();
            ZsMatcher::match_with(mapper.hyperast, src_arena, dst_arena)
        };
        use num_traits::ToPrimitive;
        assert_eq!(
            mapping.src_arena.first_descendant(&src).to_usize(),
            src_offset.to_usize()
        );
        let mappings = &mut mapping.mappings;
        for (i, t) in zs_mappings.iter() {
            //remapping
            let src: Dsrc::IdD = src_offset + cast(i).unwrap();
            let dst: Ddst::IdD = dst_offset + cast(t).unwrap();
            // use it
            if !mappings.is_src(src.shallow()) && !mappings.is_dst(dst.shallow()) {
                let tsrc = mapper
                    .hyperast
                    .resolve_type(&mapping.src_arena.original(&src));
                let tdst = mapper
                    .hyperast
                    .resolve_type(&mapping.dst_arena.original(&dst));
                if tsrc == tdst {
                    mappings.link(*src.shallow(), *dst.shallow());
                }
            }
        }
    }
}
