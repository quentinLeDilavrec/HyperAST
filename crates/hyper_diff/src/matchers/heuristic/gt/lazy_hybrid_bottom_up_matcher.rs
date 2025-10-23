use crate::decompressed_tree_store::{
    ContiguousDescendants, DecompressedTreeStore, DecompressedWithParent, PostOrder,
    PostOrderIterable,
};
use crate::decompressed_tree_store::{
    LazyDecompressed, LazyDecompressedTreeStore, LazyPOBorrowSlice, Shallow,
    ShallowDecompressedTreeStore,
};
use crate::matchers::Mapper;
use crate::matchers::mapping_store::MonoMappingStore;
use crate::matchers::{optimal::zs::ZsMatcher, similarity_metrics};
use hyperast::PrimInt;
use hyperast::types::{HyperAST, LendT, NodeId, WithHashs};
use num_traits::cast;
use std::fmt::Debug;
use std::marker::PhantomData;

use super::factorized_bounds::LazyDecompTreeBorrowBounds;

/// TODO wait for `#![feature(adt_const_params)]` #95174 to be improved
///
/// it will allow to make use complex types as const generics
/// ie. make the different threshold neater
pub struct LazyHybridBottomUpMatcher<
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
    'a,
    Dsrc: LazyDecompTreeBorrowBounds<HAST, M::Src>,
    Ddst: LazyDecompTreeBorrowBounds<HAST, M::Dst>,
    MZs: MonoMappingStore<Src = Dsrc::IdD, Dst = Ddst::IdD> + Default,
    HAST: HyperAST + Copy,
    M: MonoMappingStore + Default,
    const SIZE_THRESHOLD: usize,
    const SIM_THRESHOLD_NUM: u64,
    const SIM_THRESHOLD_DEN: u64,
>
    LazyHybridBottomUpMatcher<
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
    Dsrc::IdD: PrimInt,
    Ddst::IdD: PrimInt,
    HAST::Label: Eq,
    HAST::IdN: Debug,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
{
    pub fn match_it(mut mapper: Mapper<HAST, Dsrc, Ddst, M>) -> Mapper<HAST, Dsrc, Ddst, M> {
        (mapper.mapping.mappings).topit(mapper.src_arena.len(), mapper.dst_arena.len());
        Self::execute(&mut mapper);
        mapper
    }

    pub fn execute(mapper: &mut Mapper<HAST, Dsrc, Ddst, M>) {
        for t in mapper.mapping.src_arena.iter_df_post::<false>() {
            let a = mapper.mapping.src_arena.decompress_to(&t);
            if !mapper.mapping.mappings.is_src(&t) && mapper.src_has_children_lazy(a) {
                let candidates = mapper.get_dst_candidates_lazily(&a);
                let mut best = None;
                let mut max_sim = -1f64;
                for candidate in candidates {
                    let t_descendents = mapper.mapping.src_arena.descendants_range(&a);
                    let candidate_descendents =
                        mapper.mapping.dst_arena.descendants_range(&candidate);
                    let sim = similarity_metrics::SimilarityMeasure::range(
                        &t_descendents,
                        &candidate_descendents,
                        &mapper.mapping.mappings,
                    )
                    .chawathe();
                    let threshold = 1f64
                        / (1f64
                            + ((mapper.mapping.dst_arena.descendants_count(&candidate)
                                + mapper.mapping.src_arena.descendants_count(&a))
                                as f64)
                                .ln());
                    if sim > max_sim && sim >= threshold {
                        max_sim = sim;
                        best = Some(candidate);
                    }
                }
                if let Some(best) = best {
                    Self::last_chance_match_hybrid(mapper, a, best);
                    mapper.mapping.mappings.link(*a.shallow(), *best.shallow());
                }
            } else if mapper.mapping.mappings.is_src(&t)
                && mapper.has_unmapped_src_children_lazy(&a)
            {
                if let Some(dst) = mapper.mapping.mappings.get_dst(&t) {
                    let dst = mapper.mapping.dst_arena.decompress_to(&dst);
                    if mapper.has_unmapped_dst_children_lazy(&dst) {
                        Self::last_chance_match_hybrid(mapper, a, dst);
                    }
                }
            }
        }

        mapper.mapping.mappings.link(
            mapper.mapping.src_arena.root(),
            mapper.mapping.dst_arena.root(),
        );
        Self::last_chance_match_hybrid(
            mapper,
            mapper.mapping.src_arena.starter(),
            mapper.mapping.dst_arena.starter(),
        );
    }

    /// Hybrid recovery algorithm (finds mappings between src and dst descendants)
    /// Uses ZS (optimal) if the number of descendents is below SIZE_THRESHOLD
    /// Uses simple recovery otherwise
    fn last_chance_match_hybrid(
        mapper: &mut Mapper<HAST, Dsrc, Ddst, M>,
        src: Dsrc::IdD,
        dst: Ddst::IdD,
    ) {
        if mapper.mapping.src_arena.descendants_count(&src) < SIZE_THRESHOLD
            && mapper.mapping.dst_arena.descendants_count(&dst) < SIZE_THRESHOLD
        {
            Self::last_chance_match_zs(mapper, src, dst);
        } else {
            mapper.last_chance_match_histogram_lazy(src, dst);
        }
    }

    /// Optimal ZS recovery algorithm (finds mappings between src and dst descendants)
    fn last_chance_match_zs(
        mapper: &mut Mapper<HAST, Dsrc, Ddst, M>,
        src: Dsrc::IdD,
        dst: Ddst::IdD,
    ) {
        let stores = mapper.hyperast;
        let mapping = &mut mapper.mapping;
        let src_arena = &mut mapping.src_arena;
        let dst_arena = &mut mapping.dst_arena;
        let src_s = src_arena.descendants_count(&src);
        let dst_s = dst_arena.descendants_count(&dst);
        if !(src_s < SIZE_THRESHOLD || dst_s < SIZE_THRESHOLD) {
            return;
        }
        let src_offset;
        let dst_offset;
        let zs_mappings: MZs = {
            let src_arena = src_arena.slice_po(&src);
            src_offset = src - src_arena.root();
            let dst_arena = dst_arena.slice_po(&dst);
            dst_offset = dst - dst_arena.root();
            ZsMatcher::match_with(stores, src_arena, dst_arena)
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
                let tsrc = stores.resolve_type(&mapping.src_arena.original(&src));
                let tdst = stores.resolve_type(&mapping.dst_arena.original(&dst));
                if tsrc == tdst {
                    mappings.link(*src.shallow(), *dst.shallow());
                }
            }
        }
    }
}
