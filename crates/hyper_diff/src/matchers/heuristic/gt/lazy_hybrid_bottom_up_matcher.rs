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
use crate::matchers::similarity_metrics::SimilarityMeasure;
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
        mapper.bottom_up_with_similarity_threshold_and_recovery(
            |mapper, a, cand| {
                let a = mapper.mapping.src_arena.descendants_count(&a) as f64;
                let cand = mapper.mapping.dst_arena.descendants_count(&cand) as f64;
                1f64 / (1f64 + ((a + cand) as f64).ln())
            },
            SimilarityMeasure::chawathe,
            Self::last_chance_match_hybrid,
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
            mapper.last_chance_match_zs_lazy_slice::<MZs>(src, dst);
        } else {
            mapper.last_chance_match_histogram_lazy(src, dst);
        }
    }
}
