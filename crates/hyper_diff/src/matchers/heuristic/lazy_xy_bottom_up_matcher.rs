use std::fmt::Debug;

use hyperast::PrimInt;
use hyperast::compat::HashMap;
use hyperast::types::{HyperAST, LendT, WithHashs};

use crate::decompressed_tree_store::{
    DecompressedTreeStore, DecompressedWithParent, LazyDecompressed, LazyDecompressedTreeStore,
    Shallow,
};
use crate::mappings::MonoMappingStore;
use crate::matchers::Mapper;
use crate::matchers::heuristic::gt::Extendable;
use crate::similarity_metrics::SimilarityMeasure;

use super::factorized_bounds::LazyDecompTreeBorrowBounds;

pub struct LazyXYBottomUpMatcher<
    Mpr,
    const SIM_THRESHOLD_NUM: u64 = 1,
    const SIM_THRESHOLD_DEN: u64 = 2,
> {
    _phantom: std::marker::PhantomData<*const Mpr>,
}

impl<
    Dsrc: LazyDecompTreeBorrowBounds<HAST, M::Src>,
    Ddst: LazyDecompTreeBorrowBounds<HAST, M::Dst>,
    HAST: HyperAST + Copy,
    M: MonoMappingStore,
    const SIM_THRESHOLD_NUM: u64,
    const SIM_THRESHOLD_DEN: u64,
> LazyXYBottomUpMatcher<Mapper<HAST, Dsrc, Ddst, M>, SIM_THRESHOLD_NUM, SIM_THRESHOLD_DEN>
where
    for<'t> LendT<'t, HAST>: WithHashs,
    Dsrc::IdD: PrimInt,
    Ddst::IdD: PrimInt,
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
        mapper.bottom_up_lazy_with_similarity_threshold_and_recovery(
            |_, _, _| SIM_THRESHOLD_NUM as f64 / SIM_THRESHOLD_DEN as f64,
            SimilarityMeasure::jaccard,
            Self::last_chance_match,
        );
    }

    fn last_chance_match(mapper: &mut Mapper<HAST, Dsrc, Ddst, M>, src: Dsrc::IdD, dst: Ddst::IdD) {
        let it = mapper.prep_histogram_matching_lazy(src, dst);
        for (_t, (src_histogram, dst_histogram)) in it {
            // Matches all pairs of nodes whose types appear only once in src and dst
            if let Some(src) = src_histogram
                && let Some(dst) = dst_histogram
            {
                let src = src.to_shallow();
                let dst = dst.to_shallow();
                mapper.mappings.link_if_both_unmapped(src, dst);
            }
        }
    }
}

impl<
    HAST: HyperAST + Copy,
    Dsrc: LazyDecompressed<M::Src>,
    Ddst: LazyDecompressed<M::Dst>,
    M: MonoMappingStore,
> crate::matchers::Mapper<HAST, Dsrc, Ddst, M>
where
    M::Src: PrimInt,
    M::Dst: PrimInt,
    Dsrc::IdD: PrimInt,
    Ddst::IdD: PrimInt,
    Dsrc: DecompressedTreeStore<HAST, Dsrc::IdD, M::Src>
        + DecompressedWithParent<HAST, Dsrc::IdD>
        + LazyDecompressedTreeStore<HAST, M::Src>,
    Ddst: DecompressedTreeStore<HAST, Ddst::IdD, M::Dst>
        + DecompressedWithParent<HAST, Ddst::IdD>
        + LazyDecompressedTreeStore<HAST, M::Dst>,
    HAST::Label: Eq,
    for<'t> LendT<'t, HAST>: WithHashs,
{
    pub(crate) fn prep_histogram_matching_lazy<
        Src: Extendable<Dsrc::IdD>,
        Dst: Extendable<Ddst::IdD>,
    >(
        &mut self,
        src: Dsrc::IdD,
        dst: Ddst::IdD,
    ) -> impl Iterator<Item = (<HAST::TS as hyperast::types::TypeStore>::Ty, (Src, Dst))>
    + use<HAST, Dsrc, Ddst, Src, Dst, M> {
        use hyperast::compat::hash_map::Entry;
        let src_histogram = (self.src_arena.decompress_children(&src))
            .into_iter()
            .filter(|child| !self.mappings.is_src(child.shallow()))
            .fold(HashMap::<_, Src>::new(), |mut acc, child| {
                let t = self.hyperast.resolve_type(&self.src_arena.original(&child));
                match acc.entry(t) {
                    Entry::Occupied(mut v) => {
                        v.get_mut().push(child);
                    }
                    Entry::Vacant(v) => {
                        v.insert(Extendable::first(child));
                    }
                }
                acc
            });

        let mut dst_histogram = (self.dst_arena.decompress_children(&dst))
            .into_iter()
            .filter(|child| !self.mappings.is_dst(child.shallow()))
            .fold(HashMap::<_, Dst>::new(), |mut acc, child| {
                let t = self.hyperast.resolve_type(&self.dst_arena.original(&child));
                match acc.entry(t) {
                    Entry::Occupied(mut v) => {
                        v.get_mut().push(child);
                    }
                    Entry::Vacant(v) => {
                        v.insert(Extendable::first(child));
                    }
                }
                acc
            });

        src_histogram.into_iter().filter_map(move |(t, src_hist)| {
            dst_histogram
                .remove(&t)
                .map(|dst_hist| (t, (src_hist, dst_hist)))
        })
    }
}
