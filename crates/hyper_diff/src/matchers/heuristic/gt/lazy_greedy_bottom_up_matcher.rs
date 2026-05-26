//! makes greedy_bottom_up_matcher lazy
//! - [ ] first make post order iterator lazy
//!
use std::fmt::Debug;

use hyperast::PrimInt;
use hyperast::types::{HyperAST, LendT, Tree};
use hyperast::types::{WithHashs, WithStats};

use crate::decompressed_tree_store::LazyDecompressedTreeStore;
use crate::decompressed_tree_store::Shallow;
use crate::decompressed_tree_store::{ContiguousDescendants, DecompressedWithParent};
use crate::mappings::MonoMappingStore;
use crate::matchers::Mapper;
use crate::matchers::heuristic::factorized_bounds::LazyDecompTreeBounds;
use crate::matchers::heuristic::gt::lazy_bottom_up_matcher::candidates_aux;
use crate::similarity_metrics::SimilarityMeasure;

use super::factorized_bounds::LazyDecompTreeBorrowBounds;

pub struct LazyGreedyBottomUpMatcher<
    Mpr: crate::matchers::WithMappings,
    MZs = <Mpr as crate::matchers::WithMappings>::M,
    const SIZE_THRESHOLD: usize = 1000,
    const SIM_THRESHOLD_NUM: u64 = 1,
    const SIM_THRESHOLD_DEN: u64 = 2,
> {
    _phantom: std::marker::PhantomData<*const (Mpr, MZs)>,
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
    LazyGreedyBottomUpMatcher<
        Mapper<HAST, Dsrc, Ddst, M>,
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
    HAST::Label: Eq,
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
                mapper.match_subtree_zs_lazy_slice::<MZs>(src, dst);
            }
        };
        mapper.bottom_up_lazy_with_similarity_threshold_and_recovery(
            |_, _, _| SIM_THRESHOLD_NUM as f64 / SIM_THRESHOLD_DEN as f64,
            SimilarityMeasure::dice,
            recovery,
        );
    }
}

impl<
    Dsrc: LazyDecompTreeBounds<HAST, M::Src>,
    Ddst: LazyDecompTreeBounds<HAST, M::Dst>,
    HAST: HyperAST + Copy,
    M: MonoMappingStore,
> Mapper<HAST, Dsrc, Ddst, M>
where
    M::Src: PrimInt,
    M::Dst: PrimInt,
    Dsrc::IdD: PrimInt,
    Ddst::IdD: PrimInt,
    Dsrc: ContiguousDescendants<HAST, Dsrc::IdD, M::Src>, // enable efficient similarity computation
    Ddst: ContiguousDescendants<HAST, Ddst::IdD, M::Dst>, // enable efficient similarity computation
    HAST::Label: Eq,
{
    #[inline(always)]
    pub fn bottom_up_lazy_with_similarity_threshold_and_recovery(
        &mut self,
        threshold: impl Fn(&Self, Dsrc::IdD, Ddst::IdD) -> f64,
        similarity: impl Fn(&SimilarityMeasure) -> f64,
        recovery: impl Fn(&mut Self, Dsrc::IdD, Ddst::IdD),
    ) {
        assert!(self.src_arena.len() > 0);
        for src in self.src_arena.iter_df_post::<false>() {
            if self.mappings.is_src(&src) {
                continue;
            }
            let src_ = self.mapping.src_arena.decompress_to(&src);
            if self.src_has_children_lazy(src_) {
                let Some(dst) = self.best_dst_candidate_lazy(&threshold, &similarity, src_) else {
                    continue;
                };
                recovery(self, src_, dst);
                self.mappings.link(*src_.shallow(), *dst.shallow());
            } else if self.has_unmapped_src_descendants_lazy(&src_)
                && let Some(dst) = self.mappings.get_dst(&src)
            {
                let dst = self.dst_arena.decompress_to(&dst);
                if self.has_unmapped_dst_descendants_lazy(&dst) {
                    recovery(self, src_, dst);
                }
            }
        }
        // for root
        (self.mapping.mappings).link(
            self.src_arena.root().to_shallow(),
            self.dst_arena.root().to_shallow(),
        );
        let src = self.src_arena.starter();
        let dst = self.dst_arena.starter();
        recovery(self, src, dst);
    }

    #[inline(always)]
    pub(super) fn best_dst_candidate_lazy(
        &mut self,
        threshold: &impl Fn(&Self, Dsrc::IdD, Ddst::IdD) -> f64,
        similarity: &impl Fn(&SimilarityMeasure) -> f64,
        src: Dsrc::IdD,
    ) -> Option<Ddst::IdD> {
        let candidates = self.get_dst_candidates_lazily(&src);
        let mut best = None;
        let mut max: f64 = -1.;
        for cand in candidates {
            let sim = SimilarityMeasure::range(
                &self.src_arena.descendants_range(&src),
                &self.dst_arena.descendants_range(&cand),
                &self.mappings,
            );
            let sim = similarity(&sim);
            if sim > max && sim >= threshold(self, src, cand) {
                max = sim;
                best = Some(cand);
            }
        }
        best
    }
}

impl<
    HAST: HyperAST + Copy,
    Dsrc: DecompressedWithParent<HAST, Dsrc::IdD> + LazyDecompressedTreeStore<HAST, M::Src>,
    Ddst: DecompressedWithParent<HAST, Ddst::IdD> + LazyDecompressedTreeStore<HAST, M::Dst>,
    M: MonoMappingStore,
> crate::matchers::Mapper<HAST, Dsrc, Ddst, M>
where
    M::Src: PrimInt,
    M::Dst: PrimInt,
    Dsrc::IdD: PrimInt,
    Ddst::IdD: PrimInt,
{
    pub(super) fn get_src_candidates_lazily(&mut self, dst: &Ddst::IdD) -> Vec<Dsrc::IdD> {
        let seeds = (self.dst_arena.descendants(dst))
            .into_iter()
            .filter_map(|c| self.mapping.mappings.get_src(&c))
            .map(|m| self.mapping.src_arena.decompress_to(&m))
            .collect::<Vec<_>>();
        let s = &self.dst_arena.original(dst);

        candidates_aux(&seeds, s, &self.mapping.src_arena, self.hyperast, |x| {
            self.mapping.mappings.is_src(x)
        })
    }

    pub(in crate::matchers) fn get_dst_candidates_lazily(
        &mut self,
        src: &Dsrc::IdD,
    ) -> Vec<Ddst::IdD> {
        let seeds = (self.src_arena.descendants(src))
            .into_iter()
            .filter_map(|c| self.mapping.mappings.get_dst(&c))
            .map(|m| self.mapping.dst_arena.decompress_to(&m))
            .collect::<Vec<_>>();
        let s = &self.src_arena.original(src);
        candidates_aux(&seeds, s, &self.mapping.dst_arena, self.hyperast, |x| {
            self.mapping.mappings.is_dst(x)
        })
    }
}
