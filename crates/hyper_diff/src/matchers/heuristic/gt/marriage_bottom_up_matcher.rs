use std::fmt::Debug;

use hyperast::PrimInt;
use hyperast::types::{HyperAST, LendT, WithHashs};

use super::bottom_up_matcher::candidates_aux;
use super::greedy_bottom_up_matcher::GreedyBottomUpMatcher;
use crate::decompressed_tree_store::{
    DecompressedTreeStore, DecompressedWithParent, POBorrowSlice,
};
use crate::mappings::MonoMappingStore;
use crate::matchers::Mapper;
use crate::similarity_metrics::SimilarityMeasure;

use super::factorized_bounds::DecompTreeBounds;

pub struct MarriageBottomUpMatcher<
    Mpr: crate::matchers::WithMappings,
    MZs = <Mpr as crate::matchers::WithMappings>::M,
    const SIZE_THRESHOLD: usize = 1000,
    const SIM_THRESHOLD_NUM: u64 = 1,
    const SIM_THRESHOLD_DEN: u64 = 2,
> {
    _phantom: std::marker::PhantomData<*const (Mpr, MZs)>,
}

impl<
    Dsrc: DecompTreeBounds<HAST, M::Src> + POBorrowSlice<HAST, M::Src>,
    Ddst: DecompTreeBounds<HAST, M::Dst> + POBorrowSlice<HAST, M::Dst>,
    HAST: HyperAST + Copy,
    M: MonoMappingStore + Default,
    MZs: MonoMappingStore<Src = M::Src, Dst = M::Dst> + Default,
    const SIZE_THRESHOLD: usize,
    const SIM_THRESHOLD_NUM: u64,
    const SIM_THRESHOLD_DEN: u64,
>
    MarriageBottomUpMatcher<
        Mapper<HAST, Dsrc, Ddst, M>,
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
        assert!(mapper.src_arena.len() > 0);
        mapper.bottom_up_stable_with_similarity_threshold_and_recovery(
            |_, _, _| SIM_THRESHOLD_NUM as f64 / SIM_THRESHOLD_DEN as f64,
            |sim| sim.chawathe(),
            GreedyBottomUpMatcher::<_,SIZE_THRESHOLD,SIM_THRESHOLD_NUM,SIM_THRESHOLD_DEN>::last_chance_match_zs,
        );
    }
}

impl<
    Dsrc: DecompressedTreeStore<HAST, M::Src> + DecompressedWithParent<HAST, M::Src>,
    Ddst: DecompressedTreeStore<HAST, M::Dst> + DecompressedWithParent<HAST, M::Dst>,
    HAST: HyperAST + Copy,
    M: MonoMappingStore,
> Mapper<HAST, Dsrc, Ddst, M>
where
    M::Src: PrimInt,
    M::Dst: PrimInt,
{
    pub(super) fn get_src_candidates(&self, dst: &M::Dst) -> Vec<M::Src> {
        let mut seeds = vec![];
        let s = &self.dst_arena.original(dst);
        for c in self.dst_arena.descendants(dst) {
            if self.mappings.is_dst(&c) {
                let m = self.mappings.get_src_unchecked(&c);
                seeds.push(m);
            }
        }
        candidates_aux(&seeds, s, &self.mapping.src_arena, self.hyperast, |x| {
            self.mapping.mappings.is_src(x)
        })
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
{
    fn best_src_candidate(
        &mut self,
        threshold: &impl Fn(&Self, M::Src, M::Dst) -> f64,
        similarity: &impl Fn(&SimilarityMeasure) -> f64,
        dst: M::Dst,
    ) -> Option<M::Src> {
        let candidates = self.get_src_candidates(&dst);
        let mut best = None;
        let mut max: f64 = -1.;
        for cand in candidates {
            let sim = SimilarityMeasure::range(
                &self.src_arena.descendants_range(&cand),
                &self.dst_arena.descendants_range(&dst),
                &self.mappings,
            );
            let sim = similarity(&sim);
            if sim > max && sim >= threshold(self, cand, dst) {
                max = sim;
                best = Some(cand);
            }
        }
        best
    }

    pub fn bottom_up_stable_with_similarity_threshold_and_recovery(
        &mut self,
        thresh: impl Fn(&Self, M::Src, M::Dst) -> f64,
        sim: impl Fn(&SimilarityMeasure) -> f64,
        recovery: impl Fn(&mut Self, M::Src, M::Dst),
    ) {
        for src in self.src_arena.iter_df_post::<false>() {
            if !self.mappings.is_src(&src) && self.src_has_children(src) {
                if let Some(best_dst) = self.best_dst_candidate(&thresh, &sim, src)
                    && self.best_src_candidate(&thresh, &sim, best_dst) == Some(src)
                {
                    recovery(self, src, best_dst);
                    self.mappings.link(src, best_dst);
                }
            } else if let Some(dst) = self.mappings.get_dst(&src)
                && self.mappings.is_src(&src)
                && self.has_unmapped_src_children(&src)
                && self.has_unmapped_dst_children(
                    &self.mappings.get_dst(&src).expect("No dst found for src"),
                )
            {
                recovery(self, src, dst);
            }
        }
        // for root
        let src = self.mapping.src_arena.root();
        let dst = self.mapping.dst_arena.root();
        self.mapping.mappings.link(src, dst);
        recovery(self, src, dst);
    }
}
