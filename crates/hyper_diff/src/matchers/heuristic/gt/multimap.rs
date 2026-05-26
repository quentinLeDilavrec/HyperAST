//! Experimenting with a bottom up phase working on multimaps

use hyperast::PrimInt;
use hyperast::types::HyperAST;

use crate::decompressed_tree_store::ContiguousDescendants;
use crate::decompressed_tree_store::DecompressedWithParent;
use crate::decompressed_tree_store::PostOrder;
use crate::decompressed_tree_store::RawContiguousDescendants;
use crate::decompressed_tree_store::{Shallow, ShallowDecompressedTreeStore};
use crate::mappings::MultiMappingStore;
use crate::matchers::Mapper;
use crate::matchers::heuristic::factorized_bounds::LazyDecompTreeBounds;
use crate::similarity_metrics::SimilarityMeasure;

impl<
    Dsrc: LazyDecompTreeBounds<HAST, M::Src>,
    Ddst: LazyDecompTreeBounds<HAST, M::Dst>,
    HAST: HyperAST + Copy,
    M: MultiMappingStore,
> Mapper<HAST, Dsrc, Ddst, M>
where
    M::Src: PrimInt,
    M::Dst: PrimInt,
    Dsrc::IdD: PrimInt,
    Ddst::IdD: PrimInt,
    Dsrc: ContiguousDescendants<HAST, Dsrc::IdD, M::Src>, // enable efficient similarity computation
    Ddst: ContiguousDescendants<HAST, Ddst::IdD, M::Dst>, // enable efficient similarity computation
    HAST::Label: Eq,
    Ddst: RawContiguousDescendants<M::Dst, Ddst::IdD>,
    Dsrc: RawContiguousDescendants<M::Src, Dsrc::IdD>,
    M::Src: Shallow<Dsrc::IdD>,
    M::Dst: Shallow<Ddst::IdD>,
{
    #[inline(always)]
    pub fn bottom_up_stable_multimap_with_similarity_threshold_and_recovery(
        &mut self,
        threshold: impl Fn(&Self, Dsrc::IdD, Ddst::IdD) -> f64,
        similarity: impl Fn(&SimilarityMeasure) -> f64,
        recovery: impl Fn(&mut Self, Dsrc::IdD, Ddst::IdD),
    ) {
        assert!(self.src_arena.len() > 0);
        for a in self.src_arena.iter_df_post::<false>() {
            let is_mapped = self.mappings.is_src(&a);
            if is_mapped {
                continue;
            }
            let a = self.mapping.src_arena.decompress_to(&a);
            if !self.src_has_children_lazy(a) {
                continue;
            }
            let candidates = self.get_dst_candidates_multimap_lazily(&a).into_iter();
            let Some(best) =
                self.best_dst_candidate_multimap_lazy(&threshold, &similarity, a, candidates)
            else {
                continue;
            };
            let candidates = self.get_src_candidates_multimap_lazily(&best).into_iter();
            if Some(a)
                == self.best_src_candidate_multimap_lazy(&threshold, &similarity, best, candidates)
            {
                recovery(self, a, best);
                self.mappings.link(a.to_shallow(), best.to_shallow());
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
    pub fn best_dst_candidate_multimap_lazy(
        &mut self,
        threshold: &impl Fn(&Self, Dsrc::IdD, Ddst::IdD) -> f64,
        similarity: &impl Fn(&SimilarityMeasure) -> f64,
        src: Dsrc::IdD,
        candidates: impl Iterator<Item = Ddst::IdD>,
    ) -> Option<Ddst::IdD>
    where
        Ddst: RawContiguousDescendants<M::Dst, Ddst::IdD>,
        Dsrc: RawContiguousDescendants<M::Src, Dsrc::IdD>,
        M::Src: Shallow<Dsrc::IdD>,
        M::Dst: Shallow<Ddst::IdD>,
    {
        // let candidates = self.get_dst_candidates_multimap_lazily(&src);
        let mut best = None;
        let mut max: f64 = -1.;
        for cand in candidates {
            let sim = SimilarityMeasure::range_multimap(
                &self.src_arena.descendants_range(&src),
                &self.dst_arena.descendants_range(&cand),
                &self.mapping,
            );
            let sim = similarity(&sim);
            if sim > max && sim >= threshold(self, src, cand) {
                max = sim;
                best = Some(cand);
            }
        }
        best
    }

    #[inline(always)]
    pub fn best_src_candidate_multimap_lazy(
        &mut self,
        threshold: &impl Fn(&Self, Dsrc::IdD, Ddst::IdD) -> f64,
        similarity: &impl Fn(&SimilarityMeasure) -> f64,
        dst: Ddst::IdD,
        candidates: impl Iterator<Item = Dsrc::IdD>,
    ) -> Option<Dsrc::IdD>
    where
        Ddst: RawContiguousDescendants<M::Dst, Ddst::IdD>,
        Dsrc: RawContiguousDescendants<M::Src, Dsrc::IdD>,
        M::Src: Shallow<Dsrc::IdD>,
        M::Dst: Shallow<Ddst::IdD>,
    {
        let mut best = None;
        let mut max: f64 = -1.;
        for cand in candidates {
            let sim = SimilarityMeasure::range_multimap(
                &self.src_arena.descendants_range(&cand),
                &self.dst_arena.descendants_range(&dst),
                &self.mapping,
            );
            let sim = similarity(&sim);
            if sim > max && sim >= threshold(self, cand, dst) {
                max = sim;
                best = Some(cand);
            }
        }
        best
    }

    pub fn get_dst_candidates_multimap_lazily(&mut self, src: &Dsrc::IdD) -> Vec<Ddst::IdD> {
        let seeds = (self.src_arena.descendants(src))
            .into_iter()
            .flat_map(|c| self.mapping.mappings.get_dsts(&c))
            .map(|m| self.mapping.dst_arena.decompress_to(m))
            .collect::<Vec<_>>();
        let s = &self.src_arena.original(src);
        candidates_aux2(seeds, s, &self.mapping.dst_arena, self.hyperast, |x| {
            self.mapping.mappings.is_dst(x)
        })
    }

    pub fn get_src_candidates_multimap_lazily(&mut self, dst: &Ddst::IdD) -> Vec<Dsrc::IdD> {
        let seeds = (self.dst_arena.descendants(dst))
            .into_iter()
            .flat_map(|c| self.mapping.mappings.get_srcs(&c))
            .map(|m| self.mapping.src_arena.decompress_to(&m))
            .collect::<Vec<_>>();
        let s = &self.dst_arena.original(dst);
        candidates_aux2(seeds, s, &self.mapping.src_arena, self.hyperast, |x| {
            self.mapping.mappings.is_src(x)
        })
    }
}

/// take advantage of post order layout
fn candidates_aux2<HAST: HyperAST + Copy, D, IdD, IdS>(
    mut seeds: Vec<IdD>,
    s: &HAST::IdN,
    arena: &D,
    hyperast: HAST,
    is_mapped: impl Fn(&IdS) -> bool,
) -> Vec<IdD>
where
    D: DecompressedWithParent<HAST, IdD>,
    D: ShallowDecompressedTreeStore<HAST, IdD, IdS>,
    D: PostOrder<HAST, IdD, IdS>,
    IdD: PrimInt + Shallow<IdS>,
    IdS: Eq,
{
    // We consider all seeds and their ancestors exactly once.
    // The post order traversal gives the opportunity to guarantee that cheaply.
    // NOTE we do not need a set of visited nodes
    seeds.sort();
    let t = hyperast.resolve_type(s);
    let mut i = 0;
    let mut candidates = vec![];
    let mut curr = seeds.get(i).map_or_else(|| arena.root(), |id| *id);
    loop {
        let Some(parent) = arena.parent(&curr) else {
            break;
        };
        if let Some(next_seed) = seeds.get(i + 1)
            && next_seed < &parent
        {
            // the post order traversal guarantees that `parent` must be an ancestor of `next_seed`,
            // so we can handle the `next_seed` first.
            // NOTE reciprocally next_seed is a descendant of parent.
            // NOTE also guarantees ascending order and uniqueness of candidates.
            i += 1;
            curr = *next_seed;
            continue;
        }
        let p = &arena.original(&parent);
        let p_type = hyperast.resolve_type(p);
        if p_type == t && !is_mapped(parent.shallow()) && parent != arena.root() {
            candidates.push(parent);
        }
        curr = parent;
    }
    candidates
}
