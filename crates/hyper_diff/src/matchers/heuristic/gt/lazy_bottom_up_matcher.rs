use crate::decompressed_tree_store::{
    DecompressedTreeStore, DecompressedWithParent, LazyDecompressed, LazyDecompressedTreeStore,
    Shallow, ShallowDecompressedTreeStore,
};
use crate::matchers::Mapper;
use crate::matchers::mapping_store::{MappingStore, MonoMappingStore};
use hyperast::PrimInt;
use hyperast::compat::HashMap;
use hyperast::types::{HyperAST, LendT, NodeId, NodeStore as _, WithHashs};
use num_traits::ToPrimitive;

impl<
    Dsrc: DecompressedTreeStore<HAST, Dsrc::IdD, M::Src> + LazyDecompressedTreeStore<HAST, M::Src>,
    Ddst: DecompressedTreeStore<HAST, Ddst::IdD, M::Dst> + LazyDecompressedTreeStore<HAST, M::Dst>,
    HAST: HyperAST + Copy,
    M: MappingStore,
> Mapper<HAST, Dsrc, Ddst, M>
where
    M::Src: PrimInt,
    M::Dst: PrimInt,
    Dsrc::IdD: PrimInt,
    Ddst::IdD: PrimInt,
    HAST::Label: Eq,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
{
    /// Returns true if *all* descendants in src are unmapped
    pub(super) fn are_srcs_unmapped_lazy(&self, src: &Dsrc::IdD) -> bool {
        self.src_arena
            .descendants(src)
            .iter()
            .all(|x| !self.mappings.is_src(x))
    }

    /// Returns true if *all* descendants in dst are unmapped
    pub(super) fn are_dsts_unmapped_lazy(&self, dst: &Ddst::IdD) -> bool {
        self.dst_arena
            .descendants(dst)
            .iter()
            .all(|x| !self.mappings.is_dst(x))
    }

    /// Returns true if *any* descendants in src are unmapped
    pub(super) fn has_unmapped_src_descendants_lazy(&self, src: &Dsrc::IdD) -> bool {
        self.src_arena
            .descendants(src)
            .iter()
            .any(|x| !self.mappings.is_src(x))
    }

    /// Returns true if *any* descendants in dst are unmapped
    pub(super) fn has_unmapped_dst_descendants_lazy(&self, dst: &Ddst::IdD) -> bool {
        self.dst_arena
            .descendants(dst)
            .iter()
            .any(|x| !self.mappings.is_dst(x))
    }

    pub fn src_has_children_lazy(&mut self, src: Dsrc::IdD) -> bool {
        use hyperast::types::Tree;
        self.hyperast
            .node_store()
            .resolve(&self.src_arena.original(&src))
            .has_children()
    }

    pub(crate) fn add_mapping_recursively_lazy(&mut self, src: &Dsrc::IdD, dst: &Ddst::IdD) {
        self.mappings.link(*src.shallow(), *dst.shallow());
        // WARN check if it works well
        let src = self.src_arena.descendants(src);
        let dst = self.dst_arena.descendants(dst);
        src.iter()
            .zip(dst.iter())
            .for_each(|(src, dst)| self.mappings.link(*src, *dst));
    }

    fn lcs_matching_lazy(
        &mut self,
        src: Dsrc::IdD,
        dst: Ddst::IdD,
        cmp: impl Fn(&Self, Dsrc::IdD, Ddst::IdD) -> bool,
    ) {
        let src_children = self
            .src_arena
            .decompress_children(&src)
            .into_iter()
            .filter(|child| !self.mappings.is_src(child.shallow()))
            .collect::<Vec<_>>();

        let dst_children = self
            .dst_arena
            .decompress_children(&dst)
            .into_iter()
            .filter(|child| !self.mappings.is_dst(child.shallow()))
            .collect::<Vec<_>>();

        use crate::utils::sequence_algorithms::longest_common_subsequence;
        let lcs: Vec<(usize, usize)> =
            longest_common_subsequence(&src_children, &dst_children, |src, dst| {
                cmp(self, *src, *dst)
            });

        for x in lcs {
            let t1 = src_children.get(x.0).unwrap();
            let t2 = dst_children.get(x.1).unwrap();
            if self.are_srcs_unmapped_lazy(t1) && self.are_dsts_unmapped_lazy(t2) {
                self.add_mapping_recursively_lazy(t1, t2);
            }
        }
    }
}
impl<
    Dsrc: DecompressedTreeStore<HAST, Dsrc::IdD, M::Src> + LazyDecompressedTreeStore<HAST, M::Src>,
    Ddst: DecompressedTreeStore<HAST, Ddst::IdD, M::Dst> + LazyDecompressedTreeStore<HAST, M::Dst>,
    HAST: HyperAST + Copy,
    M: MonoMappingStore,
> Mapper<HAST, Dsrc, Ddst, M>
where
    for<'t> LendT<'t, HAST>: WithHashs,
    M::Src: PrimInt,
    M::Dst: PrimInt,
    Dsrc::IdD: PrimInt,
    Ddst::IdD: PrimInt,
    HAST::Label: Eq,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
{
    /// Matches all strictly isomorphic nodes in the descendants of src and dst (step 1 of simple recovery)
    fn lcs_equal_matching_lazy(&mut self, src: Dsrc::IdD, dst: Ddst::IdD) {
        self.lcs_matching_lazy(src, dst, move |s, src, dst| {
            s.isomorphic_lazy::<false>(src, dst)
        })
    }

    /// Matches all structurally isomorphic nodes in the descendants of src and dst (step 2 of simple recovery)
    fn lcs_structure_matching_lazy(&mut self, src: Dsrc::IdD, dst: Ddst::IdD) {
        self.lcs_matching_lazy(src, dst, move |s, src, dst| {
            if s.mapping.src_arena.descendants_count(&src) < 1 {
                return false;
            }
            s.isomorphic_lazy::<true>(src, dst)
        })
    }
    /// Checks if src and dst are (structurally) isomorphic
    fn isomorphic_lazy<const STRUCTURAL: bool>(&self, src: Dsrc::IdD, dst: Ddst::IdD) -> bool {
        let src = self.src_arena.original(&src);
        let dst = self.dst_arena.original(&dst);
        super::isomorphic::<_, true, STRUCTURAL>(self.hyperast, &src, &dst)
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
        let mut seeds = vec![];
        for c in self.mapping.dst_arena.descendants(dst) {
            if let Some(m) = self.mapping.mappings.get_src(&c) {
                let m = self.mapping.src_arena.decompress_to(&m);
                seeds.push(m);
            }
        }
        let s = &self.mapping.dst_arena.original(dst);
        candidates_aux(&seeds, s, &self.mapping.src_arena, self.hyperast, |x| {
            self.mapping.mappings.is_src(x)
        })
    }

    pub(super) fn get_dst_candidates_lazily(&mut self, src: &Dsrc::IdD) -> Vec<Ddst::IdD> {
        let mut seeds = vec![];
        for c in self.mapping.src_arena.descendants(src) {
            if let Some(m) = self.mapping.mappings.get_dst(&c) {
                let m = self.mapping.dst_arena.decompress_to(&m);
                seeds.push(m);
            }
        }
        let s = &self.mapping.src_arena.original(src);
        candidates_aux(&seeds, s, &self.mapping.dst_arena, self.hyperast, |x| {
            self.mapping.mappings.is_dst(x)
        })
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
    HAST::IdN: NodeId<IdN = HAST::IdN>,
    for<'t> LendT<'t, HAST>: WithHashs,
{
    pub fn last_chance_match_histogram_lazy(&mut self, src: Dsrc::IdD, dst: Ddst::IdD) {
        self.lcs_equal_matching_lazy(src, dst);
        self.lcs_structure_matching_lazy(src, dst);

        let src_type = (self.src_arena.parent(&src))
            .map(|p| self.src_arena.original(&p))
            .map(|p| self.hyperast.resolve_type(&p));
        let dst_type = (self.dst_arena.parent(&dst))
            .map(|p| self.dst_arena.original(&p))
            .map(|p| self.hyperast.resolve_type(&p));
        if src_type == dst_type {
            self.histogram_matching_lazy(src, dst);
        }
    }

    /// Matches all pairs of nodes whose types appear only once in src and dst (step 3 of simple recovery)
    fn histogram_matching_lazy(&mut self, src: Dsrc::IdD, dst: Ddst::IdD) {
        let src_histogram: HashMap<_, Vec<Dsrc::IdD>> = self
            .src_arena
            .decompress_children(&src)
            .into_iter()
            .filter(|child| !self.mappings.is_src(child.shallow()))
            .fold(HashMap::new(), |mut acc, child| {
                let child_type = self.hyperast.resolve_type(&self.src_arena.original(&child));
                acc.entry(child_type).or_insert_with(Vec::new).push(child);
                acc
            });

        let dst_histogram: HashMap<_, Vec<Ddst::IdD>> = self
            .dst_arena
            .decompress_children(&dst)
            .into_iter()
            .filter(|child| !self.mappings.is_dst(child.shallow()))
            .fold(HashMap::new(), |mut acc, child| {
                let child_type = self.hyperast.resolve_type(&self.dst_arena.original(&child));
                acc.entry(child_type).or_insert_with(Vec::new).push(child);
                acc
            });

        for src_type in src_histogram.keys() {
            if !dst_histogram.contains_key(src_type) {
                continue;
            }
            if src_histogram[src_type].len() == 1 && dst_histogram[src_type].len() == 1 {
                let t1 = src_histogram[src_type][0];
                let t2 = dst_histogram[src_type][0];
                self.mappings
                    .link_if_both_unmapped(*t1.shallow(), *t2.shallow());
                self.last_chance_match_histogram_lazy(t1, t2);
                continue;
            }
            let mut v: Vec<_> = src_histogram[src_type]
                .iter()
                .filter(|x| self.mapping.src_arena.descendants_count(x) == 0)
                .flat_map(|x| {
                    dst_histogram[src_type]
                        .iter()
                        .filter(|y| self.mapping.dst_arena.descendants_count(y) == 0)
                        .map(|y| (*x, *y))
                })
                .collect();
            v.sort_by(|alink, blink| {
                (alink.0.index().abs_diff(alink.1.index()))
                    .cmp(&blink.0.index().abs_diff(blink.1.index()))
            });
            for (t1, t2) in v {
                self.mappings
                    .link_if_both_unmapped(*t1.shallow(), *t2.shallow());
            }
        }
    }
    /// Optimal ZS recovery algorithm (finds mappings between src and dst descendants)
    pub fn last_chance_match_zs_lazy<
        MZs: MonoMappingStore<Src = Dsrc::IdD, Dst = Ddst::IdD> + Default,
    >(
        &mut self,
        src: Dsrc::IdD,
        dst: Ddst::IdD,
    ) {
        let stores = self.hyperast;
        let mapping = &mut self.mapping;
        let src_arena = &mut mapping.src_arena;
        let dst_arena = &mut mapping.dst_arena;
        use crate::decompressed_tree_store::ShallowDecompressedTreeStore;
        use crate::decompressed_tree_store::SimpleZsTree as ZsTree;
        use crate::matchers::Decompressible;
        use hyperast::types::DecompressedFrom;
        let o_src = src_arena.original(&src);
        let o_dst = dst_arena.original(&dst);
        let src_arena = ZsTree::<HAST::IdN, Dsrc::IdD>::decompress(self.hyperast, &o_src);
        let src_arena = crate::matchers::Decompressible {
            hyperast: stores,
            decomp: src_arena,
        };
        let src_offset = src - src_arena.root();
        let dst_arena = ZsTree::<HAST::IdN, Ddst::IdD>::decompress(self.hyperast, &o_dst);
        let dst_arena = Decompressible {
            hyperast: stores,
            decomp: dst_arena,
        };
        let dst_offset = dst - dst_arena.root();
        use crate::matchers::optimal::zs::ZsMatcher;
        let zs_mappings: MZs = ZsMatcher::match_with(self.hyperast, src_arena, dst_arena);
        // use num_traits::ToPrimitive;
        assert_eq!(
            mapping.src_arena.first_descendant(&src).to_usize(),
            src_offset.to_usize()
        );
        self.apply_mappings_lazy(src_offset, dst_offset, zs_mappings);
    }

    /// Optimal ZS recovery algorithm (finds mappings between src and dst descendants)
    /// This slicing variant directly uses a slice in each existing decompressed tree.
    pub fn last_chance_match_zs_lazy_slice<
        MZs: MonoMappingStore<Src = Dsrc::IdD, Dst = Ddst::IdD> + Default,
    >(
        &mut self,
        src: Dsrc::IdD,
        dst: Ddst::IdD,
    ) where
        Dsrc: crate::decompressed_tree_store::LazyPOBorrowSlice<HAST, Dsrc::IdD, M::Src>,
        Ddst: crate::decompressed_tree_store::LazyPOBorrowSlice<HAST, Ddst::IdD, M::Dst>,
    {
        let mapping = &mut self.mapping;
        let src_arena = &mut mapping.src_arena;
        let dst_arena = &mut mapping.dst_arena;
        use crate::decompressed_tree_store::ShallowDecompressedTreeStore;
        let src_arena = src_arena.slice_po(&src);
        let src_offset = src - src_arena.root();
        let dst_arena = dst_arena.slice_po(&dst);
        let dst_offset = dst - dst_arena.root();
        use crate::matchers::optimal::zs::ZsMatcher;
        let zs_mappings: MZs = ZsMatcher::match_with(self.hyperast, src_arena, dst_arena);
        assert_eq!(
            mapping.src_arena.first_descendant(&src).to_usize(),
            src_offset.to_usize()
        );
        self.apply_mappings_lazy(src_offset, dst_offset, zs_mappings);
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
    Dsrc: LazyDecompressedTreeStore<HAST, M::Src>,
    Ddst: LazyDecompressedTreeStore<HAST, M::Dst>,
    HAST::Label: Eq,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
    for<'t> LendT<'t, HAST>: WithHashs,
{
    pub(crate) fn apply_mappings_lazy<
        MZs: MonoMappingStore<Src = Dsrc::IdD, Dst = Ddst::IdD> + Default,
    >(
        &mut self,
        src_offset: Dsrc::IdD,
        dst_offset: Ddst::IdD,
        mappings: MZs,
    ) {
        let mapping = &mut self.mapping;
        for (i, t) in mappings.iter() {
            use num_traits::cast;
            //remapping
            let src: Dsrc::IdD = src_offset + cast(i).unwrap();
            let dst: Ddst::IdD = dst_offset + cast(t).unwrap();
            // use it
            let mappings = &mut mapping.mappings;
            if !mappings.is_src(src.shallow()) && !mappings.is_dst(dst.shallow()) {
                let osrc = mapping.src_arena.original(&src);
                let tsrc = self.hyperast.resolve_type(&osrc);
                let odst = mapping.dst_arena.original(&dst);
                let tdst = self.hyperast.resolve_type(&odst);
                if tsrc == tdst {
                    mappings.link(*src.shallow(), *dst.shallow());
                }
            }
        }
    }
}

use crate::decompressed_tree_store::ContiguousDescendants;
use crate::matchers::heuristic::factorized_bounds::LazyDecompTreeBounds;
use crate::matchers::similarity_metrics::SimilarityMeasure;
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
    HAST::IdN: NodeId<IdN = HAST::IdN>,
{
    #[inline(always)]
    pub fn bottom_up_with_similarity_threshold_and_recovery(
        &mut self,
        threshold: impl Fn(&Self, Dsrc::IdD, Ddst::IdD) -> f64,
        similarity: impl Fn(&SimilarityMeasure) -> f64,
        recovery: impl Fn(&mut Self, Dsrc::IdD, Ddst::IdD),
    ) {
        assert!(self.src_arena.len() > 0);
        for a in self.src_arena.iter_df_post::<false>() {
            if self.mappings.is_src(&a) {
                continue;
            }
            let a_ = self.mapping.src_arena.decompress_to(&a);
            if self.src_has_children_lazy(a_) {
                let Some(best) = self.best_dst_candidate_lazy(&threshold, &similarity, a_) else {
                    continue;
                };
                recovery(self, a_, best);
                self.mappings.link(*a_.shallow(), *best.shallow());
            } else if self.has_unmapped_src_descendants_lazy(&a_)
                && let Some(dst) = self.mappings.get_dst(&a)
            {
                let dst = self.dst_arena.decompress_to(&dst);
                if self.has_unmapped_dst_descendants_lazy(&dst) {
                    recovery(self, a_, dst);
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
    pub fn bottom_up_stable_with_similarity_threshold_and_recovery(
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
            let Some(best_dst) = self.best_dst_candidate_lazy(&threshold, &similarity, a) else {
                continue;
            };
            if Some(a) == self.best_src_candidate_lazy(&threshold, &similarity, best_dst) {
                recovery(self, a, best_dst);
                self.mappings.link(*a.shallow(), *best_dst.shallow());
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
    fn best_dst_candidate_lazy(
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

    #[inline(always)]
    fn best_src_candidate_lazy(
        &mut self,
        threshold: &impl Fn(&Self, Dsrc::IdD, Ddst::IdD) -> f64,
        similarity: &impl Fn(&SimilarityMeasure) -> f64,
        dst: Ddst::IdD,
    ) -> Option<Dsrc::IdD> {
        let candidates = self.get_src_candidates_lazily(&dst);
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
}

impl<
    Dsrc: LazyDecompTreeBounds<HAST, M::Src>,
    Ddst: LazyDecompTreeBounds<HAST, M::Dst>,
    HAST: HyperAST + Copy,
    M: crate::matchers::mapping_store::MultiMappingStore,
> Mapper<HAST, Dsrc, Ddst, M>
where
    M::Src: PrimInt,
    M::Dst: PrimInt,
    Dsrc::IdD: PrimInt,
    Ddst::IdD: PrimInt,
    Dsrc: ContiguousDescendants<HAST, Dsrc::IdD, M::Src>, // enable efficient similarity computation
    Ddst: ContiguousDescendants<HAST, Ddst::IdD, M::Dst>, // enable efficient similarity computation
    HAST::Label: Eq,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
    Ddst: crate::decompressed_tree_store::RawContiguousDescendants<M::Dst, Ddst::IdD>,
    Dsrc: crate::decompressed_tree_store::RawContiguousDescendants<M::Src, Dsrc::IdD>,
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
        Ddst: crate::decompressed_tree_store::RawContiguousDescendants<M::Dst, Ddst::IdD>,
        Dsrc: crate::decompressed_tree_store::RawContiguousDescendants<M::Src, Dsrc::IdD>,
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
        Ddst: crate::decompressed_tree_store::RawContiguousDescendants<M::Dst, Ddst::IdD>,
        Dsrc: crate::decompressed_tree_store::RawContiguousDescendants<M::Src, Dsrc::IdD>,
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
        let mut seeds = vec![];
        for c in self.mapping.src_arena.descendants(src) {
            for m in self.mapping.mappings.get_dsts(&c) {
                let m = self.mapping.dst_arena.decompress_to(m);
                seeds.push(m);
            }
        }
        let s = &self.mapping.src_arena.original(src);
        if cfg!(debug_assertions) {
            let mut _r = candidates_aux(&seeds, s, &self.mapping.dst_arena, self.hyperast, |x| {
                self.mapping.mappings.is_dst(x)
            });
            _r.sort();
            let r = candidates_aux2(seeds, s, &self.mapping.dst_arena, self.hyperast, |x| {
                self.mapping.mappings.is_dst(x)
            });
            assert_eq!(r, _r);
            return r;
        }
        candidates_aux2(seeds, s, &self.mapping.dst_arena, self.hyperast, |x| {
            self.mapping.mappings.is_dst(x)
        })
    }

    pub fn get_src_candidates_multimap_lazily(&mut self, dst: &Ddst::IdD) -> Vec<Dsrc::IdD> {
        let mut seeds = vec![];
        for c in self.mapping.dst_arena.descendants(dst) {
            for m in self.mapping.mappings.get_srcs(&c) {
                let m = self.mapping.src_arena.decompress_to(m);
                seeds.push(m);
            }
        }
        let s = &self.mapping.dst_arena.original(dst);
        if cfg!(debug_assertions) {
            let mut _r = candidates_aux(&seeds, s, &self.mapping.src_arena, self.hyperast, |x| {
                self.mapping.mappings.is_src(x)
            });
            _r.sort();
            let r = candidates_aux2(seeds, s, &self.mapping.src_arena, self.hyperast, |x| {
                self.mapping.mappings.is_src(x)
            });
            assert_eq!(r, _r);
            return r;
        }
        candidates_aux2(seeds, s, &self.mapping.src_arena, self.hyperast, |x| {
            self.mapping.mappings.is_src(x)
        })
    }
}

fn candidates_aux<HAST: HyperAST + Copy, D, IdD, IdS>(
    seeds: &[IdD],
    s: &HAST::IdN,
    arena: &D,
    hyperast: HAST,
    is_mapped: impl Fn(&IdS) -> bool,
) -> Vec<IdD>
where
    D: DecompressedWithParent<HAST, IdD>,
    D: ShallowDecompressedTreeStore<HAST, IdD, IdS>,
    IdD: PrimInt + Shallow<IdS>,
    IdS: Eq,
{
    let mut candidates = vec![];
    let mut visited = bitvec::bitbox![0;arena.len()];
    let t = hyperast.resolve_type(s);
    for mut seed in seeds.iter().copied() {
        while let Some(parent) = arena.parent(&seed) {
            // If visited break, otherwise mark as visited
            if visited[parent.index()] {
                break;
            }
            visited.set(parent.index(), true);

            let p = &arena.original(&parent);
            let p_type = hyperast.resolve_type(p);
            if p_type == t && !is_mapped(parent.shallow()) && parent != arena.root() {
                candidates.push(parent);
            }
            seed = parent;
        }
    }
    candidates
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
    D: crate::decompressed_tree_store::PostOrder<HAST, IdD, IdS>,
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
