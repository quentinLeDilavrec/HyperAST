//! Simple bottom-up matcher
//!
//! introduced by:
//! Jean-Remy Falleri, Matias Martinez
//! in "Fine-grained, accurate and scalable source differencing", 2024
use std::fmt::Debug;
use std::hash::Hash;

use hyperast::PrimInt;
use hyperast::compat::HashMap;
use hyperast::types::{HyperAST, LendT, TypeStore, WithHashs};

use crate::decompressed_tree_store::POBorrowSlice;
use crate::decompressed_tree_store::{DecompressedTreeStore, DecompressedWithParent};
use crate::mappings::MonoMappingStore;
use crate::matchers::Mapper;
use crate::similarity_metrics::SimilarityMeasure;
use crate::utils::sequence_algorithms::longest_common_subsequence;

use super::Extendable;
use super::factorized_bounds::DecompTreeBounds;

pub struct SimpleBottomUpMatcher<
    Mpr,
    const SIM_THRESHOLD_NUM: u64 = 1,
    const SIM_THRESHOLD_DEN: u64 = 2,
> {
    _phantom: std::marker::PhantomData<*const Mpr>,
}

impl<
    Dsrc: DecompTreeBounds<HAST, M::Src> + POBorrowSlice<HAST, M::Src>,
    Ddst: DecompTreeBounds<HAST, M::Dst> + POBorrowSlice<HAST, M::Dst>,
    HAST: HyperAST + Copy,
    M: MonoMappingStore + Default,
    const SIM_THRESHOLD_NUM: u64, // 1
    const SIM_THRESHOLD_DEN: u64, // 2
> SimpleBottomUpMatcher<Mapper<HAST, Dsrc, Ddst, M>, SIM_THRESHOLD_NUM, SIM_THRESHOLD_DEN>
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
        mapper.bottom_up_with_similarity_threshold_and_recovery(
            |_, _, _| SIM_THRESHOLD_NUM as f64 / SIM_THRESHOLD_DEN as f64,
            SimilarityMeasure::chawathe,
            Mapper::last_chance_match_histogram,
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
    <HAST::TS as TypeStore>::Ty: Copy + Send + Sync + Eq + Hash,
    M::Src: PrimInt,
    M::Dst: PrimInt,
{
    pub(super) fn lcs_matching<F: Fn(&Self, &M::Src, &M::Dst) -> bool>(
        &mut self,
        src: &M::Src,
        dst: &M::Dst,
        cmp: F,
    ) {
        let src_children = (self.src_arena.children(src))
            .into_iter()
            .filter(|x| !self.mappings.is_src(x))
            .collect::<Vec<_>>();
        let dst_children = (self.dst_arena.children(dst))
            .into_iter()
            .filter(|x| !self.mappings.is_dst(x))
            .collect::<Vec<_>>();

        let lcs: Vec<(usize, usize)> =
            longest_common_subsequence(&src_children, &dst_children, |src, dst| {
                cmp(self, src, dst)
            });
        for x in lcs {
            let t1 = &src_children[x.0];
            let t2 = &dst_children[x.1];
            if self.are_srcs_unmapped(t1) && self.are_dsts_unmapped(t2) {
                self.add_mapping_recursively(t1, t2);
            }
        }
    }

    pub(crate) fn prep_histogram_matching<Src: Extendable<M::Src>, Dst: Extendable<M::Dst>>(
        &mut self,
        src: &M::Src,
        dst: &M::Dst,
    ) -> impl Iterator<Item = (<HAST::TS as TypeStore>::Ty, (Src, Dst))>
    + use<HAST, Dsrc, Ddst, Src, Dst, M> {
        use hyperast::compat::hash_map::Entry;
        // both src and dst -histogram have type Map<Type, List<ITree>>
        let src_histogram = (self.src_arena.children(src))
            .into_iter()
            .filter(|child| !self.mappings.is_src(child))
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

        let mut dst_histogram = (self.dst_arena.children(dst))
            .into_iter()
            .filter(|child| !self.mappings.is_dst(child))
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

    /// Matches more pairs of leafs
    fn more_histogram_matching(&mut self, src_histogram: Vec<M::Src>, dst_histogram: Vec<M::Dst>) {
        let mut v: Vec<_> = src_histogram
            .iter()
            .filter(|x| self.mapping.src_arena.descendants_count(x) == 0)
            .flat_map(|x| {
                dst_histogram
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
            self.mappings.link_if_both_unmapped(t1, t2);
        }
    }
}

impl<
    Dsrc: DecompressedTreeStore<HAST, M::Src> + DecompressedWithParent<HAST, M::Src>,
    Ddst: DecompressedTreeStore<HAST, M::Dst> + DecompressedWithParent<HAST, M::Dst>,
    HAST: HyperAST + Copy,
    M: MonoMappingStore,
> Mapper<HAST, Dsrc, Ddst, M>
where
    <HAST::TS as TypeStore>::Ty: Copy + Send + Sync + Eq + Hash,
    M::Src: PrimInt,
    M::Dst: PrimInt,
    for<'t> LendT<'t, HAST>: WithHashs,
    HAST::Label: Eq,
{
    pub fn last_chance_match_histogram(&mut self, src: M::Src, dst: M::Dst) {
        self.lcs_equal_matching(&src, &dst);
        self.lcs_structure_matching(&src, &dst);

        let src_type = (self.src_arena.parent(&src))
            .map(|p| self.src_arena.original(&p))
            .map(|p| self.hyperast.resolve_type(&p));
        let dst_type = (self.dst_arena.parent(&dst))
            .map(|p| self.dst_arena.original(&p))
            .map(|p| self.hyperast.resolve_type(&p));
        if src_type != dst_type {
            return;
        }

        let it = self.prep_histogram_matching(&src, &dst);
        for (_t, (src_histogram, dst_histogram)) in it {
            if let Some(src) = src_histogram
                && let Some(dst) = dst_histogram
            {
                self.mappings.link_if_both_unmapped(src, dst);
                self.last_chance_match_histogram(src, dst);
            }
        }
    }

    // Matches all strictly isomorphic nodes in the descendants of src and dst (step 1 of simple recovery)
    pub(super) fn lcs_equal_matching(&mut self, src: &M::Src, dst: &M::Dst) {
        self.lcs_matching(src, dst, move |s, src, dst| s.isomorphic::<false>(src, dst))
        // NOTE the following impl would not be resilent to collisions
        // self.lcs_matching(src, dst, move |s, src, dst| {
        //     let a = s.hyperast.node_store().resolve(&s.src_arena.original(src));
        //     let b = s.hyperast.node_store().resolve(&s.dst_arena.original(dst));

        //     let a = WithHashs::hash(&a, &HashKind::label());
        //     let b = WithHashs::hash(&b, &HashKind::label());
        //     a == b
        // })
    }

    // Matches all structurally isomorphic nodes in the descendants of src and dst (step 2 of simple recovery)
    pub(super) fn lcs_structure_matching(&mut self, src: &M::Src, dst: &M::Dst) {
        self.lcs_matching(src, dst, move |s, src, dst| s.isomorphic::<true>(src, dst))
        // NOTE the following impl would not be resilent to collisions
        // self.lcs_matching(src, dst, move |s, src, dst| {
        //     let a = s.hyperast.node_store().resolve(&s.src_arena.original(src));
        //     let b = s.hyperast.node_store().resolve(&s.dst_arena.original(dst));

        //     let a = WithHashs::hash(&a, &HashKind::structural());
        //     let b = WithHashs::hash(&b, &HashKind::structural());
        //     a == b
        // })
    }

    pub(crate) fn isomorphic<const STRUCTURAL: bool>(&self, src: &M::Src, dst: &M::Dst) -> bool
    where
        for<'t> LendT<'t, HAST>: WithHashs,
    {
        let src = self.src_arena.original(src);
        let dst = self.dst_arena.original(dst);
        super::isomorphic::<_, true, STRUCTURAL>(self.hyperast, &src, &dst)
    }

    pub fn last_chance_match_histogram2(&mut self, src: &M::Src, dst: &M::Dst) {
        self.lcs_equal_matching(src, dst);
        self.lcs_structure_matching(src, dst);

        let src_type = (self.src_arena.parent(&src))
            .map(|p| self.src_arena.original(&p))
            .map(|p| self.hyperast.resolve_type(&p));
        let dst_type = (self.dst_arena.parent(&dst))
            .map(|p| self.dst_arena.original(&p))
            .map(|p| self.hyperast.resolve_type(&p));
        if src_type != dst_type {
            return;
        }

        let it = self.prep_histogram_matching::<Vec<_>, Vec<_>>(src, dst);
        for (_t, (src_histogram, dst_histogram)) in it {
            if src_histogram.len() == 1 && dst_histogram.len() == 1 {
                let src = src_histogram[0];
                let dst = dst_histogram[0];
                self.mappings.link_if_both_unmapped(src, dst);
                self.last_chance_match_histogram(src, dst);
                continue;
            }
            self.more_histogram_matching(src_histogram, dst_histogram);
        }
    }
}
