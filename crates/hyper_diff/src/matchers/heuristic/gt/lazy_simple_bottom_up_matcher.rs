use num_traits::ToPrimitive;
use std::fmt::Debug;

use hyperast::PrimInt;
use hyperast::types::{HyperAST, LendT, WithHashs};

use crate::decompressed_tree_store::{Decompressed, LazyDecompressedTreeStore};
use crate::decompressed_tree_store::{DecompressedWithParent, DeepDecompressedTreeStore};
use crate::decompressed_tree_store::{LazyPOBorrowSlice, Shallow};
use crate::mappings::{MappingStore, MonoMappingStore};
use crate::matchers::Mapper;
use crate::similarity_metrics::SimilarityMeasure;

use super::factorized_bounds::LazyDecompTreeBorrowBounds;

pub struct LazySimpleBottomUpMatcher<
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
    const SIM_THRESHOLD_NUM: u64, // 1
    const SIM_THRESHOLD_DEN: u64, // 2
> LazySimpleBottomUpMatcher<Mapper<HAST, Dsrc, Ddst, M>, SIM_THRESHOLD_NUM, SIM_THRESHOLD_DEN>
where
    for<'t> LendT<'t, HAST>: WithHashs,
    M::Src: PrimInt,
    M::Dst: PrimInt,
    Dsrc::IdD: PrimInt,
    Ddst::IdD: PrimInt,
    HAST::Label: Eq,
    HAST::IdN: Debug,
{
    pub fn match_it(
        mut mapper: crate::matchers::Mapper<HAST, Dsrc, Ddst, M>,
    ) -> crate::matchers::Mapper<HAST, Dsrc, Ddst, M> {
        mapper.reserve_mappings();
        Self::execute(&mut mapper);
        mapper
    }

    pub fn execute(mapper: &mut Mapper<HAST, Dsrc, Ddst, M>) {
        mapper.bottom_up_lazy_with_similarity_threshold_and_recovery(
            |_, _, _| SIM_THRESHOLD_NUM as f64 / SIM_THRESHOLD_DEN as f64,
            SimilarityMeasure::chawathe,
            Mapper::last_chance_match_histogram_lazy,
        );
    }
}

impl<
    Dsrc: DeepDecompressedTreeStore<HAST, M::Src> + LazyDecompressedTreeStore<HAST, M::Src>,
    Ddst: DeepDecompressedTreeStore<HAST, M::Dst> + LazyDecompressedTreeStore<HAST, M::Dst>,
    HAST: HyperAST + Copy,
    M: MappingStore,
> Mapper<HAST, Dsrc, Ddst, M>
where
    M::Src: PrimInt,
    M::Dst: PrimInt,
    Dsrc::IdD: PrimInt,
    Ddst::IdD: PrimInt,
    HAST::Label: Eq,
{
    fn lcs_matching_lazy(
        &mut self,
        src: Dsrc::IdD,
        dst: Ddst::IdD,
        cmp: impl Fn(&Self, Dsrc::IdD, Ddst::IdD) -> bool,
    ) {
        let src_children = (self.src_arena.decompress_children(&src))
            .into_iter()
            .filter(|child| !self.mappings.is_src(child.shallow()))
            .collect::<Vec<_>>();

        let dst_children = (self.dst_arena.decompress_children(&dst))
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
    HAST: HyperAST + Copy,
    Dsrc: Decompressed<M::Src>,
    Ddst: Decompressed<M::Dst>,
    M: MonoMappingStore,
> crate::matchers::Mapper<HAST, Dsrc, Ddst, M>
where
    M::Src: PrimInt,
    M::Dst: PrimInt,
    Dsrc::IdD: PrimInt,
    Ddst::IdD: PrimInt,
    Dsrc: DeepDecompressedTreeStore<HAST, M::Src>
        + DecompressedWithParent<HAST, Dsrc::IdD>
        + LazyDecompressedTreeStore<HAST, M::Src>,
    Ddst: DeepDecompressedTreeStore<HAST, M::Dst>
        + DecompressedWithParent<HAST, Ddst::IdD>
        + LazyDecompressedTreeStore<HAST, M::Dst>,
    HAST::Label: Eq,
    for<'t> LendT<'t, HAST>: WithHashs,
{
    pub fn last_chance_match_histogram_lazy(&mut self, src: Dsrc::IdD, dst: Ddst::IdD) {
        self.match_subtree_histogram_lazy(src, dst);
    }

    pub fn match_subtree_histogram_lazy(&mut self, src: Dsrc::IdD, dst: Ddst::IdD) {
        self.lcs_equal_matching_lazy(src, dst);
        self.lcs_structure_matching_lazy(src, dst);

        // then histogram matching
        let src_type = (self.src_arena.parent(&src))
            .map(|p| self.src_arena.original(&p))
            .map(|p| self.hyperast.resolve_type(&p));
        let dst_type = (self.dst_arena.parent(&dst))
            .map(|p| self.dst_arena.original(&p))
            .map(|p| self.hyperast.resolve_type(&p));
        if src_type != dst_type {
            return;
        }
        let it = self.prep_histogram_matching_lazy(src, dst);
        for (_t, (src_histogram, dst_histogram)) in it {
            // Matches all pairs of nodes whose types appear only once in src and dst
            if let Some(src) = src_histogram
                && let Some(dst) = dst_histogram
            {
                self.mappings
                    .link_if_both_unmapped(src.to_shallow(), dst.to_shallow());
                self.match_subtree_histogram_lazy(src, dst);
            }
        }
    }
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
    Dsrc: LazyDecompressedTreeStore<HAST, M::Src>,
    Ddst: LazyDecompressedTreeStore<HAST, M::Dst>,
    M: MonoMappingStore,
> crate::matchers::Mapper<HAST, Dsrc, Ddst, M>
where
    M::Src: PrimInt,
    M::Dst: PrimInt,
    Dsrc::IdD: PrimInt,
    Ddst::IdD: PrimInt,
    Dsrc: DeepDecompressedTreeStore<HAST, M::Src> + DecompressedWithParent<HAST, Dsrc::IdD>,
    Ddst: DeepDecompressedTreeStore<HAST, M::Dst> + DecompressedWithParent<HAST, Ddst::IdD>,
    HAST::Label: Eq,
    for<'t> LendT<'t, HAST>: WithHashs,
{
    /// Matches more pairs of leafs
    fn more_histogram_matching_lazy(
        &mut self,
        src_histogram: Vec<Dsrc::IdD>,
        dst_histogram: Vec<Ddst::IdD>,
    ) {
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
        for (src, dst) in v {
            self.mappings
                .link_if_both_unmapped(src.to_shallow(), dst.to_shallow());
        }
    }

    /// Extend [`last_chance_match_histogram_lazy`] with a more aggressive matching strategy for leafs
    pub fn match_subtree_histogram_lazy2(&mut self, src: Dsrc::IdD, dst: Ddst::IdD) {
        self.lcs_equal_matching_lazy(src, dst);
        self.lcs_structure_matching_lazy(src, dst);

        // then histogram matching
        let src_type = (self.src_arena.parent(&src))
            .map(|p| self.src_arena.original(&p))
            .map(|p| self.hyperast.resolve_type(&p));
        let dst_type = (self.dst_arena.parent(&dst))
            .map(|p| self.dst_arena.original(&p))
            .map(|p| self.hyperast.resolve_type(&p));
        if src_type != dst_type {
            return;
        }
        let it = self.prep_histogram_matching_lazy::<Vec<_>, Vec<_>>(src, dst);
        for (_t, (src_histogram, dst_histogram)) in it {
            if src_histogram.len() == 1 && dst_histogram.len() == 1 {
                let src = src_histogram[0];
                let dst = dst_histogram[0];
                self.mappings
                    .link_if_both_unmapped(src.to_shallow(), dst.to_shallow());
                self.match_subtree_histogram_lazy2(src, dst);
                continue;
            }
            self.more_histogram_matching_lazy(src_histogram, dst_histogram);
        }
    }

    /// Optimal ZS recovery algorithm (finds mappings between src and dst descendants)
    pub fn match_subtree_zs_lazy<
        MZs: MonoMappingStore<Src = Dsrc::IdD, Dst = Ddst::IdD> + Default,
    >(
        &mut self,
        src: Dsrc::IdD,
        dst: Ddst::IdD,
    ) where
        Dsrc::IdD: Shallow<Dsrc::IdD>,
        Ddst::IdD: Shallow<Ddst::IdD>,
    {
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
        assert_eq!(
            mapping.src_arena.first_descendant(&src).to_usize(),
            src_offset.to_usize()
        );
        self.apply_mappings_lazy(src_offset, dst_offset, zs_mappings);
    }

    /// Optimal ZS recovery algorithm (finds mappings between src and dst descendants)
    /// This slicing variant directly uses a slice in each existing decompressed tree.
    pub fn match_subtree_zs_lazy_slice<
        MZs: MonoMappingStore<Src = Dsrc::IdD, Dst = Ddst::IdD> + Default,
    >(
        &mut self,
        src: Dsrc::IdD,
        dst: Ddst::IdD,
    ) where
        Dsrc: LazyPOBorrowSlice<HAST, M::Src>,
        Ddst: LazyPOBorrowSlice<HAST, M::Dst>,
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
