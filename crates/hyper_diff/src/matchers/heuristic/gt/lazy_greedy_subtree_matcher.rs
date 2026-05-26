use num_traits::ToPrimitive;
use std::fmt::Debug;
use std::hash::Hash;

use hyperast::PrimInt;
use hyperast::compat::HashMap;
use hyperast::types::NodeStore as _;
use hyperast::types::{HyperAST, Labeled, LendT};
use hyperast::types::{WithHashs, WithStats};

use crate::decompressed_tree_store::Shallow;
use crate::decompressed_tree_store::{ContiguousDescendants, DecompressedWithParent};
use crate::decompressed_tree_store::{LazyDecompressed, LazyDecompressedTreeStore};
use crate::mappings::{MonoMappingStore, MultiMappingStore};
use crate::matchers::Mapper;
use crate::similarity_metrics::SimilarityMeasure;
use crate::utils::sequence_algorithms::longest_common_subsequence;

pub struct LazyGreedySubtreeMatcher<Mpr, const MIN_HEIGHT: usize = 1> {
    _phantom: std::marker::PhantomData<*const Mpr>,
}

impl<
    Dsrc: LazyDecompressed<M::Src>,
    Ddst: LazyDecompressed<M::Dst>,
    HAST: HyperAST + Copy,
    M: MonoMappingStore,
    const MIN_HEIGHT: usize, // = 2
> LazyGreedySubtreeMatcher<Mapper<HAST, Dsrc, Ddst, M>, MIN_HEIGHT>
where
    for<'t> LendT<'t, HAST>: WithHashs + WithStats,
    HAST::IdN: Clone + Eq,
    HAST::Label: Clone + Eq,
    Dsrc::IdD: PrimInt + Hash,
    Ddst::IdD: PrimInt + Hash,
    M::Src: PrimInt + Hash,
    M::Dst: PrimInt + Hash,
    Dsrc: DecompressedWithParent<HAST, Dsrc::IdD>
        + ContiguousDescendants<HAST, Dsrc::IdD, M::Src>
        + LazyDecompressedTreeStore<HAST, M::Src>,
    Ddst: DecompressedWithParent<HAST, Ddst::IdD>
        + ContiguousDescendants<HAST, Ddst::IdD, M::Dst>
        + LazyDecompressedTreeStore<HAST, M::Dst>,
{
    pub fn match_it<MM>(
        mut mapper: crate::matchers::Mapper<HAST, Dsrc, Ddst, M>,
    ) -> crate::matchers::Mapper<HAST, Dsrc, Ddst, M>
    where
        MM: MultiMappingStore<Src = Dsrc::IdD, Dst = Ddst::IdD> + Default,
    {
        mapper.mapping.mappings.topit(
            mapper.mapping.src_arena.len(),
            mapper.mapping.dst_arena.len(),
        );
        Self::execute::<MM>(&mut mapper);
        mapper
    }

    pub fn execute<MM: MultiMappingStore<Src = Dsrc::IdD, Dst = Ddst::IdD> + Default>(
        mapper: &mut Mapper<HAST, Dsrc, Ddst, M>,
    ) {
        let mm: MM = Self::compute_multi_mapping(mapper);
        Self::filter_mappings(mapper, &mm);
    }

    pub fn compute_multi_mapping<
        MM: MultiMappingStore<Src = Dsrc::IdD, Dst = Ddst::IdD> + Default,
    >(
        mapper: &mut Mapper<HAST, Dsrc, Ddst, M>,
    ) -> MM {
        let mut mm: MM = Default::default();
        mm.topit(mapper.src_arena.len(), mapper.dst_arena.len());
        Mapper::<HAST, Dsrc, Ddst, M>::compute_multimapping::<_, MIN_HEIGHT>(
            mapper.hyperast,
            &mut mapper.mapping.src_arena,
            &mut mapper.mapping.dst_arena,
            &mut mm,
        );
        mm
    }
}

impl<
    Dsrc: LazyDecompressed<M::Src>,
    Ddst: LazyDecompressed<M::Dst>,
    HAST: HyperAST + Copy,
    M: MonoMappingStore,
    const MIN_HEIGHT: usize, // = 2
> LazyGreedySubtreeMatcher<Mapper<HAST, Dsrc, Ddst, M>, MIN_HEIGHT>
where
    for<'t> LendT<'t, HAST>: WithHashs + WithStats,
    HAST::IdN: Clone,
    HAST::Label: Clone + Eq,
    Dsrc::IdD: PrimInt + Hash,
    Ddst::IdD: PrimInt + Hash,
    M::Src: PrimInt + Hash,
    M::Dst: PrimInt + Hash,
    Dsrc: DecompressedWithParent<HAST, Dsrc::IdD>
        + ContiguousDescendants<HAST, Dsrc::IdD, M::Src>
        + LazyDecompressedTreeStore<HAST, M::Src>,
    Ddst: DecompressedWithParent<HAST, Ddst::IdD>
        + ContiguousDescendants<HAST, Ddst::IdD, M::Dst>
        + LazyDecompressedTreeStore<HAST, M::Dst>,
{
    pub fn filter_mappings<MM: MultiMappingStore<Src = Dsrc::IdD, Dst = Ddst::IdD>>(
        mapper: &mut Mapper<HAST, Dsrc, Ddst, M>,
        multi_mappings: &MM,
    ) {
        let ambiguous_list = Self::handle_non_ambiguous_mappings(mapper, multi_mappings);
        Self::handle_ambiguous_mappings(mapper, ambiguous_list);
    }

    /// Integrate unique mappings, while extracting the ambiguous ones.
    fn handle_non_ambiguous_mappings<MM: MultiMappingStore<Src = Dsrc::IdD, Dst = Ddst::IdD>>(
        mapper: &mut Mapper<HAST, Dsrc, Ddst, M>,
        multi_mappings: &MM,
    ) -> Vec<(Dsrc::IdD, Ddst::IdD)> {
        let mut ambiguous_list: Vec<(Dsrc::IdD, Ddst::IdD)> = vec![];
        let mut ignored = bitvec::bitbox![0;mapper.src_arena.len()];
        for src in multi_mappings.all_mapped_srcs() {
            let mut is_mapping_unique = false;
            if multi_mappings.is_src_unique(&src) {
                let dst = multi_mappings.get_dsts(&src)[0];
                if multi_mappings.is_dst_unique(&dst) {
                    mapper.add_mapping_recursively_lazy(&src, &dst); // TODO subtree opti, do not do explicitly
                    is_mapping_unique = true;
                }
            }

            if !(ignored[src.shallow().index()] || is_mapping_unique) {
                let adsts = multi_mappings.get_dsts(&src);
                let dst = multi_mappings.get_dsts(&src)[0];
                let asrcs = multi_mappings.get_srcs(&dst);
                for asrc in asrcs {
                    ignored.set(asrc.shallow().index(), true);
                    for adst in adsts {
                        ambiguous_list.push((*asrc, *adst));
                    }
                }
            }
        }
        ambiguous_list
    }

    /// Integrate the best ambiguous mappings.
    ///
    /// Returns the rest.
    fn handle_ambiguous_mappings(
        mapper: &mut Mapper<HAST, Dsrc, Ddst, M>,
        mut ambiguous_list: Vec<(Dsrc::IdD, Ddst::IdD)>,
    ) -> Vec<(Dsrc::IdD, Ddst::IdD)> {
        ambiguous_list.sort_by(Self::ambiguous_mappings_comparator(mapper));
        ambiguous_list
            .into_iter()
            .filter_map(|(src, dst)| {
                if !(mapper.mappings.is_src(src.shallow()) || mapper.mappings.is_dst(dst.shallow()))
                {
                    mapper.add_mapping_recursively_lazy(&src, &dst);
                    None
                } else {
                    Some((src, dst))
                }
            })
            .collect::<Vec<_>>()

        // // Select the best ambiguous mappings
        // // let mut src_ignored = bitvec::bitbox![0;mapper.src_arena.len()];
        // // let mut dst_ignored = bitvec::bitbox![0;mapper.dst_arena.len()];
        // for (src, dst) in ambiguous_list {
        //     // let src_i = src.shallow().index();
        //     // let dst_i = dst.shallow().index();
        //     if !(mapper.mappings.is_src(src.shallow()) || mapper.mappings.is_dst(dst.shallow())) {
        //         mapper.add_mapping_recursively_lazy(&src, &dst);
        //     }
        //     // if !(src_ignored[src_i] || dst_ignored[dst_i]) {
        //     //     mapper.add_mapping_recursively_lazy(&src, &dst);
        //     //     src_ignored.set(src_i, true);
        //     //     (mapper.src_arena.descendants(&src))
        //     //         .iter()
        //     //         .for_each(|src| src_ignored.set(src.index(), true));
        //     //     dst_ignored.set(dst_i, true);
        //     //     (mapper.dst_arena.descendants(&dst))
        //     //         .iter()
        //     //         .for_each(|dst| dst_ignored.set(dst.index(), true));
        //     // }
        //     // TODO return additional mappings
        // }
    }

    fn ambiguous_mappings_comparator(
        mapper: &Mapper<HAST, Dsrc, Ddst, M>,
    ) -> impl FnMut(&(Dsrc::IdD, Ddst::IdD), &(Dsrc::IdD, Ddst::IdD)) -> std::cmp::Ordering {
        let mut sib_sim = HashMap::<(Dsrc::IdD, Ddst::IdD), f64>::default();
        let mut psib_sim = sib_sim.clone();
        let mut p_in_p_sim = sib_sim.clone();
        move |a, b| {
            use super::greedy_subtree_matcher::cached;
            let mut sib_sim = cached(&mut sib_sim, |l| Self::coef_sib(mapper, l));
            let mut psib_sim = cached(&mut psib_sim, |l| Self::coef_parent(mapper, l));
            let mut p_in_p_sim = cached(&mut p_in_p_sim, |l| Self::coef_pos_in_parent(mapper, l));
            if Self::same_parents(mapper, a, b) {
                std::cmp::Ordering::Equal
            } else {
                sib_sim(a, b)
                    .reverse()
                    .then_with(|| psib_sim(a, b).reverse())
            }
            .then_with(|| p_in_p_sim(a, b))
            .then_with(|| Self::compare_delta_pos(a, b))
        }
    }

    fn coef_sib(mapper: &Mapper<HAST, Dsrc, Ddst, M>, l: &(Dsrc::IdD, Ddst::IdD)) -> f64 {
        let (p_src, p_dst) = Self::parents(mapper, l);
        SimilarityMeasure::range(
            &mapper.src_arena.descendants_range(&p_src),
            &mapper.dst_arena.descendants_range(&p_dst),
            &mapper.mappings,
        )
        .dice()
    }

    fn parents(
        mapper: &Mapper<HAST, Dsrc, Ddst, M>,
        l: &(Dsrc::IdD, Ddst::IdD),
    ) -> (Dsrc::IdD, Ddst::IdD) {
        let p_src = mapper.src_arena.parent(&l.0).unwrap();
        let p_dst = mapper.dst_arena.parent(&l.1).unwrap();
        (p_src, p_dst)
    }

    fn coef_parent(mapper: &Mapper<HAST, Dsrc, Ddst, M>, l: &(Dsrc::IdD, Ddst::IdD)) -> f64 {
        let s1: Vec<_> = Dsrc::parents(&mapper.src_arena, l.0).collect();
        let s2: Vec<_> = Ddst::parents(&mapper.dst_arena, l.1).collect();
        let common: Vec<(usize, usize)> = longest_common_subsequence(&s1, &s2, |a, b| {
            // TODO directly do the kind+label comparison in hyperast to avoid some conversions
            let (t, l) = {
                let o = mapper.src_arena.original(a);
                let n = mapper.hyperast.node_store().resolve(&o);
                let t = mapper.hyperast.resolve_type(&o);
                (t, n.try_get_label().cloned())
            };
            let o = mapper.dst_arena.original(b);
            let n = mapper.hyperast.node_store().resolve(&o);
            let t2 = mapper.hyperast.resolve_type(&o);
            t == t2 && l.as_ref() == n.try_get_label()
        });
        (2 * common.len()).to_f64().unwrap() / (s1.len() + s2.len()).to_f64().unwrap()
    }

    fn coef_pos_in_parent(mapper: &Mapper<HAST, Dsrc, Ddst, M>, l: &(Dsrc::IdD, Ddst::IdD)) -> f64 {
        let srcs = positions(&mapper.src_arena, l.0);
        let dsts = positions(&mapper.dst_arena, l.1);
        srcs.zip(dsts)
            .map(|(src, dst)| (src - dst) * (src - dst))
            .sum::<f64>()
            .sqrt()
    }

    fn same_parents(
        mapper: &Mapper<HAST, Dsrc, Ddst, M>,
        a: &(Dsrc::IdD, Ddst::IdD),
        b: &(Dsrc::IdD, Ddst::IdD),
    ) -> bool {
        let ap = Self::mapping_parents(mapper, a);
        let bp = Self::mapping_parents(mapper, b);
        ap.0 == bp.0 && ap.1 == bp.1
    }

    fn mapping_parents(
        mapper: &Mapper<HAST, Dsrc, Ddst, M>,
        l: &(Dsrc::IdD, Ddst::IdD),
    ) -> (Option<Dsrc::IdD>, Option<Ddst::IdD>) {
        (mapper.src_arena.parent(&l.0), mapper.dst_arena.parent(&l.1))
    }

    fn compare_delta_pos(
        a: &(Dsrc::IdD, Ddst::IdD),
        b: &(Dsrc::IdD, Ddst::IdD),
    ) -> std::cmp::Ordering {
        usize::cmp(
            &(a.0.shallow().index()).abs_diff(a.1.shallow().index()),
            &(b.0.shallow().index()).abs_diff(b.1.shallow().index()),
        )
    }
}

fn positions<D, IdS, HAST: HyperAST + Copy>(arena: &D, x: D::IdD) -> impl Iterator<Item = f64>
where
    D: DecompressedWithParent<HAST, D::IdD> + LazyDecompressedTreeStore<HAST, IdS>,
    D::IdD: PrimInt,
    IdS: PrimInt,
{
    Some(x).into_iter().chain(arena.parents(x)).filter_map(|x| {
        arena.parent(&x).map(|p| {
            let pos = arena.position_in_parent::<usize>(&x).unwrap();
            let len = arena.children(&p).len();
            pos.to_f64().unwrap() / len.to_f64().unwrap()
        })
    })
}

impl<
    Dsrc: LazyDecompressed<M::Src>,
    Ddst: LazyDecompressed<M::Dst>,
    HAST: HyperAST + Copy,
    M: MonoMappingStore,
> crate::matchers::Mapper<HAST, Dsrc, Ddst, M>
where
    for<'t> LendT<'t, HAST>: WithHashs + WithStats,
    HAST::IdN: Clone + Eq,
    HAST::Label: Eq,
    Dsrc::IdD: Clone,
    Ddst::IdD: Clone,
    M::Src: Debug + Copy,
    M::Dst: Debug + Copy,
    Dsrc: DecompressedWithParent<HAST, Dsrc::IdD> + LazyDecompressedTreeStore<HAST, M::Src>,
    Ddst: DecompressedWithParent<HAST, Ddst::IdD> + LazyDecompressedTreeStore<HAST, M::Dst>,
{
    pub fn compute_multimapping<
        MM: MultiMappingStore<Src = Dsrc::IdD, Dst = Ddst::IdD>,
        const MIN_HEIGHT: usize,
    >(
        hyperast: HAST,
        src_arena: &mut Dsrc,
        dst_arena: &mut Ddst,
        multi_mappings: &mut MM,
    ) {
        let mut src_trees = PriorityTreeList::<'_, _, _, _, _, MIN_HEIGHT>::new(
            hyperast,
            src_arena,
            src_arena.starter(),
        );
        let mut dst_trees = PriorityTreeList::<'_, _, _, _, _, MIN_HEIGHT>::new(
            hyperast,
            dst_arena,
            dst_arena.starter(),
        );
        while src_trees.peek_height() != -1 && dst_trees.peek_height() != -1 {
            while src_trees.peek_height() != dst_trees.peek_height() {
                // open larger
                if src_trees.peek_height() > dst_trees.peek_height() {
                    src_trees.open();
                } else {
                    dst_trees.open();
                }
            }

            let current_height_src_trees = src_trees.pop().unwrap();
            let current_height_dst_trees = dst_trees.pop().unwrap();

            let mut marks_for_src_trees = bitvec::bitbox![0;current_height_src_trees.len()];
            let mut marks_for_dst_trees = bitvec::bitbox![0;current_height_dst_trees.len()];

            for (i, src) in current_height_src_trees.iter().enumerate() {
                for (j, dst) in current_height_dst_trees.iter().cloned().enumerate() {
                    let src = src.clone();
                    let iso = {
                        let src = src_trees.arena.original(&src);
                        let dst = dst_trees.arena.original(&dst);
                        super::isomorphic::<_, true, false>(hyperast, &src, &dst)
                    };
                    if iso {
                        multi_mappings.link(src, dst);
                        marks_for_src_trees.set(i, true);
                        marks_for_dst_trees.set(j, true);
                    }
                }
            }
            for i in 0..marks_for_src_trees.len() {
                if !marks_for_src_trees[i] {
                    src_trees.open_tree(&current_height_src_trees[i]);
                }
            }
            for j in 0..marks_for_dst_trees.len() {
                if !marks_for_dst_trees[j] {
                    dst_trees.open_tree(&current_height_dst_trees[j]);
                }
            }

            src_trees.update_height();
            dst_trees.update_height();
        }
    }
}

pub(super) struct PriorityTreeList<'b, D, IdS, IdD, S, const MIN_HEIGHT: usize> {
    pub trees: Vec<Option<Vec<IdD>>>,

    pub store: S,
    pub(super) arena: &'b mut D,

    pub max_height: usize,

    pub current_idx: isize,

    pub phantom: std::marker::PhantomData<IdS>,
}

impl<'b, D, IdD, HAST, const MIN_HEIGHT: usize>
    PriorityTreeList<'b, D, IdD, D::IdD, HAST, MIN_HEIGHT>
where
    D::IdD: Clone,
    D: LazyDecompressedTreeStore<HAST, IdD>,
    HAST: HyperAST + Copy,
    for<'t> LendT<'t, HAST>: WithStats,
{
    pub(super) fn new(store: HAST, arena: &'b mut D, tree: D::IdD) -> Self {
        let id = arena.original(&tree);
        let h = store.resolve(&id).height() - 1;
        let list_size = if h >= MIN_HEIGHT {
            h + 1 - MIN_HEIGHT
        } else {
            0
        };
        let mut r = Self {
            trees: vec![Default::default(); list_size],
            store,
            arena,
            max_height: h,
            current_idx: if list_size == 0 { -1 } else { 0 },
            phantom: std::marker::PhantomData,
        };
        r.add_tree_aux(tree, h);
        r
    }

    fn idx(&self, height: usize) -> usize {
        self.max_height - height
    }

    fn height(&self, idx: usize) -> usize {
        self.max_height - idx
    }

    fn add_tree(&mut self, tree: D::IdD) {
        let id = self.arena.original(&tree);
        let h = self.store.resolve(&id).height() - 1;
        self.add_tree_aux(tree, h)
    }

    pub(super) fn add_tree_aux(&mut self, tree: D::IdD, h: usize) {
        if h >= MIN_HEIGHT {
            let idx = self.idx(h);
            if self.trees[idx].is_none() {
                self.trees[idx] = Some(vec![]);
            };
            self.trees[idx].as_mut().unwrap().push(tree);
        }
    }

    pub(super) fn open(&mut self) -> Option<Vec<D::IdD>> {
        if let Some(pop) = self.pop() {
            for tree in &pop {
                self.open_tree(tree);
            }
            self.update_height();
            Some(pop)
        } else {
            None
        }
    }

    pub(super) fn pop(&mut self) -> Option<Vec<D::IdD>> {
        if self.current_idx < 0 {
            None
        } else {
            self.trees[self.current_idx as usize].take()
        }
    }

    pub(super) fn open_tree(&mut self, tree: &D::IdD) {
        for c in self.arena.decompress_children(tree) {
            self.add_tree(c);
        }
    }

    pub(super) fn peek_height(&self) -> isize {
        if self.current_idx == -1 {
            -1
        } else {
            self.height(self.current_idx as usize) as isize
        }
    }

    pub(super) fn update_height(&mut self) {
        self.current_idx = -1;
        for i in 0..self.trees.len() {
            if self.trees[i].is_some() {
                self.current_idx = i as isize;
                break;
            }
        }
    }
}
