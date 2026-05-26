//! Greedy subtree matcher
//!
//! introduced by:
//! Jean-Rémy Falleri, Floréal Morandat, Xavier Blanc, Matias Martinez, Martin Monperrus
//! in "Fine-grained and accurate source code differencing", 2014
use num_traits::{ToPrimitive, one, zero};
use std::hash::Hash;

use hyperast::PrimInt;
use hyperast::compat::HashMap;
use hyperast::types::{HyperAST, NodeStore as _};
use hyperast::types::{Labeled, LendT};
use hyperast::types::{WithChildren, WithHashs};

use crate::decompressed_tree_store::ContiguousDescendants;
use crate::decompressed_tree_store::{DecompressedTreeStore, DecompressedWithParent};
use crate::mappings::{MonoMappingStore, MultiMappingStore};
use crate::matchers::Mapper;
use crate::similarity_metrics::SimilarityMeasure;
use crate::utils::sequence_algorithms::longest_common_subsequence;

pub struct GreedySubtreeMatcher<Mpr, const MIN_HEIGHT: usize = 1> {
    _phantom: std::marker::PhantomData<*const Mpr>,
}

impl<
    Dsrc: DecompressedWithParent<HAST, M::Src> + ContiguousDescendants<HAST, M::Src>,
    Ddst: DecompressedWithParent<HAST, M::Dst> + ContiguousDescendants<HAST, M::Dst>,
    HAST: HyperAST + Copy,
    M: MonoMappingStore,
    const MIN_HEIGHT: usize, // = 2
> GreedySubtreeMatcher<Mapper<HAST, Dsrc, Ddst, M>, MIN_HEIGHT>
where
    M::Src: PrimInt + Hash,
    M::Dst: PrimInt + Hash,
    for<'t> LendT<'t, HAST>: WithHashs,
    HAST::Label: Eq + Clone,
{
    pub fn match_it<MM>(
        mut mapper: crate::matchers::Mapper<HAST, Dsrc, Ddst, M>,
    ) -> crate::matchers::Mapper<HAST, Dsrc, Ddst, M>
    where
        MM: MultiMappingStore<Src = M::Src, Dst = M::Dst> + Default,
    {
        mapper
            .mapping
            .mappings
            .topit(mapper.src_arena.len(), mapper.dst_arena.len());
        Self::execute::<MM>(&mut mapper);
        mapper
    }

    pub(crate) fn execute<MM: MultiMappingStore<Src = M::Src, Dst = M::Dst> + Default>(
        mapper: &mut Mapper<HAST, Dsrc, Ddst, M>,
    ) {
        let mut mm: MM = Default::default();
        mm.topit(mapper.src_arena.len(), mapper.dst_arena.len());
        SubtreeMatcher::<Mapper<HAST, Dsrc, Ddst, M>, MIN_HEIGHT>::matchh_to_be_filtered(
            mapper.hyperast,
            &mut mapper.mapping.src_arena,
            &mut mapper.mapping.dst_arena,
            &mut mm,
        );
        Self::filter_mappings(mapper, &mm);
    }

    fn filter_mappings<MM: MultiMappingStore<Src = M::Src, Dst = M::Dst>>(
        mapper: &mut Mapper<HAST, Dsrc, Ddst, M>,
        multi_mappings: &MM,
    ) {
        let ambiguous_list = Self::handle_non_ambiguous_mappings(mapper, multi_mappings);
        Self::handle_ambiguous_mappings(mapper, ambiguous_list);
    }

    /// Integrate unique mappings, while extracting the ambiguous ones.
    fn handle_non_ambiguous_mappings<MM: MultiMappingStore<Src = M::Src, Dst = M::Dst>>(
        mapper: &mut Mapper<HAST, Dsrc, Ddst, M>,
        multi_mappings: &MM,
    ) -> Vec<(M::Src, M::Dst)> {
        let mut ambiguous_list: Vec<(M::Src, M::Dst)> = vec![];
        let mut ignored = bitvec::bitbox![0;mapper.src_arena.len()];
        for src in multi_mappings.all_mapped_srcs() {
            let mut is_mapping_unique = false;
            if multi_mappings.is_src_unique(&src) {
                let dst = multi_mappings.get_dsts(&src)[0];
                if multi_mappings.is_dst_unique(&dst) {
                    mapper.add_mapping_recursively(&src, &dst); // TODO subtree opti, do not do explicitly
                    is_mapping_unique = true;
                }
            }

            if !(ignored[src.index()] || is_mapping_unique) {
                let adsts = multi_mappings.get_dsts(&src);
                let dst = multi_mappings.get_dsts(&src)[0];
                let asrcs = multi_mappings.get_srcs(&dst);
                for asrc in asrcs {
                    for adst in adsts {
                        ambiguous_list.push((*asrc, *adst));
                    }
                }
                asrcs.iter().for_each(|x| ignored.set(x.index(), true))
            }
        }
        ambiguous_list
    }

    // Integrate the best ambiguous mappings.
    ///
    /// Returns the rest.
    fn handle_ambiguous_mappings(
        mapper: &mut Mapper<HAST, Dsrc, Ddst, M>,
        mut ambiguous_list: Vec<(M::Src, M::Dst)>,
    ) -> Vec<(M::Src, M::Dst)> {
        log::trace!("ambiguous_mappings.len: {}", &ambiguous_list.len());
        ambiguous_list.sort_by(mapper.ambiguous_mappings_comparator());
        ambiguous_list
            .into_iter()
            .filter_map(|(src, dst)| {
                if !(mapper.mappings.is_src(&src) || mapper.mappings.is_dst(&dst)) {
                    mapper.add_mapping_recursively(&src, &dst);
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
        //     // let src_i = src.index();
        //     // let dst_i = dst.index();
        //     // if !(src_ignored[src_i] || dst_ignored[dst_i]) {
        //     if !(mapper.mappings.is_src(&src) || mapper.mappings.is_dst(&dst)) {
        //         mapper.add_mapping_recursively(&src, &dst);
        //         //     src_ignored.set(src_i, true);
        //         //     (mapper.src_arena.descendants(&src))
        //         //         .iter()
        //         //         .for_each(|src| src_ignored.set(src.index(), true));
        //         //     dst_ignored.set(dst_i, true);
        //         //     (mapper.dst_arena.descendants(&dst))
        //         //         .iter()
        //         //         .for_each(|dst| dst_ignored.set(dst.index(), true));
        //     }
        // }
    }
}

impl<
    Dsrc: DecompressedTreeStore<HAST, M::Src>
        + DecompressedWithParent<HAST, M::Src>
        + ContiguousDescendants<HAST, M::Src>,
    Ddst: DecompressedTreeStore<HAST, M::Dst>
        + DecompressedWithParent<HAST, M::Dst>
        + ContiguousDescendants<HAST, M::Dst>,
    HAST: HyperAST + Copy,
    M: MonoMappingStore,
> Mapper<HAST, Dsrc, Ddst, M>
where
    M::Src: PrimInt + Hash,
    M::Dst: PrimInt + Hash,
    for<'t> LendT<'t, HAST>: WithHashs,
    HAST::Label: Eq + Clone,
{
    fn ambiguous_mappings_comparator(
        &mut self,
    ) -> impl FnMut(&(M::Src, M::Dst), &(M::Src, M::Dst)) -> std::cmp::Ordering {
        let mut sib_sim = HashMap::<(M::Src, M::Dst), f64>::default();
        let mut psib_sim = sib_sim.clone();
        let mut p_in_p_sim = sib_sim.clone();
        move |a, b| {
            let mut sib_sim = cached(&mut sib_sim, |l| self.coef_sib(l));
            let mut psib_sim = cached(&mut psib_sim, |l| self.coef_parent(l));
            let mut p_in_p_sim = cached(&mut p_in_p_sim, |l| self.coef_pos_in_parent(l));
            if self.same_parents(a, b) {
                std::cmp::Ordering::Equal
            } else {
                sib_sim(a, b)
                    .reverse()
                    .then_with(|| psib_sim(a, b).reverse())
            }
            .then_with(|| p_in_p_sim(a, b))
            .then_with(|| self.compare_delta_pos(a, b))
        }
    }

    fn coef_sib(&self, l: &(M::Src, M::Dst)) -> f64 {
        let (p_src, p_dst) = self.parents(l);
        SimilarityMeasure::range(
            &self.src_arena.descendants_range(&p_src), //descendants
            &self.dst_arena.descendants_range(&p_dst),
            &self.mappings,
        )
        .dice()
    }

    fn parents(&self, l: &(M::Src, M::Dst)) -> (M::Src, M::Dst) {
        let p_src = self.src_arena.parent(&l.0).unwrap();
        let p_dst = self.dst_arena.parent(&l.1).unwrap();
        (p_src, p_dst)
    }

    fn coef_parent(&self, l: &(M::Src, M::Dst)) -> f64 {
        let s1: Vec<_> = Dsrc::parents(&self.src_arena, l.0).collect();
        let s2: Vec<_> = Ddst::parents(&self.dst_arena, l.1).collect();
        let common: Vec<(usize, usize)> = longest_common_subsequence(&s1, &s2, |a, b| {
            let (t, l) = {
                let o = self.src_arena.original(a);
                let n = self.hyperast.node_store().resolve(&o);
                (self.hyperast.resolve_type(&o), n.try_get_label().cloned())
            };
            let o = self.dst_arena.original(b);
            let n = self.hyperast.node_store().resolve(&o);
            t == self.hyperast.resolve_type(&o) && l.as_ref() == n.try_get_label()
        });
        (2 * common.len()).to_f64().unwrap() / (s1.len() + s2.len()).to_f64().unwrap()
    }

    fn coef_pos_in_parent(&self, l: &(M::Src, M::Dst)) -> f64 {
        let srcs = positions(&self.src_arena, l.0);
        let dsts = positions(&self.dst_arena, l.1);
        let squared_diff = |(src, dst)| (src - dst) * (src - dst);
        srcs.zip(dsts).map(squared_diff).sum::<f64>().sqrt()
    }

    fn same_parents(&self, a: &(M::Src, M::Dst), b: &(M::Src, M::Dst)) -> bool {
        let ap = self.mapping_parents(a);
        let bp = self.mapping_parents(b);
        ap.0 == bp.0 && ap.1 == bp.1
    }

    fn mapping_parents(&self, l: &(M::Src, M::Dst)) -> (Option<M::Src>, Option<M::Dst>) {
        (self.src_arena.parent(&l.0), self.dst_arena.parent(&l.1))
    }

    fn compare_delta_pos(&self, a: &(M::Src, M::Dst), b: &(M::Src, M::Dst)) -> std::cmp::Ordering {
        (a.0.index().abs_diff(a.1.index())).cmp(&b.0.index().abs_diff(b.1.index()))
    }
}

fn positions<D, I, HAST: HyperAST + Copy>(arena: &D, x: I) -> impl Iterator<Item = f64>
where
    D: crate::decompressed_tree_store::ShallowDecompressedTreeStore<HAST, I>
        + DecompressedWithParent<HAST, I>,
    I: PrimInt,
{
    Some(x).into_iter().chain(arena.parents(x)).filter_map(|x| {
        arena.parent(&x).map(|p| {
            let pos = arena.position_in_parent::<usize>(&x).unwrap();
            let len = arena.children(&p).len();
            pos.to_f64().unwrap() / len.to_f64().unwrap()
        })
    })
}

pub struct SubtreeMatcher<Mpr, const MIN_HEIGHT: usize> {
    _phantom: std::marker::PhantomData<*const Mpr>,
}

impl<
    Dsrc: DecompressedTreeStore<HAST, M::Src> + DecompressedWithParent<HAST, M::Src>,
    Ddst: DecompressedTreeStore<HAST, M::Dst> + DecompressedWithParent<HAST, M::Dst>,
    HAST,
    M: MonoMappingStore,
    const MIN_HEIGHT: usize,
> SubtreeMatcher<Mapper<HAST, Dsrc, Ddst, M>, MIN_HEIGHT>
where
    HAST: HyperAST + Copy,
    M::Src: PrimInt,
    M::Dst: PrimInt,
    for<'t> LendT<'t, HAST>: WithHashs,
    HAST::Label: Eq,
{
    fn pop_larger<'b>(
        src_trees: &mut PriorityTreeList<'b, Dsrc, M::Src, HAST, MIN_HEIGHT>,
        dst_trees: &mut PriorityTreeList<'b, Ddst, M::Dst, HAST, MIN_HEIGHT>,
    ) {
        if src_trees.peek_height() > dst_trees.peek_height() {
            src_trees.open();
        } else {
            dst_trees.open();
        }
    }

    fn matchh_to_be_filtered<MM: MultiMappingStore<Src = M::Src, Dst = M::Dst>>(
        hyperast: HAST,
        src_arena: &mut Dsrc,
        dst_arena: &mut Ddst,
        multi_mappings: &mut MM,
    ) {
        let mut src_trees =
            PriorityTreeList::<_, _, _, MIN_HEIGHT>::new(hyperast, src_arena, src_arena.root());
        let mut dst_trees =
            PriorityTreeList::<_, _, _, MIN_HEIGHT>::new(hyperast, dst_arena, dst_arena.root());
        while src_trees.peek_height() != -1 && dst_trees.peek_height() != -1 {
            while src_trees.peek_height() != dst_trees.peek_height() {
                Self::pop_larger(&mut src_trees, &mut dst_trees);
                // if src_trees.peek_height() == -1 || dst_trees.peek_height() == -1 {
                //     break;
                // }
            }

            let current_height_src_trees = src_trees.pop().unwrap();
            let current_height_dst_trees = dst_trees.pop().unwrap();

            let mut marks_for_src_trees = bitvec::bitbox![0;current_height_src_trees.len()];
            let mut marks_for_dst_trees = bitvec::bitbox![0;current_height_dst_trees.len()];

            for (i, src) in current_height_src_trees.iter().enumerate() {
                for (j, dst) in current_height_dst_trees.iter().enumerate() {
                    let iso = {
                        let src = src_trees.arena.original(&src);
                        let dst = dst_trees.arena.original(&dst);
                        super::isomorphic::<_, true, false>(hyperast, &src, &dst)
                    };
                    if iso {
                        multi_mappings.link(*src, *dst);
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

    #[allow(unused)] // alternative
    fn similarity(mapper: &Mapper<HAST, Dsrc, Ddst, M>, src: &M::Src, dst: &M::Dst) -> f64 {
        let p_src = mapper.src_arena.parent(src).unwrap();
        let p_dst = mapper.dst_arena.parent(dst).unwrap();
        let jaccard = crate::similarity_metrics::jaccard_similarity(
            &mapper.src_arena.descendants(&p_src),
            &mapper.dst_arena.descendants(&p_dst),
            &mapper.mappings,
        );
        let pos_src = if mapper.src_arena.has_parent(src) {
            zero()
        } else {
            mapper.src_arena.position_in_parent::<usize>(src).unwrap()
        };
        let pos_dst = if mapper.dst_arena.has_parent(dst) {
            zero()
        } else {
            mapper.dst_arena.position_in_parent(dst).unwrap()
        };

        let max_src_pos = if mapper.src_arena.has_parent(src) {
            one()
        } else {
            (mapper.hyperast.node_store())
                .resolve(&mapper.src_arena.original(&p_src))
                .child_count()
        };
        let max_dst_pos = if mapper.dst_arena.has_parent(dst) {
            one()
        } else {
            (mapper.hyperast.node_store())
                .resolve(&mapper.dst_arena.original(&p_dst))
                .child_count()
        };
        let max_pos_diff = std::cmp::max(max_src_pos, max_dst_pos);
        let pos: f64 = 1.0_f64
            - ((Ord::max(pos_src, pos_dst) - Ord::min(pos_dst, pos_src))
                .to_f64()
                .unwrap()
                / max_pos_diff.to_f64().unwrap());
        let po: f64 = 1.0_f64
            - ((Ord::max(src.index(), dst.index()) - Ord::min(dst.index(), src.index()))
                .to_f64()
                .unwrap()
                / Self::get_max_tree_size(mapper).to_f64().unwrap());
        100. * jaccard + 10. * pos + po
    }

    fn get_max_tree_size(mapper: &Mapper<HAST, Dsrc, Ddst, M>) -> usize {
        Ord::max(mapper.src_arena.len(), mapper.dst_arena.len())
    }
}

/// NOTE: use [`super::lazy_greedy_subtree_matcher::PriorityTreeList`] if WithStats is available
struct PriorityTreeList<'b, D, IdD, HAST, const MIN_HEIGHT: usize> {
    trees: Vec<Option<Vec<IdD>>>,

    store: HAST,
    arena: &'b D,

    max_height: usize,

    current_idx: isize,
}

impl<
    'a,
    D: DecompressedTreeStore<HAST, IdD>,
    IdD: PrimInt,
    HAST: HyperAST + Copy,
    const MIN_HEIGHT: usize,
> PriorityTreeList<'a, D, IdD, HAST, MIN_HEIGHT>
{
    pub(super) fn new(
        store: HAST,
        arena: &'a D,
        tree: IdD,
    ) -> PriorityTreeList<'a, D, IdD, HAST, MIN_HEIGHT> {
        let h = super::height(store.node_store(), &arena.original(&tree)); // TODO subtree opti, use metadata
        let list_size = if h >= MIN_HEIGHT {
            h + 1 - MIN_HEIGHT
        } else {
            0
        };
        let mut r = Self {
            trees: vec![None; list_size],
            store,
            arena,
            max_height: h,
            current_idx: if list_size == 0 { -1 } else { 0 },
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

    fn add_tree(&mut self, tree: IdD) {
        let h = super::height(self.store.node_store(), &self.arena.original(&tree));
        self.add_tree_aux(tree, h)
    }

    fn add_tree_aux(&mut self, tree: IdD, h: usize) {
        if h >= MIN_HEIGHT {
            let idx = self.idx(h);
            if self.trees[idx].is_none() {
                self.trees[idx] = Some(vec![]);
            };
            self.trees[idx].as_mut().unwrap().push(tree);
        }
    }

    pub(super) fn open(&mut self) -> Option<Vec<IdD>> {
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

    pub(super) fn pop(&mut self) -> Option<Vec<IdD>> {
        if self.current_idx < 0 {
            None
        } else {
            self.trees[self.current_idx as usize].take()
        }
    }

    pub(super) fn open_tree(&mut self, tree: &IdD) {
        for c in self.arena.children(tree) {
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

pub(super) fn cached<K: Copy + Eq + Hash, T: Copy + PartialOrd>(
    c: &mut HashMap<K, T>,
    f: impl Copy + Fn(&K) -> T,
) -> impl FnMut(&K, &K) -> std::cmp::Ordering {
    move |a, b| cached_compare(_cached(c, f), a, b)
}

pub(super) fn _cached<K: Copy + Eq + Hash, T: Copy>(
    c: &mut HashMap<K, T>,
    f: impl Fn(&K) -> T,
) -> impl FnMut(&K) -> T {
    move |l: &K| *c.entry(*l).or_insert_with(|| f(l))
}

pub(super) fn cached_compare<I, O: PartialOrd>(
    mut cached: impl FnMut(&I) -> O,
    a: &I,
    b: &I,
) -> std::cmp::Ordering {
    cached(a)
        .partial_cmp(&cached(b))
        .unwrap_or(std::cmp::Ordering::Equal)
}
