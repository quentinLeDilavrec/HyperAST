use crate::decompressed_tree_store::{
    ContiguousDescendants, DecompressedTreeStore, DecompressedWithParent,
};
use crate::matchers::mapping_store::{MonoMappingStore, MultiMappingStore};
use crate::matchers::{Mapper, similarity_metrics};
use crate::utils::sequence_algorithms::longest_common_subsequence;
use hyperast::PrimInt;
use hyperast::compat::HashMap;
use hyperast::types::{
    Childrn, DecompressedFrom, HashKind, HyperAST, Labeled, LendT, NodeId, NodeStore, Tree,
    WithChildren, WithHashs,
};
use num_traits::{ToPrimitive, one, zero};
use std::hash::Hash;

pub struct GreedySubtreeMatcher<Dsrc, Ddst, HAST, M, const MIN_HEIGHT: usize = 1> {
    mapper: SubtreeMatcher<Dsrc, Ddst, HAST, M, MIN_HEIGHT>,
}

impl<
    Dsrc: DecompressedTreeStore<HAST, M::Src>
        + DecompressedWithParent<HAST, M::Src>
        + DecompressedFrom<HAST, Out = Dsrc>
        + ContiguousDescendants<HAST, M::Src>,
    Ddst: DecompressedTreeStore<HAST, M::Dst>
        + DecompressedWithParent<HAST, M::Dst>
        + DecompressedFrom<HAST, Out = Ddst>
        + ContiguousDescendants<HAST, M::Dst>,
    HAST: HyperAST + Copy,
    M: MonoMappingStore,
    const MIN_HEIGHT: usize, // = 2
> GreedySubtreeMatcher<Dsrc, Ddst, HAST, M, MIN_HEIGHT>
where
    M::Src: PrimInt + Hash,
    M::Dst: PrimInt + Hash,
    for<'t> LendT<'t, HAST>: WithHashs,
    HAST::Label: Eq + Clone,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
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
        SubtreeMatcher::<Dsrc, Ddst, HAST, M, MIN_HEIGHT>::matchh_to_be_filtered(mapper, &mut mm);
        Self::filter_mappings(mapper, &mm);
    }

    fn filter_mappings<MM: MultiMappingStore<Src = M::Src, Dst = M::Dst>>(
        mapper: &mut Mapper<HAST, Dsrc, Ddst, M>,
        multi_mappings: &MM,
    ) {
        // Select unique mappings first and extract ambiguous mappings.
        let mut ambiguous_list: Vec<(M::Src, M::Dst)> = vec![];
        let mut ignored = bitvec::bitbox![0;mapper.src_arena.len()];
        let mut src_ignored = bitvec::bitbox![0;mapper.src_arena.len()];
        let mut dst_ignored = bitvec::bitbox![0;mapper.dst_arena.len()];
        for src in multi_mappings.all_mapped_srcs() {
            let mut is_mapping_unique = false;
            if multi_mappings.is_src_unique(&src) {
                let dst = multi_mappings.get_dsts(&src)[0];
                if multi_mappings.is_dst_unique(&dst) {
                    mapper.add_mapping_recursively(&src, &dst); // TODO subtree opti, do not do explicitly
                    is_mapping_unique = true;
                }
            }

            if !(ignored[src.to_usize().unwrap()] || is_mapping_unique) {
                let adsts = multi_mappings.get_dsts(&src);
                let asrcs = multi_mappings.get_srcs(&multi_mappings.get_dsts(&src)[0]);
                for asrc in asrcs {
                    for adst in adsts {
                        ambiguous_list.push((*asrc, *adst));
                    }
                }
                asrcs
                    .iter()
                    .for_each(|x| ignored.set(x.to_usize().unwrap(), true))
            }
        }

        let mapping_list: Vec<_> = mapper.sort(ambiguous_list).collect();

        // Select the best ambiguous mappings
        for (src, dst) in mapping_list {
            let src_i = src.to_usize().unwrap();
            let dst_i = dst.to_usize().unwrap();
            if !(src_ignored[src_i] || dst_ignored[dst_i]) {
                mapper.add_mapping_recursively(&src, &dst);
                src_ignored.set(src_i, true);
                mapper
                    .src_arena
                    .descendants(&src)
                    .iter()
                    .for_each(|src| src_ignored.set(src.to_usize().unwrap(), true));
                dst_ignored.set(dst_i, true);
                mapper
                    .dst_arena
                    .descendants(&dst)
                    .iter()
                    .for_each(|dst| dst_ignored.set(dst.to_usize().unwrap(), true));
            }
        }
    }
}

impl<
    Dsrc: DecompressedTreeStore<HAST, M::Src>
        + DecompressedWithParent<HAST, M::Src>
        + DecompressedFrom<HAST, Out = Dsrc>
        + ContiguousDescendants<HAST, M::Src>,
    Ddst: DecompressedTreeStore<HAST, M::Dst>
        + DecompressedWithParent<HAST, M::Dst>
        + DecompressedFrom<HAST, Out = Ddst>
        + ContiguousDescendants<HAST, M::Dst>,
    HAST: HyperAST + Copy,
    M: MonoMappingStore,
> Mapper<HAST, Dsrc, Ddst, M>
where
    M::Src: PrimInt + Hash,
    M::Dst: PrimInt + Hash,
    for<'t> LendT<'t, HAST>: WithHashs,
    HAST::Label: Eq + Clone,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
{
    fn sort(
        &mut self,
        mut ambiguous_mappings: Vec<(M::Src, M::Dst)>,
    ) -> impl Iterator<Item = (M::Src, M::Dst)> {
        let mut sib_sim = HashMap::<(M::Src, M::Dst), f64>::default();
        let mut psib_sim = HashMap::<(M::Src, M::Dst), f64>::default();
        let mut p_in_p_sim = HashMap::<(M::Src, M::Dst), f64>::default();
        log::trace!("ambiguous_mappings.len: {}", &ambiguous_mappings.len());
        ambiguous_mappings.sort_by(|a, b| {
            let cached_coef_sib =
                |l: &(M::Src, M::Dst)| *sib_sim.entry(*l).or_insert_with(|| self.coef_sib(l));
            let cached_coef_parent =
                |l: &(M::Src, M::Dst)| *psib_sim.entry(*l).or_insert_with(|| self.coef_parent(l));
            let (alink, blink) = (a, b);
            if self.same_parents(alink, blink) {
                std::cmp::Ordering::Equal
            } else {
                self.cached_compare(cached_coef_sib, a, b)
                    .reverse()
                    .then_with(|| self.cached_compare(cached_coef_parent, a, b).reverse())
            }
            .then_with(|| {
                self.cached_compare(
                    |l: &(M::Src, M::Dst)| {
                        *p_in_p_sim
                            .entry(*l)
                            .or_insert_with(|| self.coef_pos_in_parent(l))
                    },
                    a,
                    b,
                )
            })
            .then_with(|| self.compare_delta_pos(alink, blink))
        });
        ambiguous_mappings.into_iter()
    }

    fn cached_compare<I, F: FnMut(&I) -> O, O: PartialOrd>(
        &self,
        mut cached: F,
        a: &I,
        b: &I,
    ) -> std::cmp::Ordering {
        cached(a)
            .partial_cmp(&cached(b))
            .unwrap_or(std::cmp::Ordering::Equal)
    }

    fn coef_sib(&self, l: &(M::Src, M::Dst)) -> f64 {
        let (p_src, p_dst) = self.parents(l);
        similarity_metrics::SimilarityMeasure::range(
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
        let common = longest_common_subsequence::<_, _, usize, _>(&s1, &s2, |a, b| {
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
        let srcs = vec![l.0]
            .into_iter()
            .chain(self.src_arena.parents(l.0))
            .filter_map(|x| {
                self.src_arena.parent(&x).map(|p| {
                    self.src_arena
                        .position_in_parent::<usize>(&x)
                        .unwrap()
                        .to_f64()
                        .unwrap()
                        / self.src_arena.children(&p).len().to_f64().unwrap()
                })
            });
        let dsts = vec![l.1]
            .into_iter()
            .chain(self.dst_arena.parents(l.1))
            .filter_map(|x| {
                self.dst_arena.parent(&x).map(|p| {
                    self.dst_arena
                        .position_in_parent::<usize>(&x)
                        .unwrap()
                        .to_f64()
                        .unwrap()
                        / self.dst_arena.children(&p).len().to_f64().unwrap()
                })
            });
        srcs.zip(dsts)
            .map(|(src, dst)| (src - dst) * (src - dst))
            .sum::<f64>()
            .sqrt()
    }

    fn same_parents(&self, alink: &(M::Src, M::Dst), blink: &(M::Src, M::Dst)) -> bool {
        let ap = self.mapping_parents(alink);
        let bp = self.mapping_parents(blink);
        ap.0 == bp.0 && ap.1 == bp.1
    }

    fn mapping_parents(&self, l: &(M::Src, M::Dst)) -> (Option<M::Src>, Option<M::Dst>) {
        (self.src_arena.parent(&l.0), self.dst_arena.parent(&l.1))
    }

    fn compare_delta_pos(
        &self,
        alink: &(M::Src, M::Dst),
        blink: &(M::Src, M::Dst),
    ) -> std::cmp::Ordering {
        (alink
            .0
            .to_usize()
            .unwrap()
            .abs_diff(alink.1.to_usize().unwrap()))
        .cmp(
            &blink
                .0
                .to_usize()
                .unwrap()
                .abs_diff(blink.1.to_usize().unwrap()),
        )
    }
}

pub struct SubtreeMatcher<Dsrc, Ddst, HAST, M, const MIN_HEIGHT: usize> {
    pub(crate) mapper: Mapper<HAST, Dsrc, Ddst, M>,
}

impl<
    'a,
    Dsrc: DecompressedTreeStore<HAST, M::Src> + DecompressedWithParent<HAST, M::Src>,
    Ddst: DecompressedTreeStore<HAST, M::Dst> + DecompressedWithParent<HAST, M::Dst>,
    HAST,
    M: MonoMappingStore,
    const MIN_HEIGHT: usize,
> SubtreeMatcher<Dsrc, Ddst, HAST, M, MIN_HEIGHT>
where
    HAST: HyperAST + Copy,
    M::Src: PrimInt,
    M::Dst: PrimInt,
    for<'t> LendT<'t, HAST>: WithHashs,
    HAST::Label: Eq,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
{
    pub(crate) fn add_mapping_recursively(
        mapper: &mut Mapper<HAST, Dsrc, Ddst, M>,
        src: &M::Src,
        dst: &M::Dst,
    ) {
        mapper.mappings.link(*src, *dst);
        mapper
            .src_arena
            .descendants(src)
            .iter()
            .zip(mapper.dst_arena.descendants(dst).iter())
            .for_each(|(src, dst)| mapper.mappings.link(*src, *dst));
    }

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
        mapper: &mut Mapper<HAST, Dsrc, Ddst, M>,
        multi_mappings: &mut MM,
    ) {
        let mut src_trees = PriorityTreeList::<_, _, HAST, MIN_HEIGHT>::new(
            mapper.hyperast,
            &mapper.src_arena,
            mapper.src_arena.root(),
        );
        let mut dst_trees = PriorityTreeList::<_, _, HAST, MIN_HEIGHT>::new(
            mapper.hyperast,
            &mapper.dst_arena,
            mapper.dst_arena.root(),
        );
        // let mut aaa = 0;
        while src_trees.peek_height() != -1 && dst_trees.peek_height() != -1 {
            // aaa += 1;
            // println!("multi_mappings={}", multi_mappings.len());
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
            // println!(
            //     "{aaa} marks={},{}",
            //     marks_for_src_trees.len(),
            //     marks_for_dst_trees.len()
            // );

            for i in 0..current_height_src_trees.len() {
                for j in 0..current_height_dst_trees.len() {
                    let src = current_height_src_trees[i];
                    let dst = current_height_dst_trees[j];
                    if Self::isomorphic(mapper, &src, &dst) {
                        // println!("isomorphic={},{}", i, j);
                        // println!(
                        //     "children={},{}",
                        //     self.node_store
                        //         .resolve(&self.src_arena.original(&src))
                        //         .try_get_children()
                        //         .map_or(0, |x| x.len()),
                        //     self.node_store
                        //         .resolve(&self.dst_arena.original(&dst))
                        //         .try_get_children()
                        //         .map_or(0, |x| x.len())
                        // );
                        // println!("id={:?},{:?}", src, dst);
                        multi_mappings.link(src, dst);
                        marks_for_src_trees.set(i, true);
                        marks_for_dst_trees.set(j, true);
                    }
                }
            }
            // println!("multi_mappings'={}", multi_mappings.len());
            for i in 0..marks_for_src_trees.len() {
                if !marks_for_src_trees[i] {
                    src_trees.open_tree(&current_height_src_trees[i]);
                }
            }
            // println!("multi_mappings''={}", multi_mappings.len());
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
        let jaccard = similarity_metrics::jaccard_similarity(
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
            - ((Ord::max(src.to_usize().unwrap(), dst.to_usize().unwrap())
                - Ord::min(dst.to_usize().unwrap(), src.to_usize().unwrap()))
            .to_f64()
            .unwrap()
                / Self::get_max_tree_size(mapper).to_f64().unwrap());
        100. * jaccard + 10. * pos + po
    }

    fn get_max_tree_size(mapper: &Mapper<HAST, Dsrc, Ddst, M>) -> usize {
        Ord::max(mapper.src_arena.len(), mapper.dst_arena.len())
    }

    pub(crate) fn isomorphic(
        mapper: &Mapper<HAST, Dsrc, Ddst, M>,
        src: &M::Src,
        dst: &M::Dst,
    ) -> bool {
        let src = mapper.src_arena.original(src);
        let dst = mapper.dst_arena.original(dst);

        Self::isomorphic_aux::<true>(mapper, &src, &dst)
    }

    /// if H then test the hash otherwise do not test it,
    /// considering hash colisions testing it should only be useful once.
    pub(crate) fn isomorphic_aux<const H: bool>(
        mapper: &Mapper<HAST, Dsrc, Ddst, M>,
        src: &HAST::IdN,
        dst: &HAST::IdN,
    ) -> bool {
        if src == dst {
            return true;
        }
        let _src = mapper.hyperast.node_store().resolve(src);
        let src_h = if H {
            Some(WithHashs::hash(&_src, &HashKind::label()))
        } else {
            None
        };
        // let src_t = src.get_type();
        let src_t = mapper.hyperast.resolve_type(src);
        let src_l = if _src.has_label() {
            Some(_src.get_label_unchecked())
        } else {
            None
        };
        let src_c: Option<Vec<_>> = _src.children().map(|x| x.iter_children().collect());

        let _dst = mapper.hyperast.node_store().resolve(dst);

        if let Some(src_h) = src_h {
            let dst_h = WithHashs::hash(&_dst, &HashKind::label());
            if src_h != dst_h {
                return false;
            }
        }
        // let dst_t = dst.get_type();
        let dst_t = mapper.hyperast.resolve_type(dst);
        if src_t != dst_t {
            return false;
        }
        if _dst.has_label() && (src_l.is_none() || src_l.unwrap() != _dst.get_label_unchecked()) {
            return false;
        };

        let dst_c: Option<Vec<_>> = _dst.children().map(|x| x.iter_children().collect());

        match (src_c, dst_c) {
            (None, None) => true,
            (Some(src_c), Some(dst_c)) => {
                if src_c.len() != dst_c.len() {
                    false
                } else {
                    for (src, dst) in src_c.iter().zip(dst_c.iter()) {
                        if !Self::isomorphic_aux::<false>(mapper, src, dst) {
                            return false;
                        }
                    }
                    true
                }
            }
            _ => false,
        }
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
    'b,
    D: DecompressedTreeStore<HAST, IdD>,
    IdD: PrimInt,
    HAST: HyperAST + Copy,
    const MIN_HEIGHT: usize,
> PriorityTreeList<'b, D, IdD, HAST, MIN_HEIGHT>
where
    HAST::IdN: NodeId<IdN = HAST::IdN>,
{
    pub(super) fn new(store: HAST, arena: &'b D, tree: IdD) -> Self {
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
