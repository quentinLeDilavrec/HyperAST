use hyperast::PrimInt;
use hyperast::types::NodeStore as _;
use hyperast::types::{HyperAST, Tree};

use crate::decompressed_tree_store::{DecompressedTreeStore, DecompressedWithParent};
use crate::mappings::MonoMappingStore;
use crate::matchers::Mapper;

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
    pub(in crate::matchers) fn get_dst_candidates(&self, src: &M::Src) -> Vec<M::Dst> {
        let seeds = (self.src_arena.descendants(src))
            .into_iter()
            .filter_map(|c| self.mappings.get_dst(&c))
            .collect::<Vec<_>>();
        let s = &self.src_arena.original(src);
        candidates_aux(&seeds, s, &self.mapping.dst_arena, self.hyperast, |x| {
            self.mapping.mappings.is_dst(x)
        })
    }
}

pub(super) fn candidates_aux<HAST: HyperAST + Copy, D, IdD>(
    seeds: &[IdD],
    s: &HAST::IdN,
    arena: &D,
    hyperast: HAST,
    is_mapped: impl Fn(&IdD) -> bool,
) -> Vec<IdD>
where
    D: DecompressedWithParent<HAST, IdD>,
    D: DecompressedTreeStore<HAST, IdD>,
    IdD: PrimInt + Eq,
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
            if p_type == t && !is_mapped(&parent) && parent != arena.root() {
                candidates.push(parent);
            }
            seed = parent;
        }
    }
    candidates
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
    /// look at all src descendants in mappings
    pub(super) fn are_srcs_unmapped(&self, src: &M::Src) -> bool {
        self.src_arena
            .descendants(src)
            .iter()
            .all(|x| !self.mappings.is_src(x))
    }
    /// look at all dst descendants in mappings
    pub(super) fn are_dsts_unmapped(&self, dst: &M::Dst) -> bool {
        self.dst_arena
            .descendants(dst)
            .iter()
            .all(|x| !self.mappings.is_dst(x))
    }

    /// look at any src descendants in mappings
    pub(super) fn has_unmapped_src_children(&self, src: &M::Src) -> bool {
        self.src_arena
            .descendants(src)
            .iter()
            .any(|x| !self.mappings.is_src(x))
    }

    /// look at any dst descendants in mappings
    pub(super) fn has_unmapped_dst_children(&self, dst: &M::Dst) -> bool {
        self.dst_arena
            .descendants(dst)
            .iter()
            .any(|x| !self.mappings.is_dst(x))
    }

    pub(crate) fn add_mapping_recursively(&mut self, src: &M::Src, dst: &M::Dst) {
        self.mappings.link(*src, *dst);
        (self.src_arena.descendants(src))
            .iter()
            .zip(self.dst_arena.descendants(dst).iter())
            .for_each(|(src, dst)| self.mappings.link(*src, *dst));
    }

    /// Return true if src has *any* children
    pub(in crate::matchers) fn src_has_children(&mut self, src: M::Src) -> bool {
        (self.hyperast.node_store())
            .resolve(&self.src_arena.original(&src))
            .has_children()
    }

    pub(crate) fn apply_mappings<MZs: MonoMappingStore<Src = M::Src, Dst = M::Dst>>(
        &mut self,
        src_offset: M::Src,
        dst_offset: M::Dst,
        mappings: MZs,
    ) {
        use num_traits::cast;
        for (i, t) in mappings.iter() {
            //remapping
            let src: M::Src = src_offset + cast(i).unwrap();
            let dst: M::Dst = dst_offset + cast(t).unwrap();
            // use it
            if !self.mappings.is_src(&src) && !self.mappings.is_dst(&dst) {
                let tsrc = self.hyperast.resolve_type(&self.src_arena.original(&src));
                let tdst = self.hyperast.resolve_type(&self.dst_arena.original(&dst));
                if tsrc == tdst {
                    self.mappings.link(src, dst);
                }
            }
        }
    }
}
