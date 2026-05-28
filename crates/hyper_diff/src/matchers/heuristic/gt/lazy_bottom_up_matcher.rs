use hyperast::PrimInt;
use hyperast::types::NodeStore as _;
use hyperast::types::{HyperAST, LendT, WithHashs};

use crate::decompressed_tree_store::LazyDecompressedTreeStore;
use crate::decompressed_tree_store::{DecompressedWithParent, DeepDecompressedTreeStore};
use crate::decompressed_tree_store::{Shallow, ShallowDecompressedTreeStore};
use crate::mappings::{MappingStore, MonoMappingStore};
use crate::matchers::Mapper;

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
    /// Returns true if *all* descendants in src are unmapped
    pub(super) fn are_srcs_unmapped_lazy(&self, src: &Dsrc::IdD) -> bool {
        self.src_arena
            .it_descendants(src)
            .all(|x| !self.mappings.is_src(&x))
    }

    /// Returns true if *all* descendants in dst are unmapped
    pub(super) fn are_dsts_unmapped_lazy(&self, dst: &Ddst::IdD) -> bool {
        self.dst_arena
            .it_descendants(dst)
            .all(|x| !self.mappings.is_dst(&x))
    }

    /// Returns true if *any* descendants in src are unmapped
    pub(super) fn has_unmapped_src_descendants_lazy(&self, src: &Dsrc::IdD) -> bool {
        self.src_arena
            .it_descendants(src)
            .any(|x| !self.mappings.is_src(&x))
    }

    /// Returns true if *any* descendants in dst are unmapped
    pub(super) fn has_unmapped_dst_descendants_lazy(&self, dst: &Ddst::IdD) -> bool {
        self.dst_arena
            .it_descendants(dst)
            .any(|x| !self.mappings.is_dst(&x))
    }

    pub fn src_has_children_lazy(&mut self, src: Dsrc::IdD) -> bool {
        use hyperast::types::Tree;
        self.hyperast
            .node_store()
            .resolve(&self.src_arena.original(&src))
            .has_children()
    }

    pub(crate) fn add_mapping_recursively_lazy(&mut self, src: &Dsrc::IdD, dst: &Ddst::IdD) {
        self.mappings.link(src.to_shallow(), dst.to_shallow());

        let src = self.mapping.src_arena.it_descendants(src);
        let dst = self.mapping.dst_arena.it_descendants(dst);
        src.zip(dst)
            .for_each(|(src, dst)| self.mapping.mappings.link(src, dst));
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
    HAST::Label: Eq,
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
                    mappings.link(src.to_shallow(), dst.to_shallow());
                }
            }
        }
    }
}

/// shared between sides of candidates selection
pub(super) fn candidates_aux<HAST: HyperAST + Copy, D, IdS>(
    seeds: impl Iterator<Item = IdS>,
    s: &HAST::IdN,
    arena: &mut D, // need mutable access to call decompress_to
    hyperast: HAST,
    is_mapped: impl Fn(&IdS) -> bool,
) -> Vec<D::IdD>
where
    D: DecompressedWithParent<HAST, D::IdD>,
    D: ShallowDecompressedTreeStore<HAST, IdS>,
    D: LazyDecompressedTreeStore<HAST, IdS>,
    D::IdD: PrimInt + Shallow<IdS>,
    IdS: Eq,
{
    let mut candidates = vec![];
    let mut visited = bitvec::bitbox![0;arena.len()];
    let t = hyperast.resolve_type(s);
    for seed in seeds {
        let mut seed = arena.decompress_to(&seed);
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
