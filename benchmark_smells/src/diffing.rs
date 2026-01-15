use hyper_diff::decompressed_tree_store::ShallowDecompressedTreeStore;
use hyper_diff::decompressed_tree_store::lazy_post_order::LazyPostOrder;
use hyper_diff::matchers::heuristic::gt::lazy_greedy_subtree_matcher::LazyGreedySubtreeMatcher;
use hyper_diff::matchers::mapping_store::{DefaultMultiMappingStore, MappingStore, VecStore};
use hyper_diff::matchers::{Decompressible, Mapper, Mapping};
use hyperast::types::{self, HyperAST, NodeId};
use std::fmt::Debug;

type IdD = u32;

#[allow(type_alias_bounds)]
type LazyVecMapper<'a, HAST: HyperAST, IdD> = Mapper<
    HAST,
    Decompressible<HAST, &'a mut LazyPostOrder<HAST::IdN, IdD>>,
    Decompressible<HAST, &'a mut LazyPostOrder<HAST::IdN, IdD>>,
    VecStore<IdD>,
>;

fn _top_down<HAST: HyperAST + Copy>(mapper: &mut LazyVecMapper<'_, HAST, IdD>)
where
    HAST::IdN: Clone + Debug + Eq,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
    HAST::Label: Clone + Copy + Eq + Debug,
    HAST::Idx: Debug,
    for<'t> <HAST as hyperast::types::AstLending<'t>>::RT: types::WithHashs + types::WithStats,
{
    let mm =
        LazyGreedySubtreeMatcher::<_>::compute_multi_mapping::<DefaultMultiMappingStore<_>>(mapper);
    LazyGreedySubtreeMatcher::<_>::filter_mappings(mapper, &mm);
}

pub fn top_down<'a, HAST: HyperAST + Copy>(
    hyperast: HAST,
    src_arena: &'a mut LazyPostOrder<HAST::IdN, IdD>,
    dst_arena: &'a mut LazyPostOrder<HAST::IdN, IdD>,
) -> LazyVecMapper<'a, HAST, IdD>
where
    HAST::IdN: Clone + Debug + Eq,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
    HAST::Label: Clone + Copy + Eq + Debug,
    HAST::Idx: Debug,
    for<'t> <HAST as hyperast::types::AstLending<'t>>::RT: types::WithHashs + types::WithStats,
{
    let mappings = VecStore::<u32>::default();
    let src_arena = Decompressible {
        hyperast,
        decomp: src_arena,
    };
    let dst_arena = Decompressible {
        hyperast,
        decomp: dst_arena,
    };
    let mut mapper = Mapper {
        hyperast,
        mapping: Mapping {
            src_arena,
            dst_arena,
            mappings,
        },
    };
    mapper.mapping.mappings.topit(
        mapper.mapping.src_arena.len(),
        mapper.mapping.dst_arena.len(),
    );
    _top_down(&mut mapper);
    mapper
}
