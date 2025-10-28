use std::fmt::Debug;

use hyper_diff::{decompressed_tree_store::ShallowDecompressedTreeStore, matchers::Mapper};
use hyperast::types::{self, HyperAST};

use hyper_diff::decompressed_tree_store::hidding_wrapper;
use hyper_diff::decompressed_tree_store::lazy_post_order::LazyPostOrder;
use hyper_diff::matchers::heuristic::gt::lazy_greedy_bottom_up_matcher::LazyGreedyBottomUpMatcher;
pub use hyper_diff::matchers::heuristic::gt::lazy_greedy_subtree_matcher::LazyGreedySubtreeMatcher;
use hyper_diff::matchers::heuristic::gt::lazy_hybrid_bottom_up_matcher::LazyHybridBottomUpMatcher;
use hyper_diff::matchers::mapping_store::DefaultMultiMappingStore;
use hyper_diff::matchers::mapping_store::MappingStore;
use hyper_diff::matchers::mapping_store::VecStore;
use hyper_diff::matchers::{Decompressible, Mapping};

pub fn top_down<HAST: HyperAST + Copy>(
    hyperast: HAST,
    src_arena: &mut LazyPostOrder<HAST::IdN, u32>,
    dst_arena: &mut LazyPostOrder<HAST::IdN, u32>,
) -> DefaultMultiMappingStore<u32>
where
    HAST::IdN: Clone + Debug + Eq,
    HAST::Label: Clone + Copy + Eq + Debug,
    HAST::Idx: Debug,
    for<'t> <HAST as hyperast::types::AstLending<'t>>::RT: types::WithStats,
    for<'t> <HAST as hyperast::types::AstLending<'t>>::RT: types::WithHashs + types::WithStats,
    HAST::IdN: types::NodeId<IdN = HAST::IdN>,
{
    let mut mm: DefaultMultiMappingStore<_> = Default::default();
    let src_arena = &mut Decompressible {
        hyperast,
        decomp: src_arena,
    };
    let dst_arena = &mut Decompressible {
        hyperast,
        decomp: dst_arena,
    };
    mm.topit(src_arena.len(), dst_arena.len());
    Mapper::<_, _, _, VecStore<u32>>::compute_multimapping::<_, 1>(
        hyperast, src_arena, dst_arena, &mut mm,
    );
    mm
}

pub fn full<HAST: HyperAST + Copy>(
    hyperast: HAST,
    mapper: &mut Mapper<
        HAST,
        Decompressible<HAST, &mut LazyPostOrder<HAST::IdN, u32>>,
        Decompressible<HAST, &mut LazyPostOrder<HAST::IdN, u32>>,
        VecStore<u32>,
    >,
) where
    HAST::IdN: Clone + Debug + Eq,
    HAST::Label: Clone + Copy + Eq + Debug,
    HAST::Idx: Debug,
    for<'t> <HAST as hyperast::types::AstLending<'t>>::RT: types::WithStats,
    for<'t> <HAST as hyperast::types::AstLending<'t>>::RT: types::WithHashs + types::WithStats,
    HAST::IdN: types::NodeId<IdN = HAST::IdN>,
{
    let mm =
        LazyGreedySubtreeMatcher::<_>::compute_multi_mapping::<DefaultMultiMappingStore<_>>(mapper);
    LazyGreedySubtreeMatcher::<_>::filter_mappings(mapper, &mm);
    LazyGreedyBottomUpMatcher::<_, VecStore<_>>::execute(mapper);
}

pub fn bottom_up_hiding<'a, 'b, 's: 'a, HAST: 's + HyperAST + Copy>(
    hyperast: HAST,
    mm: &hyper_diff::matchers::mapping_store::MultiVecStore<u32>,
    mapper: &'b mut Mapper<
        HAST,
        Decompressible<HAST, &'a mut LazyPostOrder<HAST::IdN, u32>>,
        Decompressible<HAST, &'a mut LazyPostOrder<HAST::IdN, u32>>,
        VecStore<u32>,
    >,
) where
    HAST::IdN: Clone + Debug + Eq,
    HAST::Label: Clone + Copy + Eq + Debug,
    HAST::Idx: Debug,
    for<'t> <HAST as hyperast::types::AstLending<'t>>::RT: types::WithStats,
    for<'t> <HAST as hyperast::types::AstLending<'t>>::RT: types::WithHashs + types::WithStats,
    HAST::IdN: types::NodeId<IdN = HAST::IdN>,
{
    LazyGreedySubtreeMatcher::<_>::filter_mappings(mapper, mm);
    use hidding_wrapper::*;

    // # hide matched subtrees
    // from right to left map unmatched nodes in a simple vec,
    let (map_src, rev_src) = hiding_map(
        &mapper.mapping.src_arena.decomp,
        &mapper.mapping.mappings.src_to_dst,
    );
    let (map_dst, rev_dst) = hiding_map(
        &mapper.mapping.dst_arena.decomp,
        &mapper.mapping.mappings.dst_to_src,
    );
    // a simple arithmetic op allow to still have nodes in post order where root() == len() - 1
    {
        let (src_arena, dst_arena, mappings) = hide(
            &mut mapper.mapping.src_arena,
            &map_src,
            &rev_src,
            &mut mapper.mapping.dst_arena,
            &map_dst,
            &rev_dst,
            &mut mapper.mapping.mappings,
        );
        // also wrap mappings (needed because bottom up matcher reads it)
        // then do the bottomup mapping (need another mapper)
        let mut mapper = Mapper {
            hyperast: mapper.hyperast,
            mapping: Mapping {
                src_arena,
                dst_arena,
                mappings,
            },
        };
        LazyGreedyBottomUpMatcher::<_, VecStore<_>, 200, 1, 2>::execute(&mut mapper);
        // GreedyBottomUpMatcher::<_, VecStore<_>, 1000, 1, 100>::execute(
        //     &mut mapper,
        //     hyperast.label_store(),
        // );
    }
}

pub fn bottom_up<'store, 'a, 'b, HAST: HyperAST + Copy>(
    hyperast: HAST,
    mm: &hyper_diff::matchers::mapping_store::MultiVecStore<u32>,
    mapper: &'b mut Mapper<
        HAST,
        Decompressible<HAST, &'a mut LazyPostOrder<HAST::IdN, u32>>,
        Decompressible<HAST, &'a mut LazyPostOrder<HAST::IdN, u32>>,
        VecStore<u32>,
    >,
) where
    HAST::IdN: Clone + Debug + Eq,
    HAST::Label: Clone + Copy + Eq + Debug,
    HAST::Idx: Debug,
    for<'t> <HAST as hyperast::types::AstLending<'t>>::RT: types::WithStats,
    for<'t> <HAST as hyperast::types::AstLending<'t>>::RT:
        'store + types::WithHashs + types::WithStats,
    HAST::IdN: types::NodeId<IdN = HAST::IdN>,
{
    LazyGreedySubtreeMatcher::<_>::filter_mappings(mapper, mm);

    LazyGreedyBottomUpMatcher::<_, VecStore<_>>::execute(mapper);
}

pub fn leveraging_method_headers<'store, 'a, 'b, HAST: HyperAST + Copy>(
    hyperast: HAST,
    mapper: &'b mut Mapper<
        HAST,
        Decompressible<HAST, &'a mut LazyPostOrder<HAST::IdN, u32>>,
        Decompressible<HAST, &'a mut LazyPostOrder<HAST::IdN, u32>>,
        VecStore<u32>,
    >,
) where
    HAST::IdN: Clone + Debug + Eq,
    HAST::Label: Clone + Copy + Eq + Debug,
    HAST::Idx: Debug,
    for<'t> <HAST as hyperast::types::AstLending<'t>>::RT: types::WithStats,
    for<'t> <HAST as hyperast::types::AstLending<'t>>::RT:
        'store + types::WithHashs + types::WithStats,
    HAST::IdN: types::NodeId<IdN = HAST::IdN>,
{
    LazyGreedyBottomUpMatcher::<_, VecStore<_>, 2000, 1, 100>::execute(mapper);
}

pub fn full2<'a, 'b, 's: 'a, HAST: 's + HyperAST + Copy>(
    mapper: &'b mut Mapper<
        HAST,
        Decompressible<HAST, &'a mut LazyPostOrder<HAST::IdN, u32>>,
        Decompressible<HAST, &'a mut LazyPostOrder<HAST::IdN, u32>>,
        VecStore<u32>,
    >,
) where
    HAST::IdN: Clone + Debug + Eq,
    HAST::Label: Clone + Copy + Eq + Debug,
    HAST::Idx: Debug,
    for<'t> <HAST as hyperast::types::AstLending<'t>>::RT: types::WithStats,
    for<'t> <HAST as hyperast::types::AstLending<'t>>::RT: types::WithHashs + types::WithStats,
    HAST::IdN: types::NodeId<IdN = HAST::IdN>,
{
    let mut mm: DefaultMultiMappingStore<_> = Default::default();
    mm.topit(mapper.src_arena.len(), mapper.dst_arena.len());
    let now = std::time::Instant::now();
    Mapper::<HAST, _, _, VecStore<u32>>::compute_multimapping::<_, 1>(
        mapper.hyperast,
        &mut mapper.mapping.src_arena,
        &mut mapper.mapping.dst_arena,
        &mut mm,
    );
    let compute_multimapping_t = now.elapsed().as_secs_f64();
    dbg!(compute_multimapping_t);
    let now = std::time::Instant::now();
    bottom_up_hiding(mapper.hyperast, &mm, mapper);
    let bottom_up_hiding_t = now.elapsed().as_secs_f64();
    dbg!(bottom_up_hiding_t);
}

pub fn full2_gumtree_simple<'a, 'b, 's: 'a, HAST: 's + HyperAST + Copy>(
    mapper: &'b mut Mapper<
        HAST,
        Decompressible<HAST, &'a mut LazyPostOrder<HAST::IdN, u32>>,
        Decompressible<HAST, &'a mut LazyPostOrder<HAST::IdN, u32>>,
        VecStore<u32>,
    >,
) where
    HAST::IdN: Clone + Debug + Eq,
    HAST::Label: Clone + Copy + Eq + Debug,
    HAST::Idx: Debug,
    for<'t> <HAST as hyperast::types::AstLending<'t>>::RT: types::WithStats,
    for<'t> <HAST as hyperast::types::AstLending<'t>>::RT: types::WithHashs + types::WithStats,
    HAST::IdN: types::NodeId<IdN = HAST::IdN>,
{
    let mut mm: DefaultMultiMappingStore<_> = Default::default();
    mm.topit(mapper.src_arena.len(), mapper.dst_arena.len());
    let now = std::time::Instant::now();
    Mapper::<HAST, _, _, VecStore<u32>>::compute_multimapping::<_, 3>(
        mapper.hyperast,
        &mut mapper.mapping.src_arena,
        &mut mapper.mapping.dst_arena,
        &mut mm,
    );
    let compute_multimapping_t = now.elapsed().as_secs_f64();
    LazyGreedySubtreeMatcher::<_>::filter_mappings(mapper, &mm);
    dbg!(compute_multimapping_t);
    dbg!(mapper.mapping.mappings.len());
    let now = std::time::Instant::now();

    LazyHybridBottomUpMatcher::<_, VecStore<_>, 200>::execute(mapper);

    // bottom_up_hiding(mapper.hyperast, &mm, mapper);
    let bottom_up_simple_t = now.elapsed().as_secs_f64();
    dbg!(bottom_up_simple_t);
    dbg!(mapper.mapping.mappings.len());
}

pub fn full3<'store, 'a, 'b, HAST: HyperAST + Copy>(
    hyperast: HAST,
    mapper: &'b mut Mapper<
        HAST,
        Decompressible<HAST, &'a mut LazyPostOrder<HAST::IdN, u32>>,
        Decompressible<HAST, &'a mut LazyPostOrder<HAST::IdN, u32>>,
        VecStore<u32>,
    >,
) where
    HAST::IdN: Clone + Debug + Eq,
    HAST::Label: Clone + Copy + Eq + Debug,
    HAST::Idx: Debug,
    for<'t> <HAST as hyperast::types::AstLending<'t>>::RT: types::WithStats,
    for<'t> <HAST as hyperast::types::AstLending<'t>>::RT:
        'store + types::WithHashs + types::WithStats,
    HAST::IdN: types::NodeId<IdN = HAST::IdN>,
{
    let mut mm: DefaultMultiMappingStore<_> = Default::default();
    mm.topit(mapper.src_arena.len(), mapper.dst_arena.len());
    let now = std::time::Instant::now();
    Mapper::<HAST, _, _, VecStore<u32>>::compute_multimapping::<_, 1>(
        mapper.hyperast,
        &mut mapper.mapping.src_arena,
        &mut mapper.mapping.dst_arena,
        &mut mm,
    );
    let compute_multimapping_t = now.elapsed().as_secs_f64();
    dbg!(compute_multimapping_t);
    let now = std::time::Instant::now();
    bottom_up(hyperast, &mm, mapper);
    let bottom_up_t = now.elapsed().as_secs_f64();
    dbg!(bottom_up_t);
}
