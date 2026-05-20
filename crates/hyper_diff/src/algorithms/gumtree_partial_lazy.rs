use std::fmt::Debug;

use hyperast::PrimInt;
use hyperast::types::TypeStore;
use hyperast::types::UniformNodeId;
use hyperast::types::{HyperAST, LendT};
use hyperast::types::{WithHashs, WithStats};

use super::{CDS, DS, DiffRes, DiffResult, tr};
use crate::actions::script_generator2::ScriptGenerator;
use crate::decompressed_tree_store::bfs_wrapper::SimpleBfsMapper;
use crate::mappings::{DefaultMultiMappingStore, MappingStore, VecStore};
use crate::matchers::Mapper;
use crate::matchers::heuristic::gt::greedy_bottom_up_matcher::GreedyBottomUpMatcher;
use crate::matchers::heuristic::gt::lazy_greedy_subtree_matcher::LazyGreedySubtreeMatcher;

type M = VecStore<u32>;
type MM = DefaultMultiMappingStore<u32>;

pub fn diff<HAST: HyperAST + Copy>(
    hyperast: HAST,
    src: &HAST::IdN,
    dst: &HAST::IdN,
) -> DiffRes<HAST>
where
    HAST::Idx: PrimInt,
    HAST::IdN: Clone + Debug + Eq,
    HAST::IdN: UniformNodeId,
    HAST::Label: Clone + Copy + Eq + Debug,
    <HAST::TS as TypeStore>::Ty: Eq + Debug,
    for<'t> LendT<'t, HAST>: WithHashs + WithStats,
{
    let measure = super::DefaultMetricSetup::prepare();
    let mut mapper_owned: (DS<HAST>, DS<HAST>) = hyperast.decompress_pair(src, dst).1;
    let mapper = Mapper::with_mut_decompressible(&mut mapper_owned, M::default());
    let measure = measure.start();

    let mapper = LazyGreedySubtreeMatcher::<_>::match_it::<MM>(mapper);
    let subtree_mappings_s = mapper.mappings().len();
    tr!(subtree_mappings_s);

    let measure = measure.stop_then_prepare();

    // Must fully decompress the subtrees to compute the non-lazy bottomup
    let mapper = Mapper::new(hyperast, mapper.mapping.mappings, mapper_owned);
    let mapper = mapper.map(
        |src_arena| CDS::<_>::from(src_arena.map(|x| x.complete(hyperast))),
        |dst_arena| CDS::<_>::from(dst_arena.map(|x| x.complete(hyperast))),
    );
    let measure = measure.start();

    let mapper = GreedyBottomUpMatcher::<_>::match_it(mapper);
    let bottomup_mappings_s = mapper.mappings().len();

    tr!(bottomup_mappings_s);

    let measure = measure.stop_then_prepare();

    let mapper = mapper.map_dst(
        // the dst side has to be traversed in bfs for chawathe
        |dst_arena| SimpleBfsMapper::with_store(hyperast, dst_arena),
    );
    let measure = measure.start();

    let actions = ScriptGenerator::compute_actions(mapper.hyperast, &mapper.mapping).ok();

    // drop the bfs wrapper
    let mapper = mapper.map_dst(|dst_arena| dst_arena.back);

    let exec_data = measure.stop();

    DiffResult {
        mapper,
        actions,
        exec_data,
    }
}
