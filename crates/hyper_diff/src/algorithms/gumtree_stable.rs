use std::fmt::Debug;

use hyperast::PrimInt;
use hyperast::types::{HyperAST, LendT};
use hyperast::types::{WithHashs, WithStats};

use super::{CDS, DiffRes, DiffResult, tr};
use crate::actions::script_generator2::ScriptGenerator;
use crate::decompressed_tree_store::bfs_wrapper::SimpleBfsMapper;
use crate::mappings::{DefaultMultiMappingStore, MappingStore, VecStore};
use crate::matchers::Mapper;
use crate::matchers::heuristic::gt::greedy_subtree_matcher::GreedySubtreeMatcher;
use crate::matchers::heuristic::gt::marriage_bottom_up_matcher::MarriageBottomUpMatcher;

type M = VecStore<u32>;

pub fn diff<HAST: HyperAST + Copy>(
    hyperast: HAST,
    src: &HAST::IdN,
    dst: &HAST::IdN,
) -> DiffRes<HAST>
where
    HAST::Idx: PrimInt,
    HAST::IdN: Clone + Debug + Eq,
    HAST::Label: Debug + Clone + Copy + Eq,
    for<'t> LendT<'t, HAST>: WithHashs + WithStats,
{
    let measure = super::DefaultMetricSetup::prepare();
    let mapper: Mapper<_, CDS<HAST>, CDS<HAST>, VecStore<_>> =
        hyperast.decompress_pair(src, dst).into();
    let measure = measure.start();

    let mapper = GreedySubtreeMatcher::<_>::match_it::<DefaultMultiMappingStore<_>>(mapper);
    let subtree_mappings_s = mapper.mappings().len();
    tr!(subtree_mappings_s);

    let measure = measure.stop_then_skip_prepare();

    let mapper = MarriageBottomUpMatcher::<_, M, 300>::match_it(mapper);
    let bottomup_mappings_s = mapper.mappings().len();
    tr!(bottomup_mappings_s);

    let measure = measure.stop_then_prepare();

    let mapper = mapper.map_dst(SimpleBfsMapper::make);
    let measure = measure.start();
    let actions = ScriptGenerator::compute_actions(hyperast, &mapper.mapping).ok();
    let mapper = mapper.map_dst(|dst_arena| dst_arena.back);

    let exec_data = measure.stop();

    DiffResult {
        mapper,
        actions,
        exec_data,
    }
}
