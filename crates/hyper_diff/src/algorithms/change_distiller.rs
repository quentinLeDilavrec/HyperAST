use std::fmt::Debug;

use hyperast::PrimInt;
use hyperast::store::nodes::compo;
use hyperast::types::NodeId;
use hyperast::types::{HyperAST, LendT};
use hyperast::types::{WithHashs, WithMetaData, WithStats};

use super::{CDS, DiffRes, DiffResult, tr};
use crate::actions::script_generator2::ScriptGenerator;
use crate::decompressed_tree_store::bfs_wrapper::SimpleBfsMapper;
use crate::mappings::{MappingStore, VecStore};
use crate::matchers::Mapper;
use crate::matchers::heuristic::cd::bottom_up_matcher::BottomUpMatcher;
use crate::matchers::heuristic::cd::leaves_matcher::LeavesMatcher;

pub fn diff<HAST: HyperAST + Copy>(
    hyperast: HAST,
    src: &HAST::IdN,
    dst: &HAST::IdN,
) -> DiffRes<HAST>
where
    HAST::Idx: PrimInt,
    HAST::IdN: Copy + Debug + Eq,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
    HAST::Label: Debug + Clone + Copy + Eq,
    for<'t> LendT<'t, HAST>: WithHashs
        + WithStats
        + WithMetaData<compo::MemberImportCount>
        + WithMetaData<compo::StmtCount>,
{
    let measure = super::DefaultMetricSetup::prepare();
    let mapper: Mapper<_, CDS<HAST>, CDS<HAST>, VecStore<_>> =
        hyperast.decompress_pair(src, dst).into();
    let measure = measure.start();

    let mapper = LeavesMatcher::<_>::match_it(mapper);
    let subtree_mappings_s = mapper.mappings().len();
    tr!(subtree_mappings_s);

    let measure = measure.stop_then_skip_prepare();

    let mapper = BottomUpMatcher::<_>::match_it(mapper);
    let bottomup_mappings_s = mapper.mappings().len();

    tr!(bottomup_mappings_s);

    let measure = measure.stop_then_prepare();

    let mapper = mapper.map(
        |x| x,
        // the dst side has to be traversed in bfs for chawathe
        |dst_arena| SimpleBfsMapper::with_store(hyperast, dst_arena),
    );
    let measure = measure.start();
    let actions = ScriptGenerator::compute_actions(hyperast, &mapper.mapping).ok();
    let mapper = mapper.map(|x| x, |dst_arena| dst_arena.back);
    let exec_data = measure.stop();
    DiffResult {
        mapper,
        actions,
        exec_data,
    }
}
