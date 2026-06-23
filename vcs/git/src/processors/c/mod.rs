//! Handles C

mod caches;
mod commit_proc;
mod impls;
mod processor;
pub mod selection;

use hyperast::store::defaults::LabelIdentifier;
use hyperast::tree_gen;

use crate::Accumulator;
use crate::DirPrimary;
use crate::processing::ObjectName;
use crate::processing::ParametrizedProcessorHandle as PPHandle;
use crate::{FileProcessingResult, SuccessProcessing};

pub type SimpleStores = hyperast::store::SimpleStores<hyperast_gen_ts_c::TStore>;

use super::FullNode;
use super::PrecompQueries;

#[derive(Clone, PartialEq, Eq, Default)]
pub struct Parameter {
    pub(crate) query: Option<hyperast_tsquery::ZeroSepArrayStr>,
}

pub(crate) type CProcessorHolder = crate::processing::ProcessorHolder<CProc>;

pub struct CProc {
    parameter: Parameter,
    query: Option<crate::processors::Query>,
    cache: caches::CCache,
    commits: crate::processing::caches::OidMap<crate::Commit>,
}

pub struct CAcc {
    pub(crate) primary: DirPrimary,
    pub(crate) precomp_queries: PrecompQueries,
}

impl Accumulator for CAcc {
    type Unlabeled = FullNode;
}

impl hyperast::tree_gen::Accumulator for CAcc {
    type Node = (LabelIdentifier, FullNode);
    fn push(&mut self, (name, full_node): Self::Node) {
        self.primary.push(name, full_node.id, full_node.metrics);
        self.precomp_queries += full_node.precomp_queries;
    }
}

impl CAcc {
    pub(crate) fn push(&mut self, name: LabelIdentifier, full_node: impl Into<FullNode>) {
        let full_node = full_node.into();
        self.primary.push(name, full_node.id, full_node.metrics);
        self.precomp_queries += full_node.precomp_queries;
    }
}

impl CProc {
    pub fn default_handle(pr: &mut crate::processing::erased::ProcessorMap) -> PPHandle<Self> {
        type CProcessorHolder = crate::processing::ProcessorHolder<CProc>;
        let t = crate::processors::c::Parameter::default();
        let h = pr.commit_proc_mut::<CProcessorHolder>();
        h.register_param(t)
    }
}

/// Processing a single C file with new `hyperast_gen_ts_c::legion_ts_simp::CTreeGen`
pub(crate) fn handle_c_file<'a, E>(
    tree_gen: &mut hyperast_gen_ts_c::legion_ts_simp::CTreeGen<'a, 'a, E>,
    name: &ObjectName,
    text: &'a [u8],
) -> FileProcessingResult<<E::Acc as hyperast::tree_gen::Accumulator>::Node>
where
    E: hyperast::tree_gen::TsExtra<SimpleStores>,
{
    let time = std::time::Instant::now();
    let tree = tree_gen::utils_ts::tree_sitter_parse(text, &hyperast_gen_ts_c::language());
    let parsing_time = time.elapsed();
    super::report_or_fail_on_errored_tree!(name, tree, parsing_time);
    let node = tree_gen.generate_file(name.as_bytes(), text, tree.walk());
    let processing_time = time.elapsed() - parsing_time;
    Ok(SuccessProcessing {
        parsing_time,
        processing_time,
        node,
    })
}
