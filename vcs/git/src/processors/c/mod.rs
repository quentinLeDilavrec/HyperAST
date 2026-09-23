//! Handles C

mod caches;
mod commit_proc;
mod impls;
mod processor;
pub mod selection;

use hyperast::store::defaults::LabelIdentifier;

use crate::Accumulator;
use crate::DirPrimary;
use crate::processing::ParametrizedProcessorHandle as PPHandle;

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

use super::handle_file_ts_simp as handle_c_file;
