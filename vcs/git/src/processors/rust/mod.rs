//! Handles Java
//!
mod caches;
mod commit_proc;
pub mod file_sys;
mod processor;

use hyperast::store::defaults::LabelIdentifier;
use hyperast::tree_gen;

use crate::Accumulator;
use crate::DirPrimary;
use crate::processing::ParametrizedProcessorHandle as PPHandle;
use crate::processing::caches::OidMap;

use hyperast_gen_ts_rust::TStore;

pub type SimpleStores = hyperast::store::SimpleStores<TStore>;
type RustProcessorHolder = crate::processing::ProcessorHolder<RustProc>;

pub(crate) struct RustProc {
    parameter: Parameter,
    query: Option<super::Query>,
    cache: caches::Rust,
    commits: OidMap<crate::Commit>,
}

use super::FullNode;
use super::PrecompQueries;

#[derive(Clone, PartialEq, Eq, Default)]
pub struct Parameter {
    pub(crate) query: Option<hyperast_tsquery::ZeroSepArrayStr>,
}

impl Parameter {
    pub(crate) fn new(query: impl Into<hyperast_tsquery::ZeroSepArrayStr>) -> Self {
        Self {
            query: Some(query.into()),
            ..Default::default()
        }
    }
}

pub struct RustAcc {
    pub(crate) primary: DirPrimary,
    pub(crate) precomp_queries: PrecompQueries,
}

impl RustAcc {
    pub(crate) fn new(name: String) -> Self {
        Self {
            primary: DirPrimary::new(name),
            precomp_queries: Default::default(),
        }
    }
}

impl Accumulator for RustAcc {
    type Unlabeled = FullNode;
}

impl tree_gen::Accumulator for RustAcc {
    type Node = (LabelIdentifier, FullNode);
    fn push(&mut self, (name, full_node): Self::Node) {
        self.primary.push(name, full_node.id, full_node.metrics);
        self.precomp_queries += full_node.precomp_queries;
    }
}

impl From<String> for RustAcc {
    fn from(name: String) -> Self {
        Self::new(name)
    }
}

impl RustAcc {
    pub(crate) fn push(&mut self, name: LabelIdentifier, full_node: impl Into<FullNode>) {
        let full_node = full_node.into();
        let id = full_node.id;
        self.primary.push(name, id, full_node.metrics);
        self.precomp_queries += full_node.precomp_queries;
    }
}

impl RustProc {
    pub fn default_handle(pr: &mut crate::processing::erased::ProcessorMap) -> PPHandle<Self> {
        type RustProcessorHolder = crate::processing::ProcessorHolder<RustProc>;
        // let q = ["(module)"].as_slice();
        // let t = crate::processors::rust::Parameter::new(q);
        let t = crate::processors::rust::Parameter::default();
        let h = pr.commit_proc_mut::<RustProcessorHolder>();
        h.register_param(t)
    }
}

use super::handle_file_ts_simp as handle_rust_file;
