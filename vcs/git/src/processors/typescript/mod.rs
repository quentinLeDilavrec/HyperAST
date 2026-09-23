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

use hyperast_gen_ts_typescript::TStore;

pub type SimpleStores = hyperast::store::SimpleStores<TStore>;
type TypescriptProcessorHolder = crate::processing::ProcessorHolder<TypescriptProc>;

use super::FullNode;
use super::PrecompQueries;

#[derive(Clone, PartialEq, Eq, Default)]
pub struct Parameter {
    pub(crate) query: Option<hyperast_tsquery::ZeroSepArrayStr>,
}

pub(crate) struct TypescriptProc {
    parameter: Parameter,
    query: Option<super::Query>,
    cache: caches::Typescript,
    commits: OidMap<crate::Commit>,
}

impl Parameter {
    pub(crate) fn new(query: impl Into<hyperast_tsquery::ZeroSepArrayStr>) -> Self {
        Self {
            query: Some(query.into()),
            ..Default::default()
        }
    }
}

pub struct TypescriptAcc {
    pub(crate) primary: DirPrimary,
    pub(crate) precomp_queries: PrecompQueries,
}

impl TypescriptAcc {
    pub(crate) fn new(name: String) -> Self {
        Self {
            primary: DirPrimary::new(name),
            precomp_queries: Default::default(),
        }
    }
}

impl Accumulator for TypescriptAcc {
    type Unlabeled = FullNode;
}

impl tree_gen::Accumulator for TypescriptAcc {
    type Node = (LabelIdentifier, FullNode);
    fn push(&mut self, (name, full_node): Self::Node) {
        self.primary.push(name, full_node.id, full_node.metrics);
        self.precomp_queries += full_node.precomp_queries;
    }
}

impl From<String> for TypescriptAcc {
    fn from(name: String) -> Self {
        Self::new(name)
    }
}

impl TypescriptAcc {
    pub(crate) fn push(&mut self, name: LabelIdentifier, full_node: impl Into<FullNode>) {
        let full_node = full_node.into();
        let id = full_node.id;
        self.primary.push(name, id, full_node.metrics);
        self.precomp_queries += full_node.precomp_queries;
    }
}

impl TypescriptProc {
    pub fn default_handle(pr: &mut crate::processing::erased::ProcessorMap) -> PPHandle<Self> {
        type TypescriptProcessorHolder = crate::processing::ProcessorHolder<TypescriptProc>;
        // let q = ["(module)"].as_slice();
        // let t = crate::processors::typescript::Parameter::new(q);
        let t = crate::processors::typescript::Parameter::default();
        let h = pr.commit_proc_mut::<TypescriptProcessorHolder>();
        h.register_param(t)
    }
}

use super::handle_file_ts_simp as handle_typescript_file;
