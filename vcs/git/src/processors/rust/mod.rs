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
use crate::processing::ObjectName;
use crate::processing::ParametrizedProcessorHandle as PPHandle;
use crate::processing::caches::OidMap;
use crate::{FileProcessingResult, SuccessProcessing};

use hyperast_gen_ts_rust::TStore;
use hyperast_gen_ts_rust::legion as rust_tree_gen;

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

pub(crate) fn handle_rust_file<'a, E>(
    tree_gen: &mut rust_tree_gen::RustTreeGen<'a, 'a, E>,
    name: &ObjectName,
    text: &'a [u8],
) -> FileProcessingResult<<E::Acc as tree_gen::Accumulator>::Node>
where
    E: tree_gen::TsExtra<SimpleStores>,
{
    let time = std::time::Instant::now();
    let language = hyperast_gen_ts_rust::language();
    let tree = tree_gen::utils_ts::tree_sitter_parse(text, &language);
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
