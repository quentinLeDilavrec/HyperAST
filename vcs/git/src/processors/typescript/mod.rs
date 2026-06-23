//! Handles Java
//!
mod caches;
pub mod file_sys;
mod processor;

use hyperast::store::defaults::LabelIdentifier;
use hyperast::tree_gen;

use crate::Accumulator;
use crate::DirPrimary;
use crate::processing::ObjectName;
use crate::processing::ParametrizedProcessorHandle as PPHandle;
use crate::{FailedParsing, FileProcessingResult, SuccessProcessing};

use hyperast_gen_ts_typescript::TStore;
use hyperast_gen_ts_typescript::legion as typescript_tree_gen;

pub type SimpleStores = hyperast::store::SimpleStores<TStore>;

pub(crate) use processor::TypescriptProc;

use super::FullNode;

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

type PrecompQueries = tree_gen::extra_pattern_precomp::PrecompQueries;

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

pub(crate) fn handle_typescript_file<'a, E>(
    tree_gen: &mut typescript_tree_gen::TypeScriptTreeGen<'a, 'a, E>,
    name: &ObjectName,
    text: &'a [u8],
) -> FileProcessingResult<<E::Acc as tree_gen::Accumulator>::Node>
where
    E: tree_gen::TsExtra<SimpleStores>,
{
    let time = std::time::Instant::now();
    let language = hyperast_gen_ts_typescript::language();
    let tree = tree_gen::utils_ts::tree_sitter_parse(text, &language);
    let parsing_time = time.elapsed();
    if tree.root_node().has_error() {
        log::warn!("bad CST: {:?}", name.try_str());
        if crate::PROPAGATE_ERROR_ON_BAD_CST_NODE {
            return Err(FailedParsing {
                parsing_time,
                tree,
                error: "CST contains parsing errors",
            });
        }
    };
    let node = tree_gen.generate_file(name.as_bytes(), text, tree.walk());
    let processing_time = time.elapsed() - parsing_time;
    Ok(SuccessProcessing {
        parsing_time,
        processing_time,
        node,
    })
}
