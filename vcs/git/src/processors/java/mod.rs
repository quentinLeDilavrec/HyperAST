//! Handles Java

pub mod caches;
mod commit_proc;
mod parameters;
mod processor;
pub mod selection;

use hyperast::store::defaults::LabelIdentifier;
use hyperast::tree_gen;

use crate::Accumulator;
use crate::DirPrimary;
use crate::processing::ObjectName;
use crate::processing::caches::OidMap;
use crate::{FileProcessingResult, SuccessProcessing};

pub(crate) use hyperast_gen_ts_java::legion_with_refs as java_tree_gen;
use hyperast_gen_ts_java::legion_with_refs::PartialAnalysis;
use hyperast_gen_ts_java::{TStore, Type};

pub use parameters::Parameter;
pub use parameters::SUB_QUERIES; // To remove, at least in the current form

pub type SimpleStores = hyperast::store::SimpleStores<TStore>;
type JavaProcessorHolder = crate::processing::ProcessorHolder<JavaProc>;

use super::FullNode;
use super::PrecompQueries;

pub struct JavaProc {
    pub(crate) parameter: Parameter,
    pub height_counts: Vec<u32>,
    query: Option<super::Query>,
    #[cfg(feature = "tsg")]
    tsg: Option<parameters::TsgErzedSettings>,
    cache: caches::JavaCaches,
    commits: OidMap<crate::Commit>,
}

pub struct JavaAcc {
    /// Identifying elements and fundamental derived metrics used to accelerate deduplication.
    /// For example, hashing subtrees accelerates the deduplication process,
    /// but it requires to hash children and it can be done by accumulating hashes iteratively per child (see [`hyperast::hashed::inner_node_hash`]).
    pub primary: DirPrimary,
    pub skiped_ana: bool,
    pub ana: PartialAnalysis,
    pub precomp_queries: PrecompQueries,
    pub scripting_acc: Option<hyperast::scripting::Acc>,
}

impl Accumulator for JavaAcc {
    type Unlabeled = FullNode;
}

impl hyperast::tree_gen::Accumulator for JavaAcc {
    type Node = (LabelIdentifier, FullNode);
    fn push(&mut self, (name, full_node): Self::Node) {
        self.push(name, full_node);
    }
}

impl JavaAcc {
    pub fn push(&mut self, name: LabelIdentifier, full_node: FullNode) {
        self.primary.push(name, full_node.id, full_node.metrics);

        #[cfg(feature = "impact")]
        if let Some(ana) = full_node.ana {
            if ana.estimated_refs_count() < crate::MAX_REFS
                && skiped_ana == false
                && self.skiped_ana == false
            {
                ana.acc(&Type::Directory, &mut self.ana);
            } else {
                self.skiped_ana = true;
            }
        }
        self.precomp_queries += full_node.precomp_queries;
    }
}

impl JavaAcc {
    pub fn root() -> Self {
        Self::new("".to_string(), None)
    }

    pub fn new(name: String, prepro: Option<hyperast::scripting::Acc>) -> Self {
        Self {
            primary: DirPrimary::new(name),
            ana: PartialAnalysis::init(&Type::Directory, None, |_| panic!()),
            skiped_ana: false,
            precomp_queries: Default::default(),
            scripting_acc: prepro,
        }
    }
}

impl From<hyperast_gen_ts_java::legion_with_refs::Local> for super::FullNode {
    fn from(full_node: hyperast_gen_ts_java::legion_with_refs::Local) -> Self {
        Self {
            id: full_node.compressed_node,
            metrics: full_node.metrics,
            precomp_queries: PrecompQueries(full_node.precomp_queries),
        }
    }
}

pub(crate) fn handle_java_file1<'stores, 'cache, 'b: 'stores, More>(
    tree_gen: &mut java_tree_gen::JavaTreeGen<'stores, 'cache, TStore, SimpleStores, More>,
    name: &ObjectName,
    text: &'b [u8],
) -> FileProcessingResult<java_tree_gen::FNode>
where
    More: tree_gen::Prepro<SimpleStores, Scope = hyperast::scripting::Acc>
        + tree_gen::PreproTSG<SimpleStores, Acc = java_tree_gen::Acc>,
{
    let time = std::time::Instant::now();
    let tree = java_tree_gen::tree_sitter_parse(text);
    let parsing_time = time.elapsed();
    super::report_or_fail_on_errored_tree!(name, tree, parsing_time);
    let node = tree_gen.generate_file(&name.as_bytes(), text, tree.walk());
    let processing_time = time.elapsed() - parsing_time;
    Ok(SuccessProcessing {
        parsing_time,
        processing_time,
        node,
    })
}

pub(crate) fn handle_java_file2<'a, E>(
    tree_gen: &mut hyperast_gen_ts_java::legion_ts_simp::JavaTreeGen<'a, 'a, E>,
    name: &ObjectName,
    text: &'a [u8],
) -> FileProcessingResult<<E::Acc as hyperast::tree_gen::Accumulator>::Node>
where
    E: hyperast::tree_gen::TsExtra<SimpleStores>,
{
    let time = std::time::Instant::now();
    let tree = java_tree_gen::tree_sitter_parse(text);
    let parsing_time = time.elapsed();
    super::report_or_fail_on_errored_tree!(name, tree, parsing_time);
    let node = tree_gen.generate_file(&name.as_bytes(), text, tree.walk());
    let processing_time = time.elapsed() - parsing_time;
    Ok(SuccessProcessing {
        parsing_time,
        processing_time,
        node,
    })
}
