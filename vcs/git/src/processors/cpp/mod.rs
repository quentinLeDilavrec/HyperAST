//! Handles C++

mod caches;
mod commit_proc;
mod cpp_processor;
mod impls;
pub mod selection;

use hyperast::store::defaults::LabelIdentifier;
use hyperast::tree_gen;

use crate::Accumulator;
use crate::DirPrimary;
use crate::processing::ObjectName;
use crate::processing::erased::ParametrizedCommitProcessor2Handle as PCP2Handle;
use crate::{FailedParsing, FileProcessingResult, SuccessProcessing};

use hyperast_gen_ts_cpp::TStore;
use hyperast_gen_ts_cpp::legion as cpp_tree_gen;

pub type SimpleStores = hyperast::store::SimpleStores<hyperast_gen_ts_cpp::TStore>;

use super::FullNode;
use super::PrecompQueries;

#[derive(Clone, PartialEq, Eq, Default)]
pub struct Parameter {
    pub(crate) query: Option<hyperast_tsquery::ZeroSepArrayStr>,
}

pub(crate) type CppProcessorHolder = crate::processing::ProcessorHolder<CppProc>;

pub struct CppProc {
    parameter: Parameter,
    query: Option<crate::processors::Query>,
    cache: caches::CppCache,
    commits: crate::processing::caches::OidMap<crate::Commit>,
}

pub struct CppAcc {
    pub(crate) primary: DirPrimary,
    pub(crate) precomp_queries: PrecompQueries,
}

impl Accumulator for CppAcc {
    type Unlabeled = FullNode;
}

impl hyperast::tree_gen::Accumulator for CppAcc {
    type Node = (LabelIdentifier, FullNode);
    fn push(&mut self, (name, full_node): Self::Node) {
        self.primary.push(name, full_node.id, full_node.metrics);
        self.precomp_queries += full_node.precomp_queries;
    }
}

impl CppAcc {
    pub(crate) fn push(&mut self, name: LabelIdentifier, full_node: impl Into<FullNode>) {
        let full_node = full_node.into();
        self.primary.push(name, full_node.id, full_node.metrics);
        self.precomp_queries += full_node.precomp_queries;
    }
}

impl CppProc {
    pub fn default_handle(pr: &mut crate::processing::erased::ProcessorMap) -> PCP2Handle<Self> {
        type CppProcessorHolder = crate::processing::ProcessorHolder<CppProc>;
        // let q = ["(translation_unit)"].as_slice();
        // let t = crate::processors::cpp::Parameter::new(q);
        let t = crate::processors::cpp::Parameter::default();
        let h = pr.mut_or_default::<CppProcessorHolder>();
        crate::processing::erased::CommitProcExt::register_param(h, t)
    }
}

// waiting for residual stabilization https://github.com/rust-lang/rust/issues/84277
// see after the temporary solution
// It is also limiting the usability with more variants
// enum FileProcessingResult<N, D = Duration> {
//     FailedParsing {
//         parsing_time: D,
//         tree: tree_sitter::Tree,
//         error: &'static str,
//     },
//     // ParsingTimedout(D),
//     // FailedProcessing {
//     //     parsing_time: D,
//     //     processing_time: D,
//     //     node: N,
//     // },
//     Success {
//         parsing_time: D,
//         processing_time: D,
//         node: N,
//     },
// }
/// Processing a single C++ file with `hyperast_gen_ts_cpp::legion::CppTreeGen`
pub(crate) fn handle_cpp_file1<'a, More>(
    tree_gen: &mut cpp_tree_gen::CppTreeGen<'a, '_, TStore, SimpleStores, More>,
    name: &ObjectName,
    text: &'a [u8],
) -> FileProcessingResult<cpp_tree_gen::FNode>
where
    More: tree_gen::Prepro<SimpleStores>,
    More: tree_gen::PreproTSG<SimpleStores, Acc = cpp_tree_gen::Acc>,
{
    let time = std::time::Instant::now();
    let tree = tree_gen::utils_ts::tree_sitter_parse(text, &hyperast_gen_ts_cpp::language());
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

/// Processing a single C++ file with new `hyperast_gen_ts_cpp::legion_ts_simp::CppTreeGen`
pub(crate) fn handle_cpp_file2<'a, E>(
    tree_gen: &mut hyperast_gen_ts_cpp::legion_ts_simp::CppTreeGen<'a, 'a, E>,
    name: &ObjectName,
    text: &'a [u8],
) -> FileProcessingResult<<E::Acc as hyperast::tree_gen::Accumulator>::Node>
where
    E: hyperast::tree_gen::TsExtra<SimpleStores>,
{
    let time = std::time::Instant::now();
    let tree = tree_gen::utils_ts::tree_sitter_parse(text, &hyperast_gen_ts_cpp::language());
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
