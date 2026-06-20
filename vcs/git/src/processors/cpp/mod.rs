mod cpp_processor;

use hyperast::store::defaults::LabelIdentifier;
use hyperast::tree_gen;

use crate::Accumulator;
use crate::DirPrimary;
use crate::processing::ObjectName;
use crate::{FailedParsing, FileProcessingResult, SuccessProcessing};

use hyperast_gen_ts_cpp::TStore;
use hyperast_gen_ts_cpp::legion as cpp_tree_gen;

pub type SimpleStores = hyperast::store::SimpleStores<hyperast_gen_ts_cpp::TStore>;

pub(crate) use cpp_processor::CppProc;
pub use cpp_processor::SUB_QUERIES; // To remove, at least in the current form

#[derive(Clone, PartialEq, Eq, Default)]
pub struct Parameter {
    pub(crate) query: Option<hyperast_tsquery::ZeroSepArrayStr>,
}

impl Parameter {
    pub fn new(query: impl Into<hyperast_tsquery::ZeroSepArrayStr>) -> Self {
        Self {
            query: Some(query.into()),
        }
    }
}

type PrecompQueries = u16;

pub struct CppAcc {
    pub(crate) primary: DirPrimary,
    pub(crate) precomp_queries: PrecompQueries,
}

impl CppAcc {
    pub(crate) fn new(name: String) -> Self {
        Self {
            primary: DirPrimary::new(name),
            precomp_queries: 0,
        }
    }
}

impl Accumulator for CppAcc {
    type Unlabeled = (cpp_tree_gen::Local,);
}

impl hyperast::tree_gen::Accumulator for CppAcc {
    type Node = (LabelIdentifier, (cpp_tree_gen::Local,));
    fn push(&mut self, (name, (full_node,)): Self::Node) {
        self.primary
            .push(name, full_node.compressed_node, full_node.metrics);
        self.precomp_queries |= full_node.precomp_queries;
    }
}

impl From<String> for CppAcc {
    fn from(name: String) -> Self {
        Self::new(name)
    }
}

impl CppAcc {
    pub(crate) fn push(&mut self, name: LabelIdentifier, full_node: cpp_tree_gen::Local) {
        self.primary
            .push(name, full_node.compressed_node, full_node.metrics);
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
