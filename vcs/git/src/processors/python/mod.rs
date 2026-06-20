mod caches;
pub mod file_sys;
mod processor;

use hyperast::store::defaults::LabelIdentifier;
use hyperast::tree_gen;

use crate::Accumulator;
use crate::DirPrimary;
use crate::processing::ObjectName;
use crate::{FailedParsing, FileProcessingResult, SuccessProcessing};

use hyperast_gen_ts_python::TStore;
use hyperast_gen_ts_python::legion as python_tree_gen;

pub type SimpleStores = hyperast::store::SimpleStores<TStore>;

pub(crate) use processor::PythonProc;

type FullNode = (python_tree_gen::Local, PrecompQueries);

#[derive(Clone, PartialEq, Eq, Default)]
pub struct Parameter {
    pub(crate) query: Option<hyperast_tsquery::ZeroSepArrayStr>,
}

impl Parameter {
    pub(crate) fn with_query(query: impl Into<hyperast_tsquery::ZeroSepArrayStr>) -> Self {
        Self {
            query: Some(query.into()),
            ..Default::default()
        }
    }
}

type PrecompQueries = tree_gen::extra_pattern_precomp::PrecompQueries;

pub struct PythonAcc {
    pub(crate) primary: DirPrimary,
    pub(crate) precomp_queries: PrecompQueries,
}

impl PythonAcc {
    pub(crate) fn new(name: String) -> Self {
        Self {
            primary: DirPrimary::new(name),
            precomp_queries: Default::default(),
        }
    }
}

impl Accumulator for PythonAcc {
    type Unlabeled = (python_tree_gen::Local, PrecompQueries);
}

impl hyperast::tree_gen::Accumulator for PythonAcc {
    type Node = (LabelIdentifier, (python_tree_gen::Local, PrecompQueries));
    fn push(&mut self, (name, (full_node, precomp_queries)): Self::Node) {
        self.primary
            .push(name, full_node.compressed_node, full_node.metrics);
        self.precomp_queries += precomp_queries;
    }
}

impl From<String> for PythonAcc {
    fn from(name: String) -> Self {
        Self::new(name)
    }
}

impl PythonAcc {
    pub(crate) fn push(&mut self, name: LabelIdentifier, full_node: FullNode) {
        self.primary
            .push(name, full_node.0.compressed_node, full_node.0.metrics);
        self.precomp_queries += full_node.1;
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
pub(crate) fn handle_python_file<'a, E>(
    tree_gen: &mut hyperast_gen_ts_python::legion::PythonTreeGen<'a, 'a, E>,
    name: &ObjectName,
    text: &'a [u8],
) -> FileProcessingResult<<E::Acc as hyperast::tree_gen::Accumulator>::Node>
where
    E: hyperast::tree_gen::TsExtra<SimpleStores>,
{
    let time = std::time::Instant::now();
    let language = hyperast_gen_ts_python::language();
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
