mod java_processor;

use hyperast::store::defaults::LabelIdentifier;
use hyperast::tree_gen;

use crate::Accumulator;
use crate::DirPrimary;
use crate::PROPAGATE_ERROR_ON_BAD_CST_NODE;
use crate::processing::ObjectName;
use crate::{FailedParsing, FileProcessingResult, SuccessProcessing};

use hyperast_gen_ts_java::legion_with_refs as java_tree_gen;
use hyperast_gen_ts_java::legion_with_refs::PartialAnalysis;
use hyperast_gen_ts_java::{TStore, Type};

pub use java_processor::*;
pub use java_processor::{JavaProc, JavaProcessorHolder};

pub type SimpleStores = hyperast::store::SimpleStores<TStore>;

pub(crate) fn handle_java_file<'stores, 'cache, 'b: 'stores, More>(
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
    if tree.root_node().has_error() {
        log::warn!("bad CST: {:?}", name.try_str());
        log::debug!("{}", tree.root_node().to_sexp());
        if PROPAGATE_ERROR_ON_BAD_CST_NODE {
            return Err(FailedParsing {
                parsing_time,
                tree,
                error: "CST contains parsing errors",
            });
        }
    }
    let node = tree_gen.generate_file(&name.as_bytes(), text, tree.walk());
    let processing_time = time.elapsed() - parsing_time;
    Ok(SuccessProcessing {
        parsing_time,
        processing_time,
        node,
    })
}

type PrecompQueries = u16;

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

impl JavaAcc {
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

impl From<String> for JavaAcc {
    fn from(name: String) -> Self {
        Self::new(name, None)
    }
}

impl JavaAcc {
    pub fn push(&mut self, name: LabelIdentifier, full_node: java_tree_gen::Local) {
        self.primary
            .push(name, full_node.compressed_node, full_node.metrics);

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
        self.precomp_queries |= full_node.precomp_queries;
    }
}

impl hyperast::tree_gen::Accumulator for JavaAcc {
    type Node = (LabelIdentifier, (java_tree_gen::Local,));
    fn push(&mut self, (name, (full_node,)): Self::Node) {
        self.primary
            .push(name, full_node.compressed_node, full_node.metrics);

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
        self.precomp_queries |= full_node.precomp_queries;
    }
}

impl Accumulator for JavaAcc {
    type Unlabeled = (java_tree_gen::Local,);
}
