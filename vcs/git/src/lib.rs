#[cfg(feature = "impact")]
pub mod allrefs;
pub mod cpp;
pub mod git;
pub mod java;
pub mod make;
pub mod maven;

#[cfg(feature = "cpp")]
pub mod cpp_processor;
#[cfg(feature = "java")]
pub mod java_processor;
#[cfg(feature = "make")]
pub mod make_processor;
#[cfg(feature = "maven")]
pub mod maven_processor;
pub mod multi_preprocessed;
pub mod no_space;
pub mod no_space2;
/// for now only tested on maven repositories with a pom in root.
pub mod preprocessed;
pub mod processing;
mod utils;

#[cfg(test)]
pub mod tests;

use git::BasicGitObject;
use git2::Oid;
use hyperast::utils::Bytes;

mod type_store;

pub use type_store::TStore;

pub type NodeStore<'a, 'b> = hyperast::store::nodes::legion::NodeStore<
    &'a mut hyperast::store::nodes::legion::NodeStoreInner,
    &'b mut hyperast::store::nodes::legion::DedupMap,
>;

pub type SimpleStores = hyperast::store::SimpleStores<crate::TStore>;

// might also skip
pub(crate) const PROPAGATE_ERROR_ON_BAD_CST_NODE: bool = false;

pub(crate) type DefaultMetrics =
    hyperast::tree_gen::SubTreeMetrics<hyperast::hashed::SyntaxNodeHashs<u32>>;

pub struct Diffs();
pub struct Impacts();

#[derive(Clone)]
pub struct Commit {
    pub parents: Vec<git2::Oid>,
    processing_time: u128,
    memory_used: Bytes,
    pub ast_root: hyperast::store::nodes::DefaultNodeIdentifier,
    pub tree_oid: git2::Oid,
}

impl Commit {
    pub fn processing_time(&self) -> u128 {
        self.processing_time
    }
    pub fn memory_used(&self) -> Bytes {
        self.memory_used
    }
}

trait Accumulator:
    hyperast::tree_gen::Accumulator<
        Node = (hyperast::store::defaults::LabelIdentifier, Self::Unlabeled),
    >
{
    type Unlabeled;
}

pub(crate) struct StackEle<Acc, Oid = git2::Oid, O = BasicGitObject> {
    id: Oid,
    cs: Vec<O>,
    acc: Acc,
}

impl<Acc> StackEle<Acc> {
    pub(crate) fn new(id: Oid, cs: Vec<BasicGitObject>, acc: Acc) -> Self {
        Self { id, cs, acc }
    }
}

trait Processor<Acc: Accumulator, Oid = self::Oid, O = BasicGitObject> {
    fn process(&mut self) -> Acc::Unlabeled {
        loop {
            if let Some(current_dir) = self.stack().last_mut().expect("never empty").cs.pop() {
                self.pre(current_dir)
            } else if let Some(StackEle { id, acc, .. }) = self.stack().pop() {
                if let Some(x) = self.post(id, acc) {
                    return x;
                }
            } else {
                unreachable!("never empty")
            }
        }
    }
    fn stack(&mut self) -> &mut Vec<StackEle<Acc, Oid, O>>;
    fn pre(&mut self, current_dir: O);
    fn post(&mut self, oid: Oid, acc: Acc) -> Option<Acc::Unlabeled>;
}

pub(crate) enum ParseErr {
    NotUtf8(std::str::Utf8Error),
    IllFormed,
}

impl std::fmt::Debug for ParseErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NotUtf8(e) => e.fmt(f),
            Self::IllFormed => write!(f, "IllFormed"),
        }
    }
}

impl From<std::str::Utf8Error> for ParseErr {
    fn from(value: std::str::Utf8Error) -> Self {
        ParseErr::NotUtf8(value)
    }
}

#[cfg(feature = "cpp")]
fn ts_lang_cpp() -> Option<tree_sitter::Language> {
    Some(hyperast_gen_ts_cpp::language())
}
#[cfg(not(feature = "cpp"))]
fn ts_lang_cpp() -> Option<tree_sitter::Language> {
    None
}
#[cfg(feature = "java")]
fn ts_lang_java() -> Option<tree_sitter::Language> {
    Some(hyperast_gen_ts_java::language())
}
#[cfg(not(feature = "java"))]
fn ts_lang_java() -> Option<tree_sitter::Language> {
    None
}

pub fn resolve_language(language: &str) -> Option<tree_sitter::Language> {
    match language {
        "Java" | "java" => ts_lang_java(),
        "Cpp" | "cpp" => ts_lang_cpp(),
        _ => None,
    }
}

mod dir_accumulator;
use dir_accumulator::BasicDirAcc;

pub(crate) struct FailedParsing<D = std::time::Duration> {
    pub parsing_time: D,
    pub tree: tree_sitter::Tree,
    pub error: &'static str,
}

pub(crate) struct SuccessProcessing<N, D = std::time::Duration> {
    pub parsing_time: D,
    pub processing_time: D,
    pub node: N,
}

pub(crate) type FileProcessingResult<N, D = std::time::Duration> =
    Result<SuccessProcessing<N, D>, FailedParsing<D>>;
