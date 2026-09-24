#[cfg(feature = "impact")]
pub mod allrefs;
pub mod git;

pub mod multi_preprocessed;
pub mod no_space;
pub mod no_space2;
/// for now only tested on maven repositories with a pom in root.
pub mod preprocessed;
pub mod processing;
pub mod processors;

mod multi_type_store;
mod type_store;

#[macro_export]
macro_rules! multitstore {
    (#[cfg($($c:tt)+)] $t:ty, $($tt:tt)+ ) => {
        mod _multitstore {
            use super::multi_type_store;
            multitstore!{$($tt)*}
        }
        #[cfg($($c)+)]
        pub type TStore = multi_type_store::TStore<$t,_multitstore::TStore>;
        #[cfg(not($($c)+))]
        pub type TStore = _multitstore::TStore;
    };
    ($t:ty, $($tt:tt)+ ) => {
        mod _multitstore {
            use super::multi_type_store;
            multitstore!{$($tt)*}
        }
        pub type TStore = multi_type_store::TStore<$t,aaa::TStore>;
    };
    (#[cfg($($c:tt)+)] $t:ty) => {
        #[cfg($($c)+)]
        pub type TStore = multi_type_store::TStore<$t>;
        #[cfg(not($($c)+))]
        pub type TStore = multi_type_store::EmptyMultiTStore;
    };
    ($t:ty) => {
        pub type TStore = multi_type_store::TStore<$t>;
    };
}

multitstore! {
    #[cfg(feature = "java")]
    hyperast_gen_ts_java::TStore,
    #[cfg(feature = "cpp")]
    hyperast_gen_ts_cpp::TStore,
    #[cfg(feature = "c")]
    hyperast_gen_ts_c::TStore,
    #[cfg(feature = "maven")]
    hyperast_gen_ts_xml::TStore,
    #[cfg(feature = "typescript")]
    hyperast_gen_ts_typescript::TStore,
    #[cfg(feature = "rust")]
    hyperast_gen_ts_rust::TStore,
    #[cfg(feature = "python")]
    hyperast_gen_ts_python::TStore,
    #[cfg(feature = "file_sys")]
    crate::processors::file_sys::TStore
}

mod utils;
use utils::_auto_configured_line_break;
use utils::{FileProcessingResult, ParseErr, Str, SuccessProcessing};
pub use utils::{auto_configured_line_break, resolve_language};

mod precomp_patterns;

#[cfg(test)]
pub mod tests;

use git::BasicGitObject;
use git2::Oid;
use hyperast::store::defaults::{LabelIdentifier, NodeIdentifier};
use hyperast::utils::Bytes;

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
    // as_nanos
    processing_time: u128,
    memory_used: Bytes,
    pub ast_root: NodeIdentifier,
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

trait Accumulator: hyperast::tree_gen::Accumulator<Node = (LabelIdentifier, Self::Unlabeled)> {
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
            let ele = self.stack().last_mut().expect("never empty");
            if let Some(current_dir) = ele.cs.pop() {
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

mod dir_accumulator;
use dir_accumulator::BasicDirAcc;

type DirPrimary = BasicDirAcc<NodeIdentifier, LabelIdentifier, DefaultMetrics>;
