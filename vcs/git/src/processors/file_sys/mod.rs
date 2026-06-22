//! Handles raw directory structure, ignoring build systems

mod caches;
mod commit_proc;
mod processor;
pub mod types;

use hyperast::store::defaults::{LabelIdentifier, NodeIdentifier};

use crate::processing::ParametrizedProcessor2Handle as PCP2Handle;
use crate::processing::erased::ParametrizedCommitProcessorHandle as PCPHandle;
use crate::{Accumulator, BasicDirAcc, DefaultMetrics};

pub use types::{TStore, Type};

use super::FullNode;

type SimpleStores = hyperast::store::SimpleStores<TStore>;
use hyperast::tree_gen::extra_pattern_precomp::PrecompQueries;

#[derive(Clone, PartialEq, Eq)]
pub struct Parameter {
    #[cfg(feature = "cpp")]
    pub(crate) cpp_handle: PCP2Handle<super::cpp::CppProc>,
    #[cfg(feature = "java")]
    pub(crate) java_handle: PCP2Handle<super::java::JavaProc>,
    #[cfg(feature = "python")]
    pub(crate) python_handle: PCP2Handle<super::python::PythonProc>,
}

pub(crate) struct FileSysProc {
    parameter: Parameter,
    cache: caches::FileSys,
    commits: std::collections::HashMap<git2::Oid, crate::Commit>,
}

pub struct FileSysAcc {
    pub(crate) primary: BasicDirAcc<NodeIdentifier, LabelIdentifier, DefaultMetrics>,
    pub(crate) precomp_queries: PrecompQueries,
}

impl From<String> for FileSysAcc {
    fn from(name: String) -> Self {
        Self {
            primary: BasicDirAcc::new(name),
            precomp_queries: PrecompQueries::default(),
        }
    }
}

impl FileSysAcc {
    pub(crate) fn new(name: String) -> Self {
        Self {
            primary: BasicDirAcc::new(name),
            precomp_queries: PrecompQueries::default(),
        }
    }

    fn push(&mut self, name: LabelIdentifier, full_node: impl Into<FullNode>) {
        let full_node = full_node.into();
        self.primary.children.push(full_node.id);
        self.primary.children_names.push(name);
        self.primary.metrics.acc(full_node.metrics);
        self.precomp_queries += full_node.precomp_queries;
    }
}

impl Accumulator for FileSysAcc {
    type Unlabeled = FullNode;
}

impl hyperast::tree_gen::Accumulator for FileSysAcc {
    type Node = (LabelIdentifier, FullNode);
    fn push(&mut self, (name, full_node): Self::Node) {
        self.push(name, full_node);
    }
}

impl crate::preprocessed::RepositoryProcessor {
    pub fn default_config(&mut self) -> PCPHandle {
        use crate::processors::cpp::CppProc;
        use crate::processors::java::JavaProc;
        use crate::processors::python::PythonProc;
        let processor_map = &mut self.processing_systems;
        let t = crate::processors::file_sys::Parameter {
            #[cfg(feature = "cpp")]
            cpp_handle: CppProc::default_handle(processor_map),
            #[cfg(feature = "java")]
            java_handle: JavaProc::default_handle(processor_map),
            #[cfg(feature = "python")]
            python_handle: PythonProc::default_handle(processor_map),
        };
        type FileSysProcessorHolder = crate::processing::ProcessorHolder<FileSysProc>;
        let holder = processor_map.commit_proc_mut::<FileSysProcessorHolder>();
        holder.register_param(t).erase()
    }
}
