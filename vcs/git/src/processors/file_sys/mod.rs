//! Handle any directory structure

mod caches;
mod processor;
mod types;

use std::fmt::Debug;

use hyperast::store::defaults::{LabelIdentifier, NodeIdentifier};

use crate::{Accumulator, BasicDirAcc, DefaultMetrics};

pub(crate) use processor::FileSysProc;

pub use types::{TStore, Type};

pub use processor::Parameter;

type SimpleStores = hyperast::store::SimpleStores<TStore>;

type PrecompQueries = hyperast::tree_gen::extra_pattern_precomp::PrecompQueries;

pub struct FileSysAcc {
    pub(crate) primary: BasicDirAcc<NodeIdentifier, LabelIdentifier, DefaultMetrics>,
}

impl From<String> for FileSysAcc {
    fn from(name: String) -> Self {
        Self {
            primary: BasicDirAcc::new(name),
        }
    }
}

impl FileSysAcc {
    pub(crate) fn new(name: String) -> Self {
        Self {
            primary: BasicDirAcc::new(name),
        }
    }

    fn push(&mut self, name: LabelIdentifier, full_node: impl Into<FullNode>) {
        let full_node = full_node.into();
        self.primary.children.push(full_node.id);
        self.primary.children_names.push(name);
        self.primary.metrics.acc(full_node.metrics);
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

#[derive(Clone, Debug)]
pub struct FullNode {
    pub id: NodeIdentifier,
    pub metrics: DefaultMetrics,
}

impl From<hyperast_gen_ts_cpp::legion::Local> for FullNode {
    fn from(full_node: hyperast_gen_ts_cpp::legion::Local) -> Self {
        Self {
            id: full_node.compressed_node,
            metrics: full_node.metrics,
        }
    }
}

impl From<hyperast_gen_ts_java::legion_with_refs::Local> for FullNode {
    fn from(full_node: hyperast_gen_ts_java::legion_with_refs::Local) -> Self {
        Self {
            id: full_node.compressed_node,
            metrics: full_node.metrics,
        }
    }
}

impl From<hyperast::tree_gen::zipped_ts_extra::Local> for FullNode {
    fn from(full_node: hyperast::tree_gen::zipped_ts_extra::Local) -> Self {
        Self {
            id: full_node.compressed_node,
            metrics: full_node.metrics,
        }
    }
}
