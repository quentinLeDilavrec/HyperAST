mod processor;

pub mod iter_maven_module;
pub use iter_maven_module::IterMavenModules2;

mod pom;
use pom::handle_pom_file;
pub use pom::{PomParameter, PomProc};

mod scripting;

use std::fmt::Debug;
use std::path::PathBuf;

use enumset::EnumSet;

use hyperast::store::defaults::{LabelIdentifier, NodeIdentifier};
use hyperast::tree_gen::SubTreeMetrics;
use hyperast_gen_ts_xml::TStore;

use crate::Accumulator;
use crate::BasicDirAcc;
use crate::DefaultMetrics;
use crate::processing::erased::ParametrizedCommitProcessor2Handle as PCP2Handle;
use crate::processors::java::JavaProc;

use super::java::java_tree_gen;

pub type SimpleStores = hyperast::store::SimpleStores<TStore>;

pub use processor::MavenProc;
pub use processor::make;

#[derive(Clone, PartialEq, Eq)]
pub struct Parameter {
    // pub(crate) query: Option<std::sync::Arc<[String]>>,
    // pub(crate) prepo: Option<hyperast_scripting::Prepro>,
    pub java_handle: PCP2Handle<JavaProc>,
    pub pom_handle: PCP2Handle<PomProc>,
}

#[derive(Debug, Clone)]
pub struct POM {
    pub compressed_node: NodeIdentifier,
    pub metrics: DefaultMetrics,
    submodules: Vec<String>,
    source_dirs: Vec<String>,
    test_source_dirs: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct MD {
    pub(crate) metrics: DefaultMetrics,
    #[allow(unused)] // TODO needed for scalable module level reference analysis
    pub(crate) ana: MavenPartialAnalysis,
    pub(crate) status: EnumSet<SemFlag>,
}

pub struct MavenModuleAcc {
    pub(crate) primary: crate::DirPrimary,
    pub(crate) ana: MavenPartialAnalysis,
    pub(crate) sub_modules: Option<Vec<PathBuf>>,
    pub(crate) main_dirs: Option<Vec<PathBuf>>,
    pub(crate) test_dirs: Option<Vec<PathBuf>>,
    pub(crate) status: EnumSet<SemFlag>,
    pub(crate) scripting_acc: std::option::Option<hyperast::scripting::Acc>,
}

#[derive(enumset::EnumSetType, Debug)]
pub enum SemFlag {
    IsMavenModule,
    HoldMainFolder,
    HoldTestFolder,
    HoldMavenSubModule,
}

impl MavenModuleAcc {
    pub(crate) fn new(name: String) -> Self {
        Self {
            primary: BasicDirAcc::new(name),
            ana: MavenPartialAnalysis::new(),
            sub_modules: None,
            main_dirs: None,
            test_dirs: None,
            status: Default::default(),
            scripting_acc: None,
        }
    }
    pub(crate) fn with_content(
        name: String,
        sub_modules: Vec<PathBuf>,
        main_dirs: Vec<PathBuf>,
        test_dirs: Vec<PathBuf>,
    ) -> Self {
        Self {
            sub_modules: (!sub_modules.is_empty()).then_some(sub_modules),
            main_dirs: (!main_dirs.is_empty()).then_some(main_dirs),
            test_dirs: (!test_dirs.is_empty()).then_some(test_dirs),
            ..Self::new(name)
        }
    }
}

impl From<String> for MavenModuleAcc {
    fn from(name: String) -> Self {
        Self::new(name)
    }
}

impl MavenModuleAcc {
    pub(crate) fn push_pom(&mut self, name: LabelIdentifier, full_node: POM) {
        self.status |= SemFlag::IsMavenModule;
        assert!(!self.primary.children_names.contains(&name));
        self.primary.children.push(full_node.compressed_node);
        self.primary.children_names.push(name);
        self.main_dirs = Some(full_node.source_dirs.iter().map(|x| x.into()).collect());
        self.test_dirs = Some(
            full_node
                .test_source_dirs
                .iter()
                .map(|x| x.into())
                .collect(),
        );
        self.sub_modules = Some(full_node.submodules.iter().map(|x| x.into()).collect());
        self.primary.metrics.acc(full_node.metrics);
    }
    pub fn push_submodule(&mut self, name: LabelIdentifier, full_node: FullNode) {
        if full_node.hold_maven_submodule() || full_node.is_maven_module() {
            self.status |= SemFlag::HoldMavenSubModule;
        }
        self.primary.children.push(full_node.id);
        self.primary.children_names.push(name);
        self.primary.metrics.acc(full_node.md.metrics);
    }
    pub(crate) fn push_source_directory(
        &mut self,
        name: LabelIdentifier,
        full_node: java_tree_gen::Local,
    ) {
        self.status |= SemFlag::HoldMainFolder;
        self.primary.children.push(full_node.compressed_node);
        self.primary.children_names.push(name);
        self.primary.metrics.acc(SubTreeMetrics {
            line_count: 0,
            ..full_node.metrics
        });
        // TODO ana
        // full_node.2.acc(&Type::Directory, &mut self.ana);
    }
    pub(crate) fn push_test_source_directory(
        &mut self,
        name: LabelIdentifier,
        full_node: java_tree_gen::Local,
    ) {
        self.status |= SemFlag::HoldTestFolder;
        self.primary.children.push(full_node.compressed_node);
        self.primary.children_names.push(name);
        self.primary.metrics.acc(SubTreeMetrics {
            line_count: 0,
            ..full_node.metrics
        });
        // TODO ana
        // full_node.2.acc(&Type::Directory, &mut self.ana);
    }
}

#[derive(Debug, Clone)]
pub(crate) struct MavenPartialAnalysis {
    submodules: Vec<()>,
    main_dirs: Vec<()>,
    test_dirs: Vec<()>,
}

impl MavenPartialAnalysis {
    pub(crate) fn new() -> Self {
        // TODO
        Self {
            submodules: vec![],
            main_dirs: vec![],
            test_dirs: vec![],
        }
    }
    pub(crate) fn resolve(&self) -> Self {
        Self {
            submodules: self.submodules.clone(),
            main_dirs: self.main_dirs.clone(),
            test_dirs: self.test_dirs.clone(),
        }
    }
}

#[derive(Clone)]
pub struct FullNode {
    pub id: NodeIdentifier,
    pub md: MD,
}

impl FullNode {
    pub fn is_maven_module(&self) -> bool {
        self.md.status.contains(SemFlag::IsMavenModule)
    }

    pub fn hold_maven_submodule(&self) -> bool {
        self.md.status.contains(SemFlag::HoldMavenSubModule)
    }
}

impl hyperast::tree_gen::Accumulator for MavenModuleAcc {
    type Node = (LabelIdentifier, FullNode);
    fn push(&mut self, (name, full_node): Self::Node) {
        let s = full_node.md.status - SemFlag::IsMavenModule;
        assert!(!s.contains(SemFlag::IsMavenModule));
        self.status |= s;
        self.primary.children.push(full_node.id);
        self.primary.children_names.push(name);
        self.primary.metrics.acc(full_node.md.metrics);
    }
}

impl Accumulator for MavenModuleAcc {
    type Unlabeled = FullNode;
}
