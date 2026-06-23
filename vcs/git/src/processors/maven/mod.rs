//! Handles the Maven build system

mod commit_proc;
pub mod iter_maven_module;
mod pom;
mod processor;
mod scripting;

pub use iter_maven_module::IterMavenModules2;

use pom::handle_pom_file;
pub use pom::{PomParameter, PomProc};

use std::fmt::Debug;
use std::path::PathBuf;

use enumset::EnumSet;

use hyperast::store::defaults::{LabelIdentifier, NodeIdentifier};
use hyperast::tree_gen::SubTreeMetrics;
use hyperast_gen_ts_xml::TStore;

use crate::Accumulator;
use crate::BasicDirAcc;
use crate::DefaultMetrics;
use crate::processing::ParametrizedProcessorHandle as PPHandle;
use crate::processing::caches::OidMap;
use crate::processors::java::JavaProc;

pub type SimpleStores = hyperast::store::SimpleStores<TStore>;
type MavenProcessorHolder = crate::processing::ProcessorHolder<MavenProc>;

#[doc(hidden)] // only used internally to kind of compose snapshots
pub use processor::make;

pub struct MavenProc {
    parameter: Parameter,
    cache: crate::processing::caches::Maven,
    commits: OidMap<crate::Commit>,
}

#[derive(Clone, Debug)]
pub struct FullNode {
    pub id: hyperast::store::defaults::NodeIdentifier,
    pub metrics: crate::DefaultMetrics,
    pub precomp_queries: super::PrecompQueries,
    pub(crate) status: EnumSet<SemFlag>,
    // TODO even if almost everyone is using the defaults, use it
    pub(crate) ana: MavenPartialAnalysis,
}

impl crate::preprocessed::IdHolder for FullNode {
    type Id = hyperast::store::defaults::NodeIdentifier;
    fn id(&self) -> Self::Id {
        self.id
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct Parameter {
    // pub(crate) query: Option<std::sync::Arc<[String]>>,
    // pub(crate) prepo: Option<hyperast_scripting::Prepro>,
    pub java_handle: PPHandle<JavaProc>,
    pub pom_handle: PPHandle<PomProc>,
}

#[derive(Debug, Clone)]
pub struct POM {
    pub compressed_node: NodeIdentifier,
    pub metrics: DefaultMetrics,
    submodules: Vec<String>,
    source_dirs: Vec<String>,
    test_source_dirs: Vec<String>,
}

pub struct MavenModuleAcc {
    pub(crate) primary: crate::DirPrimary,
    pub(crate) ana: MavenPartialAnalysis,
    pub(crate) sub_modules: Option<Vec<PathBuf>>,
    pub(crate) main_dirs: Option<Vec<PathBuf>>,
    pub(crate) test_dirs: Option<Vec<PathBuf>>,
    pub(crate) status: EnumSet<SemFlag>,
    pub(crate) scripting_acc: Option<hyperast::scripting::Acc>,
    pub(crate) precomp_queries: super::PrecompQueries,
}

#[derive(enumset::EnumSetType, Debug)]
pub enum SemFlag {
    IsMavenModule,
    HoldMainFolder,
    HoldTestFolder,
    HoldMavenSubModule,
}

impl crate::processing::CachesHolding for MavenProc {
    type Caches = crate::processing::caches::Maven;
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
            precomp_queries: super::PrecompQueries::default(),
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
        self.primary.metrics.acc(full_node.metrics);
    }
    pub(crate) fn push_source_directory(
        &mut self,
        name: LabelIdentifier,
        full_node: super::FullNode,
    ) {
        self.status |= SemFlag::HoldMainFolder;
        self.primary.children.push(full_node.id);
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
        full_node: super::FullNode,
    ) {
        self.status |= SemFlag::HoldTestFolder;
        self.primary.children.push(full_node.id);
        self.primary.children_names.push(name);
        self.primary.metrics.acc(SubTreeMetrics {
            line_count: 0,
            ..full_node.metrics
        });
        // TODO ana
        // full_node.2.acc(&Type::Directory, &mut self.ana);
    }
}

/// The maven scheme https://maven.apache.org/guides/introduction/introduction-to-the-standard-directory-layout.html ,
/// made of nested maven modules.
/// Each maven module has a config file (often a pom.xml),
/// a src/main/java/ directory that contains production code for java,
/// a src/test/java/ directory that contains tests for java,
/// a src/test/resources/ directory that contains resources that should not be compiled (most of the time),
/// ... (see ref.)
// TODO finish implementation
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

impl FullNode {
    pub fn is_maven_module(&self) -> bool {
        self.status.contains(SemFlag::IsMavenModule)
    }

    pub fn hold_maven_submodule(&self) -> bool {
        self.status.contains(SemFlag::HoldMavenSubModule)
    }
}

impl hyperast::tree_gen::Accumulator for MavenModuleAcc {
    type Node = (LabelIdentifier, FullNode);
    fn push(&mut self, (name, full_node): Self::Node) {
        let s = full_node.status - SemFlag::IsMavenModule;
        assert!(!s.contains(SemFlag::IsMavenModule));
        self.status |= s;
        self.primary.children.push(full_node.id);
        self.primary.children_names.push(name);
        self.primary.metrics.acc(full_node.metrics);
    }
}

impl Accumulator for MavenModuleAcc {
    type Unlabeled = FullNode;
}
