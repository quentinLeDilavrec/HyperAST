//! Handles the Make build system

pub mod make_processor;
pub mod makefile;

use std::{fmt::Debug, path::PathBuf};

use hyperast::store::defaults::{LabelIdentifier, NodeIdentifier};
use hyperast_gen_ts_xml::legion::XmlTreeGen;

use crate::processing::ObjectName;
use crate::processing::ParametrizedProcessorHandle as PPHandle;
use crate::{Accumulator, BasicDirAcc, DefaultMetrics};

pub(crate) use make_processor::MakeProc;

use super::FullNode;

pub type SimpleStores = hyperast::store::SimpleStores<hyperast_gen_ts_xml::TStore>;

#[derive(Clone, PartialEq, Eq)]
pub struct Parameter {
    pub(crate) makefile_handle: PPHandle<makefile::MakefileProc>,
    pub(crate) cpp_handle: PPHandle<super::cpp::CppProc>,
}

#[derive(Debug, Clone)]
pub struct MakeFile {
    pub compressed_node: NodeIdentifier,
    pub metrics: DefaultMetrics,
    submodules: Vec<String>,
    source_dirs: Vec<String>,
    test_source_dirs: Vec<String>,
}

/// The make scheme,
/// It contains a Makefile and different directories, often src/ or lib/, tests/ or tests/, and also third-party/ docs/ script/,
/// but it is mostly community and programming language dependent.
/// TODO properly implement it
pub struct MakeModuleAcc {
    pub(crate) primary: BasicDirAcc<NodeIdentifier, LabelIdentifier, DefaultMetrics>,
    pub(crate) sub_modules: Option<Vec<PathBuf>>,
    pub(crate) main_dirs: Option<Vec<PathBuf>>,
    pub(crate) test_dirs: Option<Vec<PathBuf>>,
    pub(crate) precomp_queries: super::PrecompQueries,
}

impl hyperast::tree_gen::Accumulator for MakeModuleAcc {
    type Node = (LabelIdentifier, FullNode);
    fn push(&mut self, (name, full_node): Self::Node) {
        self.primary.children.push(full_node.id);
        self.primary.children_names.push(name);
        self.primary.metrics.acc(full_node.metrics);
    }
}

impl Accumulator for MakeModuleAcc {
    type Unlabeled = FullNode;
}

impl From<String> for MakeModuleAcc {
    fn from(name: String) -> Self {
        Self::new(name)
    }
}

impl MakeModuleAcc {
    pub(crate) fn new(name: String) -> Self {
        Self {
            primary: BasicDirAcc::new(name),
            sub_modules: None,
            main_dirs: None,
            test_dirs: None,
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
            primary: BasicDirAcc::new(name),
            sub_modules: (!sub_modules.is_empty()).then_some(sub_modules),
            main_dirs: (!main_dirs.is_empty()).then_some(main_dirs),
            test_dirs: (!test_dirs.is_empty()).then_some(test_dirs),
            precomp_queries: super::PrecompQueries::default(),
        }
    }
}

impl MakeModuleAcc {
    pub(crate) fn push_makefile(&mut self, name: LabelIdentifier, full_node: MakeFile) {
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
        self.primary.children.push(full_node.id);
        self.primary.children_names.push(name);
        self.primary.metrics.acc(full_node.metrics);
    }
    pub(crate) fn push_source_file(&mut self, name: LabelIdentifier, full_node: FullNode) {
        self.primary.children.push(full_node.id);
        self.primary.children_names.push(name);
        self.primary.metrics.acc(full_node.metrics);
    }
    pub(crate) fn push_source_directory(&mut self, name: LabelIdentifier, full_node: FullNode) {
        self.primary.children.push(full_node.id);
        self.primary.children_names.push(name);
        self.primary.metrics.acc(full_node.metrics);
    }
    pub(crate) fn push_test_source_directory(
        &mut self,
        name: LabelIdentifier,
        full_node: FullNode,
    ) {
        self.primary.children.push(full_node.id);
        self.primary.children_names.push(name);
        self.primary.metrics.acc(full_node.metrics);
    }
}

pub(crate) fn handle_makefile_file<'a, E>(
    tree_gen: &mut XmlTreeGen<'a, 'a, E>,
    name: &ObjectName,
    text: &'a [u8],
) -> Result<MakeFile, ()>
where
    E: hyperast::tree_gen::TsExtra<SimpleStores>,
{
    log::trace!("not parsing {} bytes long Makefile", text.len()); // TODO parse the makefile
    let text = b"<proj></proj>";
    let tree = hyperast_gen_ts_xml::tree_sitter_parse(text);
    if tree.root_node().has_error() {
        log::warn!("bad CST");
        log::debug!("{:?}", name.try_str());
        log::debug!("{}", tree.root_node().to_sexp());
        if crate::PROPAGATE_ERROR_ON_BAD_CST_NODE {
            return Err(());
        }
    }
    let (n, _) = tree_gen
        .generate_file(name.as_bytes(), text, tree.walk())
        .into();
    let x = n.local;
    // TODO extract submodules, dependencies and directories. maybe even more ie. artefact id, ...
    let x = MakeFile {
        compressed_node: x.compressed_node,
        metrics: x.metrics,
        submodules: vec![],
        source_dirs: vec![".".to_owned()],
        test_source_dirs: vec!["../tests".to_owned()],
    };
    Ok(x)
}

impl crate::processing::CachesHolding for MakeProc {
    type Caches = crate::processing::caches::Make;
}

impl crate::preprocessed::CommitProcessor<MakeProc> for crate::preprocessed::RepositoryProcessor {
    type Module = crate::processors::FullNode;
    fn handle_module<'b, const RMS: bool>(
        &mut self,
        repository: &git2::Repository,
        dir_path: &'b mut std::iter::Peekable<std::path::Components<'b>>,
        name: &[u8],
        tree_oid: git2::Oid,
    ) -> Self::Module {
        dbg!();
        use crate::processing::ProcessorHolder;
        type CppProcHolder = ProcessorHolder<crate::processors::cpp::CppProc>;
        type MakeProcHolder = ProcessorHolder<crate::processors::make::MakeProc>;
        type MakefileProcHolder = ProcessorHolder<crate::processors::make::makefile::MakefileProc>;

        let t = crate::processors::cpp::Parameter { query: None };
        let h_cpp = self.processing_systems.commit_proc_mut::<CppProcHolder>();
        let cpp_handle = h_cpp.register_param(t);
        let h = self.processing_systems.proc_mut::<MakefileProcHolder>();
        let t = crate::processors::make::makefile::Parameter::default();
        let makefile_handle = h.register_param(t);
        let h = self.processing_systems.commit_proc_mut::<MakeProcHolder>();
        let t = crate::processors::make::Parameter::new(makefile_handle, cpp_handle);
        let handle = h.register_param(t.clone());

        use crate::Processor;
        use crate::processors::make::MakeModuleAcc;
        use crate::processors::make::make_processor::MakeProcessor;
        let root_full_node = MakeProcessor::<true, false, MakeModuleAcc>::prepare(
            repository, self, dir_path, name, tree_oid, handle, t,
        )
        .process();
        root_full_node
    }
}
