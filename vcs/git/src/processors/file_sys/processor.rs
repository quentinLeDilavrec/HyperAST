use std::iter::Peekable;
use std::path::{Components, PathBuf};

use git2::{Oid, Repository};

use hyperast::hashed::{IndexingHashBuilder, MetaDataHashsBuilder};
use hyperast::store::nodes::legion::dyn_builder::EntityBuilder;
use hyperast::store::nodes::legion::eq_node;
use hyperast::types::ETypeStore as _;
use hyperast::types::LabelStore;

use crate::Processor;
use crate::StackEle;
use crate::git::BasicGitObject;
use crate::preprocessed::{CommitBuilder, RepositoryProcessor};
use crate::processing::erased::CommitProcessorHandle;
use crate::processing::erased::ParametrizedCommitProc2;
use crate::processing::erased::ParametrizedCommitProcessor2Handle as PCP2Handle;
use crate::processing::erased::PreparedCommitProc;
use crate::processing::{CacheHolding, InFiles, ObjectName};
use crate::processing::{ParametrizedCommitProcessorHandle as PCPHandle, ProcessorHolder};
use crate::processors::cpp as cpp_processor;
use crate::processors::java as java_processor;
use crate::processors::python as python_processor;

use super::{FileSysAcc, FullNode};

pub struct FileSysProcessor<'a, 'b, 'c, const RMS: bool, const FFWD: bool, Acc> {
    prepro: &'b mut RepositoryProcessor,
    repository: &'a Repository,
    stack: Vec<StackEle<Acc>>,
    dir_path: &'c mut Peekable<Components<'c>>,
    handle: PCPHandle,
}

type FileSysProcessorHolder = crate::processing::ProcessorHolder<FileSysProc>;

impl<'a, 'b, 'c, const RMS: bool, const FFWD: bool, Acc: From<String>>
    FileSysProcessor<'a, 'b, 'c, RMS, FFWD, Acc>
{
    pub fn prepare(
        repository: &'a Repository,
        prepro: &'b mut RepositoryProcessor,
        dir_path: &'c mut Peekable<Components<'c>>,
        name: &[u8],
        oid: git2::Oid,
        handle: PCPHandle,
    ) -> Self {
        let tree = repository.find_tree(oid).unwrap();
        let prepared = prepared_exploration(tree);
        let name = std::str::from_utf8(&name).unwrap().to_string();
        let stack = vec![StackEle::new(oid, prepared, name.into())];
        Self {
            stack,
            repository,
            prepro,
            dir_path,
            handle,
        }
    }
}

impl<'a, 'b, 'c, const RMS: bool, const FFWD: bool> Processor<FileSysAcc>
    for FileSysProcessor<'a, 'b, 'c, RMS, FFWD, FileSysAcc>
{
    fn pre(&mut self, current_dir: BasicGitObject) {
        let (oid, name) = match current_dir {
            BasicGitObject::Tree(oid, name) => {
                self.handle_tree_cached(name, oid);
                return;
            }
            BasicGitObject::Blob(oid, name) => (oid, name),
        };
        if FFWD {
            return;
        }
        if self.dir_path.peek().is_some() {
            return;
        }
        self.pre_aux(oid, &name).unwrap();
    }
    fn post(&mut self, oid: Oid, acc: FileSysAcc) -> Option<FullNode> {
        let name = acc.primary.name.clone();
        let full_node = make(acc, self.prepro.main_stores_mut().mut_with_ts());
        let cache = (self.prepro.processing_systems)
            .mut_or_default::<FileSysProcessorHolder>()
            .with_parameters_mut(self.handle.1)
            .get_caches_mut();
        cache.object_map.insert(oid, full_node.clone());

        let name = self.prepro.main_stores.label_store.get_or_insert(name);
        if self.stack.is_empty() {
            return Some(full_node);
        }
        let w = &mut self.stack.last_mut().unwrap().acc;
        assert!(
            !w.primary.children_names.contains(&name),
            "{:?} {:?}",
            w.primary.children_names,
            name
        );
        w.push(name, full_node);
        None
    }

    fn stack(&mut self) -> &mut Vec<StackEle<FileSysAcc>> {
        &mut self.stack
    }
}

impl<'a, 'b, 'c, const RMS: bool, const FFWD: bool>
    FileSysProcessor<'a, 'b, 'c, RMS, FFWD, FileSysAcc>
{
    fn handle_tree_cached(&mut self, name: ObjectName, oid: Oid) {
        if let Some(s) = self.dir_path.peek() {
            // there is a specific dir we want to analyze
            let other = std::ffi::OsStr::as_encoded_bytes(s.as_os_str());
            if name.as_bytes().eq(other) {
                // match, consume the path component and make the next StackEle
                self.dir_path.next();
                self.stack.last_mut().expect("never empty").cs.clear();
                let tree = self.repository.find_tree(oid).unwrap();
                let prepared = prepared_exploration(tree);
                let acc = FileSysAcc::new(name.try_into().unwrap());
                self.stack.push(StackEle::new(oid, prepared, acc));
            } // otherwise, ignore
            return;
        }
        let proc = (self.prepro.processing_systems)
            .mut_or_default::<FileSysProcessorHolder>()
            .with_parameters_mut(self.handle.1);
        if let Some(already) = proc.get_caches_mut().object_map.get(&oid) {
            // reinit already computed node for post order
            let full_node = already.clone();
            let w = &mut self.stack.last_mut().unwrap().acc;
            let name = self.prepro.intern_object_name(name);
            assert!(!w.primary.children_names.contains(&name));
            w.push(name, full_node);
            return;
        }
        log::debug!("file_sys tree {:?}", name.try_str());
        let tree = self.repository.find_tree(oid).unwrap();
        let prepared = prepared_exploration(tree);
        let acc = FileSysAcc::new(name.try_into().unwrap());
        self.stack.push(StackEle::new(oid, prepared, acc));
    }

    fn pre_aux(&mut self, oid: Oid, name: &ObjectName) -> Result<(), crate::ParseErr> {
        if crate::processing::file_sys::MakeFile::matches(&name) {
            // TODO reintroduce, but without any build-sys-level analysis
            // self.prepro
            //     .help_handle_makefile(
            //         oid,
            //         &mut self.stack.last_mut().unwrap().acc,
            //         name,
            //         &self.repository,
            //         PCP2Handle(self.handle.1, std::marker::PhantomData),
            //     )
            //     .unwrap();
            return Ok(());
        }
        #[cfg(feature = "cpp")]
        if crate::processing::file_sys::Cpp::matches(&name) {
            let parent = &mut self.stack.last_mut().unwrap().acc;
            let name: &ObjectName = &name;
            let repository: &Repository = self.repository;
            let p = PCP2Handle(self.handle.1, std::marker::PhantomData);
            let (full_node,) = self.prepro.handle_cpp_blob(oid, name, repository, p)?;
            let name = self.prepro.intern_object_name(name);
            assert!(!parent.primary.children_names.contains(&name));
            parent.push(name, full_node);
            return Ok(());
        }
        #[cfg(feature = "java")]
        if crate::processing::file_sys::Java::matches(&name) {
            let w = &mut self.stack.last_mut().unwrap().acc;
            let name: &ObjectName = &name;
            let repository: &Repository = &self.repository;
            let p = PCP2Handle(self.handle.1, std::marker::PhantomData);
            let (full_node,) = self.prepro.handle_java_blob(oid, name, repository, p)?;
            let name = self.prepro.intern_object_name(name);
            assert!(!w.primary.children_names.contains(&name));
            w.push(name, full_node);
            return Ok(());
        }
        #[cfg(feature = "python")]
        if crate::processors::python::file_sys::Python::matches(&name) {
            let parent = &mut self.stack.last_mut().unwrap().acc;
            let name: &ObjectName = &name;
            let repository: &Repository = self.repository;
            let p = PCP2Handle(self.handle.1, std::marker::PhantomData);
            let (full_node, _precomp) = self.prepro.handle_python_blob(oid, name, repository, p)?;
            let name = self.prepro.intern_object_name(name);
            assert!(!parent.primary.children_names.contains(&name));
            parent.push(name, full_node);
            return Ok(());
        }
        log::debug!("not a known source file {:?}", name.try_str());
        Ok(())
    }
}

// no need to modify order of objects
fn prepared_exploration(tree: git2::Tree<'_>) -> Vec<BasicGitObject> {
    tree.iter()
        .map(TryInto::try_into)
        .filter_map(|x| x.ok())
        .collect()
}

pub(crate) fn make(acc: FileSysAcc, stores: &mut super::SimpleStores) -> FullNode {
    let kind = super::Type::Directory;
    let interned_kind = super::TStore::intern(kind);
    let label_id = stores.label_store.get_or_insert(acc.primary.name.clone());

    let primary = acc
        .primary
        .map_metrics(|m| m.finalize(&interned_kind, &label_id));

    let hashable = primary.metrics.hashs.most_discriminating();

    let eq = eq_node(&interned_kind, Some(&label_id), &primary.children);

    assert_eq!(primary.children_names.len(), primary.children.len());

    let insertion = stores.node_store.prepare_insertion(&hashable, eq);
    if let Some(id) = insertion.occupied_id() {
        let metrics = primary
            .metrics
            .map_hashs(|h| MetaDataHashsBuilder::build(h));
        return FullNode { id, metrics };
    }

    log::info!("make mm {} {}", &primary.name, primary.children.len());

    let mut dyn_builder = EntityBuilder::with_lang(super::types::Lang);

    let children_is_empty = primary.children.is_empty();

    let metrics = primary.persist(&mut dyn_builder, interned_kind, label_id);
    let metrics = metrics.map_hashs(|h| h.build());
    let hashs = metrics.add_md_metrics(&mut dyn_builder, children_is_empty);
    hashs.persist(&mut dyn_builder);

    let vacant = insertion.vacant();
    let id = vacant.insert_built(dyn_builder.build());

    FullNode { id, metrics }
}

#[derive(Clone, PartialEq, Eq)]
pub struct Parameter {
    pub(crate) cpp_handle: PCP2Handle<cpp_processor::CppProc>,
    pub(crate) java_handle: PCP2Handle<java_processor::JavaProc>,
    pub(crate) python_handle: PCP2Handle<python_processor::PythonProc>,
}

impl From<Parameter> for FileSysProc {
    fn from(val: Parameter) -> Self {
        FileSysProc {
            parameter: val,
            cache: Default::default(),
            commits: Default::default(),
        }
    }
}

impl PartialEq<FileSysProc> for Parameter {
    fn eq(&self, other: &FileSysProc) -> bool {
        self == &other.parameter
    }
}

pub(crate) struct FileSysProc {
    parameter: Parameter,
    cache: super::caches::FileSys,
    commits: std::collections::HashMap<git2::Oid, crate::Commit>,
}

impl PartialEq<Parameter> for FileSysProc {
    fn eq(&self, other: &Parameter) -> bool {
        self.parameter == *other
    }
}

struct PreparedMakeCommitProc<'repo> {
    repository: &'repo git2::Repository,
    commit_builder: CommitBuilder,
    pub(crate) handle: PCPHandle,
}
impl<'repo> PreparedCommitProc for PreparedMakeCommitProc<'repo> {
    fn process(
        self: Box<PreparedMakeCommitProc<'repo>>,
        prepro: &mut RepositoryProcessor,
    ) -> hyperast::store::defaults::NodeIdentifier {
        let dir_path = PathBuf::from("");
        let mut dir_path = dir_path.components().peekable();
        let name = b"";
        // TODO check parameter in self to know it is a recursive module search
        let root_full_node = FileSysProcessor::<true, false, FileSysAcc>::prepare(
            self.repository,
            prepro,
            &mut dir_path,
            name,
            self.commit_builder.tree_oid(),
            self.handle,
        )
        .process();
        let h = prepro
            .processing_systems
            .mut_or_default::<FileSysProcessorHolder>();
        let handle = self.handle;
        let commit_oid = self.commit_builder.commit_oid();
        let commit = self.commit_builder.finish(root_full_node.id);
        h.with_parameters_mut(handle.1)
            .commits
            .insert(commit_oid, commit);
        root_full_node.id
    }
}

impl crate::processing::erased::CommitProc for FileSysProc {
    fn prepare_processing<'repo>(
        &self,
        repository: &'repo git2::Repository,
        commit_builder: CommitBuilder,
        handle: PCPHandle,
    ) -> Box<dyn PreparedCommitProc + 'repo> {
        Box::new(PreparedMakeCommitProc {
            repository,
            commit_builder,
            handle,
        })
    }

    fn get_commit(&self, commit_oid: git2::Oid) -> Option<&crate::Commit> {
        self.commits.get(&commit_oid)
    }

    fn commit_count(&self) -> usize {
        self.commits.len()
    }

    fn get_lang_handle(&self, lang: &str) -> Option<PCPHandle> {
        dbg!(self.parameter.cpp_handle.0.0);
        if lang.eq_ignore_ascii_case("cpp") {
            Some(PCPHandle(
                CommitProcessorHandle(std::any::TypeId::of::<
                    ProcessorHolder<cpp_processor::CppProc>,
                >()),
                self.parameter.cpp_handle.0,
            ))
        } else if lang.eq_ignore_ascii_case("java") {
            if cfg!(debug_assertions) {
                unimplemented!()
            }
            None
        } else {
            None
        }
    }
}

// impl CommitProcExt for FileSysProc {
//     type Holder = FileSysProcessorHolder;
// }

impl CacheHolding<super::caches::FileSys> for FileSysProc {
    fn get_caches_mut(&mut self) -> &mut super::caches::FileSys {
        &mut self.cache
    }
    fn get_caches(&self) -> &super::caches::FileSys {
        &self.cache
    }
}
