use std::iter::Peekable;
use std::path::Components;

use git2::{Oid, Repository};

use hyperast::hashed::{IndexingHashBuilder, MetaDataHashsBuilder};
use hyperast::store::nodes::legion::dyn_builder::EntityBuilder;
use hyperast::store::nodes::legion::eq_node;
use hyperast::tree_gen::add_md_precomp_queries;
use hyperast::types::ETypeStore as _;
use hyperast::types::LabelStore;

use crate::Processor;
use crate::StackEle;
use crate::git::BasicGitObject;
use crate::preprocessed::RepositoryProcessor;
use crate::processing::erased::ParametrizedCommitProc2;
use crate::processing::erased::ParametrizedCommitProcessorHandle as PCPHandle;
use crate::processing::{CacheHolding, InFiles, ObjectName};

use super::{FileSysAcc, Parameter};
use super::{FileSysProc, FullNode};

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
        log::trace!("pre: {:?}", current_dir.name.try_str().unwrap_or(""));
        let oid = current_dir.oid;
        let name = current_dir.name;
        if current_dir.kind == git2::ObjectType::Tree {
            self.handle_tree_cached(name, oid);
            return;
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
            .commit_proc_mut::<FileSysProcessorHolder>()
            .with_parameters42_mut(self.handle.try_into().unwrap())
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
            } else {
                dbg!(name.try_str().unwrap());
            } // otherwise, ignore
            return;
        }
        let proc = (self.prepro.processing_systems)
            .commit_proc_mut::<FileSysProcessorHolder>()
            .with_parameters42_mut(self.handle.try_into().unwrap());
        if let Some(already) = proc.get_caches_mut().object_map.get(&oid) {
            // reinit already computed node for post order
            dbg!(name.try_str().unwrap());
            let full_node = already.clone();
            let w = &mut self.stack.last_mut().unwrap().acc;
            let name = self.prepro.intern_object_name(name);
            assert!(!w.primary.children_names.contains(&name));
            w.push(name, full_node);
            return;
        }
        let tree = self.repository.find_tree(oid).unwrap();
        let prepared = prepared_exploration(tree);
        log::info!("file_sys tree {:?} {}", name.try_str(), prepared.len());
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
            //         PPHandle(self.handle.1, std::marker::PhantomData),
            //     )
            //     .unwrap();
            return Ok(());
        }
        #[cfg(feature = "cpp")]
        if crate::processors::cpp::selection::matches(&name) {
            let parent = &mut self.stack.last_mut().unwrap().acc;
            let name: &ObjectName = &name;
            let repository: &Repository = self.repository;
            let p = self.handle.try_into().unwrap();
            let full_node = self.prepro.handle_cpp_blob(oid, name, repository, p)?;
            let name = self.prepro.intern_object_name(name);
            assert!(!parent.primary.children_names.contains(&name));
            parent.push(name, full_node);
            return Ok(());
        }
        #[cfg(feature = "java")]
        if crate::processors::java::selection::matches(&name) {
            let w = &mut self.stack.last_mut().unwrap().acc;
            let name: &ObjectName = &name;
            let repository: &Repository = &self.repository;
            let p = self.handle.try_into().unwrap();
            let full_node = self.prepro.handle_java_blob(oid, name, repository, p)?;
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
            let p = self.handle.try_into().unwrap();
            let full_node = self.prepro.handle_python_blob(oid, name, repository, p)?;
            let name = self.prepro.intern_object_name(name);
            assert!(!parent.primary.children_names.contains(&name));
            parent.push(name, full_node);
            return Ok(());
        }
        log::info!("not a known source file {:?}", name.try_str());
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

impl PartialEq<Parameter> for FileSysProc {
    fn eq(&self, other: &Parameter) -> bool {
        self.parameter == *other
    }
}

impl PartialEq<()> for FileSysProc {
    fn eq(&self, _other: &()) -> bool {
        true
    }
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
    // Guard to avoid computing metadata for an already present subtree
    if let Some(id) = insertion.occupied_id() {
        // different git object but same name and content (that we processed)
        // NOTE we do not necessarily process every git object, avoid dead weight.
        let metrics = primary.metrics.map_hashs(|h| h.build());
        let n = stores.node_store.resolve(id);
        use hyperast::store::nodes::compo::Precomp;
        let precomp_queries = n
            .get_component::<Precomp<u16>>()
            .map(|x| x.0)
            .map(super::PrecompQueries)
            .unwrap_or(super::PrecompQueries::full());
        return FullNode {
            id,
            metrics,
            precomp_queries,
        };
    }

    log::info!("make mm {} {}", &primary.name, primary.children.len());

    let mut dyn_builder = EntityBuilder::with_lang(super::types::Lang);

    let children_is_empty = primary.children.is_empty();

    let metrics = primary.persist(&mut dyn_builder, interned_kind, label_id);
    let metrics = metrics.map_hashs(|h| h.build());
    let hashs = metrics.add_md_metrics(&mut dyn_builder, children_is_empty);
    hashs.persist(&mut dyn_builder);

    let precomp_queries = acc.precomp_queries;
    add_md_precomp_queries(&mut dyn_builder, precomp_queries.0);

    let vacant = insertion.vacant();
    let id = vacant.insert_built(dyn_builder.build());

    FullNode {
        id,
        metrics,
        precomp_queries,
    }
}
