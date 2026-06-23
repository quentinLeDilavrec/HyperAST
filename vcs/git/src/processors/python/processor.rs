use std::collections::HashMap;
use std::iter::Peekable;
use std::path::Components;

use git2::{Oid, Repository};

use hyperast::store::nodes::legion::subtree_builder;
use hyperast::tree_gen::add_md_precomp_queries;

use crate::_auto_configured_line_break;
use crate::git::BasicGitObject;
use crate::preprocessed::{CommitBuilder, RepositoryProcessor};
use crate::processing::ParametrizedProcessorHandle as PPHandle;
use crate::processing::erased::ParametrizedCommitProcTyped as _;
use crate::processing::erased::ParametrizedCommitProcessorHandle as PCPHandle;
use crate::processing::erased::PreparedCommitProc;
use crate::processing::{CacheHolding, ObjectName};
use crate::processors::Query;
use crate::processors::prepare_dir_exploration;
use crate::{Processor, StackEle};

use super::SimpleStores;
use super::{Parameter, PythonAcc};
use hyperast_gen_ts_python::legion as python_gen;
use hyperast_gen_ts_python::{TStore, Type};

type Handle = PPHandle<PythonProc>;

pub struct PythonProcessor<'repo, 'prepro, 'd, 'c, Acc> {
    repository: &'repo Repository,
    prepro: &'prepro mut RepositoryProcessor,
    stack: Vec<StackEle<Acc>>,
    // TODO reenable
    pub dir_path: &'d mut Peekable<Components<'c>>,
    handle: Handle,
}

type PythonProcessorHolder = crate::processing::ProcessorHolder<PythonProc>;

impl<'repo, 'prepro, 'd, 'c, Acc: From<String>> PythonProcessor<'repo, 'prepro, 'd, 'c, Acc> {
    pub(crate) fn prepare(
        repository: &'repo Repository,
        prepro: &'prepro mut RepositoryProcessor,
        dir_path: &'d mut Peekable<Components<'c>>,
        name: &ObjectName,
        oid: git2::Oid,
        handle: Handle,
    ) -> Self {
        let tree = repository.find_tree(oid).unwrap();
        let prepared = prepare_dir_exploration(&tree).collect::<Vec<_>>();
        let name = name.try_into().unwrap();
        let stack = vec![StackEle::new(oid, prepared, Acc::from(name))];
        Self {
            stack,
            repository,
            prepro,
            dir_path,
            handle,
        }
    }
}

impl<'repo, 'b, 'd, 'c> Processor<PythonAcc> for PythonProcessor<'repo, 'b, 'd, 'c, PythonAcc> {
    fn pre(&mut self, current_object: BasicGitObject) {
        log::trace!("pre: {:?}", current_object.name.try_str().unwrap_or(""));
        let oid = current_object.oid;
        let name = current_object.name;
        if current_object.kind == git2::ObjectType::Tree {
            self.handle_tree_cached(oid, name);
            return;
        }
        if super::file_sys::matches(&name) {
            let acc = &mut self.stack.last_mut().unwrap().acc;
            self.prepro
                .help_handle_python_file(acc, oid, &name, self.repository, self.handle)
                .unwrap();
        } else {
            log::debug!("not a python source file {:?}", name.try_str());
        }
    }

    fn post(&mut self, oid: Oid, acc: PythonAcc) -> Option<super::FullNode> {
        let name = &acc.primary.name;
        let key = (oid, name.as_bytes().into());
        let name = self.prepro.get_or_insert_label(name);
        let processor_map = &mut self.prepro.processing_systems;
        let holder = processor_map.commit_proc_mut::<PythonProcessorHolder>();
        let proc = holder.with_parameters_mut(self.handle);
        let full_node = make(acc, self.prepro.main_stores.mut_with_ts(), proc);
        proc.cache.object_map.insert(key, full_node.clone());
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
        w.push(name, full_node.clone());
        None
    }

    fn stack(&mut self) -> &mut Vec<StackEle<PythonAcc>> {
        &mut self.stack
    }
}

impl<'repo, 'prepro, 'd, 'c> PythonProcessor<'repo, 'prepro, 'd, 'c, PythonAcc> {
    fn handle_tree_cached(&mut self, oid: Oid, name: ObjectName) {
        if let Some(s) = self.dir_path.peek() {
            // there is a specific dir we want to analyze
            let other = std::ffi::OsStr::as_encoded_bytes(s.as_os_str());
            if name.as_bytes().eq(other) {
                log::trace!("found next dir {}", name.try_str().unwrap());
                // match, consume the path component and make the next StackEle
                self.dir_path.next();
                self.stack.last_mut().expect("never empty").cs.clear();
                let tree = self.repository.find_tree(oid).unwrap();
                let prepared = prepare_dir_exploration(&tree).collect::<Vec<_>>();
                let acc = PythonAcc::new(name.try_into().unwrap());
                self.stack.push(StackEle::new(oid, prepared, acc));
            } else {
                log::trace!("ignoring {}", name.try_str().unwrap());
            }
            return;
        }
        let processor_map = &mut self.prepro.processing_systems;
        let holder = processor_map.commit_proc_mut::<PythonProcessorHolder>();
        let proc = holder.with_parameters_mut(self.handle);
        if let Some(already) = proc.cache.object_map.get(&(oid, name.clone())) {
            // reinit already computed node for post order
            let full_node = already.clone();
            let w = &mut self.stack.last_mut().unwrap().acc;
            let name = self.prepro.intern_object_name(&name);
            assert!(!w.primary.children_names.contains(&name));
            w.push(name, full_node);
        } else {
            log::debug!("tree {:?}", name.try_str());
            let tree = self.repository.find_tree(oid).unwrap();
            let prepared = prepare_dir_exploration(&tree).collect::<Vec<_>>();
            let acc = PythonAcc::new(name.try_into().unwrap());
            self.stack.push(StackEle::new(oid, prepared, acc));
        }
    }
}

pub(crate) struct PythonProc {
    parameter: Parameter,
    query: Option<Query>,
    cache: super::caches::Python,
    commits: HashMap<git2::Oid, crate::Commit>,
}

impl From<Parameter> for PythonProc {
    fn from(t: Parameter) -> Self {
        let query = t.query.as_ref().map(|q| {
            use hyperast_tsquery::ArrayStr;
            Query::new(q.iter(), hyperast_gen_ts_python::language())
        });
        PythonProc {
            parameter: t,
            query,
            cache: Default::default(),
            commits: Default::default(),
        }
    }
}

impl PartialEq<PythonProc> for Parameter {
    fn eq(&self, other: &PythonProc) -> bool {
        self == &other.parameter
    }
}

impl crate::processing::erased::CommitProc for PythonProc {
    fn prepare_processing_at_path<'repo>(
        &self,
        repository: &'repo Repository,
        commit_builder: CommitBuilder,
        path: std::path::PathBuf,
        handle: PCPHandle,
    ) -> Box<dyn PreparedCommitProc + 'repo> {
        Box::new(PreparedPythonCommitProc {
            repository,
            commit_builder,
            dir_path: path,
            handle,
        })
    }

    fn get_commit(&self, commit_oid: git2::Oid) -> Option<&crate::Commit> {
        self.commits.get(&commit_oid)
    }

    fn commit_count(&self) -> usize {
        self.commits.len()
    }

    fn get_precomp_query(&self) -> Option<hyperast_tsquery::ZeroSepArrayStr> {
        dbg!(&self.parameter.query);
        self.parameter.query.clone()
    }
}

struct PreparedPythonCommitProc<'repo> {
    repository: &'repo Repository,
    commit_builder: CommitBuilder,
    dir_path: std::path::PathBuf,
    pub(crate) handle: PCPHandle,
}

impl<'repo> PreparedCommitProc for PreparedPythonCommitProc<'repo> {
    fn process(
        self: Box<PreparedPythonCommitProc<'repo>>,
        prepro: &mut RepositoryProcessor,
    ) -> hyperast::store::defaults::NodeIdentifier {
        let mut dir_path = self.dir_path.components().peekable();
        let name = ObjectName::from(b"");
        // TODO check parameter in self to know it is a recursive module search
        let root_full_node = prepro.handle_python_directory(
            self.repository,
            &mut dir_path,
            &name,
            self.commit_builder.tree_oid(),
            self.handle.try_into().unwrap(),
        );
        let h = prepro
            .processing_systems
            .commit_proc_mut::<PythonProcessorHolder>();
        let handle = self.handle.try_into().unwrap();
        let oid = self.commit_builder.commit_oid();
        let commit = self.commit_builder.finish(root_full_node.id);
        h.with_parameters_mut(handle).commits.insert(oid, commit);
        root_full_node.id
    }
}

impl CacheHolding<super::caches::Python> for PythonProc {
    fn get_caches_mut(&mut self) -> &mut super::caches::Python {
        &mut self.cache
    }
    fn get_caches(&self) -> &super::caches::Python {
        &self.cache
    }
}

impl RepositoryProcessor {
    pub(crate) fn handle_python_blob(
        &mut self,
        oid: Oid,
        name: &ObjectName,
        repository: &Repository,
        parameters: Handle,
    ) -> Result<super::FullNode, crate::ParseErr> {
        self.processing_systems
            .caching_blob_handler::<PythonProc>()
            .handle2(oid, repository, &name, parameters, |c, n, t| {
                let holder = c.commit_proc_mut::<PythonProcessorHolder>();
                let proc = holder.with_parameters_mut(parameters);
                let stores = self.main_stores.mut_with_ts::<TStore>();
                let r = handle_python_blob_aux(n, t, proc, stores)
                    .map_err(|_| crate::ParseErr::IllFormed)?;
                self.parsing_time += r.parsing_time;
                self.processing_time += r.processing_time;
                log::info!(
                    "parsing, processing, n, f: {} {} {} {}",
                    self.parsing_time.as_secs(),
                    self.processing_time.as_secs(),
                    proc.cache.md_cache.len(),
                    proc.cache.object_map.len()
                );
                Ok(r.node)
            })
    }

    pub(crate) fn help_handle_python_file(
        &mut self,
        parent: &mut PythonAcc,
        oid: Oid,
        name: &ObjectName,
        repository: &Repository,
        parameters: Handle,
    ) -> Result<(), crate::ParseErr> {
        let full_node = self.handle_python_blob(oid, name, repository, parameters)?;
        let name = self.intern_object_name(name);
        assert!(!parent.primary.children_names.contains(&name));

        parent.push(name, full_node);
        Ok(())
    }

    pub(crate) fn handle_python_directory<'b, 'd: 'b>(
        &mut self,
        repository: &Repository,
        dir_path: &'b mut Peekable<Components<'d>>,
        name: &ObjectName,
        oid: git2::Oid,
        handle: Handle,
    ) -> super::FullNode {
        PythonProcessor::<PythonAcc>::prepare(repository, self, dir_path, name, oid, handle)
            .process()
    }
}

fn handle_python_blob_aux(
    n: &ObjectName,
    t: &[u8],
    proc: &mut PythonProc,
    stores: &mut SimpleStores,
) -> Result<crate::utils::SuccessProcessing<super::FullNode>, crate::utils::FailedParsing> {
    let lb = _auto_configured_line_break(t);
    let dedup = &mut proc.cache.dedup;
    use hyperast::tree_gen::extra_pattern_precomp::PatternPrecompExtra;
    use hyperast_gen_ts_python::legion::Acc;
    use python_gen::PythonTreeGen;
    if let Some(more) = &proc.query {
        let md_cache = std::mem::take(&mut proc.cache.md_cache);
        let more = &more.0;
        let more: hyperast_tsquery::PreparedQuerying<_, TStore, Acc> = more.into();
        let mut extra = PatternPrecompExtra::<_, Acc, _>::with_cache(more, md_cache);
        let mut tree_gen = PythonTreeGen::new(stores, &mut extra).set_line_break(lb);
        let r = super::handle_python_file(tree_gen.with_dedup(dedup), n, t)
            .map(|x| x.map(|x| x.into()));
        proc.cache.md_cache = extra.md_cache;
        r
    } else {
        let mut tree_gen = PythonTreeGen::bare(stores).set_line_break(lb);
        super::handle_python_file(tree_gen.with_dedup(dedup), n, t).map(|x| x.map(|x| x.into()))
    }
}

fn make(acc: PythonAcc, stores: &mut SimpleStores, proc: &mut PythonProc) -> super::FullNode {
    use hyperast::hashed::IndexingHashBuilder as _;
    use hyperast::hashed::MetaDataHashsBuilder as _;
    use hyperast::store::nodes::legion::eq_node;
    use hyperast::types::ETypeStore as _;
    use hyperast::types::LabelStore;

    let node_store = &mut stores.node_store;
    let label_store = &mut stores.label_store;
    let kind = Type::Directory;
    let interned_kind = TStore::intern(kind);
    let label_id = label_store.get_or_insert(acc.primary.name.clone());

    let primary = acc
        .primary
        .map_metrics(|m| m.finalize(&interned_kind, &label_id));
    let hashable = primary.metrics.hashs.most_discriminating();
    let eq = eq_node(&Type::Directory, Some(&label_id), &primary.children);
    let md_cache = &mut proc.cache.md_cache;
    let dedup_cache = &mut proc.cache.dedup.0;
    let insertion = node_store
        .inner
        .prepare_insertion(dedup_cache, &hashable, eq);

    // Guard to avoid computing metadata for an already present subtree
    if let Some(id) = insertion.occupied_id() {
        // different git object but same name and content (that we processed)
        // NOTE we do not necessarily process every git object, avoid dead weight.
        let metrics = primary.metrics.map_hashs(|h| h.build());
        let precomp_queries = acc.precomp_queries;
        return super::FullNode {
            id,
            metrics,
            precomp_queries,
        };
    }

    let mut dyn_builder = subtree_builder::<TStore>(interned_kind);

    let children_is_empty = primary.children.is_empty();

    let metrics = primary.persist(&mut dyn_builder, interned_kind, label_id);
    let metrics = metrics.map_hashs(|h| h.build());
    let hashs = metrics.add_md_metrics(&mut dyn_builder, children_is_empty);
    hashs.persist(&mut dyn_builder);

    let precomp_queries = acc.precomp_queries;
    add_md_precomp_queries(&mut dyn_builder, precomp_queries.0);

    let vacant = insertion.vacant();
    let node_id = vacant.insert_built(dyn_builder.build());

    md_cache.insert(node_id, precomp_queries.clone());

    super::FullNode {
        id: node_id,
        metrics,
        precomp_queries,
    }
}
