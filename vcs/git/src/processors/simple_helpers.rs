//! Helpers used to process commits and directories.

use std::marker::PhantomData;

use git2::Repository;
use hyperast::store::SimpleStores;
use hyperast::types::{Role, RoleStore};

use crate::preprocessed::{CommitBuilder, RepositoryProcessor};
use crate::processing::ParametrizedProcessorHandle as PPHandle;
use crate::processing::erased::ParametrizedCommitProcTyped as _;
use crate::processing::erased::ParametrizedCommitProcessorHandle as PCPHandle;
use crate::processing::erased::{CommitProc, PreparedCommitProc};
use crate::processing::{CacheHolding, CachesHolding};
use crate::processing::{ObjectName, ProcessorHolder};
use crate::utils::{FailedParsing, SuccessProcessing};

use super::FileNameIdentifier;
use super::FullNode;
use super::prepare_dir_exploration;
use super::{PrecompQueries, ProcessorCache, Query};

/// Interface for an accumulator used in simple language processors.
///
/// Through `FullNode` and `PrecompQueries`, implementing this trait helps support indexed patterns.
pub(crate) trait SimpleAccumulator:
    crate::Accumulator<Unlabeled = FullNode> + From<String>
{
    fn primary(&self) -> &crate::DirPrimary;
    /// Finishes the accumulation, derived data to be compressed and persisted.
    fn finish(self) -> (crate::DirPrimary, PrecompQueries);
    /// pushes `full_node` obtained from a directory with given `name`.
    fn push2(&mut self, name: FileNameIdentifier, full_node: impl Into<FullNode>);
}

/// A trait to access config parameters needed by a simple language processor.
pub(crate) trait SimpleProcParam {
    /// Returns the tree-sitter language to be used for this processor.
    fn ts_language() -> tree_sitter::Language;
    /// Returns `true` if `name` a file name is expected to be processed by this processor.
    fn matches_file_name(name: &ObjectName) -> bool;
    /// Returns `true` if `lang` is the language name expected to be processed by this processor.
    fn matches_lang(lang: &str) -> bool;
    /// Returns the query to be used for this processor.
    ///
    /// If you do not want to index queries, simply return `None`.
    fn query(&self) -> Option<Query>;
}

/// Helper for preprocessing (processing and persisting) a given commit
pub(super) struct PreparedSimpleCommitProc<'repo, Acc, P, TS> {
    repository: &'repo Repository,
    commit_builder: CommitBuilder,
    dir_path: std::path::PathBuf,
    pub(crate) handle: PCPHandle,
    _phantom: PhantomData<(Acc, P, TS)>,
}

impl<'repo, Acc, P, TS> PreparedSimpleCommitProc<'repo, Acc, P, TS> {
    pub(super) fn new(
        repository: &'repo Repository,
        commit_builder: CommitBuilder,
        dir_path: std::path::PathBuf,
        handle: PCPHandle,
    ) -> Self {
        Self {
            repository,
            commit_builder,
            dir_path,
            handle,
            _phantom: PhantomData,
        }
    }
}

impl<'repo, Acc, P, TS> PreparedCommitProc for PreparedSimpleCommitProc<'repo, Acc, P, TS>
where
    Acc: From<String>,
    P: 'static + Send + Sync,
    SimpleProc<P, TS>: CommitProc,
    //
    P: SimpleProcParam,
    Acc: SimpleAccumulator,
    SimpleProc<P, TS>: CommitProc + Send + Sync,
    SimpleProc<P, TS>: CachesHolding<Caches = ProcessorCache>,
    SimpleProc<P, TS>: CacheHolding<ProcessorCache>,
    crate::type_store::TStore: hyperast::store::TyDown<TS>,
    TS: 'static + hyperast::tree_gen::TsEnabledTS,
    TS: RoleStore<IdF = u16, Role = Role>,
    TS::Ty2: hyperast::tree_gen::TsType,
{
    fn process(
        self: Box<PreparedSimpleCommitProc<'repo, Acc, P, TS>>,
        prepro: &mut RepositoryProcessor,
    ) -> hyperast::store::defaults::NodeIdentifier {
        let mut dir_path = self.dir_path.components().peekable();
        let name = ObjectName::from(b"");
        // TODO check parameter in self to know it is a recursive module search
        use crate::Processor as _;
        let root_full_node = SimpleProcessor::<Acc, SimpleProc<P, TS>>::prepare(
            self.repository,
            prepro,
            &mut dir_path,
            &name,
            self.commit_builder.tree_oid(),
            self.handle.try_into().unwrap(),
        )
        .process();
        let h = prepro
            .processing_systems
            .commit_proc_mut::<ProcessorHolder<SimpleProc<P, TS>>>();
        let handle = self.handle.try_into().unwrap();
        let oid = self.commit_builder.commit_oid();
        let commit = self.commit_builder.finish(root_full_node.id);
        h.with_parameters_mut(handle).commits.insert(oid, commit);
        root_full_node.id
    }
}

/// A simple language processor holding processed commits and cached derived data.
///
/// additional features:
/// - indexed tree-sitter queries: enable indexing code patterns,
///     such that, large tree-sitter queries can be efficiently executed,
///     skipping subtrees without mandatory patterns
///
/// Note: If you want more features for your specific language,
/// do not hesitate to duplicate this structs and associated impls.
pub struct SimpleProc<P, TS: 'static> {
    /// Parameters used to configure the processor.
    /// Must implement `PartialEq` against specific `SimpleProc`
    /// to distinguish between different processing contexts
    ///
    /// additional features:
    ///  - indexed tree-sitter queries:
    ///     - `P` is expected to implement [`SimpleProcParam`]
    ///       to provide the raw query string and tree-sitter language.
    pub(super) parameter: P,
    /// holds the tree-sitter query containing the code pattern to index (feat: indexed tree-sitter queries)
    pub(super) query: Option<Query>,
    /// Caches for incremental processing of git Objects and derived data.
    ///
    /// additional features:
    ///  - indexed tree-sitter queries:
    ///     - `ProcessorCache` is configured by default to cache code pattern indexes.
    pub(super) cache: ProcessorCache,
    /// index of commits by oid
    pub(super) commits: crate::processing::caches::OidMap<crate::Commit>,
    _ts_store: PhantomData<&'static TS>,
}

/// SAFETY: TS is held in a PhantomData
unsafe impl<P: Send, TS: 'static> Send for SimpleProc<P, TS> {}
/// SAFETY: TS is held in a PhantomData
unsafe impl<P: Sync, TS: 'static> Sync for SimpleProc<P, TS> {}

impl<P, TS: 'static> SimpleProc<P, TS> {
    pub(crate) fn new(query: Option<Query>, parameter: P) -> Self {
        Self {
            parameter,
            query,
            cache: Default::default(),
            commits: Default::default(),
            _ts_store: PhantomData,
        }
    }

    /// Registers a default processor in the given [`ProcessorMap`].
    pub(crate) fn default_handle(pr: &mut crate::processing::erased::ProcessorMap) -> PPHandle<Self>
    where
        Self: CommitProc,
        P: SimpleProcParam,
        P: 'static + Send + Sync + Default,
        P: std::cmp::PartialEq<SimpleProc<P, TS>>,
    {
        let t = <P as Default>::default();
        let h = pr.commit_proc_mut::<ProcessorHolder<SimpleProc<P, TS>>>();
        h.register_param(t)
    }

    pub(crate) fn matches_file_name(name: &crate::processing::ObjectName) -> bool
    where
        P: SimpleProcParam,
    {
        P::matches_file_name(name)
    }

    pub(crate) fn matches_lang(lang: &str) -> bool
    where
        P: SimpleProcParam,
    {
        P::matches_lang(lang)
    }
}

impl<P: SimpleProcParam, TS> From<P> for SimpleProc<P, TS> {
    fn from(t: P) -> Self {
        SimpleProc::new(t.query(), t)
    }
}

/// A simple processor that handles directory exploration.
///
/// Note: If you want more features for your specific language,
/// do not hesitate to duplicate this structs and associated impls.
pub(crate) struct SimpleProcessor<'repo, 'prepro, 'd, 'c, Acc, Proc> {
    repository: &'repo git2::Repository,
    prepro: &'prepro mut RepositoryProcessor,
    stack: Vec<crate::StackEle<Acc>>,
    pub dir_path: &'d mut std::iter::Peekable<std::path::Components<'c>>,
    handle: PPHandle<Proc>,
    _phantom: PhantomData<*const Proc>,
}

impl<'repo, 'prepro, 'd, 'c, Acc: From<String>, Proc>
    SimpleProcessor<'repo, 'prepro, 'd, 'c, Acc, Proc>
{
    /// Prepare processing at given `oid` and `dir_path`.
    ///
    /// Once prepared call `crate::Processor::process` on `Self`.
    ///
    /// - `oid` is expected to be a tree in `repository`.
    /// - `handle` points to a processor in `prepro`.
    pub(crate) fn prepare(
        repository: &'repo git2::Repository,
        prepro: &'prepro mut RepositoryProcessor,
        dir_path: &'d mut std::iter::Peekable<std::path::Components<'c>>,
        name: &ObjectName,
        oid: git2::Oid,
        handle: PPHandle<Proc>,
    ) -> Self {
        let tree = repository.find_tree(oid).unwrap();
        let prepared = prepare_dir_exploration(&tree).collect::<Vec<_>>();
        let name = name.try_into().unwrap();
        let stack = vec![crate::StackEle::new(oid, prepared, Acc::from(name))];
        Self {
            stack,
            repository,
            prepro,
            dir_path,
            handle,
            _phantom: PhantomData,
        }
    }
}

impl<'repo, 'b, 'd, 'c, Acc, P, TS> crate::Processor<Acc>
    for SimpleProcessor<'repo, 'b, 'd, 'c, Acc, SimpleProc<P, TS>>
where
    P: SimpleProcParam,
    P: 'static + Send + Sync,
    Acc: SimpleAccumulator,
    SimpleProc<P, TS>: CommitProc,
    SimpleProc<P, TS>: CachesHolding<Caches = ProcessorCache>,
    SimpleProc<P, TS>: CacheHolding<ProcessorCache>,
    crate::type_store::TStore: hyperast::store::TyDown<TS>,
    TS: 'static + hyperast::tree_gen::TsEnabledTS,
    TS: RoleStore<IdF = u16, Role = Role>,
    TS::Ty2: hyperast::tree_gen::TsType,
{
    fn pre(&mut self, current_object: crate::git::BasicGitObject) {
        log::trace!("pre: {:?}", current_object.name.try_str().unwrap_or(""));
        let oid = current_object.oid;
        let name = current_object.name;
        if current_object.kind == git2::ObjectType::Tree {
            self.handle_tree_cached(oid, name);
            return;
        }
        if P::matches_file_name(&name) {
            let acc = &mut self.stack.last_mut().unwrap().acc;
            SimpleProcessor::<_, SimpleProc<P, TS>>::help_handle_file(
                &mut self.prepro,
                acc,
                oid,
                &name,
                self.repository,
                self.handle,
            )
            .unwrap();
        } else {
            log::debug!(
                "source file {:?} does not match the the expected language extension",
                name.try_str()
            );
        }
    }

    fn post(&mut self, oid: git2::Oid, acc: Acc) -> Option<Acc::Unlabeled> {
        let name = &acc.primary().name;
        let key = (oid, name.as_bytes().into());
        let name = self.prepro.get_or_insert_label(name);
        let processor_map = &mut self.prepro.processing_systems;
        let holder = processor_map.commit_proc_mut::<ProcessorHolder<SimpleProc<P, TS>>>();
        use crate::processing::erased::ParametrizedCommitProcTyped;
        let proc = holder.with_parameters_mut(self.handle);
        let full_node = make(acc, self.prepro.main_stores.mut_with_ts::<TS>(), proc);
        let old = proc.cache.object_map.insert(key, full_node.clone());
        if let Some(old) = old {
            debug_assert_eq!(full_node.id, old.id,);
        }
        if self.stack.is_empty() {
            return Some(full_node);
        }
        let w = &mut self.stack.last_mut().unwrap().acc;
        assert!(
            !w.primary().children_names.contains(&name),
            "{:?} {:?}",
            w.primary().children_names,
            name
        );
        w.push2(name, full_node.clone());
        None
    }

    fn stack(&mut self) -> &mut Vec<crate::StackEle<Acc>> {
        &mut self.stack
    }
}

pub(crate) struct SimpleAcc {
    pub(crate) primary: crate::DirPrimary,
    pub(crate) precomp_queries: PrecompQueries,
}

impl hyperast::tree_gen::Accumulator for SimpleAcc {
    type Node = (FileNameIdentifier, FullNode);
    fn push(&mut self, (name, full_node): Self::Node) {
        self.primary.push(name, full_node.id, full_node.metrics);
        self.precomp_queries += full_node.precomp_queries;
    }
}

impl crate::Accumulator for SimpleAcc {
    type Unlabeled = FullNode;
}

impl From<String> for SimpleAcc {
    fn from(name: String) -> Self {
        Self {
            primary: crate::DirPrimary::new(name),
            precomp_queries: Default::default(),
        }
    }
}

impl SimpleAccumulator for SimpleAcc {
    fn primary(&self) -> &crate::DirPrimary {
        &self.primary
    }
    fn finish(self) -> (crate::DirPrimary, PrecompQueries) {
        (self.primary, self.precomp_queries)
    }
    fn push2(&mut self, name: FileNameIdentifier, full_node: impl Into<FullNode>) {
        use hyperast::tree_gen::Accumulator;
        self.push((name, full_node.into()));
    }
}

impl<'repo, 'prepro, 'd, 'c, Acc: SimpleAccumulator, Proc>
    SimpleProcessor<'repo, 'prepro, 'd, 'c, Acc, Proc>
where
    Proc: 'static + Send + Sync + CommitProc,
    Proc: CachesHolding<Caches = ProcessorCache>,
    Proc: CacheHolding<ProcessorCache>,
{
    fn handle_tree_cached(&mut self, oid: git2::Oid, name: ObjectName) {
        if let Some(s) = self.dir_path.peek() {
            // there is a specific dir we want to analyze
            let other = std::ffi::OsStr::as_encoded_bytes(s.as_os_str());
            if name.as_bytes().eq(other) {
                log::trace!("found next dir {}", name.try_str().unwrap());
                // match! consume the path component and make the next StackEle
                self.dir_path.next();
                self.stack.last_mut().expect("never empty").cs.clear();
                let tree = self.repository.find_tree(oid).unwrap();
                let prepared = prepare_dir_exploration(&tree).collect::<Vec<_>>();
                let name: String = name.try_into().unwrap();
                let acc = Acc::from(name);
                self.stack.push(crate::StackEle::new(oid, prepared, acc));
            } else {
                log::trace!("ignoring {}", name.try_str().unwrap());
            }
            return;
        }
        let processor_map = &mut self.prepro.processing_systems;
        let holder = processor_map.commit_proc_mut::<ProcessorHolder<Proc>>();
        use crate::processing::erased::ParametrizedCommitProcTyped;
        let proc = holder.with_parameters_mut(self.handle);
        if let Some(already) = proc.get_caches().object_map.get(&(oid, name.clone())) {
            // reinit already computed node for post order
            let full_node = already.clone();
            let w = &mut self.stack.last_mut().unwrap().acc;
            let name = self.prepro.intern_object_name(&name);
            assert!(!w.primary().children_names.contains(&name));
            w.push2(name, full_node);
        } else {
            log::debug!("tree {:?}", name.try_str());
            let tree = self.repository.find_tree(oid).unwrap();
            let prepared = prepare_dir_exploration(&tree).collect::<Vec<_>>();
            let acc = Acc::from(name.try_into().unwrap());
            self.stack.push(crate::StackEle::new(oid, prepared, acc));
        }
    }
}

impl<'repo, 'b, 'd, 'c, Acc: SimpleAccumulator, P, TS>
    SimpleProcessor<'repo, 'b, 'd, 'c, Acc, SimpleProc<P, TS>>
where
    P: 'static + Send + Sync,
    SimpleProc<P, TS>: CommitProc,
    SimpleProc<P, TS>: CachesHolding<Caches = ProcessorCache>,
    SimpleProc<P, TS>: CacheHolding<ProcessorCache>,
    P: SimpleProcParam,
    TS: 'static + hyperast::tree_gen::TsEnabledTS,
    TS: RoleStore<IdF = u16, Role = Role>,
    TS::Ty2: hyperast::tree_gen::TsType,
    crate::type_store::TStore: hyperast::store::TyDown<TS>,
{
    /// Handles the processing of a file.
    pub(crate) fn help_handle_file(
        prepro: &mut RepositoryProcessor,
        parent: &mut Acc,
        oid: git2::Oid,
        name: &ObjectName,
        repository: &git2::Repository,
        parameters: PPHandle<SimpleProc<P, TS>>,
    ) -> Result<(), crate::ParseErr> {
        let full_node = SimpleProcessor::<Acc, SimpleProc<P, TS>>::handle_blob(
            prepro, oid, name, repository, parameters,
        )?;
        let name = prepro.intern_object_name(name);
        assert!(!parent.primary().children_names.contains(&name));

        parent.push2(name, full_node);
        Ok(())
    }

    /// Handles the processing of a blob.
    pub(crate) fn handle_blob(
        prepro: &mut RepositoryProcessor,
        oid: git2::Oid,
        name: &ObjectName,
        repository: &git2::Repository,
        parameters: PPHandle<SimpleProc<P, TS>>,
    ) -> Result<FullNode, crate::ParseErr> {
        prepro
            .processing_systems
            .caching_blob_handler::<SimpleProc<P, TS>>()
            .handle2(oid, repository, name, parameters, |c, n, t| {
                let holder = c.commit_proc_mut::<ProcessorHolder<SimpleProc<P, TS>>>();
                use crate::processing::erased::ParametrizedCommitProcTyped;
                let proc = holder.with_parameters_mut(parameters);
                let stores = prepro.main_stores.mut_with_ts::<TS>();
                let r = handle_blob_aux(n, t, proc, stores).map_err(|e| {
                    log::trace!("{}", e);
                    crate::utils::ParseErr::from(e)
                })?;
                prepro.parsing_time += r.parsing_time;
                prepro.processing_time += r.processing_time;
                log::info!(
                    "parsing, processing, n, f: {} {} {} {}",
                    prepro.parsing_time.as_secs(),
                    prepro.processing_time.as_secs(),
                    proc.cache.md_cache.len(),
                    proc.cache.object_map.len()
                );
                Ok(r.node)
            })
    }
}

fn handle_blob_aux<TS, P>(
    n: &ObjectName,
    t: &[u8],
    proc: &mut SimpleProc<P, TS>,
    stores: &mut SimpleStores<TS>,
) -> Result<SuccessProcessing<FullNode>, FailedParsing>
where
    P: SimpleProcParam,
    TS: 'static + hyperast::tree_gen::TsEnabledTS,
    TS: RoleStore<IdF = u16, Role = Role>,
    TS::Ty2: hyperast::tree_gen::TsType,
{
    use hyperast::tree_gen::zipped_ts_extra::TsTreeGen;
    let lb = crate::_auto_configured_line_break(t);
    let lang = &P::ts_language();
    let cache = &mut proc.cache;
    let dedup = &mut cache.dedup;
    if let Some(more) = &proc.query {
        use hyperast::tree_gen::extra_pattern_precomp::PatternPrecompExtra;
        use hyperast::tree_gen::zipped_ts_extra::Acc;
        let more = &more.0;
        let more: hyperast_tsquery::PreparedQuerying<_, TS, Acc<_>> = more.into();
        let md_cache = std::mem::take(&mut cache.md_cache);
        let mut extra = PatternPrecompExtra::<_, Acc<_>, _>::with_cache(more, md_cache);
        let mut tree_gen = TsTreeGen::new(stores, &mut extra).set_line_break(lb);

        let r = crate::processors::handle_file_ts_simp(tree_gen.with_dedup(dedup), lang, n, t)
            .map(|x| x.map(|x| x.into()));
        cache.md_cache = extra.md_cache;
        r
    } else {
        let mut tree_gen = TsTreeGen::bare(stores).set_line_break(lb);
        crate::processors::handle_file_ts_simp(tree_gen.with_dedup(dedup), lang, n, t)
            .map(|x| x.map(|x| x.into()))
    }
}

/// Core logic to create directory nodes in the HyperAST.
///
/// Duplicate and modify if you expect a different/extended behavior.
fn make<Acc: SimpleAccumulator, P, TS>(
    acc: Acc,
    stores: &mut SimpleStores<TS>,
    proc: &mut SimpleProc<P, TS>,
) -> Acc::Unlabeled
where
    TS: hyperast::types::ETypeStore,
    <TS as hyperast::types::ETypeStore>::Ty2: hyperast::tree_gen::TsType,
    TS: hyperast::tree_gen::TsEnabledTS,
{
    use hyperast::hashed::IndexingHashBuilder as _;
    use hyperast::hashed::MetaDataHashsBuilder as _;
    use hyperast::store::nodes::legion::eq_node;
    use hyperast::types::LabelStore;

    let node_store = &mut stores.node_store;
    let label_store = &mut stores.label_store;
    let kind = TS::directory();
    let interned_kind = TS::intern(kind);
    let label_id = label_store.get_or_insert(acc.primary().name.clone());

    let (primary, precomp_queries) = acc.finish();
    let primary = primary.map_metrics(|m| m.finalize(&interned_kind, &label_id));
    let hashable = primary.metrics.hashs.most_discriminating();
    let eq = eq_node(&interned_kind, Some(&label_id), &primary.children);
    let cache = &mut proc.cache;
    let md_cache = &mut cache.md_cache;
    let dedup_cache = &mut cache.dedup.0;
    let insertion = node_store
        .inner
        .prepare_insertion(dedup_cache, &hashable, eq);

    // Guard to avoid computing metadata for an already present subtree
    if let Some(id) = insertion.occupied_id() {
        // different git object but same name and content (that we processed)
        // NOTE we do not necessarily process every git object, avoid dead weight.
        let metrics = primary.metrics.map_hashs(|h| h.build());
        return FullNode {
            id,
            metrics,
            precomp_queries,
        };
    }

    let mut dyn_builder = hyperast::store::nodes::legion::subtree_builder::<TS>(interned_kind);

    let children_is_empty = primary.children.is_empty();

    let metrics = primary.persist(&mut dyn_builder, interned_kind, label_id);
    let metrics = metrics.map_hashs(|h| h.build());
    let hashs = metrics.add_md_metrics(&mut dyn_builder, children_is_empty);
    hashs.persist(&mut dyn_builder);

    hyperast::tree_gen::add_md_precomp_queries(&mut dyn_builder, precomp_queries.0);

    let vacant = insertion.vacant();
    let node_id = vacant.insert_built(dyn_builder.build());

    md_cache.insert(node_id, precomp_queries.clone());

    FullNode {
        id: node_id,
        metrics,
        precomp_queries,
    }
}
