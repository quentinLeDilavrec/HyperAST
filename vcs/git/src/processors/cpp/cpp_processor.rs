use std::collections::HashMap;
use std::iter::Peekable;
use std::path::Components;
use std::sync::Arc;

use git2::{Oid, Repository};

use hyperast::store::nodes::legion::subtree_builder;
use hyperast::tree_gen::add_md_precomp_queries;

use crate::git::BasicGitObject;
use crate::preprocessed::CommitBuilder;
use crate::preprocessed::RepositoryProcessor;
use crate::processing::ParametrizedCommitProcessorHandle as PCPHandle;
use crate::processing::erased::ParametrizedCommitProc2;
use crate::processing::erased::PreparedCommitProc;
#[cfg(feature = "cpp")]
use crate::processing::file_sys;
use crate::processing::{CacheHolding, InFiles, ObjectName};
#[cfg(feature = "cpp")]
use crate::processors::make::MakeModuleAcc;
use crate::{Processor, StackEle};

// use super::MakeModuleAcc;
use super::SimpleStores;
use super::{CppAcc, Parameter};
use hyperast_gen_ts_cpp::Type;
use hyperast_gen_ts_cpp::legion as cpp_gen;

type Handle = crate::processing::erased::ParametrizedCommitProcessor2Handle<CppProc>;

pub(crate) fn prepare_dir_exploration(tree: git2::Tree) -> Vec<BasicGitObject> {
    tree.iter()
        .rev()
        .map(TryInto::try_into)
        .filter_map(|x| x.ok())
        .collect()
}

pub struct CppProcessor<'repo, 'prepro, 'd, 'c, Acc> {
    repository: &'repo Repository,
    prepro: &'prepro mut RepositoryProcessor,
    stack: Vec<StackEle<Acc>>,
    pub dir_path: &'d mut Peekable<Components<'c>>,
    handle: &'d Handle,
}

impl<'repo, 'prepro, 'd, 'c, Acc: From<String>> CppProcessor<'repo, 'prepro, 'd, 'c, Acc> {
    pub(crate) fn new(
        repository: &'repo Repository,
        prepro: &'prepro mut RepositoryProcessor,
        dir_path: &'d mut Peekable<Components<'c>>,
        name: &ObjectName,
        oid: git2::Oid,
        parameters: &'d Handle,
    ) -> Self {
        let tree = repository.find_tree(oid).unwrap();
        let prepared = prepare_dir_exploration(tree);
        let name = name.try_into().unwrap();
        let stack = vec![StackEle::new(oid, prepared, Acc::from(name))];
        Self {
            stack,
            repository,
            prepro,
            dir_path,
            handle: parameters,
        }
    }
}

pub static SUB_QUERIES: &[&str] = &[
    r#"(declaration
    type: (primitive_type) (#EQ? "char")
)"#,
    r#"(preproc_if)"#,
    r#"(call_expression
    (field_expression
        (call_expression
            (qualified_identifier
                (namespace_identifier) (#EQ? "base")
                (qualified_identifier
                    (namespace_identifier) (#EQ? "CommandLine")
                    (identifier) (#EQ? "ForCurrentProcess")
                )
            )
            (argument_list)
        )
        "->"
        (field_identifier) (#EQ? "HasSwitch")
    )
)"#,
    r#"(qualified_identifier
    (namespace_identifier) (#EQ? "switches")
    (identifier)
)"#,
];

impl<'repo, 'b, 'd, 'c> Processor<CppAcc> for CppProcessor<'repo, 'b, 'd, 'c, CppAcc> {
    fn pre(&mut self, current_object: BasicGitObject) {
        match current_object {
            BasicGitObject::Tree(oid, name) => {
                self.handle_tree_cached(oid, name);
            }
            BasicGitObject::Blob(oid, name) => {
                if file_sys::Cpp::matches(&name) {
                    self.prepro
                        .help_handle_cpp_file(
                            oid,
                            &mut self.stack.last_mut().unwrap().acc,
                            &name,
                            self.repository,
                            *self.handle,
                        )
                        .unwrap();
                } else {
                    log::debug!("not cpp source file {:?}", name.try_str());
                }
            }
        }
    }
    fn post(&mut self, oid: Oid, acc: CppAcc) -> Option<(cpp_gen::Local,)> {
        let name = &acc.primary.name;
        let key = (oid, name.as_bytes().into());
        let name = self.prepro.get_or_insert_label(name);
        let holder = self
            .prepro
            .processing_systems
            .mut_or_default::<CppProcessorHolder>();
        let proc = holder.with_parameters_mut(self.handle.0);
        let full_node = make(acc, self.prepro.main_stores.mut_with_ts(), proc);
        proc.cache.object_map.insert(key, (full_node.clone(),));
        if self.stack.is_empty() {
            return Some((full_node,));
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

    fn stack(&mut self) -> &mut Vec<StackEle<CppAcc>> {
        &mut self.stack
    }
}

impl<'repo, 'prepro, 'd, 'c> CppProcessor<'repo, 'prepro, 'd, 'c, CppAcc> {
    fn handle_tree_cached(&mut self, oid: Oid, name: ObjectName) {
        let prepro = &mut self.prepro;
        let holder = prepro
            .processing_systems
            .mut_or_default::<CppProcessorHolder>();
        let proc = holder.with_parameters_mut(self.handle.0);
        if let Some(already) = proc.cache.object_map.get(&(oid, name.clone())) {
            // reinit already computed node for post order
            let (full_node,) = already.clone();
            let w = &mut self.stack.last_mut().unwrap().acc;
            let name = prepro.intern_object_name(&name);
            assert!(!w.primary.children_names.contains(&name));
            w.push(name, full_node);
        } else {
            log::debug!("tree {:?}", name.try_str());
            let tree = self.repository.find_tree(oid).unwrap();
            let prepared = prepare_dir_exploration(tree);
            let acc = CppAcc::new(name.try_into().unwrap());
            self.stack.push(StackEle::new(oid, prepared, acc));
        }
    }
}

pub(crate) type CppProcessorHolder = crate::processing::ProcessorHolder<CppProc>;

pub(crate) struct CppProc {
    parameter: Parameter,
    query: Option<Query>,
    cache: crate::processing::caches::Cpp,
    commits: HashMap<git2::Oid, crate::Commit>,
}

impl Into<CppProc> for Parameter {
    fn into(self) -> CppProc {
        let query = self.query.as_ref().map(|q| {
            use hyperast_tsquery::ArrayStr;
            Query::new(q.iter())
        });
        CppProc {
            parameter: self,
            query,
            cache: Default::default(),
            commits: Default::default(),
        }
    }
}

impl PartialEq<CppProc> for Parameter {
    fn eq(&self, other: &CppProc) -> bool {
        self == &other.parameter
    }
}

#[derive(Clone)]
pub(crate) struct Query(pub(crate) hyperast_tsquery::Query, Arc<str>);

impl PartialEq for Query {
    fn eq(&self, other: &Self) -> bool {
        self.1 == other.1
    }
}
impl Eq for Query {}

impl Query {
    fn new<'a>(precomputeds: impl Iterator<Item = &'a str>) -> Self {
        use crate::precomp_patterns::only_parse_query_precomp;
        let language = hyperast_gen_ts_cpp::language();
        let precomputeds = precomputeds.collect::<Vec<_>>();
        let precomp = only_parse_query_precomp(precomputeds.as_slice(), language);
        Self(precomp.unwrap(), precomputeds.join("\n").into())
    }
}

impl crate::processing::erased::CommitProc for CppProc {
    fn prepare_processing(
        &self,
        _repository: &git2::Repository,
        _builder: CommitBuilder,
        _handle: PCPHandle,
    ) -> Box<dyn PreparedCommitProc> {
        unimplemented!("required for processing cpp at the root of a project")
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

impl CacheHolding<crate::processing::caches::Cpp> for CppProc {
    fn get_caches_mut(&mut self) -> &mut crate::processing::caches::Cpp {
        &mut self.cache
    }
    fn get_caches(&self) -> &crate::processing::caches::Cpp {
        &self.cache
    }
}

#[cfg(feature = "cpp")]
impl RepositoryProcessor {
    pub(crate) fn handle_cpp_blob(
        &mut self,
        oid: Oid,
        name: &ObjectName,
        repository: &Repository,
        parameters: Handle,
    ) -> Result<(cpp_gen::Local,), crate::ParseErr> {
        let mut handler = self
            .processing_systems
            .caching_blob_handler::<file_sys::Cpp>();
        handler.handle2(oid, repository, &name, parameters, |c, n, t| {
            let line_break = crate::_auto_configured_line_break(t);
            let holder = c.mut_or_default::<CppProcessorHolder>();
            let cpp_proc = holder.with_parameters_mut(parameters.0);
            let md_cache = &mut cpp_proc.cache.md_cache;
            let dedup = &mut cpp_proc.cache.dedup;
            let stores = self
                .main_stores
                .mut_with_ts::<hyperast_gen_ts_cpp::TStore>();
            let r = if let Some(more) = &cpp_proc.query {
                let more = &more.0;
                let more: hyperast_tsquery::PreparedQuerying<_, _, _> = more.into();
                let mut cpp_tree_gen = cpp_gen::CppTreeGen::with_preprocessing_and_dedup(
                    stores, dedup, md_cache, more,
                )
                .set_line_break(line_break);
                super::handle_cpp_file1::<_>(&mut cpp_tree_gen, n, t)
            } else {
                use hyperast_gen_ts_cpp::legion_ts_simp::CppTreeGen;
                let mut cpp_tree_gen = CppTreeGen::bare(stores) //
                    .set_line_break(line_break);
                super::handle_cpp_file2(&mut cpp_tree_gen, n, t).map(empty2full)
            }
            .map_err(|_| crate::ParseErr::IllFormed)?;

            self.parsing_time += r.parsing_time;
            self.processing_time += r.processing_time;
            log::info!(
                "parsing, processing, n, f: {} {} {} {}",
                self.parsing_time.as_secs(),
                self.processing_time.as_secs(),
                cpp_proc.cache.md_cache.len(),
                cpp_proc.cache.object_map.len()
            );

            let r = r.node;
            Ok((r.local.clone(),))
        })
    }

    pub(crate) fn help_handle_cpp_file(
        &mut self,
        oid: Oid,
        parent: &mut CppAcc,
        name: &ObjectName,
        repository: &Repository,
        parameters: Handle,
    ) -> Result<(), crate::ParseErr> {
        let (full_node,) = self.handle_cpp_blob(oid, name, repository, parameters)?;
        let name = self.intern_object_name(name);
        assert!(!parent.primary.children_names.contains(&name));

        parent.push(name, full_node);
        Ok(())
    }
    pub(crate) fn help_handle_cpp_file2(
        &mut self,
        oid: Oid,
        parent: &mut MakeModuleAcc,
        name: &ObjectName,
        repository: &Repository,
        parameters: Handle,
    ) -> Result<(), crate::ParseErr> {
        let (full_node,) = self.handle_cpp_blob(oid, name, repository, parameters)?;
        let name = self.intern_object_name(name);
        // assert!(!parent_acc.children_names.contains(&name));
        // parent_acc.push_pom(name, x);
        assert!(!parent.primary.children_names.contains(&name));

        parent.push_source_file(name, full_node);
        Ok(())
    }

    pub(crate) fn handle_cpp_directory<'b, 'd: 'b>(
        &mut self,
        repository: &Repository,
        dir_path: &'b mut Peekable<Components<'d>>,
        name: &ObjectName,
        oid: git2::Oid,
        handle: Handle,
    ) -> (cpp_gen::Local,) {
        CppProcessor::<CppAcc>::new(repository, self, dir_path, name, oid, &handle).process()
    }

    pub(crate) fn help_handle_cpp_folder<'a, 'b, 'c, 'd: 'c>(
        &'a mut self,
        repository: &'b Repository,
        dir_path: &'c mut Peekable<Components<'d>>,
        oid: Oid,
        name: &ObjectName,
        handle: Handle,
    ) -> <CppAcc as hyperast::tree_gen::Accumulator>::Node {
        let full_node = self.handle_cpp_directory(repository, dir_path, name, oid, handle);
        let name = self.intern_object_name(name);
        (name, full_node)
    }
}

fn empty2full(
    x: crate::utils::SuccessProcessing<
        hyperast::tree_gen::extra::NodeWithExtra<
            hyperast::full::FullNode<
                hyperast::tree_gen::BasicGlobalData,
                hyperast::tree_gen::zipped_ts_extra::Local,
            >,
            hyperast::tree_gen::zipped_ts_extra::EmptyExtra,
        >,
    >,
) -> crate::utils::SuccessProcessing<cpp_gen::FNode> {
    let local: hyperast::tree_gen::zipped_ts_extra::Local = x.node.node.local;
    let local = cpp_gen::Local {
        compressed_node: local.compressed_node,
        metrics: local.metrics,
        role: local.role,
        precomp_queries: Default::default(),
        viz_cs_count: 0,
    };
    let global = x.node.node.global;
    crate::utils::SuccessProcessing {
        parsing_time: x.parsing_time,
        processing_time: x.processing_time,
        node: cpp_gen::FNode { global, local },
    }
}

fn make(acc: CppAcc, stores: &mut SimpleStores, cpp_proc: &mut CppProc) -> cpp_gen::Local {
    use hyperast::hashed::IndexingHashBuilder as _;
    use hyperast::hashed::MetaDataHashsBuilder as _;
    use hyperast::store::nodes::legion::eq_node;
    use hyperast::types::ETypeStore as _;
    use hyperast::types::LabelStore;

    let node_store = &mut stores.node_store;
    let label_store = &mut stores.label_store;
    let kind = Type::Directory;
    let interned_kind = hyperast_gen_ts_cpp::TStore::intern(kind);
    let label_id = label_store.get_or_insert(acc.primary.name.clone());

    let primary = acc
        .primary
        .map_metrics(|m| m.finalize(&interned_kind, &label_id));
    let hashable = primary.metrics.hashs.most_discriminating();
    let eq = eq_node(&Type::Directory, Some(&label_id), &primary.children);
    let md_cache = &mut cpp_proc.cache.md_cache;
    let dedup_cache = &mut cpp_proc.cache.dedup.0;
    let insertion = node_store
        .inner
        .prepare_insertion(dedup_cache, &hashable, eq);

    if let Some(id) = insertion.occupied_id() {
        // NOTE this situation should not happen often, due to cache based on oids, so there is no point caching md.
        // If git objects are changed but ignored, then it goes through this branch.
        // TODO bench
        // TODO in the oid cache the values could be NodeIdentifiers, then current cache would be used with an indirection.

        let md = md_cache.get(&id).unwrap();

        return cpp_gen::Local { ..md.local(id) };
    }

    let mut dyn_builder = subtree_builder::<hyperast_gen_ts_cpp::TStore>(interned_kind);

    add_md_precomp_queries(&mut dyn_builder, acc.precomp_queries);

    let children_is_empty = primary.children.is_empty();

    let metrics = primary.persist(&mut dyn_builder, interned_kind, label_id);
    let metrics = metrics.map_hashs(|h| h.build());
    let hashs = metrics.add_md_metrics(&mut dyn_builder, children_is_empty);
    hashs.persist(&mut dyn_builder);

    let vacant = insertion.vacant();
    let node_id = vacant.insert_built(dyn_builder.build());

    md_cache.insert(
        node_id,
        cpp_gen::MD {
            metrics,
            precomp_queries: acc.precomp_queries,
        },
    );

    let full_node = cpp_gen::Local {
        compressed_node: node_id,
        metrics,
        role: None,
        precomp_queries: acc.precomp_queries,
        viz_cs_count: 0,
    };
    full_node
}
