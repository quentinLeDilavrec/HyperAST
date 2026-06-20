use std::collections::HashMap;
use std::iter::Peekable;
use std::path::Components;

use git2::{Oid, Repository};

use hyperast::store::nodes::legion::subtree_builder;
use hyperast::tree_gen::add_md_precomp_queries;

use crate::git::BasicGitObject;
use crate::preprocessed::RepositoryProcessor;
use crate::processing::erased::ParametrizedCommitProc2;
use crate::processing::{CacheHolding, InFiles, ObjectName};
use crate::processors::java::Query;
use crate::{Processor, StackEle};

use super::SimpleStores;
use super::{Parameter, PythonAcc};
use hyperast_gen_ts_python::legion as python_gen;
use hyperast_gen_ts_python::{TStore, Type};

type Handle = crate::processing::erased::ParametrizedCommitProcessor2Handle<PythonProc>;

pub(crate) fn prepare_dir_exploration(tree: git2::Tree) -> Vec<BasicGitObject> {
    tree.iter()
        .rev()
        .map(TryInto::try_into)
        .filter_map(|x| x.ok())
        .collect()
}

pub struct PythonProcessor<'repo, 'prepro, 'd, 'c, Acc> {
    repository: &'repo Repository,
    prepro: &'prepro mut RepositoryProcessor,
    stack: Vec<StackEle<Acc>>,
    pub dir_path: &'d mut Peekable<Components<'c>>,
    handle: &'d Handle,
}

type PythonProcessorHolder = crate::processing::ProcessorHolder<PythonProc>;

impl<'repo, 'prepro, 'd, 'c, Acc: From<String>> PythonProcessor<'repo, 'prepro, 'd, 'c, Acc> {
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

impl<'repo, 'b, 'd, 'c> Processor<PythonAcc> for PythonProcessor<'repo, 'b, 'd, 'c, PythonAcc> {
    fn pre(&mut self, current_object: BasicGitObject) {
        match current_object {
            BasicGitObject::Tree(oid, name) => {
                self.handle_tree_cached(oid, name);
            }
            BasicGitObject::Blob(oid, name) => {
                if super::file_sys::Python::matches(&name) {
                    self.prepro
                        .help_handle_python_file(
                            oid,
                            &mut self.stack.last_mut().unwrap().acc,
                            &name,
                            self.repository,
                            *self.handle,
                        )
                        .unwrap();
                } else {
                    log::debug!("not a python source file {:?}", name.try_str());
                }
            }
        }
    }
    fn post(&mut self, oid: Oid, acc: PythonAcc) -> Option<super::FullNode> {
        let name = &acc.primary.name;
        let key = (oid, name.as_bytes().into());
        let name = self.prepro.get_or_insert_label(name);
        let holder = self
            .prepro
            .processing_systems
            .mut_or_default::<PythonProcessorHolder>();
        let proc = holder.with_parameters_mut(self.handle.0);
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
        let holder = self
            .prepro
            .processing_systems
            .mut_or_default::<PythonProcessorHolder>();
        let proc = holder.with_parameters_mut(self.handle.0);
        if let Some(
            // (already, skiped_ana)
            already,
        ) = proc.cache.object_map.get(&(oid, name.clone()))
        {
            // reinit already computed node for post order
            let full_node = already.clone();
            // let skiped_ana = *skiped_ana;
            let w = &mut self.stack.last_mut().unwrap().acc;
            let name = self.prepro.intern_object_name(&name);
            assert!(!w.primary.children_names.contains(&name));
            hyperast::tree_gen::Accumulator::push(w, (name, full_node));
            // w.push(name, full_node, skiped_ana);
        } else {
            log::debug!("tree {:?}", name.try_str());
            let tree = self.repository.find_tree(oid).unwrap();
            let prepared: Vec<BasicGitObject> = prepare_dir_exploration(tree);
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
            Query::new(q.iter())
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
    fn prepare_processing(
        &self,
        _repository: &git2::Repository,
        _builder: crate::preprocessed::CommitBuilder,
        _handle: crate::processing::ParametrizedCommitProcessorHandle,
    ) -> Box<dyn crate::processing::erased::PreparedCommitProc> {
        unimplemented!("required for processing python at the root of a project")
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

// impl crate::processing::erased::CommitProcExt for PythonProc {
//     type Holder = PythonProcessorHolder;
// }

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
            .caching_blob_handler::<super::file_sys::Python>()
            .handle2(oid, repository, &name, parameters, |c, n, t| {
                let line_break = if t.contains(&b'\r') { "\r\n" } else { "\n" }
                    .as_bytes()
                    .to_vec();
                let holder = c.mut_or_default::<PythonProcessorHolder>();
                let proc = holder.with_parameters_mut(parameters.0);
                let md_cache = &mut proc.cache.md_cache;
                let dedup = &mut proc.cache.dedup;
                let stores = self.main_stores.mut_with_ts::<TStore>();
                let r: crate::utils::SuccessProcessing<super::FullNode> =
                    if let Some(more) = &proc.query {
                        let more = &more.0;

                        // use hyperast::store::defaults::NodeIdentifier;
                        // use hyperast::tree_gen::extra_pattern_precomp::PatternPrecompExtra;
                        // type Local = hyperast_gen_ts_python::legion::Local;
                        // type Acc = hyperast_gen_ts_python::legion::Acc;
                        // type PrepQuery<'q> = hyperast_tsquery::PreparedQuerying<
                        //     &'q hyperast_tsquery::Query,
                        //     TStore,
                        //     Acc,
                        // >;
                        // let more: hyperast_tsquery::PreparedQuerying<_, TStore, Acc> = more.into();
                        // let mut extra = PatternPrecompExtra::<_, Acc, _>::from(&more);
                        // let mut tree_gen = python_gen::PythonTreeGen::new(stores, &mut extra)
                        //     .set_line_break(line_break);
                        // let tree = hyperast::tree_gen::utils_ts::tree_sitter_parse(
                        //     t,
                        //     &hyperast_gen_ts_python::language(),
                        // );
                        // eprintln!("{}", tree.root_node().to_sexp());

                        // let f = tree_gen.generate_file(n, t, tree.walk());
                        todo!()
                        // super::handle_python_file(&mut tree_gen, n, t).map(|x| todo!())
                    } else {
                        use python_gen::PythonTreeGen;
                        let mut tree_gen = PythonTreeGen::bare(stores) //
                            .set_line_break(line_break);
                        super::handle_python_file(&mut tree_gen, n, t).map(|x| todo!())
                    }
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

                let r = r.node;
                // Ok((r.local.clone(), acc.precomp_queries))
                todo!()
            })
    }

    pub(crate) fn help_handle_python_file(
        &mut self,
        oid: Oid,
        parent: &mut PythonAcc,
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
        PythonProcessor::<PythonAcc>::new(repository, self, dir_path, name, oid, &handle).process()
    }

    pub(crate) fn help_handle_python_folder<'a, 'b, 'c, 'd: 'c>(
        &'a mut self,
        repository: &'b Repository,
        dir_path: &'c mut Peekable<Components<'d>>,
        oid: Oid,
        name: &ObjectName,
        handle: Handle,
    ) -> <PythonAcc as hyperast::tree_gen::Accumulator>::Node {
        let full_node = self.handle_python_directory(repository, dir_path, name, oid, handle);
        let name = self.intern_object_name(name);
        (name, full_node)
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

    if let Some(id) = insertion.occupied_id() {
        // NOTE this situation should not happen often, due to cache based on oids, so there is no point caching md.
        // If git objects are changed but ignored, then it goes through this branch.
        // TODO bench
        // TODO in the oid cache the values could be NodeIdentifiers, then current cache would be used with an indirection.

        let md = md_cache.get(&id).unwrap();
        let metrics = primary.metrics;
        let metrics = metrics.map_hashs(|h| h.build());
        return (
            python_gen::Local {
                compressed_node: id,
                metrics,
                role: None,
            },
            md.clone(),
        );
    }

    let mut dyn_builder = subtree_builder::<TStore>(interned_kind);

    add_md_precomp_queries(&mut dyn_builder, acc.precomp_queries.0);

    let children_is_empty = primary.children.is_empty();

    let metrics = primary.persist(&mut dyn_builder, interned_kind, label_id);
    let metrics = metrics.map_hashs(|h| h.build());
    let hashs = metrics.add_md_metrics(&mut dyn_builder, children_is_empty);
    hashs.persist(&mut dyn_builder);

    let vacant = insertion.vacant();
    let node_id = vacant.insert_built(dyn_builder.build());

    md_cache.insert(node_id, acc.precomp_queries.clone());

    let local = python_gen::Local {
        compressed_node: node_id,
        metrics,
        role: None,
    };
    (local, acc.precomp_queries)
}
