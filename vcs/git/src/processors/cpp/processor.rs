use std::iter::Peekable;
use std::path::Components;

use git2::{Oid, Repository};

use hyperast::store::nodes::legion::subtree_builder;
use hyperast::tree_gen::add_md_precomp_queries;
use hyperast_gen_ts_cpp::TStore;

use crate::git::BasicGitObject;
use crate::preprocessed::RepositoryProcessor;
use crate::processing::ObjectName;
use crate::processing::ParametrizedProcessorHandle as PPHandle;
use crate::processing::erased::ParametrizedCommitProc2;
use crate::processors::prepare_dir_exploration;
use crate::{Processor, StackEle};

use super::CppAcc;
use super::CppProc;
use super::CppProcessorHolder;
use super::SimpleStores;

use hyperast_gen_ts_cpp::Type;
use hyperast_gen_ts_cpp::legion as cpp_gen;

pub struct CppProcessor<'repo, 'prepro, 'd, 'c, Acc> {
    repository: &'repo Repository,
    prepro: &'prepro mut RepositoryProcessor,
    stack: Vec<StackEle<Acc>>,
    // TODO reenable
    pub dir_path: &'d mut Peekable<Components<'c>>,
    handle: PPHandle<CppProc>,
}

impl<'repo, 'prepro, 'd, 'c, Acc: From<String>> CppProcessor<'repo, 'prepro, 'd, 'c, Acc> {
    pub(crate) fn new(
        repository: &'repo Repository,
        prepro: &'prepro mut RepositoryProcessor,
        dir_path: &'d mut Peekable<Components<'c>>,
        name: &ObjectName,
        oid: git2::Oid,
        parameters: PPHandle<CppProc>,
    ) -> Self {
        let tree = repository.find_tree(oid).unwrap();
        let prepared = prepare_dir_exploration(&tree).collect();
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

impl<'repo, 'b, 'd, 'c> Processor<CppAcc> for CppProcessor<'repo, 'b, 'd, 'c, CppAcc> {
    fn pre(&mut self, current_object: BasicGitObject) {
        let oid = current_object.oid;
        let name = current_object.name;
        if current_object.kind == git2::ObjectType::Tree {
            self.handle_tree_cached(oid, name);
            return;
        }
        if super::selection::matches(&name) {
            self.prepro
                .help_handle_cpp_file(
                    &mut self.stack.last_mut().unwrap().acc,
                    oid,
                    &name,
                    self.repository,
                    self.handle,
                )
                .unwrap();
        } else {
            log::debug!("not cpp source file {:?}", name.try_str());
        }
    }

    fn post(&mut self, oid: Oid, acc: CppAcc) -> Option<super::FullNode> {
        let name = &acc.primary.name;
        let key = (oid, name.as_bytes().into());
        let name = self.prepro.get_or_insert_label(name);
        let holder = self
            .prepro
            .processing_systems
            .commit_proc_mut::<CppProcessorHolder>();
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

    fn stack(&mut self) -> &mut Vec<StackEle<CppAcc>> {
        &mut self.stack
    }
}

impl<'repo, 'prepro, 'd, 'c> CppProcessor<'repo, 'prepro, 'd, 'c, CppAcc> {
    fn handle_tree_cached(&mut self, oid: Oid, name: ObjectName) {
        let prepro = &mut self.prepro;
        let holder = prepro
            .processing_systems
            .commit_proc_mut::<CppProcessorHolder>();
        let proc = holder.with_parameters_mut(self.handle);

        if let Some(already) = proc.cache.object_map.get(&(oid, name.clone())) {
            // reinit already computed node for post order
            let full_node = already.clone();
            let w = &mut self.stack.last_mut().unwrap().acc;
            let name = prepro.intern_object_name(&name);
            assert!(!w.primary.children_names.contains(&name));
            w.push(name, full_node);
            return;
        }

        log::debug!("tree {:?}", name.try_str());
        let tree = self.repository.find_tree(oid).unwrap();
        let prepared = prepare_dir_exploration(&tree).collect::<Vec<_>>();
        let acc = CppAcc::new(name.try_into().unwrap());
        self.stack.push(StackEle::new(oid, prepared, acc));
    }
}

impl RepositoryProcessor {
    pub(crate) fn handle_cpp_blob(
        &mut self,
        oid: Oid,
        name: &ObjectName,
        repository: &Repository,
        parameters: PPHandle<CppProc>,
    ) -> Result<super::FullNode, crate::ParseErr> {
        let mut handler = self.processing_systems.caching_blob_handler::<CppProc>();
        handler.handle2(oid, repository, &name, parameters, |c, n, t| {
            let line_break = crate::_auto_configured_line_break(t);
            let holder = c.commit_proc_mut::<CppProcessorHolder>();
            let cpp_proc = holder.with_parameters_mut(parameters);
            let md_cache = &mut cpp_proc.cache.md_cache;
            let dedup = &mut cpp_proc.cache.dedup;
            let stores = self.main_stores.mut_with_ts::<TStore>();
            let r = if let Some(more) = &cpp_proc.query {
                let more = &more.0;
                let more: hyperast_tsquery::PreparedQuerying<_, _, _> = more.into();
                let mut cpp_tree_gen = cpp_gen::CppTreeGen::with_preprocessing_and_dedup(
                    stores, dedup, md_cache, more,
                )
                .set_line_break(line_break);
                super::handle_cpp_file1::<_>(&mut cpp_tree_gen, n, t)
                    .map(|x| x.map(|x| x.local.into()))
            } else {
                use hyperast_gen_ts_cpp::legion_ts_simp::CppTreeGen;
                let mut cpp_tree_gen = CppTreeGen::bare(stores).set_line_break(line_break);
                super::handle_cpp_file2(&mut cpp_tree_gen, n, t).map(|x| x.map(|x| x.into()))
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
            Ok(r)
        })
    }

    pub(crate) fn help_handle_cpp_file(
        &mut self,
        parent: &mut CppAcc,
        oid: Oid,
        name: &ObjectName,
        repository: &Repository,
        parameters: PPHandle<CppProc>,
    ) -> Result<(), crate::ParseErr> {
        let full_node = self.handle_cpp_blob(oid, name, repository, parameters)?;
        let name = self.intern_object_name(name);
        assert!(!parent.primary.children_names.contains(&name));
        parent.push(name, full_node);
        Ok(())
    }

    #[cfg(feature = "make")]
    pub(crate) fn help_handle_cpp_file2(
        &mut self,
        oid: Oid,
        parent: &mut crate::processors::make::MakeModuleAcc,
        name: &ObjectName,
        repository: &Repository,
        parameters: PPHandle<CppProc>,
    ) -> Result<(), crate::ParseErr> {
        let full_node = self.handle_cpp_blob(oid, name, repository, parameters)?;
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
        handle: PPHandle<CppProc>,
    ) -> super::FullNode {
        CppProcessor::<CppAcc>::new(repository, self, dir_path, name, oid, handle).process()
    }

    pub(crate) fn help_handle_cpp_folder<'a, 'b, 'c, 'd: 'c>(
        &'a mut self,
        repository: &'b Repository,
        dir_path: &'c mut Peekable<Components<'d>>,
        oid: Oid,
        name: &ObjectName,
        handle: PPHandle<CppProc>,
    ) -> <CppAcc as hyperast::tree_gen::Accumulator>::Node {
        let full_node = self.handle_cpp_directory(repository, dir_path, name, oid, handle);
        let name = self.intern_object_name(name);
        (name, full_node)
    }
}

fn make(acc: CppAcc, stores: &mut SimpleStores, cpp_proc: &mut CppProc) -> super::FullNode {
    use hyperast::hashed::IndexingHashBuilder as _;
    use hyperast::hashed::MetaDataHashsBuilder as _;
    use hyperast::store::nodes::legion::eq_node;
    use hyperast::types::ETypeStore as _;
    use hyperast::types::LabelStore as _;

    let node_store = &mut stores.node_store;
    let label_store = &mut stores.label_store;
    let kind = Type::Directory;
    let interned_kind = TStore::intern(kind);
    let label_id = label_store.get_or_insert(acc.primary.name.clone());

    let primary = acc
        .primary
        .map_metrics(|m| m.finalize(&interned_kind, &label_id));
    let hashable = primary.metrics.hashs.most_discriminating();
    let eq = eq_node(&kind, Some(&label_id), &primary.children);
    // let md_cache = &mut cpp_proc.cache.md_cache;
    let dedup = &mut cpp_proc.cache.dedup.0;
    let insertion = node_store.inner.prepare_insertion(dedup, &hashable, eq);

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
    let id = vacant.insert_built(dyn_builder.build());

    super::FullNode {
        id,
        metrics,
        precomp_queries,
    }
}
