use std::iter::Peekable;
use std::path::Components;
use std::sync::Arc;

use git2::{Oid, Repository};

use hyperast::hashed::{IndexingHashBuilder as _, MetaDataHashsBuilder as _};
use hyperast::store::nodes::legion::RawHAST;
use hyperast::store::nodes::legion::subtree_builder;
use hyperast::tree_gen::add_md_precomp_queries;

use hyperast_gen_ts_java::TType;
use hyperast_gen_ts_java::legion_with_refs as java_tree_gen;
use hyperast_gen_ts_java::legion_with_refs::Acc;
use hyperast_gen_ts_java::{TStore, Type};

use crate::_auto_configured_line_break;
use crate::git::BasicGitObject;
use crate::preprocessed::RepositoryProcessor;
use crate::processing::ObjectName;
use crate::processing::ParametrizedProcessorHandle as PPHandle;
use crate::processing::erased::ParametrizedCommitProc2;
use crate::{Processor, StackEle};

use super::JavaProc;
use super::SimpleStores;
use super::{JavaAcc, JavaProcessorHolder};

type Handle = PPHandle<JavaProc>;

pub struct JavaProcessor<'repo, 'prepro, 'd, 'c, Acc> {
    repository: &'repo Repository,
    prepro: &'prepro mut RepositoryProcessor,
    stack: Vec<StackEle<Acc>>,
    // TODO reenable
    pub dir_path: &'d mut Peekable<Components<'c>>,
    handle: Handle,
}

impl<'repo, 'b, 'd, 'c> JavaProcessor<'repo, 'b, 'd, 'c, JavaAcc> {
    pub(crate) fn prepare(
        repository: &'repo Repository,
        prepro: &'b mut RepositoryProcessor,
        dir_path: &'d mut Peekable<Components<'c>>,
        name: &ObjectName,
        oid: git2::Oid,
        handle: Handle,
    ) -> Self {
        let tree = repository.find_tree(oid).unwrap();
        let prepared = prepare_dir_exploration(tree);
        let name = name.try_into().unwrap();
        let prep_scripting = prep_scripting(prepro, handle);
        use hyperast::scripting::Prepro;
        use hyperast::tree_gen::Prepro as _;
        let scripting_acc = (prep_scripting.cloned())
            .map(Prepro::<SimpleStores, &Acc>::from)
            .map(|x| x.preprocessing(Type::Directory).unwrap());
        let acc = JavaAcc::new(name, scripting_acc);
        let stack = vec![StackEle::new(oid, prepared, acc)];
        Self {
            stack,
            repository,
            prepro,
            dir_path,
            handle,
        }
    }
}

pub(crate) fn prepare_dir_exploration(tree: git2::Tree) -> Vec<BasicGitObject> {
    (tree.iter().rev())
        .map(TryInto::try_into)
        .filter_map(|x| x.ok())
        .collect()
}

impl<'repo, 'b, 'd, 'c> Processor<JavaAcc> for JavaProcessor<'repo, 'b, 'd, 'c, JavaAcc> {
    fn pre(&mut self, current_object: BasicGitObject) {
        log::trace!("pre: {:?}", current_object.name.try_str().unwrap_or(""));
        let oid = current_object.oid;
        let name = current_object.name;
        if current_object.kind == git2::ObjectType::Tree {
            self.handle_dir(oid, name);
            return;
        }
        if super::selection::matches(&name) {
            self.prepro
                .help_handle_java_file(
                    &mut self.stack.last_mut().unwrap().acc,
                    oid,
                    &name,
                    self.repository,
                    self.handle,
                )
                .unwrap();
        } else {
            log::debug!("not java source file {:?}", name.try_str());
        }
    }

    fn post(&mut self, oid: Oid, acc: JavaAcc) -> Option<super::FullNode> {
        let ty = Type::Directory;
        let name = &acc.primary.name;
        let key = (oid, name.as_bytes().into());
        let name = self.prepro.get_or_insert_label(name);
        let holder = self
            .prepro
            .processing_systems
            .commit_proc_mut::<JavaProcessorHolder>();
        let java_proc = holder.with_parameters42_mut(self.handle);
        let full_node = make(acc, self.prepro.main_stores.mut_with_ts(), java_proc);
        java_proc.cache.object_map.insert(key, full_node.clone());
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
        let id = full_node.id;
        w.push(name, full_node.clone());

        let acc = w.scripting_acc.as_mut()?;
        // SAFETY: this side should be fine, issue when unerasing
        let store = unsafe { self.prepro.main_stores.erase_ts_unchecked() };
        acc.acc::<_, TType, _>(store, ty, id.into()).unwrap();

        None
    }

    fn stack(&mut self) -> &mut Vec<StackEle<JavaAcc>> {
        &mut self.stack
    }
}

impl<'repo, 'b, 'd, 'c> JavaProcessor<'repo, 'b, 'd, 'c, JavaAcc> {
    fn handle_dir(&mut self, oid: Oid, name: ObjectName) {
        let java_proc = (self.prepro.processing_systems)
            .commit_proc_mut::<JavaProcessorHolder>()
            .with_parameters42(self.handle);
        let k = (oid, name.clone());
        if let Some(already) = java_proc.cache.object_map.get(&k) {
            // reinit already computed node for post order
            let full_node = already.clone();
            // let skiped_ana = *skiped_ana;
            let id = full_node.id;
            let w = &mut self.stack.last_mut().unwrap().acc;
            let name = self.prepro.intern_object_name(&name);
            assert!(!w.primary.children_names.contains(&name));
            hyperast::tree_gen::Accumulator::push(w, (name, full_node));
            // w.push(name, full_node, skiped_ana);
            if let Some(acc) = &mut w.scripting_acc {
                // SAFETY: this side should be fine, issue when unerasing
                let store = unsafe { self.prepro.main_stores.erase_ts_unchecked() };
                acc.acc::<_, TType, _>(store, Type::Directory, id.into())
                    .unwrap();
            }
            return;
        }
        log::info!("tree {:?}", name.try_str());
        let tree = self.repository.find_tree(oid).unwrap();
        let prepared: Vec<BasicGitObject> = prepare_dir_exploration(tree);

        use hyperast::scripting::Prepro;
        use hyperast::tree_gen::Prepro as _;
        let prepro_acc = prep_scripting(&self.prepro, self.handle)
            .cloned()
            .map(Prepro::<RawHAST<TStore>, &Acc>::from)
            .map(|more| more.preprocessing(Type::Directory).unwrap());
        let acc = JavaAcc::new(name.try_into().unwrap(), prepro_acc);
        self.stack.push(StackEle::new(oid, prepared, acc));
    }
}

// TODO generalize and factor similar preps
// and use the type in Parametrized
//Processor2Handle to get the Holder
fn prep_scripting(prepro: &RepositoryProcessor, handle: PPHandle<JavaProc>) -> Option<&Arc<str>> {
    let java_proc = prepro
        .processing_systems
        .get::<JavaProcessorHolder>()
        .as_ref()?
        .with_parameters42(handle);
    java_proc.parameter.prepro.as_ref()
}

fn make(acc: JavaAcc, stores: &mut SimpleStores, java_proc: &mut JavaProc) -> super::FullNode {
    use hyperast::cyclomatic::Mcc;
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

    let eq = eq_node(&interned_kind, Some(&label_id), &primary.children);

    let md_cache = &mut java_proc.cache.md_cache;
    let mut alt_dedup = java_proc.query.is_some() || java_proc.parameter.prepro.is_some();
    #[cfg(feature = "tsg")]
    {
        alt_dedup |= java_proc.tsg.is_some();
    }
    let dedup = if alt_dedup {
        &mut java_proc.cache.dedup.0
    } else {
        &mut node_store.dedup
    };
    let insertion = node_store.inner.prepare_insertion(dedup, &hashable, eq);

    #[cfg(feature = "impact")]
    let compute_ana = || {
        {
            let ana = acc.ana;
            let ana = if acc.skiped_ana {
                log::info!(
                    "show ana with at least {} refs",
                    ana.lower_estimate_refs_count()
                );
                None
            } else {
                log::info!(
                    "ref count lower estimate in dir {}",
                    ana.lower_estimate_refs_count()
                );
                log::debug!("refs in directory");
                for x in ana.display_refs(label_store) {
                    log::debug!("    {}", x);
                }
                log::debug!("decls in directory");
                for x in ana.display_decls(label_store) {
                    log::debug!("    {}", x);
                }
                let c = ana.estimated_refs_count();
                if c < crate::MAX_REFS {
                    Some(ana.resolve())
                } else {
                    Some(ana)
                }
            };
            // log::info!(
            //     "ref count in dir after resolver {}",
            //     ana.lower_estimate_refs_count()
            // );
            // log::debug!("refs in directory after resolve: ");
            // for x in ana.display_refs(label_store) {
            //     log::debug!("    {}", x);
            // }
            ana
        }
        None
    };

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

    #[cfg(feature = "impact")]
    let ana = compute_ana();

    let mut dyn_builder = subtree_builder::<TStore>(interned_kind);
    let children_is_empty = primary.children.is_empty();
    if acc.skiped_ana {
        dyn_builder.add(hyperast::filter::BloomSize::None);
    } else {
        #[cfg(feature = "impact")]
        java_tree_gen::add_md_ref_ana(&mut dyn_builder, children_is_empty, ana.as_ref());
    }
    let metrics = primary.persist(&mut dyn_builder, interned_kind, label_id);
    let metrics = metrics.map_hashs(|h| h.build());
    let hashs = metrics.add_md_metrics(&mut dyn_builder, children_is_empty);
    hashs.persist(&mut dyn_builder);

    let precomp_queries = acc.precomp_queries;
    add_md_precomp_queries(&mut dyn_builder, precomp_queries.0);

    if let Some(acc) = acc.scripting_acc {
        let subtr = hyperast::scripting::Subtr(kind, &dyn_builder);
        let ss = acc.finish(&subtr).unwrap();
        log::error!("dir {:?}", ss.0);
        dyn_builder.add(ss);
    };

    let vacant = insertion.vacant();
    let compressed_node = vacant.insert_built(dyn_builder.build());

    md_cache.insert(
        compressed_node,
        java_tree_gen::MD {
            metrics,
            #[cfg(feature = "impact")]
            ana: None,
            mcc: Mcc::new(&kind),
            precomp_queries: precomp_queries.0,
        },
    );
    // let full_node = java_tree_gen::Local {
    //     compressed_node,
    //     metrics,
    //     #[cfg(feature = "impact")]
    //     ana,
    //     mcc: Mcc::new(&kind),
    //     role: None,
    //     precomp_queries: acc.precomp_queries.0,
    //     stmt_count: 0,
    //     // TODO precise the exact semantics
    //     member_import_count: 0,
    //     // is_named: kind.is_named(),
    // };
    super::FullNode {
        id: compressed_node,
        metrics,
        precomp_queries,
    }
}

impl JavaProc {
    pub fn default_handle(pr: &mut crate::processing::erased::ProcessorMap) -> PPHandle<Self> {
        type JavaProcessorHolder = crate::processing::ProcessorHolder<JavaProc>;
        let t = crate::processors::java::Parameter::default();
        let h = pr.commit_proc_mut::<JavaProcessorHolder>();
        h.register_param(t)
    }
}

impl RepositoryProcessor {
    pub(crate) fn handle_java_file(
        &mut self,
        name: &ObjectName,
        text: &[u8],
    ) -> Result<java_tree_gen::FNode, ()> {
        let line_break = _auto_configured_line_break(text);
        let mut md_cache = Default::default();
        let stores = self.main_stores.mut_with_ts::<TStore>();

        let mut java_tree_gen =
            java_tree_gen::JavaTreeGen::<TStore, _, _>::new(stores, &mut md_cache)
                .set_line_break(line_break);
        super::handle_java_file1(&mut java_tree_gen, name, text)
            .map(|x| x.node)
            .map_err(|e| {
                eprintln!(
                    "{}\ntree size={}\tparsing time={:?}",
                    e.error,
                    e.tree.root_node().descendant_count(),
                    e.parsing_time,
                )
            })
    }

    pub(crate) fn help_handle_java_folder<'a, 'b, 'c, 'd: 'c>(
        &'a mut self,
        repository: &'b Repository,
        dir_path: &'c mut Peekable<Components<'d>>,
        oid: Oid,
        name: &ObjectName,
        handle: Handle,
    ) -> <JavaAcc as hyperast::tree_gen::Accumulator>::Node {
        let full_node = self.handle_java_directory(repository, dir_path, name, oid, handle);
        let name = self.intern_object_name(name);
        (name, full_node)
    }

    pub(crate) fn handle_java_blob(
        &mut self,
        oid: Oid,
        name: &ObjectName,
        repository: &Repository,
        parameters: Handle,
    ) -> Result<super::FullNode, crate::ParseErr> {
        self.processing_systems
            .caching_blob_handler::<JavaProc>()
            .handle2(oid, repository, name, parameters, |c, n, t| {
                let line_break = _auto_configured_line_break(t);

                let holder = c.commit_proc_mut::<JavaProcessorHolder>();
                let java_proc = holder.with_parameters42_mut(parameters);
                let md_cache = &mut java_proc.cache.md_cache;
                let dedup = &mut java_proc.cache.dedup;
                let stores = self.main_stores.mut_with_ts::<TStore>();
                // let java_tree_gen =
                //     java_tree_gen::JavaTreeGen::new(stores, md_cache).with_line_break(line_break);
                #[cfg(not(feature = "tsg"))]
                let tsg: Option<()> = None;
                #[cfg(feature = "tsg")]
                let tsg = java_proc.tsg.as_ref();
                let r = if let Some(tsg) = tsg {
                    #[cfg(not(feature = "tsg"))]
                    {
                        let _ = tsg;
                        panic!();
                    }
                    #[cfg(feature = "tsg")]
                    {
                        let spec: &crate::processors::java::parameters::GraphQuery =
                            tsg.file.downcast_ref().unwrap();
                        let query = java_proc.query.as_ref().map(|x| &x.0);
                        let functions = tsg.fcts.clone();
                        let more = hyperast_tsquery::PreparedOverlay {
                            query,
                            overlayer: spec,
                            functions,
                        };
                        let mut java_tree_gen =
                            java_tree_gen::JavaTreeGen::with_preprocessing_and_dedup(
                                stores, dedup, md_cache, more,
                            )
                            .set_line_break(line_break);
                        super::handle_java_file1(&mut java_tree_gen, n, t)
                            .map(|x| x.map(|x| Into::<super::FullNode>::into(x.local)))
                    }
                } else if let Some(precomp) = &java_proc.parameter.prepro {
                    let more = hyperast::scripting::Prepro::<_, _>::from_arc(precomp.clone());
                    // let mut java_tree_gen = java_tree_gen.with_more(more);
                    let mut java_tree_gen =
                        java_tree_gen::JavaTreeGen::with_preprocessing_and_dedup(
                            stores, dedup, md_cache, more,
                        )
                        .set_line_break(line_break);
                    super::handle_java_file1(&mut java_tree_gen, n, t)
                        .map(|x| x.map(|x| x.local.into()))
                } else if let Some(more) = &java_proc.query {
                    let more = &more.0;
                    // let more: hyperast_tsquery::PreparedQuerying<_, _, _> = more.into();

                    type Acc = hyperast::tree_gen::zipped_ts_extra::Acc<Type>;
                    let more = hyperast_tsquery::PreparedQuerying::<_, TStore, Acc>::from(more);
                    use hyperast::tree_gen::extra_pattern_precomp::PatternPrecompExtra;
                    let mut extra = PatternPrecompExtra::<_, Acc, _>::from(more);

                    let mut java_tree_gen =
                        hyperast_gen_ts_java::legion_ts_simp::JavaTreeGen::new(stores, &mut extra)
                            .set_line_break(line_break);
                    super::handle_java_file2(&mut java_tree_gen, n, t).map(|x| x.map(|x| x.into()))
                    // .map(simp2full)
                } else {
                    use hyperast_gen_ts_java::legion_ts_simp::JavaTreeGen;
                    let mut java_tree_gen = JavaTreeGen::bare(stores) //
                        .set_line_break(line_break);
                    super::handle_java_file2(&mut java_tree_gen, n, t).map(|x| x.map(|x| x.into()))
                    // .map(empty2full)
                }
                .map_err(|_| crate::ParseErr::IllFormed)?;

                self.parsing_time += r.parsing_time;
                self.processing_time += r.processing_time;
                log::info!(
                    "parsing, processing, n, f: {} {} {} {}",
                    self.parsing_time.as_secs(),
                    self.processing_time.as_secs(),
                    java_proc.cache.md_cache.len(),
                    java_proc.cache.object_map.len()
                );

                let r: super::FullNode = r.node;

                #[cfg(debug_assertions)]
                if let Ok(dd) = stores
                    .node_store
                    .resolve(r.id)
                    .get_component::<hyperast::scripting::DerivedData>()
                {
                    // log::info!("native: {:?} {:?}", r.local.mcc, r.metrics);
                    log::info!("native:  {:?}", r.metrics);
                    log::info!("script: {:?}", dd.0);
                }
                Ok(r)
            })
    }

    pub(crate) fn help_handle_java_file(
        &mut self,
        w: &mut JavaAcc,
        oid: Oid,
        name: &ObjectName,
        repository: &Repository,
        parameters: Handle,
    ) -> Result<(), crate::ParseErr> {
        let full_node = self.handle_java_blob(oid, name, repository, parameters)?;
        let name = self.intern_object_name(name);
        assert!(!w.primary.children_names.contains(&name));
        let id = full_node.id;
        w.push(name, full_node);
        if let Some(acc) = &mut w.scripting_acc {
            // SAFETY: this side should be fine, issue when unerasing
            let store = unsafe { self.main_stores.erase_ts_unchecked() };
            acc.acc::<_, TType, _>(store, Type::Directory, id.into())
                .unwrap();
        }
        Ok(())
    }

    /// oid : Oid of a dir such that */src/main/java/ or */src/test/java/
    fn handle_java_directory<'b, 'd: 'b>(
        &mut self,
        repository: &Repository,
        dir_path: &'b mut Peekable<Components<'d>>,
        name: &ObjectName,
        oid: git2::Oid,
        handle: Handle,
    ) -> super::FullNode {
        JavaProcessor::<JavaAcc>::prepare(repository, self, dir_path, name, oid, handle).process()
    }
}

// TODO try to separate processing from caching from git
#[cfg(test)]
#[allow(unused)]
mod experiments {
    use super::*;
    use crate::Accumulator;
    use crate::processing::{InFiles, NamedObject};
    use crate::processing::{ObjectType, TypedObject, UniqueObject};

    pub(crate) struct GitProcessorMiddleWare<'repo, 'prepro, 'd, 'c> {
        repository: &'repo Repository,
        prepro: &'prepro mut RepositoryProcessor,
        dir_path: &'d mut Peekable<Components<'c>>,
    }

    impl<'repo, 'b, 'd, 'c> GitProcessorMiddleWare<'repo, 'b, 'd, 'c> {
        pub(crate) fn prepare_dir_exploration<It>(&self, current_object: It::Item) -> Vec<It::Item>
        where
            It: Iterator,
            It::Item: NamedObject + UniqueObject<Id = Oid>,
        {
            let tree = self.repository.find_tree(*current_object.id()).unwrap();
            tree.iter()
                .rev()
                .map(|_| todo!())
                // .filter_map(|x| x.ok())
                .collect()
        }
    }

    impl<'repo, 'b, 'd, 'c> JavaProcessor<'repo, 'b, 'd, 'c, JavaAcc> {
        pub(crate) fn prepare_dir_exploration<T>(&self, current_object: &T) -> Vec<T>
        where
            T: NamedObject + UniqueObject<Id = git2::Oid>,
        {
            let tree = self.repository.find_tree(*current_object.id()).unwrap();
            todo!()
        }
        pub(crate) fn stack(
            &mut self,
            current_object: BasicGitObject,
            prepared: Vec<BasicGitObject>,
            acc: JavaAcc,
        ) {
            let tree = self.repository.find_tree(*current_object.id()).unwrap();
            self.stack
                .push(StackEle::new(*current_object.id(), prepared, acc));
        }
        fn pre(
            &mut self,
            current_object: BasicGitObject,
            already: Option<<JavaAcc as Accumulator>::Unlabeled>,
        ) -> Option<<JavaAcc as Accumulator>::Unlabeled> {
            match current_object.r#type() {
                ObjectType::Dir => {
                    if let Some(already) = already {
                        let full_node = already.clone();
                        return Some(full_node);
                    }
                    log::info!("tree {:?}", current_object.name().try_str());
                    let prepared: Vec<BasicGitObject> =
                        self.prepare_dir_exploration(&current_object);
                    let acc = JavaAcc::new(current_object.name().try_into().unwrap(), todo!());
                    self.stack(current_object, prepared, acc);
                    None
                }
                ObjectType::File => {
                    if super::super::selection::matches(current_object.name()) {
                        self.prepro
                            .help_handle_java_file(
                                &mut self.stack.last_mut().unwrap().acc,
                                *current_object.id(),
                                current_object.name(),
                                self.repository,
                                self.handle,
                            )
                            .unwrap();
                    } else {
                        log::debug!("not java source file {:?}", current_object.name().try_str());
                    }
                    None
                }
            }
        }
        fn post(&mut self, oid: Oid, acc: JavaAcc) -> Option<super::super::FullNode> {
            let name = &acc.primary.name;
            // let key = (oid, name.as_bytes().into());
            let name = self.prepro.intern_label(name);

            let holder = self
                .prepro
                .processing_systems
                .commit_proc_mut::<JavaProcessorHolder>();
            let java_proc = holder.with_parameters42_mut(self.handle);
            let full_node = make(acc, self.prepro.main_stores.mut_with_ts(), java_proc);
            todo!(
              // self.prepro
              // .processing_systems
              // .mut_or_default::<JavaProcessorHolder>()
              // .with_parameters_mut(?)
              // .cache.object_map
              // .insert(key, full_node.clone());
            );
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
    }
}
