use std::collections::HashMap;
use std::iter::Peekable;
use std::path::{Components, PathBuf};

use git2::{Oid, Repository};

use hyperast::hashed::MetaDataHashsBuilder;
use hyperast::store::nodes::compo;
use hyperast::store::nodes::legion::dyn_builder::EntityBuilder;
use hyperast::tree_gen::Accumulator;
use hyperast::tree_gen::add_md_precomp_queries;
use hyperast::types::ETypeStore as _;
use hyperast::types::LabelStore;
use hyperast_gen_ts_xml::Type;

use crate::Processor;
use crate::StackEle;
use crate::git::BasicGitObject;
use crate::preprocessed::{CommitBuilder, RepositoryProcessor};
use crate::processing::ParametrizedProcessorHandle as PPHandle;
use crate::processing::caches::Maven as MavenCaches;
use crate::processing::erased::CommitProc;
use crate::processing::erased::ParametrizedCommitProc2;
use crate::processing::erased::ParametrizedCommitProcessorHandle as PCPHandle;
use crate::processing::erased::PreparedCommitProc;
use crate::processing::file_sys;
use crate::processing::{CacheHolding, InFiles, ObjectName};
use crate::processors::java::JavaProc;
use crate::processors::maven::PomProc;
use crate::utils::drain_filter_strip;

use super::SimpleStores;
use super::scripting::ScriptingPrepro;
use super::scripting::prep_scripting;
use super::{FullNode, MavenModuleAcc};

/// RMS: Resursive Module Search
/// FFWD: Fast ForWarD to java directories without looking at maven stuff
pub struct MavenProcessor<'a, 'b, 'c, const RMS: bool, const FFWD: bool, Acc> {
    prepro: &'b mut RepositoryProcessor,
    repository: &'a Repository,
    stack: Vec<StackEle<Acc>>,
    dir_path: &'c mut Peekable<Components<'c>>,
    maven_handle: PPHandle<MavenProc>,
    pom_handle: PPHandle<PomProc>,
    java_handle: PPHandle<JavaProc>,
}

type MavenProcessorHolder = crate::processing::ProcessorHolder<MavenProc>;

pub struct MavenProc {
    parameter: super::Parameter,
    cache: MavenCaches,
    commits: HashMap<git2::Oid, crate::Commit>,
}

impl From<super::Parameter> for MavenProc {
    fn from(parameter: super::Parameter) -> Self {
        Self {
            parameter,
            cache: MavenCaches::default(),
            commits: HashMap::default(),
        }
    }
}

impl PartialEq<MavenProc> for super::Parameter {
    fn eq(&self, other: &MavenProc) -> bool {
        self == &other.parameter
    }
}

impl<'a, 'b, 'c, const RMS: bool, const FFWD: bool>
    MavenProcessor<'a, 'b, 'c, RMS, FFWD, MavenModuleAcc>
{
    pub fn prepare(
        repository: &'a Repository,
        prepro: &'b mut RepositoryProcessor,
        mut dir_path: &'c mut Peekable<Components<'c>>,
        name: &[u8],
        oid: git2::Oid,
        maven_handle: PPHandle<MavenProc>,
        pom_handle: PPHandle<PomProc>,
        java_handle: PPHandle<JavaProc>,
    ) -> Self {
        let tree = repository.find_tree(oid).unwrap();
        let prepared = prepare_dir_exploration(tree, &mut dir_path);
        let name = std::str::from_utf8(&name).unwrap().to_string();
        let acc = MavenModuleAcc::new(name);
        let prep_scripting = prep_scripting(prepro, java_handle);
        let prep_scripting = prep_scripting.cloned().map(ScriptingPrepro::from);
        let acc = acc.init_scripting(prep_scripting.as_ref());
        let stack = vec![StackEle::new(oid, prepared, acc)];
        Self {
            stack,
            repository,
            prepro,
            dir_path,
            maven_handle,
            pom_handle,
            java_handle,
        }
    }
}

impl<'a, 'b, 'c, const RMS: bool, const FFWD: bool> Processor<MavenModuleAcc>
    for MavenProcessor<'a, 'b, 'c, RMS, FFWD, MavenModuleAcc>
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
        if file_sys::Pom::matches(&name) {
            let parent_acc = &mut self.stack.last_mut().unwrap().acc;
            // TODO find a better conversion, ie. first safe conv to MavenProc handle then into()
            // let parameters = PPHandle(ConfigParametersHandle(0), PhantomData);
            let parameters = self.pom_handle;
            if let Err(err) =
                self.prepro
                    .handle_pom(parent_acc, oid, name, &self.repository, parameters)
            {
                log::debug!("{:?}", err);
            }
        }
    }
    fn post(&mut self, oid: Oid, acc: MavenModuleAcc) -> Option<FullNode> {
        let name = acc.primary.name.clone();
        let full_node = Self::make(acc, self.prepro.main_stores_mut().mut_with_ts());
        log::info!("tree size: {}", full_node.metrics.size);
        self.prepro
            .processing_systems
            .commit_proc_mut::<MavenProcessorHolder>()
            .with_parameters42_mut(self.maven_handle)
            .get_caches_mut()
            .object_map
            .insert(oid, full_node.clone());
        let name = self.prepro.intern_label(&name);
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
        if full_node.is_maven_module() {
            w.push_submodule(name, full_node);
        } else {
            w.push((name, full_node));
        }

        if let Some(acc) = &mut w.scripting_acc {
            // SAFETY: this side should be fine, issue when unerasing
            let store = unsafe { self.prepro.main_stores.erase_ts_unchecked() };
            let child: hyperast::scripting::SubtreeHandle<hyperast_gen_ts_xml::TType> = id.into();
            acc.acc(store, Type::Directory, child).unwrap();
        }
        None
    }

    fn stack(&mut self) -> &mut Vec<StackEle<MavenModuleAcc>> {
        &mut self.stack
    }
}

impl<'a, 'b, 'c, const RMS: bool, const FFWD: bool>
    MavenProcessor<'a, 'b, 'c, RMS, FFWD, MavenModuleAcc>
{
    fn make(acc: MavenModuleAcc, stores: &mut SimpleStores) -> FullNode {
        make(acc, stores)
    }

    fn handle_tree_cached(&mut self, name: ObjectName, oid: Oid) {
        if let Some(s) = self.dir_path.peek() {
            if name
                .as_bytes()
                .eq(std::ffi::OsStr::as_encoded_bytes(s.as_os_str()))
            {
                self.dir_path.next();
                self.stack.last_mut().expect("never empty").cs.clear();
                let tree = self.repository.find_tree(oid).unwrap();
                let prepared = prepare_dir_exploration(tree, &mut self.dir_path);
                let acc = MavenModuleAcc::new(name.try_into().unwrap());
                let prep_scripting = prep_scripting(&self.prepro, self.java_handle);
                let acc = acc.init_scripting(
                    prep_scripting
                        .map(|x| ScriptingPrepro::from(x.clone()))
                        .as_ref(),
                );
                self.stack.push(StackEle::new(oid, prepared, acc));
                return;
            } else {
                return;
            }
        }
        let maven_proc = self
            .prepro
            .processing_systems
            .commit_proc_mut::<MavenProcessorHolder>()
            .with_parameters42_mut(self.maven_handle);
        let java_handle = maven_proc.parameter.java_handle;
        if let Some(already) = maven_proc.get_caches_mut().object_map.get(&oid) {
            // reinit already computed node for post order
            let full_node = already.clone();
            let w = &mut self.stack.last_mut().unwrap().acc;
            let name = self.prepro.intern_object_name(&name);
            assert!(!w.primary.children_names.contains(&name));
            let id = full_node.id;
            if full_node.is_maven_module() {
                w.push_submodule(name, full_node);
            } else {
                w.push((name, full_node));
            }
            if let Some(acc) = &mut w.scripting_acc {
                // SAFETY: this side should be fine, issue when unerasing
                let store = unsafe { self.prepro.main_stores.erase_ts_unchecked() };
                acc.acc::<_, hyperast_gen_ts_xml::TType, _>(store, Type::Directory, id.into())
                    .unwrap();
            }
            return;
        }

        log::debug!("maven tree {:?}", name.try_str());
        let parent_acc = &mut self.stack.last_mut().unwrap().acc;
        if FFWD {
            let (name, full_node) = self.prepro.help_handle_java_folder(
                &self.repository,
                &mut self.dir_path,
                oid,
                &name,
                java_handle,
            );
            let id = full_node.id;
            assert!(!parent_acc.primary.children_names.contains(&name));
            parent_acc.push_source_directory(name, full_node);
            if let Some(acc) = &mut parent_acc.scripting_acc {
                // SAFETY: this side should be fine, issue when unerasing
                let store = unsafe { self.prepro.main_stores.erase_ts_unchecked() };
                acc.acc::<_, hyperast_gen_ts_java::TType, _>(store, Type::Directory, id.into())
                    .unwrap();
            }
            return;
        }
        let helper = MavenModuleHelper::new(parent_acc, &name);
        if helper.source_directories.0 || helper.test_source_directories.0 {
            // handle as source dir
            let (name, full_node) = self.prepro.help_handle_java_folder(
                &self.repository,
                self.dir_path,
                oid,
                &name,
                java_handle,
            );
            let id = full_node.id;
            let parent_acc = &mut self.stack.last_mut().unwrap().acc;
            assert!(!parent_acc.primary.children_names.contains(&name));
            if helper.source_directories.0 {
                parent_acc.push_source_directory(name, full_node);
            } else {
                // test_source_folders.0
                parent_acc.push_test_source_directory(name, full_node);
            }

            if let Some(acc) = &mut parent_acc.scripting_acc {
                // SAFETY: this side should be fine, issue when unerasing
                let store = unsafe { self.prepro.main_stores.erase_ts_unchecked() };
                acc.acc::<_, hyperast_gen_ts_java::TType, _>(store, Type::Directory, id.into())
                    .unwrap();
            }
        }
        // check if module or src/main/java or src/test/java
        // TODO use maven pom.xml to find source_dir  and tests_dir ie. ignore resources, maybe also tests
        // TODO maybe at some point try to handle maven modules and source dirs that reference parent directory in their path

        // TODO check it we can use more info from context and prepare analysis more specifically
        if helper.submodules.0
            || !helper.submodules.1.is_empty()
            || !helper.source_directories.1.is_empty()
            || !helper.test_source_directories.1.is_empty()
        {
            let tree = self.repository.find_tree(oid).unwrap();
            let prepared = prepare_dir_exploration(tree, &mut self.dir_path);
            let prep_scripting = prep_scripting(&self.prepro, self.java_handle);
            if helper.submodules.0 {
                // handle as maven module
                let acc = helper.into_acc().init_scripting(
                    prep_scripting
                        .map(|x| ScriptingPrepro::from(x.clone()))
                        .as_ref(),
                );
                self.stack.push(StackEle::new(oid, prepared, acc));
            } else {
                // search further inside
                let acc = helper.into_acc().init_scripting(
                    prep_scripting
                        .map(|x| ScriptingPrepro::from(x.clone()))
                        .as_ref(),
                );
                self.stack.push(StackEle::new(oid, prepared, acc));
            };
        } else if RMS && !(helper.source_directories.0 || helper.test_source_directories.0) {
            let tree = self.repository.find_tree(oid).unwrap();
            // anyway try to find maven modules, but maybe can do better
            let prepared = prepare_dir_exploration(tree, &mut self.dir_path);

            let prep_scripting = prep_scripting(&self.prepro, self.java_handle);
            let acc = helper.into_acc().init_scripting(
                prep_scripting
                    .map(|x| ScriptingPrepro::from(x.clone()))
                    .as_ref(),
            );
            self.stack.push(StackEle::new(oid, prepared, acc));
        }
    }
}

pub fn make(mut acc: MavenModuleAcc, stores: &mut SimpleStores) -> FullNode {
    use hyperast::hashed::IndexingHashBuilder;
    let node_store = &mut stores.node_store;
    let label_store = &mut stores.label_store;
    use hyperast::store::nodes::legion::eq_node;
    let kind = Type::MavenDirectory;
    let interned_kind = hyperast_gen_ts_xml::TStore::intern(kind);
    let label_id = label_store.get_or_insert(acc.primary.name.clone());

    let primary = acc
        .primary
        .map_metrics(|m| m.finalize(&interned_kind, &label_id));

    let hashable = primary.metrics.hashs.most_discriminating();

    let eq = eq_node(&interned_kind, Some(&label_id), &primary.children);

    let insertion = node_store.prepare_insertion(&hashable, eq);

    // Guard to avoid computing metadata for an already present subtree
    if let Some(id) = insertion.occupied_id() {
        // different git object but same name and content (that we processed)
        // NOTE we do not necessarily process every git object, avoid dead weight.
        let metrics = primary.metrics.map_hashs(|h| h.build());
        let status = acc.status;
        let ana = acc.ana;
        return FullNode {
            id: id,
            metrics,
            precomp_queries: acc.precomp_queries,
            status,
            ana,
        };
    }

    let ana = {
        let new_sub_modules = drain_filter_strip(&mut acc.sub_modules, b"..");
        let new_main_dirs = drain_filter_strip(&mut acc.main_dirs, b"..");
        let new_test_dirs = drain_filter_strip(&mut acc.test_dirs, b"..");
        let ana = acc.ana;
        if !new_sub_modules.is_empty() || !new_main_dirs.is_empty() || !new_test_dirs.is_empty() {
            log::error!(
                "{:?} {:?} {:?}",
                new_sub_modules,
                new_main_dirs,
                new_test_dirs
            );
            todo!(
                "also prepare search for modules and sources in parent, should also tell from which module it is required"
            );
        }
        ana.resolve()
    };

    log::info!("make mm {} {}", &primary.name, primary.children.len());
    assert_eq!(primary.children_names.len(), primary.children.len());
    let mut dyn_builder = EntityBuilder::with_lang(hyperast_gen_ts_xml::Lang);
    let children_is_empty = primary.children.is_empty();
    if !acc.status.is_empty() {
        dyn_builder.add(compo::Flags(acc.status));
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
        log::error!("mm {:?}", ss.0);
        dyn_builder.add(ss);
    };

    let vacant = insertion.vacant();
    let id = vacant.insert_built(dyn_builder.build());

    let status = acc.status;

    FullNode {
        id,
        metrics,
        precomp_queries,
        status,
        ana,
    }
}

struct MavenModuleHelper {
    name: String,
    submodules: (bool, Vec<PathBuf>),
    source_directories: (bool, Vec<PathBuf>),
    test_source_directories: (bool, Vec<PathBuf>),
}

impl MavenModuleHelper {
    fn new(parent_acc: &mut MavenModuleAcc, name: &ObjectName) -> Self {
        let process = |mut v: &mut Option<Vec<PathBuf>>| {
            let mut v = drain_filter_strip(&mut v, name.as_bytes());
            let c = vec_extract_if_polyfill::MakeExtractIf::extract_if(&mut v, |x| {
                x.components().next().is_none()
            })
            .count();
            (c > 0, v)
        };
        Self {
            name: name.try_into().unwrap(),
            submodules: process(&mut parent_acc.sub_modules),
            source_directories: process(&mut parent_acc.main_dirs),
            test_source_directories: process(&mut parent_acc.test_dirs),
        }
    }
}

impl MavenModuleHelper {
    pub fn into_acc(self) -> MavenModuleAcc {
        MavenModuleAcc::with_content(
            self.name,
            self.submodules.1,
            self.source_directories.1,
            self.test_source_directories.1,
        )
    }
}

/// Sometimes order of files/dirs can be important,
/// similarly to order of statements.
pub(crate) fn prepare_dir_exploration(
    tree: git2::Tree,
    dir_path: &mut Peekable<Components>,
) -> Vec<BasicGitObject> {
    let mut children_objects = (tree.iter())
        .map(TryInto::try_into)
        .filter_map(Result::ok)
        .collect::<Vec<_>>();
    if dir_path.peek().is_none() {
        let p = children_objects.iter().position(|x: &BasicGitObject| {
            git2::ObjectType::Blob == x.kind && file_sys::Pom::matches(&x.name)
        });
        if let Some(p) = p {
            children_objects.swap(0, p); // priority to pom.xml processing
            children_objects.reverse(); // we use it like a stack
        }
    }
    children_objects
}

struct PreparedMavenCommitProc<'repo> {
    repository: &'repo git2::Repository,
    commit_builder: CommitBuilder,
    pub(crate) maven_handle: PPHandle<MavenProc>,
    pub(crate) pom_handle: PPHandle<PomProc>,
    pub(crate) java_handle: PPHandle<JavaProc>,
}

impl<'repo> PreparedCommitProc for PreparedMavenCommitProc<'repo> {
    fn process(
        self: Box<PreparedMavenCommitProc<'repo>>,
        prepro: &mut RepositoryProcessor,
    ) -> hyperast::store::defaults::NodeIdentifier {
        let dir_path = PathBuf::from("");
        let mut dir_path = dir_path.components().peekable();
        let name = b"";
        // TODO check parameter in self to know it is a recursive module search
        let root_full_node = MavenProcessor::<true, false, MavenModuleAcc>::prepare(
            self.repository,
            prepro,
            &mut dir_path,
            name,
            self.commit_builder.tree_oid(),
            self.maven_handle,
            self.pom_handle,
            self.java_handle,
        )
        .process();
        let h = prepro
            .processing_systems
            .commit_proc_mut::<MavenProcessorHolder>();
        let commit_oid = self.commit_builder.commit_oid();
        let commit = self.commit_builder.finish(root_full_node.id);
        h.with_parameters42_mut(self.maven_handle)
            .commits
            .insert(commit_oid, commit);
        root_full_node.id
    }
}

impl CommitProc for MavenProc {
    fn prepare_processing<'repo>(
        &self,
        repository: &'repo git2::Repository,
        commit_builder: CommitBuilder,
        handle: PCPHandle,
    ) -> Box<dyn PreparedCommitProc + 'repo> {
        let maven_handle = handle.try_into().unwrap_or_else(|err| {
            eprintln!("{}", err);
            unreachable!("caller of prepare_processing should properly dispatch")
        });
        let pom_handle = self.parameter.pom_handle;
        let java_handle = self.parameter.java_handle;
        Box::new(PreparedMavenCommitProc {
            repository,
            commit_builder,
            maven_handle,
            pom_handle,
            java_handle,
        })
    }

    fn get_commit(&self, commit_oid: git2::Oid) -> Option<&crate::Commit> {
        self.commits.get(&commit_oid)
    }

    fn commit_count(&self) -> usize {
        self.commits.len()
    }

    fn get_lang_handle(&self, lang: &str) -> Option<PCPHandle> {
        if lang.eq_ignore_ascii_case("java") {
            Some(self.parameter.java_handle.erase())
        } else if lang.eq_ignore_ascii_case("cpp") {
            if cfg!(debug_assertions) {
                unimplemented!()
            }
            None
        } else {
            None
        }
    }
}

impl CacheHolding<MavenCaches> for MavenProc {
    fn get_caches_mut(&mut self) -> &mut MavenCaches {
        &mut self.cache
    }
    fn get_caches(&self) -> &MavenCaches {
        &self.cache
    }
}
