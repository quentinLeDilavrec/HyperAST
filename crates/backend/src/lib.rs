// #![feature(array_chunks)]
// #![feature(map_many_mut)]
// #![feature(iter_collect_into)]
#![allow(unused)]
use std::{
    net::SocketAddr,
    sync::{Arc, RwLock},
};

use dashmap::DashMap;
use hyper_diff::matchers::mapping_store::VecStore;
use hyperast_vcs_git::multi_preprocessed::PreProcessedRepositories;

use axum::body::Bytes;
use hyperast::store::nodes::legion::NodeIdentifier;

pub mod app;
mod changes;
pub mod cli;
mod commit;
pub mod examples;
mod fetch;
mod file;
mod matching;
mod pull_requests;
mod querying;
mod scriptingv1;
pub mod smells;
pub mod track;
#[cfg(feature = "tsg")]
mod tsg;
mod utils;
mod view;
mod ws;
pub use ws::ws_handler;

type DocState = (
    RwLock<automerge::AutoCommitWithObs<automerge::transaction::UnObserved>>,
    (
        tokio::sync::broadcast::Sender<(SocketAddr, Vec<automerge::Change>)>,
        tokio::sync::broadcast::Receiver<(SocketAddr, Vec<automerge::Change>)>,
    ),
    RwLock<Vec<tokio::sync::mpsc::Sender<Option<Vec<u8>>>>>,
);

// #[derive(Default)]
pub struct AppState {
    pub db: DashMap<String, Bytes>,
    pub repositories: RwLock<PreProcessedRepositories>,
    // configs: RwLock<RepoConfigs>,
    mappings: MappingCache,
    mappings_alone: MappingAloneCache,
    partial_decomps: PartialDecompCache,
    // Single shared doc
    doc: Arc<DocState>,
    // Multiple shared docs
    doc2: ws::SharedDocs,
    pr_cache: RwLock<std::collections::HashMap<commit::Param, pull_requests::RawPrData>>,
}

impl Default for AppState {
    fn default() -> Self {
        Self {
            db: Default::default(),
            repositories: Default::default(),
            mappings: Default::default(),
            mappings_alone: Default::default(),
            partial_decomps: Default::default(),
            doc: Arc::new((
                RwLock::new(automerge::AutoCommit::new()),
                tokio::sync::broadcast::channel(50),
                Default::default(),
            )),
            doc2: Default::default(),
            pr_cache: Default::default(),
        }
    }
}

// pub(crate) type PartialDecompCache = DashMap<NodeIdentifier, DS<NodeIdentifier>>;
pub(crate) type PartialDecompCache = clashmap::ClashMap<NodeIdentifier, DS<NodeIdentifier>>;
pub(crate) type MappingAloneCache<IdN = NodeIdentifier, M = VecStore<u32>> =
    DashMap<(IdN, IdN), (MappingStage, M)>;
pub(crate) type MappingAloneCacheRef<'a, IdN = NodeIdentifier, M = VecStore<u32>> =
    dashmap::mapref::one::Ref<'a, (IdN, IdN), (MappingStage, M)>;

#[derive(PartialEq, Eq)]
pub(crate) enum MappingStage {
    Subtree,
    Bottomup,
    Decls,
}

type DS<T> = hyper_diff::decompressed_tree_store::lazy_post_order::LazyPostOrder<T, u32>;
pub type PersistableMappings<I> = hyper_diff::matchers::Mapping<DS<I>, DS<I>, VecStore<u32>>;
pub(crate) type MappingCache =
    DashMap<(NodeIdentifier, NodeIdentifier), PersistableMappings<NodeIdentifier>>;
type SharedState = Arc<AppState>;

pub(crate) use hyperast_vcs_git::no_space;

#[cfg(feature = "rerun")]
pub mod log_languages;

#[ignore] // ignore (from normal cargo test) for now, later make a feature
#[test]
// slow test, more of an integration test, try using release
fn test_measuring_size() -> std::result::Result<(), Box<dyn std::error::Error>> {
    tracing_subscriber::fmt()
        .with_env_filter("backend=debug,hyperast_vcs_git=info,hyperast=error")
        .try_init()
        .unwrap();

    let repo_spec = hyperast_vcs_git::git::Forge::Github.repo("graphhopper", "graphhopper");
    let config = hyperast_vcs_git::processing::RepoConfig::JavaMaven;
    let commit = "f5f2b7765e6b392c5e8c7855986153af82cc1abe";
    let language = "Java";
    let prepro = hyperast::scripting::lua_scripting::PREPRO_SIZE_WITH_FINISH;
    run_scripting(repo_spec, config, commit, language, prepro, "size")
}

#[ignore] // ignore (from normal cargo test) for now, later make a feature
#[test]
// slow test, more of an integration test, try using release
fn test_measuring_mcc() -> std::result::Result<(), Box<dyn std::error::Error>> {
    tracing_subscriber::fmt()
        .with_env_filter("backend=debug,hyperast_vcs_git=info,hyperast=error")
        .try_init()
        .unwrap();

    let repo_spec = hyperast_vcs_git::git::Forge::Github.repo("graphhopper", "graphhopper");
    let config = hyperast_vcs_git::processing::RepoConfig::JavaMaven;
    let commit = "f5f2b7765e6b392c5e8c7855986153af82cc1abe";
    let language = "Java";
    let prepro = hyperast::scripting::lua_scripting::PREPRO_MCC_WITH_FINISH;
    run_scripting(repo_spec, config, commit, language, prepro, "mcc")
}

#[ignore] // ignore (from normal cargo test) for now, later make a feature
#[test]
// slow test, more of an integration test, try using release
fn test_measuring_loc() -> std::result::Result<(), Box<dyn std::error::Error>> {
    tracing_subscriber::fmt()
        .with_env_filter("backend=debug,hyperast_vcs_git=info,hyperast=error")
        .try_init()
        .unwrap();

    let repo_spec = hyperast_vcs_git::git::Forge::Github.repo("graphhopper", "graphhopper");
    let config = hyperast_vcs_git::processing::RepoConfig::JavaMaven;
    let commit = "f5f2b7765e6b392c5e8c7855986153af82cc1abe";
    let language = "Java";
    let prepro = hyperast::scripting::lua_scripting::PREPRO_LOC;
    run_scripting(repo_spec, config, commit, language, prepro, "LoC")
}

fn run_scripting(
    repo_spec: hyperast_vcs_git::git::Repo,
    config: hyperast_vcs_git::processing::RepoConfig,
    commit: &str,
    language: &str,
    prepro: &str,
    show: &str,
) -> std::result::Result<(), Box<dyn std::error::Error>> {
    let state = crate::AppState::default();
    state
        .repositories
        .write()
        .unwrap()
        .register_config_with_prepro(repo_spec.clone(), config, prepro.into());
    // state
    //     .repositories
    //     .write()
    //     .unwrap()
    //     .register_config(repo_spec.clone(), config);
    let repo = state
        .repositories
        .read()
        .unwrap()
        .get_config(repo_spec)
        .ok_or_else(|| "missing config for repository".to_string())?;
    let mut repository = repo.fetch();
    log::debug!("done cloning {}", repository.spec);
    let commits = state.repositories.write().unwrap().pre_process_with_limit(
        &mut repository,
        "",
        commit,
        1,
    )?;
    {
        let repositories = state.repositories.read().unwrap();
        let commit = repositories
            .get_commit(&repository.config, &commits[0])
            .unwrap();
        let store = &state.repositories.read().unwrap().processor.main_stores;
        let n = store.node_store.resolve(commit.ast_root);
        let dd = n
            .get_component::<hyperast::scripting::DerivedData>()
            .unwrap();
        let s = dd.0.get(show);
        log::debug!("{show} ! {:?}", s);
        log::debug!("size:{}", n.size());
        log::debug!("size_no_spaces:{}", n.size_no_spaces());
        log::debug!("height:{}", n.height());
        if let Ok(mcc) = n.get_component::<hyperast::cyclomatic::Mcc>() {
            log::debug!("Mcc:{:?}", mcc);
        }
    }
    let commits = state.repositories.write().unwrap().pre_process_with_limit(
        &mut repository,
        "",
        commit,
        2,
    )?;
    let repositories = state.repositories.read().unwrap();
    let commit = repositories
        .get_commit(&repository.config, &commits[1])
        .unwrap();
    let stores = &state.repositories.read().unwrap().processor.main_stores;
    let n = stores.node_store.resolve(commit.ast_root);
    let dd = n
        .get_component::<hyperast::scripting::DerivedData>()
        .unwrap();
    let s = dd.0.get(show);
    log::debug!("{:?}", s);
    use hyperast::types::WithStats;
    log::debug!("size:{}", n.size());
    log::debug!("size_no_spaces:{}", n.size_no_spaces());
    log::debug!("height:{}", n.height());
    if let Ok(mcc) = n.get_component::<hyperast::cyclomatic::Mcc>() {
        log::debug!("Mcc:{:?}", mcc);
    }

    #[cfg(feature = "subtree-stats")]
    {
        log::error!(
            "height_counts_non_dedup : {:3?}",
            stores.node_store.inner.height_counts_non_dedup
        );
        log::error!(
            "height_counts           : {:3?}",
            stores.node_store.inner.height_counts
        );
        log::error!(
            "height_counts_label     : {:3?}",
            stores.node_store.inner.height_counts_label
        );
        log::error!(
            "height_counts_structural: {:3?}",
            stores.node_store.inner.height_counts_structural
        );
    }

    Ok(())
}

#[ignore] // ignore (from normal cargo test) for now, later make a feature
#[test]
// slow test, more of an integration test, try using release
fn test_tsg_incr() -> std::result::Result<(), Box<dyn std::error::Error>> {
    tracing_subscriber::fmt()
        .with_env_filter("backend=debug,hyperast_vcs_git=info,hyperast=error")
        .try_init()
        .unwrap();

    let repo_spec = hyperast_vcs_git::git::Forge::Github.repo("INRIA", "spoon");
    let config = hyperast_vcs_git::processing::RepoConfig::JavaMaven;
    let commit = "56e12a0c0e0e69ea70863011b4f4ca3305e0542b";
    let language = "Java";
    let tsg = r#"
(class_declaration name:(_)@name)@class {
    node @class.decl
    attr (@class.decl) name = (source-text @name)
}
"#;

    run_tsg(repo_spec, config, commit, language, tsg)
}

fn run_tsg(
    repo_spec: hyperast_vcs_git::git::Repo,
    config: hyperast_vcs_git::processing::RepoConfig,
    commit: &str,
    language: &str,
    tsg: &str,
) -> std::result::Result<(), Box<dyn std::error::Error>> {
    let state = crate::AppState::default();
    state
        .repositories
        .write()
        .unwrap()
        .register_config_with_tsg(repo_spec.clone(), config, tsg.into());
    let repo = state
        .repositories
        .read()
        .unwrap()
        .get_config(repo_spec)
        .ok_or_else(|| "missing config for repository".to_string())?;
    let mut repository = repo.fetch();
    log::debug!("done cloning {}", repository.spec);
    let commits = state.repositories.write().unwrap().pre_process_with_limit(
        &mut repository,
        "",
        commit,
        1,
    )?;
    #[cfg(feature = "subtree-stats")]
    dbg!(
        &state
            .repositories
            .read()
            .unwrap()
            .processor
            .main_stores
            .node_store
            .inner
            .height_counts
    );
    Ok(())
}
