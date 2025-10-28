use crate::{SharedState, smells::globalize};
use axum::{Json, response::IntoResponse};
use http::{HeaderMap, StatusCode};
use hyper_diff::decompressed_tree_store::ShallowDecompressedTreeStore;
use hyper_diff::decompressed_tree_store::lazy_post_order::LazyPostOrder;
use hyper_diff::matchers::mapping_store::{MappingStore, VecStore};
use hyper_diff::matchers::similarity_metrics::SimilarityMeasure;
use hyper_diff::matchers::{Decompressible, mapping_store::MultiMappingStore};
use hyper_diff::matchers::{Mapper, Mapping};
use hyperast::nodes::TextSerializer;
use hyperast::position::StructuralPosition;
use hyperast::position::position_accessors::{SolvedPosition, WithPreOrderOffsets};
use hyperast::store::SimpleStores;
use hyperast::store::defaults::NodeIdentifier;
use hyperast::types::{
    Children, Childrn, HyperAST, HyperType, Labeled, Typed, WithChildren, WithStats,
};
use hyperast_vcs_git::TStore;
use hyperast_vcs_git::git::Oid;
use serde::{Deserialize, Serialize};
use std::time::Instant;

#[derive(Serialize, Deserialize, Clone)]
pub struct Param {
    user: String,
    name: String,
    commit: String,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct Content {
    pub language: String,
    pub query: String,
    pub precomp: Option<String>,
    pub commits: usize,
    // TODO disable the incriminated pattern for subsequent matches
    /// checked per individual match
    /// if triggered on first search (ie. first commit searched) it return directly
    /// if triggered later, divide the numer of commits remaining to analyze by 2 each time (ie. `commits`` field)
    #[serde(default = "default_max_matches")]
    pub max_matches: u64,
    /// checked each match (in milli seconds)
    #[serde(default = "default_timeout")]
    pub timeout: u64,
}
#[derive(Serialize, Deserialize, Clone)]
pub struct ContentDifferential {
    #[serde(flatten)]
    pub content: Content,
    #[serde(default)]
    pub full_diff: bool,
}

fn default_max_matches() -> u64 {
    500
}

fn default_timeout() -> u64 {
    1000
}

#[derive(Serialize)]
pub enum QueryingError {
    ProcessingError(String),
    MissingLanguage(String),
    ParsingError(String),
    MatchingErrOnFirst(MatchingError<ComputeResultIdentified>),
    MatchingError(MatchingError<ComputeResult>),
}

#[derive(Debug, Serialize, Clone)]
pub enum MatchingError<T> {
    TimeOut(T),
    MaxMatches(T),
}
impl<T> MatchingError<T> {
    fn map<U>(self, f: impl Fn(T) -> U) -> MatchingError<U> {
        match self {
            MatchingError::TimeOut(x) => MatchingError::TimeOut(f(x)),
            MatchingError::MaxMatches(x) => MatchingError::MaxMatches(f(x)),
        }
    }
}

#[derive(Serialize)]
pub struct ComputeResults {
    pub prepare_time: f64,
    pub matching_error_count: usize,
    pub results: Vec<Result<ComputeResultIdentified, MatchingError<ComputeResultIdentified>>>,
}

#[derive(Debug, Serialize, Clone)]
pub struct ComputeResultIdentified {
    pub commit: String,
    #[serde(flatten)]
    pub inner: ComputeResult,
}

#[derive(Debug, Serialize, Clone)]
pub struct ComputeResult {
    pub compute_time: f64,
    pub result: Vec<u64>,
}
impl ComputeResult {
    fn with(self, commit_oid: &Oid) -> ComputeResultIdentified {
        ComputeResultIdentified {
            commit: commit_oid.to_string(),
            inner: self,
        }
    }
}

const INCREMENTAL_QUERIES: bool = true;

pub fn simple(
    query: Content,
    state: SharedState,
    path: Param,
) -> Result<ComputeResults, QueryingError> {
    let now = Instant::now();
    let Param { user, name, commit } = path.clone();
    let mut additional = commit.split("/");
    let commit = additional.next().unwrap();
    let Content {
        language,
        query,
        precomp,
        commits,
        max_matches,
        timeout,
    } = query;
    let timeout = std::time::Duration::from_millis(timeout);
    let mut proc_commit_limit = commits;
    let config = if language == "Java" {
        hyperast_vcs_git::processing::RepoConfig::JavaMaven
    } else if language == "Cpp" {
        hyperast_vcs_git::processing::RepoConfig::CppMake
    } else {
        hyperast_vcs_git::processing::RepoConfig::Any
    };
    let lang = &language;
    let language = hyperast_vcs_git::resolve_language(&language)
        .ok_or_else(|| QueryingError::MissingLanguage(language.to_string()))?;
    let repo_spec = hyperast_vcs_git::git::Forge::Github.repo(user, name);
    let repo = state
        .repositories
        .read()
        .unwrap()
        .get_config(repo_spec.clone());
    let repo = match repo {
        Some(repo) => repo,
        None => {
            let configs = &mut state.repositories.write().unwrap();
            if let Some(precomp) = precomp {
                configs.register_config_with_prequeries(repo_spec.clone(), config, &[&precomp]);
            } else {
                // configs.register_config_alt_lang(repo_spec.clone(), config, "C");
                configs.register_config(repo_spec.clone(), config);
            }
            log::error!("missing config for {}", repo_spec);
            configs.get_config(repo_spec.clone()).unwrap()
        }
    };

    let mut repo = repo.fetch();
    log::warn!("done cloning {}", &repo.spec);
    let commits = crate::utils::handle_pre_processing(&state, &mut repo, "", commit, commits)
        .map_err(|x| QueryingError::ProcessingError(x.to_string()))?;
    log::info!("done construction of {commits:?} in  {}", repo.spec);
    let language: tree_sitter::Language = language.clone();

    let precomputeds = INCREMENTAL_QUERIES
        .then(|| {
            state
                .repositories
                .read()
                .unwrap()
                .get_precomp_query(repo.config, lang)
        })
        .flatten();

    let query = if let Some(precomputeds) = precomputeds {
        hyperast_tsquery::Query::with_precomputed(&query, language, precomputeds).map(|x| x.1)
    } else {
        hyperast_tsquery::Query::new(&query, language)
    }
    .map_err(|e| QueryingError::ParsingError(e.to_string()))?;

    log::info!("done query construction");
    let prepare_time = now.elapsed().as_secs_f64();
    let mut results = vec![];
    let mut matching_error_count = 0;
    for commit_oid in &commits {
        if results.len() > proc_commit_limit {
            return Ok(ComputeResults {
                prepare_time,
                matching_error_count,
                results,
            });
        }
        let mut oid = commit_oid.to_string();
        oid.truncate(6);
        log::info!("start querying {}", oid);
        let repositories = state.repositories.read().unwrap();
        let commit = repositories.get_commit(&repo.config, commit_oid).unwrap();
        let code = commit.ast_root;
        let stores = &repositories.processor.main_stores;
        let result = simple_aux(stores, code, &query, timeout, max_matches);
        let result = match result {
            Ok(inner) => Ok(inner.with(commit_oid)),
            Err(err) if results.is_empty() => {
                return Err(QueryingError::MatchingErrOnFirst(
                    err.map(|inner| inner.with(commit_oid)),
                ));
            }
            Err(err) => {
                matching_error_count += 1;
                proc_commit_limit /= 2;
                Err(err.map(|inner| inner.with(commit_oid)))
            }
        };
        log::info!("-st {}", oid);
        results.push(result);
    }
    log::info!("done querying of {commits:?} in  {}", repo.spec);
    Ok(ComputeResults {
        prepare_time,
        matching_error_count,
        results,
    })
}

pub fn streamed(mut state: SharedState, path: Param, content: Content) -> axum::response::Response {
    let now = Instant::now();

    let mut headers = HeaderMap::new();

    let language: tree_sitter::Language =
        match hyperast_vcs_git::resolve_language(&content.language) {
            Some(x) => x,
            None => {
                let err = QueryingError::MissingLanguage(content.language);
                headers.insert(
                    "error_parsing",
                    serde_json::to_string(&err).unwrap().try_into().unwrap(),
                );

                return (StatusCode::BAD_REQUEST, headers, "").into_response();
            }
        };

    let (repo, commits) = match pre_repo(&mut state, &path, &content) {
        Ok((x, y)) => (x, y),
        Err(err) => {
            headers.insert("error_parsing", err.to_string().try_into().unwrap());

            return (StatusCode::BAD_REQUEST, headers, "").into_response();
        }
    };

    headers.insert("commits", commits.len().into());

    let pre_query = pre_query(&mut state, &path, &content, repo.config);
    let Content {
        commits: mut proc_commit_limit,
        max_matches,
        timeout,
        ..
    } = content.clone();
    let timeout = std::time::Duration::from_millis(timeout);
    log::info!("done query construction");
    let prepare_time = now.elapsed().as_secs_f64();
    headers.insert("prepare_time", prepare_time.to_string().try_into().unwrap());
    let query = match pre_query {
        Ok(x) => x,
        Err(err) => {
            headers.insert(
                "error_query",
                serde_json::to_string(&err).unwrap().try_into().unwrap(),
            );
            return (StatusCode::BAD_REQUEST, headers, "").into_response();
        }
    };

    headers.insert(
        "table_head",
        serde_json::to_string(
            &(0..query.enabled_pattern_count())
                .map(|x| x.to_string())
                .collect::<Vec<_>>(),
        )
        .unwrap()
        .try_into()
        .unwrap(),
    );

    let it = commits
        .into_iter() // TODO use chunks to reduce presure on state.repositories' lock, need some bench before doing this opt ;)
        .enumerate()
        .map_while(move |(i, commit_oid)| {
            if proc_commit_limit == 0 {
                return None;
            }
            if i >= proc_commit_limit {
                return None;
            }
            let mut oid = commit_oid.to_string();
            oid.truncate(6);
            log::trace!("start querying {} in  {}", oid, repo.spec);
            let repositories = state.repositories.read().unwrap();
            let commit = repositories.get_commit(&repo.config, &commit_oid).unwrap();
            let code = commit.ast_root;
            let stores = &repositories.processor.main_stores;
            let result = simple_aux(stores, code, &query, timeout, max_matches);
            let result = match result {
                Ok(inner) => Ok(inner.with(&commit_oid)),
                Err(err) => {
                    log::warn!("{:?}", err);
                    Err(err.map(|inner| inner.with(&commit_oid)))
                }
            };
            log::info!("done querying-st {}", oid);
            if result.is_err() {
                proc_commit_limit /= 2;
                if i == 0 {
                    proc_commit_limit = 0;
                    log::warn!(
                        "stopping early, because of error on first result {:?}",
                        result
                    );
                    // no need for a special error for first occ.,
                    // it is also obvious to the client when there is an error on first commit
                }
            }
            Some(result)
        });

    let st_vals = futures::stream::iter(it.map(|x| {
        match x {
            Ok(x) => serde_json::to_string(&x).map_err(|e| e.to_string()),
            Err(x) => serde_json::to_string(&x).map_err(|e| e.to_string()),
        }
        // x.map(|x| serde_json::to_string(&x).unwrap())
        //     .map_err(|x| serde_json::to_string(&x).unwrap())
    }));

    (
        StatusCode::OK,
        headers,
        axum::body::Body::from_stream(st_vals),
    )
        .into_response()
}

fn pre_repo(
    state: &mut SharedState,
    path: &Param,
    content: &Content,
) -> Result<(hyperast_vcs_git::processing::ConfiguredRepo2, Vec<Oid>), Box<dyn std::error::Error>> {
    let Param { user, name, commit } = path.clone();
    let mut additional = commit.split("/");
    let commit = additional.next().unwrap();
    let Content {
        language,
        query: _,
        precomp,
        commits,
        max_matches: _,
        timeout: _,
    } = content.clone();
    let config = if language == "Java" {
        hyperast_vcs_git::processing::RepoConfig::JavaMaven
    } else if language == "Cpp" {
        hyperast_vcs_git::processing::RepoConfig::CppMake
    } else {
        hyperast_vcs_git::processing::RepoConfig::Any
    };
    let repo_spec = hyperast_vcs_git::git::Forge::Github.repo(user, name);
    let repo = state
        .repositories
        .read()
        .unwrap()
        .get_config(repo_spec.clone());
    let repo = match repo {
        Some(_) | None => {
            let configs = &mut state.repositories.write().unwrap();
            if let Some(precomp) = precomp {
                let precomp = precomp
                    .split("\n\n")
                    .filter(|x| !x.is_empty())
                    .collect::<Vec<_>>();
                configs.register_config_with_prequeries(
                    repo_spec.clone(),
                    config,
                    precomp.as_slice(),
                );
            } else {
                configs.register_config(repo_spec.clone(), config);
            }
            log::error!("missing config for {}", repo_spec);
            configs.get_config(repo_spec.clone()).unwrap()
        }
    };
    let repo = repo.fetch();
    log::warn!("done cloning {}", &repo.spec);
    let afters = [commit].into_iter().chain(additional);
    let rw = crate::utils::walk_commits_multi(&repo, afters)?.take(commits);
    assert!(state.repositories.try_write().is_ok());
    let commits = crate::utils::handle_pre_processing_aux(state, &repo, rw);
    log::info!("done construction of {commits:?} in  {}", repo.spec);

    Ok((repo, commits))
}

fn pre_query(
    state: &mut SharedState,
    path: &Param,
    content: &Content,
    repo_config: hyperast_vcs_git::processing::ParametrizedCommitProcessorHandle,
) -> Result<hyperast_tsquery::Query, QueryingError> {
    let Param { user, name, commit } = path.clone();
    let mut additional = commit.split("/");
    let commit = additional.next().unwrap();
    let Content {
        language,
        query,
        precomp: _,
        commits: _,
        max_matches: _,
        timeout: _,
    } = &content;
    let config = if language == "Java" {
        hyperast_vcs_git::processing::RepoConfig::JavaMaven
    } else if language == "Cpp" {
        hyperast_vcs_git::processing::RepoConfig::CppMake
    } else {
        hyperast_vcs_git::processing::RepoConfig::Any
    };
    let lang = &language;
    let language: tree_sitter::Language = hyperast_vcs_git::resolve_language(language)
        .ok_or_else(|| QueryingError::MissingLanguage(language.to_string()))?;
    let language: tree_sitter::Language = language.clone();

    let precomputeds = INCREMENTAL_QUERIES.then(|| {
        state
            .repositories
            .read()
            .unwrap()
            .get_precomp_query(repo_config, lang)
    });
    if let Some(Some(precomputeds)) = precomputeds {
        hyperast_tsquery::Query::with_precomputed(query, language, precomputeds).map(|x| x.1)
    } else {
        hyperast_tsquery::Query::new(query, language)
    }
    .map_err(|e| QueryingError::ParsingError(e.to_string()))
}

fn simple_aux(
    stores: &hyperast::store::SimpleStores<hyperast_vcs_git::TStore>,
    code: NodeIdentifier,
    query: &hyperast_tsquery::Query,
    timeout: std::time::Duration,
    max_matches: u64,
) -> Result<ComputeResult, MatchingError<ComputeResult>> {
    let height = stores.node_store.resolve(code).height();
    log::trace!("Queried tree height: {}", height);
    let mut result = vec![0; query.enabled_pattern_count()];
    let now = Instant::now();
    let ex = |i| {
        result[i] += 1;
        let compute_time = now.elapsed();
        if compute_time >= timeout {
            return Some(MatchingError::TimeOut(compute_time));
        } else if result[i] > max_matches {
            // TODO disable the pattern, return the new query
            return Some(MatchingError::MaxMatches(compute_time));
        }
        None
    };
    log::info!("Starting query on tree with height {}", height);
    let r = if height < 128 {
        aux_opt128(stores, code, query, ex)
    } else if height < 512 {
        aux_opt(stores, code, query, ex)
    } else {
        aux_opt(stores, code, query, ex)
        // aux_default(stores, code, query, ex)
    };
    match r {
        Some(MatchingError::TimeOut(compute_time)) => {
            return Err(MatchingError::TimeOut(ComputeResult {
                result,
                compute_time: compute_time.as_secs_f64(),
            }));
        }
        Some(MatchingError::MaxMatches(compute_time)) => {
            return Err(MatchingError::MaxMatches(ComputeResult {
                result,
                compute_time: compute_time.as_secs_f64(),
            }));
        }
        _ => (),
    };

    let compute_time = now.elapsed().as_secs_f64();
    Ok(ComputeResult {
        result,
        compute_time,
    })
}

fn aux_opt<T>(
    stores: &hyperast::store::SimpleStores<hyperast_vcs_git::TStore>,
    code: NodeIdentifier,
    query: &hyperast_tsquery::Query,
    mut ex: impl FnMut(usize) -> Option<T>,
) -> Option<T> {
    let pos = hyperast::position::structural_pos::CursorWithPersistence::new(code);
    let cursor = hyperast_tsquery::hyperast_opt::TreeCursor::new(stores, pos);
    let qcursor = query.matches(cursor);
    for m in qcursor {
        let i = m.pattern_index;
        let i = query.enabled_pattern_index(i).unwrap();
        if let Some(value) = ex(i as usize) {
            return Some(value);
        }
    }
    None
}

fn aux_opt128<T>(
    stores: &hyperast::store::SimpleStores<hyperast_vcs_git::TStore>,
    code: NodeIdentifier,
    query: &hyperast_tsquery::Query,
    mut ex: impl FnMut(usize) -> Option<T>,
) -> Option<T> {
    let pos = hyperast::position::structural_pos::CursorWithPersistence::new(code);
    use hyperast_opt::opt9_parent_types::PersistCursor;
    use hyperast_tsquery::hyperast_opt;
    let cursor = hyperast_opt::TreeCursor::new(stores, pos);
    let qcursor = query.matches(cursor);
    for m in qcursor {
        let i = m.pattern_index;
        let i = query.enabled_pattern_index(i).unwrap();
        if let Some(value) = ex(i as usize) {
            return Some(value);
        }
    }
    None
}

fn aux_default<T>(
    stores: &hyperast::store::SimpleStores<hyperast_vcs_git::TStore>,
    code: NodeIdentifier,
    query: &hyperast_tsquery::Query,
    mut ex: impl FnMut(usize) -> Option<T>,
) -> Option<T> {
    let pos = hyperast::position::StructuralPosition::new(code);
    let cursor = hyperast_tsquery::hyperast_cursor::TreeCursor::new(stores, pos);
    let qcursor = query.matches(cursor);
    for m in qcursor {
        let i = m.pattern_index;
        let i = query.enabled_pattern_index(i).unwrap();
        if let Some(value) = ex(i as usize) {
            return Some(value);
        }
    }
    None
}

#[derive(Serialize)]
pub struct ComputeResultsDifferential {
    pub prepare_time: f64,
    pub results: Vec<(crate::smells::CodeRange, crate::smells::CodeRange)>,
}
#[derive(Serialize, Deserialize, Clone)]
pub struct ParamDifferential {
    user: String,
    name: String,
    commit: String,
    baseline: String,
}

pub fn differential(
    query: ContentDifferential,
    state: SharedState,
    path: ParamDifferential,
) -> Result<ComputeResultsDifferential, QueryingError> {
    let now = Instant::now();
    let ParamDifferential {
        user,
        name,
        commit,
        baseline,
    } = path.clone();
    let ContentDifferential {
        content:
            Content {
                language,
                query,
                precomp,
                max_matches,
                timeout,
                ..
            },
        full_diff,
    } = query;
    let timeout = std::time::Duration::from_millis(timeout);
    let config = if language == "Java" {
        hyperast_vcs_git::processing::RepoConfig::JavaMaven
    } else if language == "Cpp" {
        hyperast_vcs_git::processing::RepoConfig::CppMake
    } else {
        hyperast_vcs_git::processing::RepoConfig::Any
    };
    let lang = &language;
    let language: tree_sitter::Language = hyperast_vcs_git::resolve_language(&language)
        .ok_or_else(|| QueryingError::MissingLanguage(language.to_string()))?;
    let repo_spec = hyperast_vcs_git::git::Forge::Github.repo(user, name);
    let repo = state
        .repositories
        .read()
        .unwrap()
        .get_config(repo_spec.clone());
    let repo = match repo {
        Some(repo) => repo,
        None => {
            let configs = &mut state.repositories.write().unwrap();
            if let Some(precomp) = precomp {
                configs.register_config_with_prequeries(repo_spec.clone(), config, &[&precomp]);
            } else {
                configs.register_config(repo_spec.clone(), config);
            }
            log::error!("missing config for {}", repo_spec);
            configs.get_config(repo_spec.clone()).unwrap()
        }
    };
    let mut repo = repo.fetch();
    log::info!("done cloning {}", &repo.spec);
    let commit = crate::utils::handle_pre_processing(&state, &mut repo, "", &commit, 1)
        .map_err(|x| QueryingError::ProcessingError(x.to_string()))?[0];
    let baseline = crate::utils::handle_pre_processing(&state, &mut repo, "", &baseline, 1)
        .map_err(|x| QueryingError::ProcessingError(x.to_string()))?[0];
    log::info!(
        "done construction of {commit:?} and {baseline:?} in  {}",
        repo.spec
    );
    let language: tree_sitter::Language = language.clone();

    let precomputeds = INCREMENTAL_QUERIES.then(|| {
        state
            .repositories
            .read()
            .unwrap()
            .get_precomp_query(repo.config, lang)
    });
    let query = if let Some(Some(precomputeds)) = precomputeds {
        hyperast_tsquery::Query::with_precomputed(&query, language, precomputeds).map(|x| x.1)
    } else {
        hyperast_tsquery::Query::new(&query, language)
    }
    .map_err(|e| QueryingError::ParsingError(e.to_string()))?
    .with_one_pattern_enabled(0)
    .map_err(|_| {
        QueryingError::ParsingError("exactly one enabled pattern is expected".to_string())
    })?;

    let mut named = hashbrown::HashMap::<Vec<NodeIdentifier>, (usize, usize)>::default();

    log::info!("done query construction");
    let prepare_time = now.elapsed().as_secs_f64();
    let current_tr;
    let baseline_results: Vec<_> = {
        let commit_oid = &baseline;
        let mut oid = commit_oid.to_string();
        oid.truncate(6);
        log::info!("start querying {}", oid);
        let repositories = state.repositories.read().unwrap();
        let commit = repositories.get_commit(&repo.config, commit_oid).unwrap();
        let code = commit.ast_root;
        current_tr = code;
        let stores = &repositories.processor.main_stores;

        let (r, names) = differential_aux(stores, code, &query, timeout, max_matches)
            .map_err(QueryingError::MatchingError)?;
        for (i, names) in names.into_iter().enumerate() {
            if names.is_empty() {
                continue;
            }
            named.insert(names, (i, usize::MAX));
        }
        r
    };
    let other_tr;
    let other_results: Vec<_> = {
        let commit_oid = &commit;
        let mut oid = commit_oid.to_string();
        oid.truncate(6);
        log::info!("start querying {}", oid);
        let repositories = state.repositories.read().unwrap();
        let commit = repositories.get_commit(&repo.config, commit_oid).unwrap();
        let code = commit.ast_root;
        other_tr = code;
        let stores = &repositories.processor.main_stores;

        let (r, names) = differential_aux(stores, code, &query, timeout, max_matches)
            .map_err(QueryingError::MatchingError)?;
        for (i, names) in names.into_iter().enumerate() {
            if names.is_empty() {
                continue;
            }
            print!("other:");
            for name in &names {
                print!("{},", TextSerializer::new(stores, *name))
            }
            println!();
            (named.entry(names))
                .and_modify(|x| x.1 = i)
                .or_insert((usize::MAX, i));
        }
        r
    };
    log::info!(
        "done querying of {commit:?} and {baseline:?} in {}",
        repo.spec
    );

    log::info!(
        "lengths results/baseline_results: {}/{}",
        other_results.len(),
        baseline_results.len()
    );

    let repositories = state.repositories.read().unwrap();
    let stores = &repositories.processor.main_stores;

    let (baseline_results, other_results) = if named.is_empty() {
        (baseline_results, other_results)
    } else {
        let mut baseline_idx: Vec<usize> = vec![];
        let mut other_idx: Vec<usize> = vec![];
        for (names, (baseline, other)) in named.into_iter() {
            if baseline != usize::MAX && other != usize::MAX {
                continue; // matching
            }
            if baseline != usize::MAX {
                for name in &names {
                    println!("{}", TextSerializer::new(stores, *name));
                }
                println!("bl: {}", baseline);
                baseline_idx.push(baseline);
            }
            if other != usize::MAX {
                for name in names {
                    println!("{}", TextSerializer::new(stores, name));
                }
                println!("ot: {}", other);
                other_idx.push(other);
            }
        }
        baseline_idx.sort();
        other_idx.sort();
        let mut baseline_results = baseline_results
            .into_iter()
            .map(|x| Some(x))
            .collect::<Vec<_>>();
        let mut other_results = other_results
            .into_iter()
            .map(|x| Some(x))
            .collect::<Vec<_>>();
        let baseline_results = baseline_idx
            .into_iter()
            .map(|idx| baseline_results[idx].take().unwrap())
            .collect::<Vec<_>>();
        let other_results = other_idx
            .into_iter()
            .map(|idx| other_results[idx].take().unwrap())
            .collect::<Vec<_>>();
        (baseline_results, other_results)
    };

    let hyperast = &hyperast_vcs_git::no_space::as_nospaces(stores);
    let binding = crate::utils::bind_tree_pair(&state.partial_decomps, &current_tr, &other_tr);
    let mut locked = binding.lock();
    let (src_tree, dst_tree) = locked.as_mut(hyperast);

    let src_tree = Decompressible {
        hyperast,
        decomp: src_tree,
    };
    let dst_tree = Decompressible {
        hyperast,
        decomp: dst_tree,
    };

    let mut mapper = Mapper {
        hyperast,
        mapping: Mapping {
            src_arena: src_tree,
            dst_arena: dst_tree,
            mappings: VecStore::<u32>::default(),
        },
    };

    log::info!(
        "starting top_down mapping {} to {}",
        mapper.mapping.src_arena.len(),
        mapper.mapping.dst_arena.len()
    );

    let subtree_mappings = {
        crate::matching::top_down(
            hyperast,
            mapper.mapping.src_arena.decomp,
            mapper.mapping.dst_arena.decomp,
        )
    };

    log::info!(
        "done top_down mapping {} with {} results",
        subtree_mappings.len(),
        baseline_results.len()
    );
    let (baseline_results, other_results) = if full_diff {
        mapper
            .mapping
            .mappings
            .topit(mapper.src_arena.len(), mapper.dst_arena.len());

        hyper_diff::matchers::heuristic::gt::lazy_greedy_subtree_matcher::LazyGreedySubtreeMatcher::<
            _,
            _,
            _,
            hyper_diff::matchers::mapping_store::VecStore<_>,
            1,
        >::filter_mappings(&mut mapper, &subtree_mappings);

        Mapper::bottom_up_stable_with_similarity_threshold_and_recovery(
            &mut mapper,
            Mapper::adaptive_threshold,
            SimilarityMeasure::chawathe,
            |_, _, _| {},
        );
        diferential_filter(
            &repo,
            baseline,
            commit,
            current_tr,
            other_tr,
            stores,
            hyperast,
            baseline_results,
            other_results,
            subtree_mappings,
            mapper,
        )
    } else {
        let mut mapper = Mapper {
            hyperast,
            mapping: Mapping {
                src_arena: mapper.mapping.src_arena,
                dst_arena: mapper.mapping.dst_arena,
                mappings: subtree_mappings.clone(),
            },
        };
        diferential_filter(
            &repo,
            baseline,
            commit,
            current_tr,
            other_tr,
            stores,
            hyperast,
            baseline_results,
            other_results,
            subtree_mappings,
            mapper,
        )
    };

    let baseline_results = baseline_results.into_iter().map(|p| {
        log::trace!("globalizing");
        let baseline_pos = p.make_position(stores);
        let mut pos = baseline_pos.clone();
        pos.set_len(1);
        (
            globalize(&repo, baseline, (baseline_pos, p.iter_offsets().collect())),
            globalize(&repo, commit, (pos, p.iter_offsets().collect())),
        )
    });
    let other_results = other_results.into_iter().map(|p| {
        log::trace!("globalizing");
        let other_pos = p.make_position(stores);
        let mut pos = other_pos.clone();
        pos.set_len(1);
        (
            globalize(&repo, baseline, (pos, p.iter_offsets().collect())),
            globalize(&repo, commit, (other_pos, p.iter_offsets().collect())),
        )
    });
    let results: Vec<(_, _)> = baseline_results.chain(other_results).collect();

    log::info!(
        "done finding {} evolutions from {baseline:?} to {commit:?} in {}",
        results.len(),
        repo.spec
    );

    Ok(ComputeResultsDifferential {
        prepare_time,
        results,
    })
}

fn remap(
    stores: &SimpleStores<TStore>,
    x: &StructuralPosition<IdN, u16>,
    arena: &mut Decompressible<
        &hyperast::store::SimpleStores<hyperast_vcs_git::TStore>,
        &mut LazyPostOrder<IdN, u32>,
    >,
    tr: IdN,
) -> u32 {
    let (_, _, no_spaces_path_to_target) =
        hyperast::position::compute_position_with_no_spaces(tr, &mut x.iter_offsets(), stores);
    let mut src = arena.root();
    dbg!(&no_spaces_path_to_target);
    for i in no_spaces_path_to_target {
        use hyper_diff::decompressed_tree_store::LazyDecompressedTreeStore;
        let cs = arena.decompress_children(&src);
        dbg!(i, src, cs.len());
        let Some(s) = cs.get(i as usize) else {
            return src;
        };
        src = *s;
    }
    src
}

type IdN = NodeIdentifier;

type NoS<'a> = hyperast::store::SimpleStores<
    hyperast_vcs_git::TStore,
    hyperast_vcs_git::no_space::NoSpaceNodeStoreWrapper<'a>,
    &'a hyperast::store::labels::LabelStore,
>;

fn diferential_filter(
    repo: &hyperast_vcs_git::processing::ConfiguredRepo2,
    baseline: Oid,
    commit: Oid,
    current_tr: IdN,
    other_tr: IdN,
    stores: &SimpleStores<hyperast_vcs_git::TStore>,
    hyperast: &NoS,
    baseline_results: Vec<StructuralPosition<IdN, u16>>,
    other_results: Vec<StructuralPosition<IdN, u16>>,
    subtree_mappings: hyper_diff::matchers::mapping_store::MultiVecStore<u32>,
    mut mapper: Mapper<
        &NoS,
        Decompressible<&NoS, &mut LazyPostOrder<IdN, u32>>,
        Decompressible<&NoS, &mut LazyPostOrder<IdN, u32>>,
        impl MappingStore<Src = u32, Dst = u32>,
    >,
) -> (
    Vec<StructuralPosition<IdN, u16>>,
    Vec<StructuralPosition<IdN, u16>>,
) {
    let baseline_results: Vec<_> = baseline_results
        .into_iter()
        .filter(|x| {
            log::trace!("filtering");
            let (_, _x, no_spaces_path_to_target) =
                hyperast::position::compute_position_with_no_spaces(
                    current_tr,
                    &mut x.iter_offsets(),
                    stores,
                );
            assert_eq!(
                _x,
                x.node(),
                "{:?} != {:?} for {} vs {}",
                x.node(),
                _x,
                hyperast.resolve_type(&x.node()).as_static_str(),
                hyperast.resolve_type(&_x).as_static_str()
            );

            let mut src = mapper.src_arena.root();
            for i in no_spaces_path_to_target {
                if subtree_mappings.is_src(&src) {
                    let a = globalize(
                        repo,
                        baseline,
                        (x.make_position(stores), x.iter_offsets().collect()),
                    );
                    log::trace!("mapped: {a:?}");
                    return false;
                }
                use hyper_diff::decompressed_tree_store::LazyDecompressedTreeStore;
                let cs = mapper.src_arena.decompress_children(&src);
                if cs.is_empty() {
                    let ty = hyperast.resolve_type(&mapper.src_arena.original(&src));
                    log::debug!("empty {}", ty.as_static_str());
                    return true;
                }
                // Gracefully handling possibly wrong param
                // before: // src = cs[i as usize];
                let Some(s) = cs.get(i as usize) else {
                    let a = globalize(
                        repo,
                        baseline,
                        (x.make_position(stores), x.iter_offsets().collect()),
                    );
                    let id = mapper.src_arena.original(&src);
                    let t = hyperast.resolve_type(&id);
                    let (_, _, no_spaces_path_to_target) =
                        hyperast::position::compute_position_with_no_spaces(
                            current_tr,
                            &mut x.iter_offsets(),
                            stores,
                        );
                    log::warn!(
                        "no such child: {a:?} {t:?} {} {} {:?}",
                        cs.len(),
                        i,
                        no_spaces_path_to_target
                    );
                    return true;
                };
                src = *s;
            }
            let mappings = &mapper.mapping.mappings;
            log::debug!("mapped = {}", mappings.is_src(&src));
            !(mappings.is_src(&src) || subtree_mappings.is_src(&src))
        })
        .collect();
    log::debug!("filtering other");
    let arena = &mut mapper.mapping.dst_arena;
    let tr = other_tr;
    let is_subtree_mapped = |node| subtree_mappings.is_dst(&node);
    let is_mapped = |node| mapper.mapping.mappings.is_dst(&node);
    let other_results: Vec<_> = other_results
        .into_iter()
        .filter(|x| {
            log::trace!("filtering");
            let (_, _x, no_spaces_path_to_target) =
                hyperast::position::compute_position_with_no_spaces(
                    tr,
                    &mut x.iter_offsets(),
                    stores,
                );
            assert_eq!(
                _x,
                x.node(),
                "{:?} != {:?} for {} vs {}",
                x.node(),
                _x,
                hyperast.resolve_type(&x.node()).as_static_str(),
                hyperast.resolve_type(&_x).as_static_str()
            );

            let mut target = arena.root();
            for i in no_spaces_path_to_target {
                if subtree_mappings.is_dst(&target) {
                    let a = globalize(
                        repo,
                        commit,
                        (x.make_position(stores), x.iter_offsets().collect()),
                    );
                    log::trace!("mapped: {a:?}");
                    return false;
                }
                use hyper_diff::decompressed_tree_store::LazyDecompressedTreeStore;
                let cs = arena.decompress_children(&target);
                if cs.is_empty() {
                    let ty = hyperast.resolve_type(&arena.original(&target));
                    log::debug!("empty {}", ty.as_static_str());
                    return true;
                }
                let Some(s) = cs.get(i as usize) else {
                    let a = globalize(
                        repo,
                        commit,
                        (x.make_position(stores), x.iter_offsets().collect()),
                    );
                    let id = arena.original(&target);
                    let t = hyperast.resolve_type(&id);
                    let (_, _, no_spaces_path_to_target) =
                        hyperast::position::compute_position_with_no_spaces(
                            tr,
                            &mut x.iter_offsets(),
                            stores,
                        );
                    log::warn!(
                        "no such child: {a:?} {t:?} {} {} {:?}",
                        cs.len(),
                        i,
                        no_spaces_path_to_target
                    );
                    return true;
                };
                target = *s;
            }
            let mappings = &mapper.mapping.mappings;
            log::debug!("mapped = {}", mappings.is_dst(&target));
            !(mappings.is_dst(&target) || subtree_mappings.is_dst(&target))
        })
        .collect();
    log::info!(
        "done filtering to {}+{} evolutions",
        baseline_results.len(),
        other_results.len()
    );
    (baseline_results, other_results)
}

fn differential_aux(
    stores: &hyperast::store::SimpleStores<hyperast_vcs_git::TStore>,
    code: NodeIdentifier,
    query: &hyperast_tsquery::Query,
    _timeout: std::time::Duration,
    _max_matches: u64,
) -> Result<
    (
        Vec<hyperast::position::StructuralPosition<NodeIdentifier, u16>>,
        Vec<Vec<NodeIdentifier>>,
    ),
    MatchingError<ComputeResult>,
> {
    let pos = hyperast::position::StructuralPosition::new(code);
    let cursor = hyperast_tsquery::hyperast_cursor::TreeCursor::new(stores, pos);
    let qcursor = query.matches(cursor);
    let now = Instant::now();
    assert_eq!(
        query.enabled_pattern_count(),
        1,
        "details on a single pattern"
    );
    let mut results = vec![];
    let mut result_names = vec![];
    let rrr = query
        .capture_index_for_name("root")
        .expect("@root at the end of query pattern");
    let name_cid = query.capture_index_for_name("name");
    for m in qcursor {
        let i = m.pattern_index;
        let i = query.enabled_pattern_index(i).unwrap();
        assert_eq!(i, 0, "details on a single pattern");
        let node = m
            .nodes_for_capture_index(rrr)
            .next()
            .expect("@root at the end of query pattern");
        let pos = node.pos.clone();
        let names = name_cid
            .iter()
            .flat_map(|name_cid| {
                m.nodes_for_capture_index(*name_cid)
                    .map(|node| node.pos.node())
            })
            .collect::<Vec<_>>();
        results.push(pos);
        result_names.push(names);
        let compute_time = now.elapsed();
    }
    let compute_time = now.elapsed().as_secs_f64();
    Ok((results, result_names))
}
