//! Benchmark of different search approaches (incremental listing):
//! - original TS
//! - `+` port of the executor
//! - `+` HyperAST tree cursor
//! - `+` HyperAST tree cursor with some path compression
//!   - including variants resulting from profile driven optimizations
//! - `+` using preprocessed subpatterns

use std::fmt::{Debug, Display};
use std::ops::AddAssign;

use git2::Oid;
use hyperast::compat::HashMap;
use hyperast::position::structural_pos::{CursorHead, CursorHeadMove, CursorWithPersistence};
use hyperast::store::SimpleStores;
use hyperast::store::defaults::NodeIdentifier;
use hyperast::types::{HyperAST, HyperType, NodeStore, WithPrecompQueries, WithRoles, WithStats};
use hyperast::utils::memusage;
use hyperast_tsquery::Query;
use hyperast_vcs_git::multi_preprocessed::PreProcessedRepositories;
use hyperast_vcs_git::processing::{ConfiguredRepo2, RepoConfig};

use crate::TimeoutError as Error;
use crate::commit_rw;
use crate::{Cumulative, NonBlockingResLogger, ResultLogger, Timeout};

type Idx = u16;

pub(crate) fn multi_run<TS: 'static, H, P, Ex: Executor<TS>, R, C: ResultLogger<R>>(
    cumulative: &mut C,
    repositories: &mut H,
    language: &tree_sitter::Language,
    sub: &[&str],
    queries: impl Iterator<Item = String>,
    parse_and_execute: impl Fn(&mut C, &mut H, Ex) -> Result<(), Error>,
) where
    TS: hyperast::types::TypeStore + hyperast::types::RoleStore,
    <TS as hyperast::types::RoleStore>::IdF: Into<u16> + From<u16>,
{
    for query in queries {
        let executor = match Ex::create(sub, &query, language) {
            Ok(executor) => executor,
            Err(err) => {
                eprintln!("{err}");
                break;
            }
        };
        if let Err(err) = cumulative.query_compiled() {
            eprintln!("{err}");
            break;
        }
        if let Err(err) = parse_and_execute(cumulative, repositories, executor) {
            eprintln!("{err}");
            break;
        }
    }
}

pub trait Executor<TS: 'static> {
    type P<IdN, Idx>;
    type R;
    fn create(
        sub: &[&str],
        query: &str,
        language: &tree_sitter::Language,
    ) -> Result<Self, tree_sitter::QueryError>
    where
        Self: Sized;

    fn execute<HAST: HyperAST<TS = TS>>(
        &self,
        stores: &HAST,
        pos: Self::P<HAST::IdN, HAST::Idx>,
        count: &mut Self::R,
    ) -> Self::P<HAST::IdN, HAST::Idx>
    where
        HAST::IdN: Copy + Debug,
        for<'t> <HAST as hyperast::types::AstLending<'t>>::RT:
            WithPrecompQueries + WithRoles + WithStats,
        HAST::IdN: hyperast::types::NodeId<IdN = HAST::IdN>;
}

pub trait SkippingExecutor<TS: 'static>: Executor<TS> {
    fn can_skip<N: WithPrecompQueries>(&self, n: &N) -> bool;
}

#[derive(Default, Debug, Clone)]
pub struct RichResult<R> {
    result: R,
    status_count: usize,
    goto_count: usize,
    node_count: usize,
}

impl<R: AddAssign> AddAssign<RichResult<R>> for RichResult<R> {
    fn add_assign(&mut self, other: RichResult<R>) {
        self.result += other.result;
        self.status_count += other.status_count;
        self.goto_count += other.goto_count;
        self.node_count += other.node_count;
    }
}

impl<R: AddAssign + Clone> AddAssign<&RichResult<R>> for RichResult<R> {
    fn add_assign(&mut self, other: &RichResult<R>) {
        self.result += other.result.clone();
        // self.status_count += other.status_count;
        // self.goto_count += other.goto_count;
        // self.node_count += other.node_count;
    }
}

impl<R> Display for crate::CsvHeader<RichResult<R>> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "value;status_count;goto_count;node_count")
    }
}

impl<R: Display> Display for RichResult<R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}; {}; {}; {}",
            self.result, self.status_count, self.goto_count, self.node_count
        )
    }
}

impl<TS: 'static> Executor<TS> for hyperast_tsquery::Query
where
    TS: hyperast::types::TypeStore + hyperast::types::RoleStore,
    <TS as hyperast::types::RoleStore>::IdF: Into<u16> + From<u16>,
{
    type P<IdN, Idx> = CursorWithPersistence<IdN, Idx>;
    type R = RichResult<usize>;

    fn create(
        sub: &[&str],
        query: &str,
        language: &tree_sitter::Language,
    ) -> Result<Self, tree_sitter::QueryError> {
        compile_query(query, sub, language.clone())
    }

    fn execute<HAST: HyperAST<TS = TS>>(
        &self,
        stores: &HAST,
        pos: Self::P<HAST::IdN, HAST::Idx>,
        count: &mut Self::R,
    ) -> Self::P<HAST::IdN, HAST::Idx>
    where
        HAST::IdN: Copy + Debug,
        for<'t> <HAST as hyperast::types::AstLending<'t>>::RT:
            WithPrecompQueries + WithRoles + WithStats,
        HAST::IdN: hyperast::types::NodeId<IdN = HAST::IdN>,
    {
        use hyperast_tsquery::hyperast_opt::TreeCursor;
        {
            let n = stores.node_store().resolve(&pos.node());
            count.node_count += n.size();
        }
        let cursor = TreeCursor::new(stores, pos);
        let mut qcursor = self.matches(cursor);
        while let Some(_) = qcursor.next() {
            count.result += 1;
        }
        count.status_count += qcursor.status_count;
        count.goto_count += qcursor.goto_count;
        while qcursor.cursor.pos.node() != qcursor.cursor.pos.p.node() {
            assert!(qcursor.cursor.pos.up());
        }
        qcursor.cursor.pos.pos
    }
}

impl<TS: 'static> SkippingExecutor<TS> for hyperast_tsquery::Query
where
    TS: hyperast::types::TypeStore + hyperast::types::RoleStore,
    <TS as hyperast::types::RoleStore>::IdF: Into<u16> + From<u16>,
{
    fn can_skip<N: WithPrecompQueries>(&self, n: &N) -> bool {
        self.used_precomputed != 0 && n.wont_match_given_precomputed_queries(self.used_precomputed)
    }
}

pub fn tsquery_execute_count<TS: 'static>(
    query: &str,
    language: &tree_sitter::Language,
) -> impl Fn(
    &SimpleStores<TS>,
    CursorWithPersistence<NodeIdentifier>,
    &mut usize,
) -> CursorWithPersistence<NodeIdentifier>
+ use<TS>
where
    TS: hyperast::types::TypeStore + hyperast::types::RoleStore,
    <TS as hyperast::types::RoleStore>::IdF: Into<u16> + From<u16>,
{
    use hyperast_tsquery::hyperast_opt::TreeCursor;
    let query = hyperast_tsquery::Query::new(query, language.clone()).unwrap();
    move |stores, pos, count| {
        let cursor = TreeCursor::new(stores, pos);
        let mut qcursor = query.matches(cursor);
        while let Some(_) = qcursor.next() {
            *count += 1;
        }
        while qcursor.cursor.pos.node() != qcursor.cursor.pos.p.node() {
            assert!(qcursor.cursor.pos.up());
        }
        qcursor.cursor.pos.pos
    }
}

pub fn per_blob(
    repo: hyperast_vcs_git::git::Repo,
    sub: &[&str],
    commit: &str,
    depth: usize,
    language: &tree_sitter::Language,
    queries: impl Iterator<Item = String>,
    timeout: Timeout,
) {
    let config = crate::Config {
        config: RepoConfig::Java,
        first_chunk: 1,
        chunk_interval: 1,
        depth,
    };

    // let mut cumulative = Cumulative::<usize>::with_timeout(timeout);
    let mut cumulative = NonBlockingResLogger::with_timeout(std::io::stdout(), timeout);
    dbg!(memusage().to_string());

    let (mut repositories, repository) = prepare_hyperast(repo, config.config, sub);
    if let Err(err) = cumulative.repo_prepared() {
        eprintln!("Error preparing repository: {}", err);
        cumulative.finish();
        return;
    }

    let first_chunk = depth.min(config.first_chunk);

    dbg!(memusage().to_string());

    use hyperast::position::structural_pos::CursorWithPersistence;
    use hyperast_gen_ts_java::types::TStore;
    multi_run::<TStore, _, CursorWithPersistence<NodeIdentifier>, hyperast_tsquery::Query, _, _>(
        &mut cumulative,
        &mut repositories,
        language,
        sub,
        queries,
        |cumulative, mut repositories, executor| {
            dbg!(memusage().to_string());
            let mut rw = commit_rw(commit, Some(depth), &repository.repo).unwrap();
            let commits = repositories.pre_process_chunk(&mut rw, &repository, first_chunk);
            cumulative.commit_prepared(commits.len())?;
            dbg!(memusage().to_string());
            execute_on_commits_per_blob::<_, TStore, _>(
                config,
                &mut repositories,
                &repository,
                cumulative,
                rw,
                commits.into_iter(),
                |root| CursorWithPersistence::new(root),
                executor,
            )
        },
    );
    cumulative.finish();
}

pub fn per_blob_nospaces(
    repo: hyperast_vcs_git::git::Repo,
    sub: &[&str],
    commit: &str,
    depth: usize,
    language: &tree_sitter::Language,
    queries: impl Iterator<Item = String>,
    timeout: Timeout,
) {
    let config = crate::Config {
        config: RepoConfig::Java,
        first_chunk: 1,
        chunk_interval: 1,
        depth,
    };

    // let mut cumulative = Cumulative::<usize>::with_timeout(timeout);
    let mut cumulative = NonBlockingResLogger::with_timeout(std::io::stdout(), timeout);
    dbg!(memusage().to_string());

    let (mut repositories, repository) = prepare_hyperast(repo, config.config, sub);
    if let Err(err) = cumulative.repo_prepared() {
        eprintln!("Error preparing repository: {}", err);
        cumulative.finish();
        return;
    }

    let first_chunk = depth.min(config.first_chunk);

    dbg!(memusage().to_string());

    use hyperast::position::structural_pos::CursorWithPersistence;
    use hyperast_gen_ts_java::types::TStore;
    multi_run::<TStore, _, CursorWithPersistence<NodeIdentifier>, hyperast_tsquery::Query, _, _>(
        &mut cumulative,
        &mut repositories,
        language,
        sub,
        queries,
        |cumulative, mut repositories, executor| {
            dbg!(memusage().to_string());
            let mut rw = commit_rw(commit, Some(depth), &repository.repo).unwrap();
            let commits = repositories.pre_process_chunk(&mut rw, &repository, first_chunk);
            cumulative.commit_prepared(commits.len())?;
            dbg!(memusage().to_string());
            execute_on_commits_per_blob_nospaces::<_, TStore, _>(
                config,
                &mut repositories,
                &repository,
                cumulative,
                rw,
                commits.into_iter(),
                |root| CursorWithPersistence::new(root),
                executor,
            )
        },
    );
    cumulative.finish();
}

pub fn per_blob_cached(
    repo: hyperast_vcs_git::git::Repo,
    sub: &[&str],
    commit: &str,
    depth: usize,
    language: &tree_sitter::Language,
    queries: impl Iterator<Item = String>,
    timeout: Timeout,
) {
    let config = crate::Config {
        config: RepoConfig::Java,
        first_chunk: 1,
        chunk_interval: 1,
        depth,
    };
    // let mut cumulative = Cumulative::<usize>::with_timeout(timeout);
    let mut cumulative = NonBlockingResLogger::with_timeout(std::io::stdout(), timeout);
    dbg!(memusage().to_string());

    let (mut repositories, repository) = prepare_hyperast(repo, config.config, sub);
    if let Err(err) = cumulative.repo_prepared() {
        eprintln!("Error preparing repository: {}", err);
        cumulative.finish();
        return;
    }

    let first_chunk = depth.min(config.first_chunk);

    dbg!(memusage().to_string());

    use hyperast::position::structural_pos::CursorWithPersistence;
    use hyperast_gen_ts_java::types::TStore;
    multi_run::<TStore, _, CursorWithPersistence<NodeIdentifier>, hyperast_tsquery::Query, _, _>(
        &mut cumulative,
        &mut repositories,
        language,
        sub,
        queries,
        |cumulative, mut repositories, executor| {
            dbg!(memusage().to_string());
            let mut rw = commit_rw(commit, Some(depth), &repository.repo).unwrap();
            let commits = repositories.pre_process_chunk(&mut rw, &repository, first_chunk);
            cumulative.commit_prepared(commits.len())?;
            dbg!(memusage().to_string());
            execute_on_commits_per_blob_cached::<_, TStore, _>(
                config,
                &mut repositories,
                &repository,
                cumulative,
                rw,
                commits.into_iter(),
                |root| CursorWithPersistence::new(root),
                executor,
            )
        },
    );
    cumulative.finish();
}

pub fn polyglot(
    repo: hyperast_vcs_git::git::Repo,
    sub: &[&str],
    commit: &str,
    depth: usize,
    language: &tree_sitter::Language,
    queries: impl Iterator<Item = String>,
    timeout: Timeout,
) {
    let config = crate::Config {
        config: RepoConfig::Java,
        first_chunk: 1,
        chunk_interval: 1,
        depth,
    };

    let mut cumulative = Cumulative::<RichResult<usize>>::with_timeout(timeout);
    dbg!(memusage().to_string());

    let (mut repositories, repository) = prepare_hyperast(repo, config.config, sub);
    if let Err(err) = cumulative.repo_prepared() {
        eprintln!("Error preparing repository: {}", err);
        cumulative.write_to(std::io::stdout());
        return;
    }

    let first_chunk = depth.min(config.first_chunk);

    dbg!(memusage().to_string());

    use hyperast::position::structural_pos::CursorWithPersistence;
    use hyperast_vcs_git::TStore;
    multi_run::<TStore, _, CursorWithPersistence<NodeIdentifier>, hyperast_tsquery::Query, _, _>(
        &mut cumulative,
        &mut repositories,
        language,
        sub,
        queries,
        |mut cumulative, mut repositories, executor| {
            dbg!(memusage().to_string());
            let mut rw = commit_rw(commit, Some(depth), &repository.repo).unwrap();
            let commits = repositories.pre_process_chunk(&mut rw, &repository, first_chunk);
            cumulative.commit_prepared(commits.len())?;
            dbg!(memusage().to_string());
            execute_on_commits_polyglot::<_, _>(
                config,
                &mut repositories,
                &repository,
                &mut cumulative,
                rw,
                commits.into_iter(),
                |root| CursorWithPersistence::new(root),
                executor,
            )
        },
    );
    cumulative.write_to(std::io::stdout());
}

pub(crate) fn execute_on_commits_per_blob<P, TS: 'static, R: Default + Display>(
    config: crate::Config,
    repositories: &mut PreProcessedRepositories,
    repository: &ConfiguredRepo2,
    cumulative: &mut impl ResultLogger<R>,
    mut rw: impl Iterator<Item = Oid>,
    mut commits: std::vec::IntoIter<Oid>,
    make_pos: impl Fn(NodeIdentifier) -> P,
    executor: impl SkippingExecutor<TS, P<NodeIdentifier, Idx> = P, R = R>,
) -> Result<(), Error>
where
    hyperast_vcs_git::TStore: hyperast::store::TyDown<TS>,
    TS: hyperast::types::TypeStore + hyperast::types::RoleStore,
    P: CursorHead<NodeIdentifier, Idx>,
    P: CursorHeadMove<NodeIdentifier, Idx>,
{
    let crate::Config { chunk_interval, .. } = config;

    // # iterate over commits
    loop {
        let oid = if let Some(oid) = commits.next() {
            oid
        } else {
            let c = repositories.pre_process_chunk(&mut rw, &repository, chunk_interval);
            if c.is_empty() {
                break;
            }
            cumulative.commit_prepared(c.len())?;
            commits = c.into_iter();
            continue;
        };
        let commit = repositories.get_commit(&repository.config, &oid).unwrap();
        let root = commit.ast_root;
        let mut pos = make_pos(root);

        let stores = repositories.processor.main_stores.with_ts::<TS>();

        let mut count = R::default();
        let mut down = true;
        // iterate over java files
        loop {
            use hyperast::types::WithChildren;
            if down {
                let id = pos.node();
                let k = stores.resolve_type(&id);
                let n = stores.node_store.resolve(id);

                if executor.can_skip(&n) {
                    down = false;
                    continue;
                }
                if k.is_directory() {
                    if let Some(id) = n.child(&0) {
                        pos.down(id, 0);
                    } else {
                        down = false;
                    }
                } else if k.is_file() {
                    pos = executor.execute(stores, pos, &mut count);
                    down = false;
                }
            } else {
                let Some(p) = pos.parent() else {
                    break;
                };
                let n = stores.node_store.resolve(p);

                let o = pos.offset() + 1;
                if let Some(id) = n.child(&o) {
                    pos.inc(id);
                    down = true;
                } else {
                    pos.up();
                }
            }
        }
        let size = stores.node_store.resolve(root).size();
        cumulative.commit_result_with_size(count, size)?;
    }
    Ok(())
}

pub(crate) fn execute_on_commits_per_blob_nospaces<P, TS: 'static, R: Default + Display>(
    config: crate::Config,
    repositories: &mut PreProcessedRepositories,
    repository: &ConfiguredRepo2,
    cumulative: &mut impl ResultLogger<R>,
    mut rw: impl Iterator<Item = Oid>,
    mut commits: std::vec::IntoIter<Oid>,
    make_pos: impl Fn(NodeIdentifier) -> P,
    executor: impl SkippingExecutor<TS, P<NodeIdentifier, Idx> = P, R = R>,
) -> Result<(), Error>
where
    hyperast_vcs_git::TStore: hyperast::store::TyDown<TS>,
    TS: hyperast::types::TypeStore + hyperast::types::RoleStore,
    P: CursorHead<NodeIdentifier, Idx>,
    P: CursorHeadMove<NodeIdentifier, Idx>,
{
    let crate::Config { chunk_interval, .. } = config;

    // # iterate over commits
    loop {
        let oid = if let Some(oid) = commits.next() {
            oid
        } else {
            let c = repositories.pre_process_chunk(&mut rw, &repository, chunk_interval);
            if c.is_empty() {
                break;
            }
            cumulative.commit_prepared(c.len())?;
            commits = c.into_iter();
            continue;
        };
        let commit = repositories.get_commit(&repository.config, &oid).unwrap();
        let root = commit.ast_root;
        let mut pos = make_pos(root);

        let stores = repositories.processor.main_stores.with_ts::<TS>();
        todo!("rework no_space")
    }
    Ok(())
}

pub(crate) fn execute_on_commits_per_blob_cached<
    P,
    TS: 'static,
    R: Default + AddAssign + for<'a> AddAssign<&'a R> + Clone + Debug,
>(
    config: crate::Config,
    repositories: &mut PreProcessedRepositories,
    repository: &ConfiguredRepo2,
    cumulative: &mut impl ResultLogger<R>,
    mut rw: impl Iterator<Item = Oid>,
    mut commits: std::vec::IntoIter<Oid>,
    make_pos: impl Fn(NodeIdentifier) -> P,
    executor: impl SkippingExecutor<TS, P<NodeIdentifier, Idx> = P, R = R>,
) -> Result<(), Error>
where
    hyperast_vcs_git::TStore: hyperast::store::TyDown<TS>,
    TS: hyperast::types::TypeStore + hyperast::types::RoleStore,
    P: CursorHead<NodeIdentifier, Idx>,
    P: CursorHeadMove<NodeIdentifier, Idx>,
{
    let crate::Config { chunk_interval, .. } = config;
    let mut cache = HashMap::<NodeIdentifier, R>::new();

    // # iterate over commits
    loop {
        let oid = if let Some(oid) = commits.next() {
            oid
        } else {
            let c = repositories.pre_process_chunk(&mut rw, &repository, chunk_interval);
            if c.is_empty() {
                break;
            }
            cumulative.commit_prepared(c.len())?;
            commits = c.into_iter();
            continue;
        };
        let commit = repositories.get_commit(&repository.config, &oid).unwrap();
        let root = commit.ast_root;
        let mut pos = make_pos(root);

        let stores = repositories.processor.main_stores.with_ts::<TS>();

        let mut count = R::default();
        let mut down = true;
        // iterate over java files
        loop {
            use hyperast::types::WithChildren;
            if down {
                let id = pos.node();
                let k = stores.resolve_type(&id);
                let n = stores.node_store.resolve(id);

                if executor.can_skip(&n) {
                    down = false;
                    continue;
                }
                if k.is_directory() {
                    if let Some(id) = n.child(&0) {
                        pos.down(id, 0);
                    } else {
                        down = false;
                    }
                } else if k.is_file() {
                    if let Some(c) = cache.get(&id) {
                        count += c;
                    } else {
                        let mut c = Default::default();
                        pos = executor.execute(stores, pos, &mut c);
                        cache.insert(id, c.clone());
                        count += c;
                    }
                    down = false;
                }
            } else {
                let Some(p) = pos.parent() else {
                    break;
                };
                let n = stores.node_store.resolve(p);

                let o = pos.offset() + 1;
                if let Some(id) = n.child(&o) {
                    pos.inc(id);
                    down = true;
                } else {
                    pos.up();
                }
            }
        }
        let size = stores.node_store.resolve(root).size();
        cumulative.commit_result_with_size(count, size)?;
    }
    Ok(())
}

fn compile_query(
    query: &str,
    sub: &[&str],
    language: tree_sitter::Language,
) -> Result<Query, tree_sitter::QueryError> {
    log::error!("input sub: {:?}", sub);
    log::error!("input query: {:?}", query);

    let query = if sub.is_empty() {
        Query::new(query, language.clone())?
    } else {
        let query = Query::with_precomputed(query, language.clone(), sub);
        let query = query?;
        log::error!("sub: {}", query.0);
        query.1
    };
    log::error!("main: {}", query);
    Ok(query)
}

fn prepare_hyperast(
    repo: hyperast_vcs_git::git::Repo,
    config: RepoConfig,
    sub: &[&str],
) -> (PreProcessedRepositories, ConfiguredRepo2) {
    let mut repositories = PreProcessedRepositories::default();
    if sub.is_empty() {
        // no need to set empty prequeries
        repositories.register_config(repo.clone(), config);
    } else {
        repositories.register_config_with_prequeries(repo.clone(), config, sub);
    }
    let repo_h = repositories.get_config(repo).unwrap();
    // nofetch avoids potential noise and unintentional dataset changes
    let repository = repo_h.nofetch();
    (repositories, repository)
}

pub fn avg_sub_first(repo: hyperast_vcs_git::git::Repo, commit: &str, depth: usize, query: &str) {
    const SUB: &'static [&'static str] = &[
        r#"(method_invocation
        (identifier) (#EQ? "fail")
    )"#,
        r#"(try_statement
    )"#,
    ];
    let start_instant = std::time::Instant::now();

    // ## compile query
    let language = tree_sitter::Language::new(tree_sitter_java::LANGUAGE);
    let _sub: &[&str] = SUB;
    let (_precomp, query) = Query::with_precomputed(query, language.clone(), _sub).unwrap();
    // let query = hyperast_tsquery::Query::new(query, language.clone()).unwrap();
    let q_comp_dur = start_instant.elapsed();

    // # build
    let config = RepoConfig::Java;
    let limit = Some(depth);
    let mut repositories = PreProcessedRepositories::default();
    repositories.register_config_with_prequeries(repo.clone(), config, SUB);
    let repo_h = repositories
        .get_config(repo)
        .ok_or_else(|| "missing config for repository".to_string())
        .unwrap();
    let repository = repo_h.nofetch();
    let mut rw = commit_rw(commit, limit, &repository.repo).unwrap();
    // ## parse files
    // ## dedup subtrees
    // ## index patterns
    let mut commits = repositories
        .pre_process_chunk(&mut rw, &repository, 1)
        .into_iter();
    let start_instant = std::time::Instant::now();

    // # code search

    // ### find subpatterns
    // ## execute query
    let Some(oid) = commits.next() else { panic!() };
    let commit = repositories.get_commit(&repository.config, &oid).unwrap();
    let root = commit.ast_root;
    let mut pos = hyperast::position::structural_pos::CursorWithPersistence::new(root);

    let stores = &repositories
        .processor
        .main_stores
        .with_ts::<hyperast_gen_ts_java::types::TStore>();

    let mut repeat = 100;
    let mut count = 0;
    let mut down = true;
    loop {
        use hyperast::types::WithChildren;
        if down {
            let id = pos.node();
            let k = stores.resolve_type(&id);
            let n = stores.node_store.resolve(id);
            if k.is_directory() {
                if let Some(id) = n.child(&0) {
                    pos.down(id, 0);
                } else {
                    down = false;
                }
            } else if k.is_file() {
                use hyperast_tsquery::hyperast_opt::TreeCursor;
                let cursor = TreeCursor::new(stores, pos);
                let mut qcursor = query.matches(cursor);
                while let Some(_) = qcursor.next() {
                    count += 1;
                }
                while qcursor.cursor.pos.node() != id {
                    assert!(qcursor.cursor.pos.up());
                }
                pos = qcursor.cursor.pos.pos;
                down = false;
            } else {
                panic!("{}", k.as_static_str())
            }
        } else {
            let p = if let Some(p) = pos.parent() {
                p
            } else if repeat > 0 {
                repeat -= 1;
                count = 0;
                down = true;
                pos = hyperast::position::structural_pos::CursorWithPersistence::new(root);
                continue;
            } else {
                break;
            };
            let n = stores.node_store.resolve(p);

            let o = pos.offset() + 1;
            if let Some(id) = n.child(&o) {
                pos.inc(id);
                down = true;
            } else {
                pos.up();
            }
        }
    }

    dbg!(count);
    let commit_duration = start_instant.elapsed() / 100;
    println!("duration {:?}", commit_duration);
    eprintln!("Finished in {:?}", start_instant.elapsed());
    eprintln!("Query compilation duration: {:?}", q_comp_dur);
    eprintln!("ELAPSED for computing STATUS: {:?}", unsafe {
        hyperast_tsquery::ELAPSED_STATUS
    });
}

pub fn execute_on_commits_polyglot<P, R: Default + Display>(
    config: crate::Config,
    repositories: &mut PreProcessedRepositories,
    repository: &ConfiguredRepo2,
    cumulative: &mut Cumulative<R>,
    mut rw: impl Iterator<Item = Oid>,
    mut commits: std::vec::IntoIter<Oid>,
    make_pos: impl Fn(NodeIdentifier) -> P,
    executor: impl SkippingExecutor<hyperast_vcs_git::TStore, P<NodeIdentifier, Idx> = P, R = R>,
) -> Result<(), Error>
where
    P: CursorHead<NodeIdentifier, Idx>,
    P: CursorHeadMove<NodeIdentifier, Idx>,
{
    let chunk_interval = config.chunk_interval;
    // # code search
    loop {
        let Some(oid) = commits.next() else {
            let c = repositories.pre_process_chunk(&mut rw, &repository, chunk_interval);
            if c.is_empty() {
                break;
            }
            cumulative.commit_prepared(c.len())?;
            commits = c.into_iter();
            continue;
        };
        let commit = repositories.get_commit(&repository.config, &oid).unwrap();
        let root = commit.ast_root;
        let pos = make_pos(root);

        let stores = &repositories.processor.main_stores;

        let mut count = R::default();
        executor.execute(stores, pos, &mut count);

        let size = stores.node_store.resolve(root).size();
        cumulative.commit_result_with_size(count, size)?;
    }
    Ok(())
}
// TODO generalize it too
pub fn sub_java_maven0(
    repo: hyperast_vcs_git::git::Repo,
    sub: &[&str],
    commit: &str,
    depth: usize,
    query: &str,
    timeout: Timeout,
) -> Result<(), Error> {
    let mut cumulative = Cumulative::with_timeout(timeout);

    // # build
    let config = RepoConfig::JavaMaven;
    let limit = Some(depth);
    let mut repositories = PreProcessedRepositories::default();
    repositories.register_config_with_prequeries(repo.clone(), config, sub);
    let repo_h = repositories
        .get_config(repo)
        .ok_or_else(|| "missing config for repository".to_string())
        .unwrap();
    let repository = repo_h.nofetch();
    cumulative.repo_prepared()?;
    let mut rw = commit_rw(commit, limit, &repository.repo).unwrap();
    // ## parse files
    // ## dedup subtrees
    // ## index patterns
    let mut commits = repositories
        .pre_process_chunk(&mut rw, &repository, 100)
        .into_iter();
    cumulative.commit_prepared(commits.len())?;

    // ## compile query
    let language = tree_sitter::Language::new(tree_sitter_java::LANGUAGE);
    let query = compile_query(query, sub, language).unwrap();
    cumulative.query_compiled()?;

    let mut result = HashMap::<Oid, usize>::new();

    // # code search
    loop {
        let Some(oid) = commits.next() else {
            let c = repositories.pre_process_chunk(&mut rw, &repository, 1);
            if c.is_empty() {
                break;
            }
            cumulative.commit_prepared(c.len())?;
            commits = c.into_iter();
            continue;
        };
        let commit = repositories.get_commit(&repository.config, &oid).unwrap();
        let root = commit.ast_root;
        let pos = hyperast::position::structural_pos::CursorWithPersistence::new(root);
        // let pos = hyperast::position::StructuralPosition::new(root);
        // use hyperast_tsquery::hyperast_cursor::TreeCursor;
        // use hyperast_tsquery::hyperast_opt::hyperast_opt4::TreeCursor;
        use hyperast_tsquery::hyperast_opt::opt5::tree_cursor;

        let stores = &repositories.processor.main_stores;

        let cursor = tree_cursor(stores, pos);
        let qcursor = query.matches(cursor);
        let count = qcursor.count();

        let size = stores.node_store.resolve(root).size();
        cumulative.commit_result_with_size(count, size)?;
        result.insert(oid, count);
    }

    cumulative.write_to(std::io::stdout());
    Ok(())
}
