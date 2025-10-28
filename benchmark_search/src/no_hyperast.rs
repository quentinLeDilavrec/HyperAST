use git2::Oid;
use hyperast::{compat::HashMap, utils::memusage};
use std::fmt::Display;
use std::ops::AddAssign;

use crate::NonBlockingResLogger;
use crate::TimeoutError as Error;
use crate::commit_rw;
use crate::{Cumulative, ResultLogger, Timeout};

type FileId = Oid;

pub trait Executor {
    type P<'t>: Copy;
    type R;
    fn create(
        sub: &[&str],
        query: &str,
        language: &tree_sitter::Language,
    ) -> Result<Self, tree_sitter::QueryError>
    where
        Self: Sized;

    fn execute(&self, pos: Self::P<'_>, res: &mut Self::R);
}

pub trait FileSkippingExecutor: Executor {
    type S;
    fn can_skip(&self, precomp: &Self::S) -> bool;
    fn precomp_execute(&self, pos: Self::P<'_>) -> Self::S;
    fn both_execute(&self, pos: Self::P<'_>, res: &mut Self::R) -> Self::S {
        // TODO run in a single pass, both precomp and query
        let precomp = self.precomp_execute(pos);
        if !self.can_skip(&precomp) {
            self.execute(pos, res);
        }
        precomp
    }
}

impl Executor for tree_sitter::Query {
    type P<'t> = (&'t [u8], &'t tree_sitter::Tree);
    type R = RichResult<usize>;

    fn create(
        _sub: &[&str],
        query: &str,
        language: &tree_sitter::Language,
    ) -> Result<Self, tree_sitter::QueryError>
    where
        Self: Sized,
    {
        tree_sitter::Query::new(language, query)
    }

    fn execute(&self, pos: Self::P<'_>, res: &mut Self::R) {
        let tree = pos.1;
        let content = pos.0;
        let mut query_cursor = tree_sitter::QueryCursor::new();
        let matches = query_cursor.matches(self, tree.root_node(), content);
        res.result += matches.count();
    }
}

pub fn tree_sitter_execute_count<O>(
    query: &str,
    language: &tree_sitter::Language,
    _: &O,
) -> impl Fn(&[u8], &tree_sitter::Tree, &mut RichResult<usize>) + use<O> {
    let query = <tree_sitter::Query as Executor>::create(&[], query, language).unwrap();
    move |content, tree, res| {
        <tree_sitter::Query as Executor>::execute(&query, (content, tree), res)
    }
}

impl Executor for hyperast_tsquery::Query {
    type P<'t> = (&'t [u8], &'t tree_sitter::Tree);
    type R = RichResult<usize>;

    fn create(
        _sub: &[&str],
        query: &str,
        language: &tree_sitter::Language,
    ) -> Result<Self, tree_sitter::QueryError>
    where
        Self: Sized,
    {
        hyperast_tsquery::Query::new(query, language.clone())
    }

    fn execute(&self, pos: Self::P<'_>, res: &mut Self::R) {
        let tree = pos.1;
        let content = pos.0;
        res.node_count += tree.root_node().descendant_count();
        let tree_cursor = tree.walk();
        let cursor = hyperast_tsquery::default_impls::TreeCursor::new(content, tree_cursor);
        let mut matches = self.matches(cursor);
        while let Some(_) = matches.next() {
            res.result += 1;
        }
        res.status_count += matches.status_count;
        res.goto_count += matches.goto_count;
    }
}

pub fn tsquery_execute_count<O>(
    query: &str,
    language: &tree_sitter::Language,
    _: &O,
) -> impl Fn(&[u8], &tree_sitter::Tree, &mut RichResult<usize>) + use<O> {
    let query = <hyperast_tsquery::Query as Executor>::create(&[], query, language).unwrap();
    move |content, tree, res| {
        <hyperast_tsquery::Query as Executor>::execute(&query, (content, tree), res)
    }
}

pub fn tsquery_execute_count_precomp<O>(
    query: &str,
    language: &tree_sitter::Language,
    other: &(&[&str], O),
) -> (hyperast_tsquery::Query, hyperast_tsquery::Query) {
    let precomputed = other.0;
    Executor::create(precomputed, query, language).unwrap()
}

/// (precomp, query)
impl Executor for (hyperast_tsquery::Query, hyperast_tsquery::Query) {
    type P<'t> = (&'t [u8], &'t tree_sitter::Tree);
    type R = RichResult<usize>;

    fn create(
        precomputeds: &[&str],
        query: &str,
        language: &tree_sitter::Language,
    ) -> Result<Self, tree_sitter::QueryError>
    where
        Self: Sized,
    {
        let (precomp, query) =
            hyperast_tsquery::Query::with_precomputed(query, language.clone(), precomputeds)?;
        Ok((precomp, query))
    }

    fn execute(&self, pos: Self::P<'_>, res: &mut Self::R) {
        let tree = pos.1;
        let content = pos.0;
        let query = &self.1;
        res.node_count += tree.root_node().descendant_count();
        let tree_cursor = tree.walk();
        let cursor = hyperast_tsquery::default_impls::TreeCursor::new(content, tree_cursor);
        let mut matches = query.matches(cursor);
        while let Some(_) = matches.next() {
            res.result += 1;
        }
        res.status_count += matches.status_count;
        res.goto_count += matches.goto_count;
    }
}

type Precomp = u16;

/// (precomp, query)
impl FileSkippingExecutor for (hyperast_tsquery::Query, hyperast_tsquery::Query) {
    type S = Precomp;
    fn can_skip(&self, precomp: &Self::S) -> bool {
        *precomp & self.1.used_precomputed != self.1.used_precomputed
    }
    fn precomp_execute(&self, pos: Self::P<'_>) -> Self::S {
        let mut res = 0;
        let tree = pos.1;
        let content = pos.0;
        let precomp = &self.0;
        let tree_cursor = tree.walk();
        let cursor = hyperast_tsquery::default_impls::TreeCursor::new(content, tree_cursor);
        let all = precomp.used_precomputed;
        let mut matches = precomp.matches(cursor);
        while let Some(m) = matches.next() {
            res |= 1 << m.pattern_index.to_usize();
            if res == all {
                break;
            }
        }
        return res;
    }
}

pub(crate) fn multi_run<'a, R, Ex, O, C: ResultLogger<R>>(
    cumulative: &mut C,
    repository: &'a git2::Repository,
    mut other: O,
    language: &tree_sitter::Language,
    queries: impl Iterator<Item = String>,
    executor_factory: impl Fn(&str, &tree_sitter::Language, &O) -> Ex,
    parse_and_execute: impl Fn(&mut C, &'a git2::Repository, &mut O, Ex) -> Result<(), Error>,
) {
    for query in queries {
        let executor = executor_factory(&query, language, &other);
        if let Err(err) = cumulative.query_compiled() {
            eprintln!("{err}");
            break;
        }
        if let Err(err) = parse_and_execute(cumulative, repository, &mut other, executor) {
            eprintln!("{err}");
            break;
        }
    }
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

/// Special bench approach for original tree-sitter (only file based).
/// Incremental computation at file level associating git Object ids to results.
pub fn baseline(
    repository: git2::Repository,
    commit: &str,
    depth: usize,
    language: &tree_sitter::Language,
    queries: impl Iterator<Item = String>,
    timeout: Timeout,
) {
    let mut cumulative = Cumulative::with_timeout(timeout);
    multi_run(
        &mut cumulative,
        &repository,
        (), // the result cache cannot be reused across different queries
        language,
        queries,
        tree_sitter_execute_count,
        |cumulative, repository, _, executor| {
            parse_and_execute_on_commits_once_per_file(
                cumulative,
                repository,
                commit,
                depth,
                &language,
                |name| name.ends_with(".java"),
                executor,
            )
        },
    );
    cumulative.finish();
}

pub fn baseline_prepare_blob_tree(
    repository: git2::Repository,
    commit: &str,
    depth: usize,
    language: &tree_sitter::Language,
    queries: impl Iterator<Item = String>,
    timeout: Timeout,
) {
    let mut cumulative = Cumulative::with_timeout(timeout);
    let cache = HashMap::<FileId, (tree_sitter::Tree, git2::Blob)>::new();
    multi_run(
        &mut cumulative,
        &repository,
        cache,
        language,
        queries,
        tree_sitter_execute_count,
        |cumulative, repository, cache, executor| {
            parse_and_execute_on_commits_prepare_cache_trees_and_blobs2(
                cumulative,
                cache,
                repository,
                commit,
                depth,
                &language,
                |name| name.ends_with(".java"),
                executor,
            )
        },
    );
    cumulative.finish();
}

/// Same but with our query executor
pub fn baseline_our_executor(
    repository: git2::Repository,
    commit: &str,
    depth: usize,
    language: &tree_sitter::Language,
    queries: impl Iterator<Item = String>,
    timeout: Timeout,
) {
    let mut cumulative = NonBlockingResLogger::with_timeout(std::io::stdout(), timeout);
    multi_run(
        &mut cumulative,
        &repository,
        (),
        language,
        queries,
        tsquery_execute_count,
        |cumulative, repository, _, executor| {
            parse_and_execute_on_commits_once_per_file(
                cumulative,
                repository,
                commit,
                depth,
                &language,
                |name| name.ends_with(".java"),
                executor,
            )
        },
    );
    cumulative.finish();
}

pub fn baseline_our_executor_cache_trees_and_blobs(
    repository: git2::Repository,
    commit: &str,
    depth: usize,
    language: &tree_sitter::Language,
    queries: impl Iterator<Item = String>,
    timeout: Timeout,
) {
    let mut cumulative = NonBlockingResLogger::with_timeout(std::io::stdout(), timeout);
    // the cache which avoids re-parsing files and re-resolving blobs
    let cache = HashMap::<FileId, (tree_sitter::Tree, git2::Blob)>::new();

    multi_run(
        &mut cumulative,
        &repository,
        cache,
        language,
        queries,
        tsquery_execute_count,
        |cumulative, repository, cache, executor| {
            parse_and_execute_on_commits_cache_trees_and_blobs(
                cumulative,
                cache,
                repository,
                commit,
                depth,
                &language,
                |name| name.ends_with(".java"),
                executor,
            )
        },
    );
    cumulative.finish();
}

pub fn baseline_our_executor_prepare_cache_trees_and_blobs(
    repository: git2::Repository,
    commit: &str,
    depth: usize,
    language: &tree_sitter::Language,
    queries: impl Iterator<Item = String>,
    timeout: Timeout,
) {
    let mut cumulative = NonBlockingResLogger::with_timeout(std::io::stdout(), timeout);
    let cache = HashMap::<FileId, (tree_sitter::Tree, git2::Blob)>::new();
    multi_run(
        &mut cumulative,
        &repository,
        cache,
        language,
        queries,
        tsquery_execute_count,
        |cumulative, repository, cache, executor| {
            parse_and_execute_on_commits_prepare_cache_trees_and_blobs2(
                cumulative,
                cache,
                repository,
                commit,
                depth,
                &language,
                |name| name.ends_with(".java"),
                executor,
            )
        },
    );
    cumulative.finish();
}

pub fn baseline_our_executor_cache_trees_and_blobs_memo(
    repository: git2::Repository,
    commit: &str,
    depth: usize,
    language: &tree_sitter::Language,
    queries: impl Iterator<Item = String>,
    timeout: Timeout,
) {
    let mut cumulative = NonBlockingResLogger::with_timeout(std::io::stdout(), timeout);
    // the cache which avoids re-parsing files and re-resolving blobs
    let cache = HashMap::<FileId, (tree_sitter::Tree, git2::Blob)>::new();

    multi_run(
        &mut cumulative,
        &repository,
        cache,
        language,
        queries,
        tsquery_execute_count,
        |cumulative, repository, cache, executor| {
            parse_and_execute_on_commits_cache_trees_and_blobs_memo(
                cumulative,
                cache,
                repository,
                commit,
                depth,
                &language,
                |name| name.ends_with(".java"),
                executor,
            )
        },
    );
    cumulative.finish();
}

pub fn baseline_our_executor_prepare_cache_trees_and_blobs_precomp(
    repository: git2::Repository,
    commit: &str,
    depth: usize,
    language: &tree_sitter::Language,
    precomp: &[&str],
    queries: impl Iterator<Item = String>,
    timeout: Timeout,
) {
    let mut cumulative = NonBlockingResLogger::with_timeout(std::io::stdout(), timeout);
    let cache = (
        precomp,
        HashMap::<FileId, (tree_sitter::Tree, git2::Blob, Precomp)>::new(),
    );
    multi_run(
        &mut cumulative,
        &repository,
        cache,
        language,
        queries,
        tsquery_execute_count_precomp,
        |cumulative, repository, cache, executor| {
            parse_and_execute_on_commits_prepare_cache_trees_and_blobs_precomp2(
                cumulative,
                &mut cache.1,
                repository,
                commit,
                depth,
                &language,
                |name| name.ends_with(".java"),
                executor,
            )
        },
    );
    cumulative.finish();
}

fn parse_and_execute_on_commits_once_per_file<
    R: Default + AddAssign + for<'a> AddAssign<&'a R> + Clone,
>(
    cumulative: &mut impl ResultLogger<R>,
    repository: &git2::Repository,
    commit: &str,
    depth: usize,
    language: &tree_sitter::Language,
    filter: impl Fn(&str) -> bool,
    executor: impl Fn(&[u8], &tree_sitter::Tree, &mut R),
) -> Result<(), Error> {
    let repository: &git2::Repository = &repository;
    let rw = commit_rw(commit, Some(depth), repository).unwrap();

    let mut cache = HashMap::<FileId, R>::new();
    dbg!(memusage().to_string());

    // for each commit
    for commit in rw {
        log::trace!("Commit: {}", commit);
        let commit = repository.find_commit(commit).unwrap();

        let root = commit.tree().unwrap();
        let mut res = R::default();
        //   for each file only if never seen before
        root.walk(git2::TreeWalkMode::PreOrder, |_name, entry| {
            if entry.kind() != Some(git2::ObjectType::Blob) {
                return git2::TreeWalkResult::Ok;
            }
            let name = entry.name().unwrap();
            if !filter(name) {
                return git2::TreeWalkResult::Ok;
            }
            let oid = entry.id();
            let file_id = oid;
            if let Some(value) = cache.get(&file_id) {
                res += value;
            } else {
                let blob = repository.find_blob(oid).unwrap();
                let content = blob.content();

                // parse file
                let mut parser = tree_sitter::Parser::new();
                parser.set_language(&language).unwrap();
                let tree = parser.parse(content, None).unwrap();

                let mut r = R::default();
                // execute query
                executor(content, &tree, &mut r);

                // let duration = start_instant.elapsed();
                cache.insert(file_id, r.clone());
                res += r;
                // cumulative.push((Task::ExecuteQueryOnFile(())));
            }
            git2::TreeWalkResult::Ok
        })
        .unwrap();

        cumulative.commit_result(res)?;
        dbg!(memusage().to_string());
    }
    Ok(())
}

fn parse_and_execute_on_commits_cache_trees_and_blobs<'a, R: Default>(
    cumulative: &mut impl ResultLogger<R>,
    cache: &mut HashMap<FileId, (tree_sitter::Tree, git2::Blob<'a>)>,
    repository: &'a git2::Repository,
    commit: &str,
    depth: usize,
    language: &tree_sitter::Language,
    filter: impl Fn(&str) -> bool,
    executor: impl Fn(&[u8], &tree_sitter::Tree, &mut R),
) -> Result<(), Error> {
    let rw = commit_rw(commit, Some(depth), repository).unwrap();

    dbg!(memusage().to_string());

    // for each commit
    for commit in rw {
        log::trace!("Commit: {}", commit);
        let commit = repository.find_commit(commit).unwrap();

        let root = commit.tree().unwrap();
        let mut res = R::default();
        let mut nodes_per_commit = 0;
        //   for each file only if never seen before
        root.walk(git2::TreeWalkMode::PreOrder, |_name, entry| {
            if entry.kind() != Some(git2::ObjectType::Blob) {
                return git2::TreeWalkResult::Ok;
            }
            let name = entry.name().unwrap();
            if !filter(name) {
                return git2::TreeWalkResult::Ok;
            }
            let oid = entry.id();
            let file_id = oid;
            if let Some((tree, blob)) = cache.get(&file_id) {
                let content = blob.content();
                // execute query
                executor(content, &tree, &mut res);
                nodes_per_commit += tree.root_node().descendant_count();
                // let duration = start_instant.elapsed();
                // cumulative.push((Task::ExecuteQueryOnFile(())));
            } else {
                let blob = repository.find_blob(oid).unwrap();
                let content = blob.content();

                // parse file
                let mut parser = tree_sitter::Parser::new();
                parser.set_language(&language).unwrap();
                let tree = parser.parse(content, None).unwrap();

                //execute query
                executor(content, &tree, &mut res);

                nodes_per_commit += tree.root_node().descendant_count();

                // let duration = start_instant.elapsed();
                cache.insert(file_id, (tree, blob));
                // cumulative.push((Task::ExecuteQueryOnFile(())));
            }
            // TODO also skip directories already seen
            git2::TreeWalkResult::Ok
        })
        .unwrap();

        cumulative.commit_result_with_size(res, nodes_per_commit)?;
        dbg!(memusage().to_string());
    }
    Ok(())
}

fn parse_and_execute_on_commits_cache_trees_and_blobs_memo<
    'a,
    R: Default + AddAssign + for<'b> AddAssign<&'b R> + Clone + std::fmt::Debug,
>(
    cumulative: &mut impl ResultLogger<R>,
    cache: &mut HashMap<FileId, (tree_sitter::Tree, git2::Blob<'a>)>,
    repository: &'a git2::Repository,
    commit: &str,
    depth: usize,
    language: &tree_sitter::Language,
    filter: impl Fn(&str) -> bool,
    executor: impl Fn(&[u8], &tree_sitter::Tree, &mut R),
) -> Result<(), Error> {
    let rw = commit_rw(commit, Some(depth), repository).unwrap();

    dbg!(memusage().to_string());
    let mut cache_res = HashMap::<FileId, R>::new();

    // for each commit
    for commit in rw {
        log::trace!("Commit: {}", commit);
        let commit = repository.find_commit(commit).unwrap();

        let root = commit.tree().unwrap();
        let mut res = R::default();
        let mut nodes_per_commit = 0;
        //   for each file only if never seen before
        root.walk(git2::TreeWalkMode::PreOrder, |_name, entry| {
            if entry.kind() != Some(git2::ObjectType::Blob) {
                return git2::TreeWalkResult::Ok;
            }
            let name = entry.name().unwrap();
            if !filter(name) {
                return git2::TreeWalkResult::Ok;
            }
            let oid = entry.id();
            let file_id = oid;
            if let Some(r) = cache_res.get(&file_id) {
                res += r;
                if let Some((tree, _)) = cache.get(&file_id) {
                    nodes_per_commit += tree.root_node().descendant_count();
                }
            } else if let Some((tree, blob)) = cache.get(&file_id) {
                let content = blob.content();

                let mut r = R::default();
                //execute query
                executor(content, &tree, &mut r);

                nodes_per_commit += tree.root_node().descendant_count();

                cache_res.insert(file_id, r.clone());
                res += r;
            } else {
                let blob = repository.find_blob(oid).unwrap();
                let content = blob.content();

                // parse file
                let mut parser = tree_sitter::Parser::new();
                parser.set_language(&language).unwrap();
                let tree = parser.parse(content, None).unwrap();

                let mut r = R::default();
                //execute query
                executor(content, &tree, &mut r);

                nodes_per_commit += tree.root_node().descendant_count();

                cache.insert(file_id, (tree, blob));
                cache_res.insert(file_id, r.clone());
                res += r;
                // cumulative.push((Task::ExecuteQueryOnFile(())));
            }
            // TODO also skip directories already seen
            git2::TreeWalkResult::Ok
        })
        .unwrap();

        cumulative.commit_result_with_size(res, nodes_per_commit)?;
        dbg!(memusage().to_string());
    }
    Ok(())
}

#[expect(unused)]
fn parse_and_execute_on_commits_prepare_cache_trees_and_blobs<'a, R: Default>(
    cumulative: &mut impl ResultLogger<R>,
    cache: &mut HashMap<FileId, (tree_sitter::Tree, git2::Blob<'a>)>,
    repository: &'a git2::Repository,
    commit: &str,
    depth: usize,
    language: &tree_sitter::Language,
    filter: impl Fn(&str) -> bool,
    executor: impl Fn(&[u8], &tree_sitter::Tree, &mut R),
) -> Result<(), Error> {
    let mut rw = commit_rw(commit, Some(depth), repository)
        .unwrap()
        .peekable();

    dbg!(memusage().to_string());

    // prepare first commit
    if let Some(&commit) = rw.peek() {
        log::trace!("Commit: {}", commit);
        let commit = repository.find_commit(commit).unwrap();

        let root = commit.tree().unwrap();
        // for each file only if never seen before
        root.walk(git2::TreeWalkMode::PreOrder, |_name, entry| {
            if entry.kind() != Some(git2::ObjectType::Blob) {
                return git2::TreeWalkResult::Ok;
            }
            let name = entry.name().unwrap();
            if !filter(name) {
                return git2::TreeWalkResult::Ok;
            }
            let oid = entry.id();
            let file_id = oid;
            if let Some(_) = cache.get(&file_id) {
                // nothing here, just warming up the cache
            } else {
                let blob = repository.find_blob(oid).unwrap();
                let content = blob.content();

                // parse file
                let mut parser = tree_sitter::Parser::new();
                parser.set_language(&language).unwrap();
                let tree = parser.parse(content, None).unwrap();

                // let duration = start_instant.elapsed();
                cache.insert(file_id, (tree, blob));
                // cumulative.push((Task::ExecuteQueryOnFile(())));
            }
            // TODO also skip directories already seen
            git2::TreeWalkResult::Ok
        })
        .unwrap();

        cumulative.commit_prepared(1)?;
        dbg!(memusage().to_string());
    }

    // for each commit
    for commit in rw {
        log::trace!("Commit: {}", commit);
        let commit = repository.find_commit(commit).unwrap();

        let root = commit.tree().unwrap();
        let mut res = R::default();
        // for each file only if never seen before
        root.walk(git2::TreeWalkMode::PreOrder, |_name, entry| {
            if entry.kind() != Some(git2::ObjectType::Blob) {
                return git2::TreeWalkResult::Ok;
            }
            let name = entry.name().unwrap();
            if !filter(name) {
                return git2::TreeWalkResult::Ok;
            }
            let oid = entry.id();
            let file_id = oid;
            if let Some((tree, blob)) = cache.get(&file_id) {
                let content = blob.content();

                // execute query
                executor(content, &tree, &mut res);

                // let duration = start_instant.elapsed();
                // cumulative.push((Task::ExecuteQueryOnFile(())));
            } else {
                let blob = repository.find_blob(oid).unwrap();
                let content = blob.content();

                // parse file
                let mut parser = tree_sitter::Parser::new();
                parser.set_language(&language).unwrap();
                let tree = parser.parse(content, None).unwrap();

                //execute query
                executor(content, &tree, &mut res);

                // let duration = start_instant.elapsed();
                cache.insert(file_id, (tree, blob));
                // cumulative.push((Task::ExecuteQueryOnFile(())));
            }
            // TODO also skip directories already seen
            git2::TreeWalkResult::Ok
        })
        .unwrap();

        cumulative.commit_result(res)?;
        dbg!(memusage().to_string());
    }
    Ok(())
}

#[allow(unused)]
fn parse_and_execute_on_commits_prepare_cache_trees_and_blobs2<'a, R: Default>(
    cumulative: &mut impl ResultLogger<R>,
    cache: &mut HashMap<FileId, (tree_sitter::Tree, git2::Blob<'a>)>,
    repository: &'a git2::Repository,
    commit: &str,
    depth: usize,
    language: &tree_sitter::Language,
    filter: impl Fn(&str) -> bool,
    executor: impl Fn(&[u8], &tree_sitter::Tree, &mut R),
) -> Result<(), Error> {
    let rw = commit_rw(commit, Some(depth), repository)
        .unwrap()
        .peekable();

    dbg!(memusage().to_string());

    // for each commit
    for commit in rw {
        log::trace!("Commit: {}", commit);
        let commit = repository.find_commit(commit).unwrap();

        let root = commit.tree().unwrap();
        let mut to_search = vec![];
        // for each file only if never seen before
        root.walk(git2::TreeWalkMode::PreOrder, |_name, entry| {
            if entry.kind() != Some(git2::ObjectType::Blob) {
                return git2::TreeWalkResult::Ok;
            }
            let name = entry.name().unwrap();
            if !filter(name) {
                return git2::TreeWalkResult::Ok;
            }
            let oid = entry.id();
            let file_id = oid;
            to_search.push(file_id);
            if let Some(_) = cache.get(&file_id) {
            } else {
                let blob = repository.find_blob(oid).unwrap();
                let content = blob.content();

                // parse file
                let mut parser = tree_sitter::Parser::new();
                parser.set_language(&language).unwrap();
                let tree = parser.parse(content, None).unwrap();

                // let duration = start_instant.elapsed();
                cache.insert(file_id, (tree, blob));
                // cumulative.push((Task::ExecuteQueryOnFile(())));
            }
            // TODO also skip directories already seen
            git2::TreeWalkResult::Ok
        })
        .unwrap();
        cumulative.commit_prepared(1)?;

        let mut res = R::default();
        for file_id in to_search {
            if let Some((tree, blob)) = cache.get(&file_id) {
                let content = blob.content();
                //execute query
                executor(content, &tree, &mut res);
            }
        }

        cumulative.commit_result(res)?;
        dbg!(memusage().to_string());
    }
    Ok(())
}

#[expect(unused)]
fn parse_and_execute_on_commits_prepare_cache_trees_and_blobs_precomp<'a, R: Default>(
    cumulative: &mut impl ResultLogger<R>,
    cache: &mut HashMap<FileId, (tree_sitter::Tree, git2::Blob<'a>, Precomp)>,
    repository: &'a git2::Repository,
    commit: &str,
    depth: usize,
    language: &tree_sitter::Language,
    filter: impl Fn(&str) -> bool,
    executor: impl for<'t> FileSkippingExecutor<
        P<'t> = (&'t [u8], &'t tree_sitter::Tree),
        R = R,
        S = Precomp,
    >,
) -> Result<(), Error> {
    let mut rw = commit_rw(commit, Some(depth), repository)
        .unwrap()
        .peekable();

    dbg!(memusage().to_string());

    // prepare first commit
    if let Some(&commit) = rw.peek() {
        log::trace!("Commit: {}", commit);
        let commit = repository.find_commit(commit).unwrap();

        let root = commit.tree().unwrap();
        // for each file only if never seen before
        root.walk(git2::TreeWalkMode::PreOrder, |_name, entry| {
            if entry.kind() != Some(git2::ObjectType::Blob) {
                return git2::TreeWalkResult::Ok;
            }
            let name = entry.name().unwrap();
            if !filter(name) {
                return git2::TreeWalkResult::Ok;
            }
            let oid = entry.id();
            let file_id = oid;
            if let Some(_) = cache.get(&file_id) {
                // nothing here, just warming up the cache
            } else {
                let blob = repository.find_blob(oid).unwrap();
                let content = blob.content();

                // parse file
                let mut parser = tree_sitter::Parser::new();
                parser.set_language(&language).unwrap();
                let tree = parser.parse(content, None).unwrap();

                let content = blob.content();
                let precomp = executor.precomp_execute((content, &tree));
                cache.insert(file_id, (tree, blob, precomp));
            }
            // TODO also skip directories already seen
            git2::TreeWalkResult::Ok
        })
        .unwrap();

        cumulative.commit_prepared(1)?;
        dbg!(memusage().to_string());
    }

    // for each commit
    for commit in rw {
        log::trace!("Commit: {}", commit);
        let commit = repository.find_commit(commit).unwrap();

        let root = commit.tree().unwrap();
        let mut res = R::default();
        // for each file only if never seen before
        root.walk(git2::TreeWalkMode::PreOrder, |_name, entry| {
            if entry.kind() != Some(git2::ObjectType::Blob) {
                return git2::TreeWalkResult::Ok;
            }
            let name = entry.name().unwrap();
            if !filter(name) {
                return git2::TreeWalkResult::Ok;
            }
            let oid = entry.id();
            let file_id = oid;
            if let Some((tree, blob, precomp)) = cache.get(&file_id) {
                let content = blob.content();

                if !executor.can_skip(precomp) {
                    // execute query
                    executor.execute((content, &tree), &mut res);
                }
            } else {
                let blob = repository.find_blob(oid).unwrap();
                let content = blob.content();

                // parse file
                let mut parser = tree_sitter::Parser::new();
                parser.set_language(&language).unwrap();
                let tree = parser.parse(content, None).unwrap();

                // TODO also make a new parse_and_execute for non prep using that
                let precomp = executor.both_execute((content, &tree), &mut res);
                // // execute precomp
                // let precomp = executor.precomp_execute((content, &tree));
                // if !executor.can_skip(&precomp) {
                //     // execute query
                //     executor.execute((content, &tree), &mut res);
                // }
                cache.insert(file_id, (tree, blob, precomp));
            }
            // TODO also skip directories already seen
            git2::TreeWalkResult::Ok
        })
        .unwrap();

        cumulative.commit_result(res)?;
        dbg!(memusage().to_string());
    }
    Ok(())
}

fn parse_and_execute_on_commits_prepare_cache_trees_and_blobs_precomp2<'a, R: Default>(
    cumulative: &mut impl ResultLogger<R>,
    cache: &mut HashMap<FileId, (tree_sitter::Tree, git2::Blob<'a>, Precomp)>,
    repository: &'a git2::Repository,
    commit: &str,
    depth: usize,
    language: &tree_sitter::Language,
    filter: impl Fn(&str) -> bool,
    executor: impl for<'t> FileSkippingExecutor<
        P<'t> = (&'t [u8], &'t tree_sitter::Tree),
        R = R,
        S = Precomp,
    >,
) -> Result<(), Error> {
    let rw = commit_rw(commit, Some(depth), repository)
        .unwrap()
        .peekable();

    dbg!(memusage().to_string());

    // for each commit
    for commit in rw {
        log::trace!("Commit: {}", commit);
        let commit = repository.find_commit(commit).unwrap();

        let root = commit.tree().unwrap();
        let mut to_search = vec![];
        // for each file only if never seen before
        root.walk(git2::TreeWalkMode::PreOrder, |_name, entry| {
            if entry.kind() != Some(git2::ObjectType::Blob) {
                return git2::TreeWalkResult::Ok;
            }
            let name = entry.name().unwrap();
            if !filter(name) {
                return git2::TreeWalkResult::Ok;
            }
            let oid = entry.id();
            let file_id = oid;
            to_search.push(file_id);
            if let Some(_) = cache.get(&file_id) {
            } else {
                let blob = repository.find_blob(oid).unwrap();
                let content = blob.content();

                // parse file
                let mut parser = tree_sitter::Parser::new();
                parser.set_language(&language).unwrap();
                let tree = parser.parse(content, None).unwrap();

                // execute precomp
                let precomp = executor.precomp_execute((content, &tree));
                cache.insert(file_id, (tree, blob, precomp));
            }
            // TODO also skip directories already seen
            git2::TreeWalkResult::Ok
        })
        .unwrap();
        cumulative.commit_prepared(1)?;

        let mut res = R::default();
        for file_id in to_search {
            if let Some((tree, blob, precomp)) = cache.get(&file_id) {
                let content = blob.content();
                if !executor.can_skip(precomp) {
                    // execute query
                    executor.execute((content, &tree), &mut res);
                }
            }
        }

        cumulative.commit_result(res)?;
        dbg!(memusage().to_string());
    }
    Ok(())
}
