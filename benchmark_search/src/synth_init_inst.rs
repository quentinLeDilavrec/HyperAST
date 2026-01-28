use std::fmt::{Debug, Display};
use std::ops::AddAssign;
use std::time::Instant;

use hyperast::position::structural_pos::{CursorHead, CursorWithPersistence};
use hyperast::store::TyDown;
use hyperast::store::defaults::NodeIdentifier;
use hyperast::types::{HyperAST, NodeStore};
use hyperast::types::{WithHashs, WithPrecompQueries, WithRoles, WithStats};
use hyperast::utils::memusage;
use hyperast_vcs_git::multi_preprocessed::PreProcessedRepositories;
use hyperast_vcs_git::processing::{ConfiguredRepo2, RepoConfig};
use num_traits::ToPrimitive;

use crate::commit_rw;
use crate::with_hyperast::{CreatedExecutor, Executor, SkippingExecutor};
use crate::with_hyperast::{compile_query, execute_on_commits_per_blob, prepare_hyperast};
use crate::{NonBlockingResLogger, ResultLogger, Timeout};

struct ChangesDetection {
    query: hyperast_tsquery::Query,
    cid: hyperast_tsquery::CaptureId,
}

#[derive(Clone, Default)]
/// accumulates node hashes
///
/// when comparing commits if respective hashes are different,
/// then the set of matched nodes is different
struct HashesAccumulator {
    struc: u32,
    label: u32,
    count: u32,
}

impl AddAssign<HashesAccumulator> for HashesAccumulator {
    fn add_assign(&mut self, other: HashesAccumulator) {
        self.struc = self.struc.wrapping_add(other.struc);
        self.label = self.label.wrapping_add(other.label);
        self.count = self.count.checked_add(other.count).unwrap_or(u32::MAX);
    }
}

impl AddAssign<&HashesAccumulator> for HashesAccumulator {
    fn add_assign(&mut self, other: &HashesAccumulator) {
        self.struc = self.struc.wrapping_add(other.struc);
        self.label = self.label.wrapping_add(other.label);
        self.count = self.count.checked_add(other.count).unwrap_or(u32::MAX);
    }
}

impl Display for crate::CsvHeader<HashesAccumulator> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "struct;label;count")
    }
}

impl Display for HashesAccumulator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:x<08}; {:x<08}; {}",
            self.struc, self.label, self.count
        )
    }
}

impl CreatedExecutor for ChangesDetection {
    fn create(
        sub: &[&str],
        query: &str,
        language: &tree_sitter::Language,
    ) -> Result<Self, tree_sitter::QueryError> {
        let query = compile_query(query, sub, language.clone())?;
        Ok(ChangesDetection {
            cid: query
                .capture_index_for_name("root")
                .expect("@root on root patterns"),
            query,
        })
    }
}

impl<HAST: HyperAST> Executor<HAST> for ChangesDetection
where
    HAST::TS: hyperast::types::TypeStore + hyperast::types::RoleStore,
    <HAST::TS as hyperast::types::RoleStore>::IdF: Into<u16> + From<u16>,
    HAST::IdN: Copy + Debug,
    for<'t> <HAST as hyperast::types::AstLending<'t>>::RT:
        WithPrecompQueries + WithRoles + WithStats + WithHashs,
    HAST::IdN: hyperast::types::NodeId<IdN = HAST::IdN>,
{
    type P<IdN, Idx> = CursorWithPersistence<IdN, Idx>;
    type R = HashesAccumulator;

    fn execute(
        &self,
        stores: &HAST,
        pos: Self::P<HAST::IdN, HAST::Idx>,
        acc: &mut Self::R,
    ) -> Self::P<HAST::IdN, HAST::Idx>
    where
        HAST::IdN: Copy + Debug,
        for<'t> <HAST as hyperast::types::AstLending<'t>>::RT:
            WithPrecompQueries + WithRoles + WithStats + WithHashs,
        HAST::IdN: hyperast::types::NodeId<IdN = HAST::IdN>,
    {
        use hyperast_tsquery::hyperast_opt::TreeCursor;
        let cursor = TreeCursor::new(stores, pos);
        let mut qcursor = self.query.matches(cursor);
        while let Some(m) = qcursor.next() {
            for n in m.nodes_for_capture_index(self.cid) {
                acc.count += 1;
                let id = n.pos.node();
                let nn = stores.node_store().resolve(&id);
                let h = nn.hash_structural();
                acc.struc = acc.struc.wrapping_add(h.to_u32().unwrap());
                let h = nn.hash_label();
                acc.label = acc.label.wrapping_add(h.to_u32().unwrap());
            }
        }
        while qcursor.cursor.pos.node() != qcursor.cursor.pos.p.node() {
            assert!(qcursor.cursor.pos.up());
        }
        qcursor.cursor.pos.pos
    }
}

impl<HAST: HyperAST> SkippingExecutor<HAST> for ChangesDetection
where
    HAST::TS: hyperast::types::TypeStore + hyperast::types::RoleStore,
    <HAST::TS as hyperast::types::RoleStore>::IdF: Into<u16> + From<u16>,
    HAST::IdN: Copy + Debug,
    for<'t> <HAST as hyperast::types::AstLending<'t>>::RT:
        WithPrecompQueries + WithRoles + WithStats + WithHashs,
    HAST::IdN: hyperast::types::NodeId<IdN = HAST::IdN>,
{
    fn can_skip<N: WithPrecompQueries>(&self, n: &N) -> bool {
        let used_precomputed = self.query.used_precomputed;
        used_precomputed != 0 && n.wont_match_given_precomputed_queries(used_precomputed)
    }
}

pub fn per_blob<TS>(
    repo: hyperast_vcs_git::git::Repo,
    sub: &[&str],
    commit: &str,
    depth: usize,
    language: &tree_sitter::Language,
    queries: impl Iterator<Item = String>,
    timeout: Timeout,
) where
    TS: 'static + hyperast::types::TypeStore + hyperast::types::RoleStore,
    hyperast_vcs_git::TStore: TyDown<TS>,
    <TS as hyperast::types::RoleStore>::IdF: Into<u16> + From<u16>,
{
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
    crate::with_hyperast::multi_run::<_, ChangesDetection, _, _>(
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
            execute_on_commits_per_blob::<_, TS, _>(
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

struct UniqCollection {
    query: hyperast_tsquery::Query,
    cid: hyperast_tsquery::CaptureId,
}

impl CreatedExecutor for UniqCollection {
    fn create(
        sub: &[&str],
        query: &str,
        language: &tree_sitter::Language,
    ) -> Result<Self, tree_sitter::QueryError> {
        let query = compile_query(query, sub, language.clone())?;
        Ok(Self {
            cid: query
                .capture_index_for_name("root")
                .expect("@root on root patterns"),
            query,
        })
    }
}

impl<HAST: HyperAST> Executor<HAST> for UniqCollection
where
    HAST::TS: hyperast::types::TypeStore + hyperast::types::RoleStore,
    <HAST::TS as hyperast::types::RoleStore>::IdF: Into<u16> + From<u16>,
    HAST::IdN: Copy + Debug,
    for<'t> <HAST as hyperast::types::AstLending<'t>>::RT:
        WithPrecompQueries + WithRoles + WithStats + WithHashs,
    HAST::IdN: hyperast::types::NodeId<IdN = HAST::IdN>,
{
    type P<IdN, Idx> = CursorWithPersistence<IdN, Idx>;
    type R = HashInstAccumulator<HAST::IdN>;

    fn execute(
        &self,
        stores: &HAST,
        pos: Self::P<HAST::IdN, HAST::Idx>,
        acc: &mut Self::R,
    ) -> Self::P<HAST::IdN, HAST::Idx> {
        use hyperast_tsquery::hyperast_opt::TreeCursor;
        let cursor = TreeCursor::new(stores, pos);
        let mut qcursor = self.query.matches(cursor);
        while let Some(m) = qcursor.next() {
            for n in m.nodes_for_capture_index(self.cid) {
                acc.accu.count += 1;
                acc.vec.inst.push(n.pos.node());
                let id = n.pos.node();
                let nn = stores.node_store().resolve(&id);
                let h = nn.hash_structural();
                let h = h.to_u32().unwrap();
                acc.vec.struc.push(h);
                acc.accu.struc = acc.accu.struc.wrapping_add(h);
                let h = nn.hash_label();
                let h = h.to_u32().unwrap();
                acc.vec.label.push(h);
                acc.accu.label = acc.accu.label.wrapping_add(h);
            }
        }
        while qcursor.cursor.pos.node() != qcursor.cursor.pos.p.node() {
            assert!(qcursor.cursor.pos.up());
        }
        qcursor.cursor.pos.pos
    }
}

impl<HAST: HyperAST> SkippingExecutor<HAST> for UniqCollection
where
    HAST::TS: hyperast::types::TypeStore + hyperast::types::RoleStore,
    <HAST::TS as hyperast::types::RoleStore>::IdF: Into<u16> + From<u16>,
    HAST::IdN: Copy + Debug,
    for<'t> <HAST as hyperast::types::AstLending<'t>>::RT:
        WithPrecompQueries + WithRoles + WithStats + WithHashs,
    HAST::IdN: hyperast::types::NodeId<IdN = HAST::IdN>,
{
    fn can_skip<N: WithPrecompQueries>(&self, n: &N) -> bool {
        let used_precomputed = self.query.used_precomputed;
        used_precomputed != 0 && n.wont_match_given_precomputed_queries(used_precomputed)
    }
}

pub fn find_uniques<TS>(
    repositories: &mut PreProcessedRepositories,
    repository: ConfiguredRepo2,
    sub: &[&str],
    commit: &str,
    language: &tree_sitter::Language,
    query: &str,
    config: crate::Config,
    timeout: Timeout,
) -> UniqInst
where
    TS: 'static + hyperast::types::TypeStore + hyperast::types::RoleStore,
    hyperast_vcs_git::TStore: TyDown<TS>,
    <TS as hyperast::types::RoleStore>::IdF: Into<u16> + From<u16>,
{
    // let mut cumulative = Cumulative::<usize>::with_timeout(timeout);
    // let mut cumulative = NonBlockingResLogger::with_timeout(std::io::stdout(), timeout);
    let mut cumulative = UniqInst::with_timeout(timeout);
    dbg!(memusage().to_string());

    if let Err(err) = cumulative.repo_prepared() {
        eprintln!("Error preparing repository: {}", err);
        cumulative.finish();
        return cumulative;
    }

    let first_chunk = config.depth.min(config.first_chunk);

    dbg!(memusage().to_string());

    let executor: UniqCollection = match CreatedExecutor::create(sub, &query, language) {
        Ok(executor) => executor,
        Err(err) => {
            std::eprintln!("{err}");
            return cumulative;
        }
    };
    if let Err(err) = cumulative.query_compiled() {
        std::eprintln!("{err}");
        return cumulative;
    }
    std::dbg!(memusage().to_string());
    let mut rw = commit_rw(commit, Some(config.depth), &repository.repo).unwrap();
    let commits = repositories.pre_process_chunk(&mut rw, &repository, first_chunk);
    if let Err(err) = cumulative.commit_prepared(commits.len()) {
        std::eprintln!("{err}");
    }
    std::dbg!(memusage().to_string());
    if let Err(err) = execute_on_commits_per_blob::<_, TS, _>(
        config,
        repositories,
        &repository,
        &mut cumulative,
        rw,
        commits.into_iter(),
        |root| CursorWithPersistence::new(root),
        executor,
    ) {
        std::eprintln!("{err}");
    }
    cumulative.finish();
    cumulative
}

#[derive(Clone)]
struct HashInstVec<IdN> {
    struc: Vec<u32>,
    label: Vec<u32>,
    inst: Vec<IdN>,
}

impl<IdN> Default for HashInstVec<IdN> {
    fn default() -> Self {
        Self {
            struc: Default::default(),
            label: Default::default(),
            inst: Default::default(),
        }
    }
}

impl<IdN> HashInstVec<IdN> {
    fn into_iter(self) -> impl Iterator<Item = ((u32, u32), IdN)> {
        let keys = self.struc.into_iter().zip(self.label);
        keys.zip(self.inst)
    }
}

impl<IdN> AddAssign<HashInstVec<IdN>> for HashInstVec<IdN> {
    fn add_assign(&mut self, other: HashInstVec<IdN>) {
        self.struc.extend(other.struc);
        self.label.extend(other.label);
        self.inst.extend(other.inst);
    }
}

impl<IdN: Clone> AddAssign<&HashInstVec<IdN>> for HashInstVec<IdN> {
    fn add_assign(&mut self, other: &HashInstVec<IdN>) {
        self.struc.extend(other.struc.iter().cloned());
        self.label.extend(other.label.iter().cloned());
        self.inst.extend(other.inst.iter().cloned());
    }
}

#[derive(Clone)]
struct HashInstAccumulator<IdN = NodeIdentifier> {
    accu: HashesAccumulator,
    vec: HashInstVec<IdN>,
}

impl<IdN> Default for HashInstAccumulator<IdN> {
    fn default() -> Self {
        Self {
            accu: Default::default(),
            vec: Default::default(),
        }
    }
}

impl<IdN> AddAssign<HashInstAccumulator<IdN>> for HashInstAccumulator<IdN> {
    fn add_assign(&mut self, other: HashInstAccumulator<IdN>) {
        self.accu.add_assign(other.accu);
        self.vec.add_assign(other.vec);
    }
}

impl<IdN: Clone> AddAssign<&HashInstAccumulator<IdN>> for HashInstAccumulator<IdN> {
    fn add_assign(&mut self, other: &HashInstAccumulator<IdN>) {
        self.accu.add_assign(&other.accu);
        self.vec.add_assign(&other.vec);
    }
}

pub struct UniqInst {
    pub set: std::collections::HashMap<(u32, u32), NodeIdentifier>,
    // prev_struc: u32,
    // prev_label: u32,
    start_time: Instant,
    timeout: Timeout,
}

impl UniqInst {
    fn with_timeout(timeout: Timeout) -> Self {
        Self {
            set: Default::default(),
            // prev_struc: 0,
            // prev_label: 0,
            start_time: Instant::now(),
            timeout,
        }
    }
    fn finish(&mut self) {}
}

impl ResultLogger<HashInstAccumulator> for UniqInst {
    fn log(
        &mut self,
        entry: crate::LogEntry<HashInstAccumulator>,
    ) -> Result<(), crate::TimeoutError> {
        if let crate::LogEntry::ExecuteQueryOnCommit(r, _) = entry {
            dbg!(r.vec.inst.len());
            assert_eq!(r.vec.inst.len(), r.vec.struc.len());
            assert_eq!(r.vec.inst.len(), r.vec.label.len());
            // if self.prev_struc != r.accu.struc && self.prev_label != r.accu.label {
            for (k, v) in r.vec.into_iter() {
                self.set.insert(k, v);
            }
            dbg!(self.set.len());
            // }
        }

        let duration = self.start_time.elapsed();
        if duration > self.timeout.0 {
            Err(crate::TimeoutError(duration))
        } else {
            Ok(())
        }
    }
}
