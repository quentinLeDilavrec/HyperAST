//! Benchmark of diff using the hyperAST, compared against https://github.com/GumTreeDiff/gumtree

//! algorithm: gumtree zs changedistiller rted
//! implementation: gumtree (gumtree, gumtreesimple)

//! validity: baseline gumtree, identity comparison mappings and edit scripts
//! performances: baseline gumtree, time/memory
//! code: repository (reuse ASE repositories and add some code so that gumtree works on whole commits ) / files (reuse gumtree dataset)

//! scenario #1: buggy/fixed
//! scenario #2: consecutive commits
//! scenario #2: quadratic commits ? consequence of usage ? related to precision of diff (because if we do not loose information (in result) we should get consitent results)

//! RQ 1: validity: is our implementation computing the same edit scripts that gumtree ?
//! RQ 2: performances: how our performances compare for the task of computing edit scripts on consecutive commits ? on a set of buggy/fixed files ?
//! RQ 3: scaling: what is the maximum number of commits that can be incremetally processed while staying in RAM ?
//!                what is the maximum size of the window where we can compute all combination of edit scripts ?
#[cfg(test)]
mod random_sample_diff;
#[cfg(test)]
mod swap_diff;
// #[cfg(test)]
pub mod buggy_fixed;
pub mod window_combination;
// #[cfg(test)]
// pub mod bin::window_combination;
pub mod bottom_up_routines;
pub mod cross_repo;
pub mod diff_output;
pub mod other_tools;
pub mod postprocess;
pub mod preprocess;
mod repo_dataset;
pub use repo_dataset::REPOSITORIES;

use std::{env, fs, io, path, time};

use hyperast_vcs_git::multi_preprocessed::PreProcessedRepositories;

use hyperast::store::defaults::NodeIdentifier;

pub type OwnedLazyMapping = bottom_up_routines::OwnedLazyMapping<NodeIdentifier>;

pub fn tempfile() -> io::Result<(path::PathBuf, fs::File)> {
    let mut path = env::temp_dir();
    let file_name = time::SystemTime::UNIX_EPOCH;
    path.push(file_name.elapsed().unwrap().as_nanos().to_string());
    let file = fs::File::create(&path)?;
    Ok((path, file))
}

pub fn with_profiling<F: Fn()>(out: &path::Path, f: F) {
    let guard = pprof::ProfilerGuardBuilder::default()
        .frequency(1000)
        .blocklist(&[
            // "libc",
            "libgcc", "pthread", "vdso",
        ])
        .build()
        .unwrap();
    f();
    if let Ok(report) = guard.report().build() {
        let mut file = fs::File::create(out).unwrap();
        let profile = report.pprof().unwrap();
        use pprof::protos::Message;
        let mut content = Vec::new();
        profile.encode(&mut content).unwrap();
        use io::Write;
        file.write_all(&content).unwrap();
    };
}

pub fn setup_env_logger() {
    env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("debug"))
        .format(|buf, record| {
            use std::io::Write;
            if record.level().to_level_filter() > log::LevelFilter::Debug {
                writeln!(buf, "{}", record.args())
            } else {
                writeln!(
                    buf,
                    "[{} {}] {}",
                    buf.timestamp_millis(),
                    record.level(),
                    record.args()
                )
            }
        })
        .init();
}

#[derive(Clone)]
pub struct InputRepo {
    pub user: &'static str,
    pub name: &'static str,
    pub commit: &'static str,
    pub config: hyperast_vcs_git::processing::RepoConfig,
    pub fetch: bool,
}

pub struct Input {
    pub repo: InputRepo,
    pub id: usize,
    pub depth: usize,
}

impl InputRepo {
    pub fn gh(&self) -> hyperast_vcs_git::git::Repo {
        hyperast_vcs_git::git::Forge::Github.repo(self.user, self.name)
    }

    pub fn with(self, id: usize, depth: usize) -> Input {
        Input {
            repo: self,
            id,
            depth,
        }
    }
}
impl Input {
    pub fn try_fetch(mut self) -> Option<Self> {
        let fetch = self.repo.fetch;
        self.repo.fetch = false; // should have already been fetched in bin/fetch_repositories.rs
        if let Err(e) = check_commit(&self) {
            eprintln!(
                "repo {}/{} failed to find commit (run bin/fetch_repositories.rs or change commit): {}",
                self.user, self.name, e
            );
            if !fetch {
                eprintln!(
                    "repo {}/{} was not fetched, it is probably a big one",
                    self.user, self.name
                );
                eprintln!("skipping is fine");
                return None;
            }
            // still try for convenience
            self.repo.fetch = true;
            if let Err(e) = check_commit(&self) {
                eprintln!(
                    "repo {}/{} fetch and check failed: {}",
                    self.user, self.name, e
                );
            }
            self.repo.fetch = false;
        }
        Some(self)
    }
}

impl std::ops::Deref for Input {
    type Target = InputRepo;

    fn deref(&self) -> &Self::Target {
        &self.repo
    }
}

pub fn check_commit(p: &InputRepo) -> Result<(), hyperast_vcs_git::git::FetchRepoError> {
    use hyperast_vcs_git::git::Oid;
    let oid = Oid::from_str(p.commit).unwrap();
    let repository = if p.fetch {
        p.gh().fetch()
    } else {
        p.gh().nofetch()
    };
    repository.find_commit(oid).map(|_| ()).map_err(Into::into)
}
pub fn prep_commits(
    p: &InputRepo,
    repositories: &mut PreProcessedRepositories,
    depth: usize,
) -> Vec<NodeIdentifier> {
    let repo = repositories
        .get_config(p.gh())
        .ok_or_else(|| "missing config for repository".to_string())
        .unwrap();
    use hyperast_vcs_git::git::Oid;
    let oid = Oid::from_str(p.commit).unwrap();
    let repository = if p.fetch && repositories.get_commit(&repo.config, &oid).is_none() {
        repo.fetch()
    } else {
        repo.nofetch()
    };

    let oids = repositories
        .pre_process_with_limit(&repository, "", p.commit, depth)
        .unwrap()
        .into_iter();
    oids.map(|c| {
        repositories
            .get_commit(&repository.config, &c)
            .unwrap()
            .ast_root
    })
    .collect()
}

pub fn prep_commit_pair(
    p: &InputRepo,
    repositories: &mut PreProcessedRepositories,
) -> (NodeIdentifier, NodeIdentifier) {
    let commits = prep_commits(p, repositories, 2);
    (commits[0], commits[1])
}

pub fn prep_commit_pair_after(
    p: &InputRepo,
    repositories: &mut PreProcessedRepositories,
    skip: usize,
) -> (NodeIdentifier, NodeIdentifier) {
    let commits = prep_commits(p, repositories, skip + 2);
    (commits[skip], commits[skip + 1])
}

pub trait MapperLending<'a, __ImplBound = &'a Self> {
    type Mpr;
}

pub trait Runner: for<'a> MapperLending<'a> {
    fn name(&mut self, name: String) -> &mut Self;
    fn prep(&mut self, prep: Prep) -> &mut Self;
    type Output;
    fn routine<Mping>(
        &mut self,
        stats: impl Fn(Mping) -> Stats,
        f: impl for<'a> FnMut(<Self as MapperLending<'a>>::Mpr) -> Mping + Clone,
    ) -> Self::Output;
}

#[derive(Clone)]
#[repr(u8)]
pub enum Prep {
    None = 0,
    GT = 1,
    CD = 2,
}

#[derive(Default)]
pub struct Stats {
    pub size: (usize, usize),
    pub mapped: (usize, usize),
    pub decomp: (usize, usize),
}

#[allow(dead_code)]
pub enum ThroughputKind {
    // proportional to snapshot sizes
    SnapshotSize,
    // need to do the subtree mapping
    UnmappedNodesGiven,
    // not sure if I can do this one
    MappedNodes,
}

impl std::fmt::Display for ThroughputKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ThroughputKind::SnapshotSize => write!(f, "SnapshotSize"),
            ThroughputKind::UnmappedNodesGiven => write!(f, "UnmappedNodesGiven"),
            ThroughputKind::MappedNodes => write!(f, "MappedNodes"),
        }
    }
}

impl std::str::FromStr for ThroughputKind {
    type Err = std::convert::Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "SnapshotSize" => Ok(ThroughputKind::SnapshotSize),
            "UnmappedNodesGiven" => Ok(ThroughputKind::UnmappedNodesGiven),
            "MappedNodes" => Ok(ThroughputKind::MappedNodes),
            _ => Ok(ThroughputKind::UnmappedNodesGiven),
        }
    }
}

type HAST = hyperast::store::SimpleStores<hyperast_vcs_git::TStore>;
impl ThroughputKind {
    pub fn size(hyperast: &HAST, src: NodeIdentifier, dst: NodeIdentifier) -> u64 {
        use hyperast::types::HyperAST;
        use hyperast::types::WithStats;
        (hyperast.resolve(&src).size() + hyperast.resolve(&dst).size()).div_ceil(2) as u64
    }

    pub fn unmapped_nodes_given(mappings: &hyper_diff::mappings::VecStore<u32>) -> u64 {
        use hyper_diff::mappings::MappingStore;
        (mappings.capacity().0 - mappings.len()) as u64
            + (mappings.capacity().1 - mappings.len()) as u64
    }
}

macro_rules! subtree_matching {
    ($mapper:expr, $prep:expr) => {
        match $prep {
            Prep::GT => {
                use hyper_diff::matchers::heuristic::gt;

                use gt::lazy_greedy_subtree_matcher::LazyGreedySubtreeMatcher;
                type MM = hyper_diff::mappings::DefaultMultiMappingStore<u32>;
                LazyGreedySubtreeMatcher::<_>::match_it::<MM>($mapper)
            }
            Prep::CD => {
                use hyper_diff::matchers::heuristic::cd;

                use cd::lazy_leaves_matcher::LazyLeavesMatcher;
                LazyLeavesMatcher::<_>::match_it($mapper)
            }
            Prep::None => panic!(),
        }
    };
}

pub fn initial_lazy_mapping(
    hyperast: &HAST,
    src: NodeIdentifier,
    dst: NodeIdentifier,
    prep: &Prep,
) -> OwnedLazyMapping {
    use hyper_diff::decompressed_tree_store::lazy_post_order::LazyPostOrder;
    use hyper_diff::matchers::{Decompressible, Mapper};
    use hyperast::types::{HyperAST as _, HyperASTShared};

    #[allow(type_alias_bounds)]
    type DS<HAST: HyperASTShared> = Decompressible<HAST, LazyPostOrder<HAST::IdN, u32>>;
    type M = hyper_diff::mappings::VecStore<u32>;
    let mut mapper_owned: (DS<_>, DS<_>) = hyperast.decompress_pair(&src, &dst).1;
    let mapper = Mapper::with_mut_decompressible(&mut mapper_owned, M::default());
    let mapper = subtree_matching!(mapper, prep);
    let mappings = mapper.mapping.mappings;
    (
        [mapper_owned.0.decomp, mapper_owned.1.decomp],
        [mappings, Default::default()],
    )
}
