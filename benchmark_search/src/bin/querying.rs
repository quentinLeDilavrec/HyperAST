//! Provides different search approaches (incremental listing):
//! - original TS
//! - `+` port of the executor
//! - `+` HyperAST tree cursor
//! - `+` HyperAST tree cursor with some path compression
//!   - including variants resulting from profile driven optimizations
//! - `+` using preprocessed subpatterns

use std::str::FromStr;

use clap::Parser;
use hyperast_benchmark_search::{Timeout, no_hyperast};

#[cfg(not(target_env = "msvc"))]
use jemallocator::Jemalloc;

#[cfg(not(target_env = "msvc"))]
#[global_allocator]
static GLOBAL: Jemalloc = Jemalloc;

#[derive(clap::clap_derive::Parser)]
struct Cli {
    user: String,
    name: String,
    commit: String,
    depth: usize,
    #[clap(long)]
    fetch: bool,
    #[clap(long, value_parser = hyperast_benchmark_search::parse_timeout, default_value_t = Timeout::MAX)]
    timeout: Timeout,
    #[clap(subcommand)]
    bench: Option<Bench>,
    #[clap(long)]
    input: Option<std::path::PathBuf>,
}

#[derive(clap::clap_derive::Subcommand, Clone)]
// #[derive(clap::clap_derive::ValueEnum, Clone)]
// #[clap(rename_all = "SCREAMING_SNAKE_CASE")]
#[allow(non_camel_case_types)]
enum Bench {
    /// Tree-Sitter baseline
    /// using git2 to traverse the repository
    #[clap(alias = "TS")]
    TS {
        #[clap(long)]
        blob: bool,
        #[clap(long)]
        tree: bool,
        #[clap(long)]
        prepare: bool,
        #[clap(long)]
        cache: bool,
    },
    /// Baseline using our reimplementation of the executor,
    /// but still only using tree-sitter and git2 to process source code.
    #[clap(alias = "TSQ2")]
    TSQ2 {
        #[clap(long)]
        blob: bool,
        #[clap(long)]
        tree: bool,
        #[clap(long)]
        prepare: bool,
        #[clap(long)]
        cache: bool,
        #[clap(long)]
        sub: Option<std::path::PathBuf>,
        #[clap(short = 's')]
        s: Vec<usize>,
    },
    /// Our approach using our reimplementation of the executor and HyperAST's AST representation.
    #[clap(alias = "OURS")]
    OURS {
        #[clap(long)]
        sub: Option<std::path::PathBuf>,
        #[clap(short = 's')]
        s: Vec<usize>,
        language: String,
        #[clap(long)]
        /// cache the result of the search, associated to files in the case of Java, in the case of JavaMaven I still don't know.
        cached: bool,
        #[clap(long)]
        nospace: bool,
    },
    /// Write speed (not really part of the benchmark)
    /// It just give a good order of magnitude of hw perfs)
    WRITE,
}

fn main() {
    // let (non_blocking, _guard) = tracing_appender::non_blocking(std::io::stdout());
    // let layer = tracing_subscriber::EnvFilter::try_from_default_env();
    // tracing_subscriber::fmt()
    //     .with_writer(non_blocking)
    //     .with_env_filter(layer.unwrap_or_else(|_| "off".into()))
    //     .init();
    enable_logging();
    let args = Cli::parse();
    let user = &args.user;
    let name = &args.name;
    let commit = &args.commit;
    let depth = args.depth;
    let repo = hyperast_vcs_git::git::Forge::Github.repo(user, name);

    if args.fetch {
        repo.fetch();
        eprintln!("fetched {}/{}", user, name);
        return;
    }

    let timeout = args.timeout;

    let bench = args.bench.unwrap_or(Bench::TS {
        cache: false,
        prepare: false,
        blob: false,
        tree: false,
    });

    match bench {
        Bench::TS {
            blob,
            tree,
            prepare,
            cache,
        } => {
            let repository = repo.nofetch();
            let language = "Java";
            let language = hyperast_vcs_git::resolve_language(language).unwrap();
            let queries = hyperast_benchmark_search::ReadSearches::default();
            (if cache {
                if !prepare && !blob && !tree {
                    no_hyperast::baseline
                } else {
                    unimplemented!()
                }
            } else if prepare && blob && tree {
                no_hyperast::baseline_prepare_blob_tree
            } else {
                unimplemented!()
            })(repository, commit, depth, &language, queries, timeout);
        }
        Bench::TSQ2 {
            prepare,
            blob,
            tree,
            cache,
            sub,
            s,
        } => {
            let repository = repo.nofetch();
            let language = "Java";
            let language = hyperast_vcs_git::resolve_language(language).unwrap();
            let queries = hyperast_benchmark_search::ReadSearches::default();
            let sub = sub
                .map(|path| read_subpatterns_file(&path))
                .unwrap_or_default();
            assert!(s.len() <= sub.len());
            // only select wanted subpatterns
            let sub = s.iter().map(|i| sub[*i].as_str()).collect::<Vec<_>>();
            if prepare {
                if blob && tree {
                    if sub.is_empty() {
                        no_hyperast::baseline_our_executor_prepare_cache_trees_and_blobs(
                            repository, commit, depth, &language, queries, timeout,
                        );
                    } else {
                        no_hyperast::baseline_our_executor_prepare_cache_trees_and_blobs_precomp(
                            repository, commit, depth, &language, &sub, queries, timeout,
                        );
                    }
                } else if tree {
                    todo!()
                } else {
                    use clap::CommandFactory;
                    let mut cmd = Cli::command();
                    cmd.error(
                        clap::error::ErrorKind::ArgumentConflict,
                        "Preparing blobs alone is a memory heavy operation that would only slow down processing",
                    );
                }
            } else if blob && tree && cache {
                no_hyperast::baseline_our_executor_cache_trees_and_blobs_memo(
                    repository, commit, depth, &language, queries, timeout,
                );
            } else if blob && tree {
                no_hyperast::baseline_our_executor_cache_trees_and_blobs(
                    repository, commit, depth, &language, queries, timeout,
                );
            } else if tree {
                todo!()
            } else if blob {
                todo!()
            } else {
                no_hyperast::baseline_our_executor(
                    repository, commit, depth, &language, queries, timeout,
                );
            }
        }
        Bench::OURS {
            sub,
            language,
            s,
            cached,
            nospace,
        } => {
            use hyperast_vcs_git::processing::RepoConfig;
            let c: RepoConfig = FromStr::from_str(&language).unwrap();
            let queries = if let Some(input) = args.input {
                hyperast_benchmark_search::ReadSearches::new(input)
            } else {
                hyperast_benchmark_search::ReadSearches::default()
            };
            let sub = sub
                .map(|path| read_subpatterns_file(&path))
                .unwrap_or_default();
            assert!(s.len() <= sub.len());
            // only select wanted subpatterns
            let sub = s.iter().map(|i| sub[*i].as_str()).collect::<Vec<_>>();

            use hyperast_benchmark_search::with_hyperast;
            if language == "Java" {
                let language = hyperast_vcs_git::resolve_language(&language).unwrap();
                let per_blob = if cached {
                    with_hyperast::per_blob_cached
                } else if nospace {
                    with_hyperast::per_blob_nospaces
                } else {
                    with_hyperast::per_blob
                };
                per_blob(repo, &sub, commit, depth, &language, queries, timeout);
            } else if c == RepoConfig::JavaMaven {
                assert!(!cached);
                let language = hyperast_vcs_git::resolve_language("Java").unwrap();
                with_hyperast::polyglot(repo, &sub, commit, depth, &language, queries, timeout);
                // } else if language == RepoConfig::Cpp {
                //     todo!();
            } else if c == RepoConfig::CppMake {
                todo!();
            } else {
                unimplemented!("new generators are needed to support additional languages")
            }
        }
        Bench::WRITE => {
            for s in hyperast_benchmark_search::ReadSearches::default() {
                println!("----------");
                print!("{}", s);
            }
            write_speed::run_write_benchmark();
        }
    }
}

fn read_subpatterns_file(sub: &std::path::Path) -> Vec<String> {
    std::fs::read_to_string(sub)
        .expect("Failed to read provided subpattern file")
        .lines()
        .fold(vec![String::new()], |mut acc, line| {
            if line.trim().is_empty() {
                acc.push(String::new());
            } else {
                acc.last_mut().unwrap().push_str(line);
            }
            acc
        })
}

fn enable_logging() {
    use tracing_subscriber::layer::SubscriberExt as _;
    use tracing_subscriber::util::SubscriberInitExt;
    let layer = tracing_subscriber::EnvFilter::try_from_default_env();
    tracing_subscriber::registry()
        .with(layer.unwrap_or_else(|_| "off".into()))
        .with(tracing_subscriber::fmt::layer())
        .init();
}

// impl From<Bench> for clap::builder::OsStr {
//     fn from(value: Bench) -> Self {
//         match value {
//             Bench::TS {
//                 blob,
//                 tree,
//                 prepare,
//                 cache, } => "TS".into(),
//             Bench::TSQ2 {
//                 blob,
//                 tree,
//                 prepare,
//                 cache,
//             } => if prepare {
//                 if blob && tree {
//                     "TSQ2 --PREPARE --BLOB --TREE"
//                 } else if tree {
//                     "TSQ2 --PREPARE --TREE"
//                 } else {
//                     panic!()
//                 }
//             } else if blob && tree {
//                 "TSQ2 --BLOB --TREE"
//             } else if tree {
//                 "TSQ2 --TREE"
//             } else if blob {
//                 "TSQ2 --BLOB"
//             } else {
//                 "TSQ2"
//             }
//             .into(),
//             Bench::OURS {
//                 sub,
//                 language,
//                 s: _,
//                 cached: _,
//                 nospace: _,
//             } => if let Some(_) = sub {
//                 if language == "JavaMaven" {
//                     "OURS --sub <sub> JavaMaven"
//                 } else if language == "Java" {
//                     "OURS --sub <sub> Java"
//                 } else if language == "Cpp" {
//                     "OURS --sub <sub> Cpp"
//                 } else if language == "CppMake" {
//                     "OURS --sub <sub> CppMake"
//                 } else {
//                     panic!()
//                 }
//             } else if language == "JavaMaven" {
//                 "OURS JavaMaven"
//             } else if language == "Java" {
//                 "OURS Java"
//             } else if language == "Cpp" {
//                 "OURS Cpp"
//             } else if language == "CppMake" {
//                 todo!()
//             } else {
//                 unimplemented!()
//             }
//             .into(),
//             Bench::WRITE => "WRITE".into(),
//         }
//     }
// }

mod write_speed {
    use std::time::Instant;

    struct XorShift64 {
        state: u64,
    }

    impl XorShift64 {
        fn new(seed: u64) -> Self {
            Self { state: seed }
        }

        fn next_u64(&mut self) -> u64 {
            let mut x = self.state;
            x ^= x << 13;
            x ^= x >> 7;
            x ^= x << 17;
            self.state = x;
            x
        }
    }

    pub fn run_write_benchmark() {
        const SIZE: usize = 2 * 1024 * 1024 * 1024; // 2 GiB

        // Allocate 2 GiB
        let mut data = vec![0u8; SIZE];

        let mut rng = XorShift64::new(0xDEADBEEF);

        let start = Instant::now();

        // Fill the buffer in 8-byte chunks
        let chunks = data.chunks_exact_mut(8);
        for chunk in chunks {
            let rand = rng.next_u64();
            chunk.copy_from_slice(&rand.to_ne_bytes());
        }

        // Handle remainder (if SIZE isn’t divisible by 8)
        let remainder = data.chunks_exact_mut(8).into_remainder();
        if !remainder.is_empty() {
            let rand = rng.next_u64();
            for (i, b) in remainder.iter_mut().enumerate() {
                *b = rand.to_ne_bytes()[i];
            }
        }

        let duration = start.elapsed();

        println!("Wrote 2 GiB random data in {:?}", duration);
        println!(
            "Throughput: {:.2} GiB/s",
            (SIZE as f64 / (1 << 30) as f64) / duration.as_secs_f64()
        );
    }
}
