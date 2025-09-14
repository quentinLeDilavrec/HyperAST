//! Provides different search approaches (incremental listing):
//! - original TS
//! - `+` port of the executor
//! - `+` HyperAST tree cursor
//! - `+` HyperAST tree cursor with some path compression
//!   - including variants resulting from profile driven optimizations
//! - `+` using preprocessed subpatterns

use clap::Parser;
use hyperast_benchmark_search::{no_hyperast, queries};

#[derive(clap::clap_derive::Parser)]
struct Cli {
    user: String,
    name: String,
    commit: String,
    depth: usize,
    #[clap(long)]
    fetch: bool,
    #[clap(value_enum, default_value = Bench::TS)]
    bench: Bench,
}

#[derive(clap::clap_derive::ValueEnum, Clone)]
#[clap(rename_all = "SCREAMING_SNAKE_CASE")]
enum Bench {
    TS,
    TSQ2,
}

fn main() {
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

    let query = queries::Q3;

    match args.bench {
        Bench::TS => {
            let repository = repo.nofetch();
            no_hyperast::baseline(repository, commit, depth, query);
        }
        Bench::TSQ2 => {
            let repository = repo.nofetch();
            no_hyperast::baseline_our_executor(repository, commit, depth, query);
        }
    }
}

fn enable_logging() {
    use tracing_subscriber::layer::SubscriberExt as _;
    use tracing_subscriber::util::SubscriberInitExt;
    let filter = tracing_subscriber::EnvFilter::try_from_default_env();
    tracing_subscriber::registry()
        .with(filter.unwrap_or_else(|_| "off".into()))
        .with(tracing_subscriber::fmt::layer())
        .init();
}

impl From<Bench> for clap::builder::OsStr {
    fn from(value: Bench) -> Self {
        match value {
            Bench::TS => "TS".into(),
            Bench::TSQ2 => "TSQ2".into(),
        }
    }
}

// impl std::str::FromStr for Bench {
//     type Err = String;

//     fn from_str(s: &str) -> Result<Self, Self::Err> {
//         match s {
//             "TS" => Ok(Bench::TS),
//             "TSQ2" => Ok(Bench::TSQ2),
//             _ => Err(format!("Invalid bench type: {}", s)),
//         }
//     }
// }
