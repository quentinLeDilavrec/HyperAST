use std::io::stdin;

use clap::{Parser, Subcommand};
use tracing_subscriber::EnvFilter;

#[derive(Parser)]
/// Measure metrics in source code histories
///
/// supported languages:
/// - Java
/// - Cpp (soon)
struct Cli {
    /// The owner of the repository
    owner: String,
    /// The name of the repository
    name: String,
    /// The start commit
    commit: String,
    /// File containing a script to execute
    #[clap(short, long, default_value_t = 10)]
    depth: usize,
    #[clap(short, long)]
    file: Option<std::path::PathBuf>,
    /// Examples scripts
    #[clap(short, long)]
    example: Option<String>,
    #[clap(short, long)]
    interative: bool,
}

fn main() -> std::result::Result<(), Box<dyn std::error::Error>> {
    let args = Cli::parse();

    // let _ = tracing_subscriber::fmt()
    //     .with_env_filter(EnvFilter::from_default_env())
    //     .try_init()
    //     .unwrap();

    // let repo_spec = hyper_ast_cvs_git::git::Forge::Github.repo("graphhopper", "graphhopper");
    // let commit = "f5f2b7765e6b392c5e8c7855986153af82cc1abe";
    // let script = hyper_ast::scripting::lua_scripting::PREPRO_LOC.into();
    let repo_spec = hyper_ast_cvs_git::git::Forge::Github.repo(&args.owner, &args.name);
    let config = hyper_ast_cvs_git::processing::RepoConfig::JavaMaven;
    if let Some(file) = args.file {
        let script = std::fs::read_to_string(file).unwrap();
        scripting(repo_spec, config, &args.commit, &script, args.depth)
    } else if let Some(example) = args.example {
        let script = match example.as_str() {
            "size" => hyper_ast::scripting::lua_scripting::PREPRO_SIZE_WITH_FINISH,
            "mcc" => hyper_ast::scripting::lua_scripting::PREPRO_MCC_WITH_FINISH,
            "LoC" => hyper_ast::scripting::lua_scripting::PREPRO_LOC,
            x => {
                eprintln!("{x} is not an available example. Try: size, mcc, LoC");
                std::process::exit(1)
            }
        };
        scripting(repo_spec, config, &args.commit, &script, args.depth)
    } else if args.interative {
        let mut script = String::new();
        for l in stdin().lines() {
            let l = l.unwrap();
            if l.is_empty() {
                break;
            }
            script += &l;
        }
        scripting(repo_spec, config, &args.commit, &script, args.depth)
    } else {
        eprintln!(
            "You need to select an example, give a file or run a subcommand, use -h to show help."
        );
        std::process::exit(1)
    }
}

fn scripting(
    repo_spec: hyper_ast_cvs_git::git::Repo,
    config: hyper_ast_cvs_git::processing::RepoConfig,
    commit: &str,
    script: &str,
    depth: usize,
) -> std::result::Result<(), Box<dyn std::error::Error>> {
    let state = client::AppState::default();
    state
        .repositories
        .write()
        .unwrap()
        .register_config_with_prepro(repo_spec.clone(), config, script.into());
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
    let repository = repo.fetch();
    log::debug!("done cloning {}", repository.spec);
    let mut rw = hyper_ast_cvs_git::git::Builder::new(&repository.repo)
        .unwrap()
        .first_parents()
        .unwrap()
        .after(commit)?
        .walk()?
        .take(depth)
        .map(|x| x.unwrap());

    let mut commits =
        state
            .repositories
            .write()
            .unwrap()
            .pre_process_chunk(&mut rw, &repository, 1);
    for _ in 0..200 {
        if commits.is_empty() {
            break;
        }
        dbg!(commits.len());
        for oid in commits {
            after_prepared(&state, &repository, oid);
        }
        commits = state
            .repositories
            .write()
            .unwrap()
            .pre_process_chunk(&mut rw, &repository, 100);
    }
    Ok(())
}

fn after_prepared(
    state: &client::AppState,
    repository: &hyper_ast_cvs_git::processing::ConfiguredRepo2,
    oid: hyper_ast_cvs_git::git::Oid,
) {
    let repositories = state.repositories.read().unwrap();
    let commit = repositories.get_commit(&repository.config, &oid).unwrap();
    let store = &state.repositories.read().unwrap().processor.main_stores;
    let n = store.node_store.resolve(commit.ast_root);
    let dd = n
        .get_component::<hyper_ast::scripting::lua_scripting::DerivedData>()
        .unwrap();
    println!("{} {} {:?}", &oid, commit.processing_time(), &dd.0);
}