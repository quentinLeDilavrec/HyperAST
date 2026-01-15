use std::time::Instant;

use clap::Parser as _;
use hyperast_benchmark_search::{Timeout, enable_logging, read_subpatterns_file, synth_init_inst};

use hyperast_gen_ts_tsquery::lattice_graph::GroupedLattices;
use hyperast_vcs_git::multi_preprocessed::PreProcessedRepositories;
use synth_init_inst::per_blob;

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
}

#[derive(clap::clap_derive::Subcommand, Clone)]
// #[derive(clap::clap_derive::ValueEnum, Clone)]
// #[clap(rename_all = "SCREAMING_SNAKE_CASE")]
#[allow(non_camel_case_types)]
enum Bench {
    // /// Tree-Sitter baseline
    // /// using git2 to traverse the repository
    // #[clap(alias = "TS")]
    // TS {
    //     #[clap(long)]
    //     blob: bool,
    //     #[clap(long)]
    //     tree: bool,
    //     #[clap(long)]
    //     prepare: bool,
    //     #[clap(long)]
    //     cache: bool,
    // },
    // /// Baseline using our reimplementation of the executor,
    // /// but still only using tree-sitter and git2 to process source code.
    // #[clap(alias = "TSQ2")]
    // TSQ2 {
    //     #[clap(long)]
    //     blob: bool,
    //     #[clap(long)]
    //     tree: bool,
    //     #[clap(long)]
    //     prepare: bool,
    //     #[clap(long)]
    //     cache: bool,
    //     #[clap(long)]
    //     sub: Option<std::path::PathBuf>,
    //     #[clap(short = 's')]
    //     s: Vec<usize>,
    // },
    #[clap(alias = "DIFF")]
    Diff {
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
        #[clap(long)]
        input: std::path::PathBuf,
    },
    #[clap(alias = "UNIQ")]
    UNIQ {
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
        #[clap(long)]
        input: std::path::PathBuf,
        #[clap(long)]
        output: std::path::PathBuf,
    },
    #[clap(alias = "DETECTION")]
    DETECTION {
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
        #[clap(long)]
        input: std::path::PathBuf,
    },
}

// initialize synth:
// * UNIQ: per unique code instances.
//   cannot take into account the number of occurrences
// * CHNGS: on changes.
//   deletes or moves containing deletes
// * INST: on changes over match instances.
//   increase robustness relative to commits which are unsuited for synth
// * when detecting changes based on a search query and hashes
//   less work by not doing diffs on all commits
//   * on changes.
//   * on changes over match instances.
fn main() {
    enable_logging();
    let args = Cli::parse();
    let user = &args.user;
    let name = &args.name;
    let commit = &args.commit;
    let depth = args.depth;
    let repo = hyperast_vcs_git::git::Forge::Github.repo(user, name);

    let meta_simp = hyperast_benchmark_search::meta_queries::META_SIMP;
    hyperast_tsquery::Query::new(&meta_simp, hyperast_gen_ts_tsquery::language()).unwrap();

    if args.fetch {
        repo.fetch();
        eprintln!("fetched {}/{}", user, name);
        return;
    }

    let start = Instant::now();

    let timeout = args.timeout;

    let bench = args.bench.unwrap();

    let language = "Java";
    use hyperast_vcs_git::processing::RepoConfig;
    let c: RepoConfig = language.parse().unwrap();
    match bench {
        Bench::Diff {
            sub,
            s,
            language,
            cached,
            nospace,
            input,
        } => {
            dbg!(sub, s, language, cached, nospace, input);
            todo!()
        }
        Bench::UNIQ {
            sub,
            s,
            language,
            cached,
            nospace,
            input,
            output,
        } => {
            if cached {
                todo!();
            }
            if nospace {
                todo!();
            }
            let query =
                std::fs::read_to_string(&input).expect("Failed to read provided pattern file");
            let config = hyperast_benchmark_search::Config {
                config: c,
                first_chunk: 1,
                chunk_interval: 1,
                depth,
            };
            let sub = sub
                .map(|path| read_subpatterns_file(&path))
                .unwrap_or_default();
            assert!(s.len() <= sub.len());
            // only select wanted subpatterns
            let sub = s.iter().map(|i| sub[*i].as_str()).collect::<Vec<_>>();

            let lang = hyperast_vcs_git::resolve_language(&language).unwrap();
            if language == "Java" {
                use hyperast_gen_ts_java as ts_gen;
                let meta_simp = hyperast_benchmark_search::meta_queries::META_SIMP.to_string();
                hyperast_tsquery::Query::new(&meta_simp, hyperast_gen_ts_tsquery::language())
                    .unwrap();
                let elapsed_meta_simp_comp = start.elapsed();
                let mut repositories = PreProcessedRepositories::default();
                if sub.is_empty() {
                    // no need to set empty prequeries
                    repositories.register_config(repo.clone(), config.config);
                } else {
                    repositories.register_config_with_prequeries(repo.clone(), config.config, &sub);
                }
                let repo_h = repositories.get_config(repo).unwrap();
                // nofetch avoids potential noise and unintentional dataset changes
                let repository = repo_h.nofetch();
                let elapsed_load_repo = start.elapsed();
                let uniqs = synth_init_inst::find_uniques::<ts_gen::types::TStore>(
                    &mut repositories,
                    repository,
                    &sub,
                    commit,
                    &lang,
                    &query,
                    config,
                    timeout.clone(),
                );
                let elapsed_compute_examples = start.elapsed();
                let stores = &repositories.processor.main_stores;
                let sss: &hyperast::store::SimpleStores<ts_gen::types::TStore> = stores.with_ts();
                let meta_gen = hyperast_benchmark_search::meta_queries::META_GEN;

                let mut inst = uniqs.set.values().copied().collect::<Vec<_>>();
                dbg!(inst.len());
                inst.sort_by_key(|x| unsafe { std::mem::transmute::<_, u64>(x) });

                // let inst = inst.into_iter().take(1000).copied().collect::<Vec<_>>();

                let elapsed_compute_examples_post = start.elapsed();

                if inst.is_empty() {
                    log::warn!("No instances found");
                    return;
                }
                let config = hyperast_benchmark_search::synth::SynthConfig {
                    size_threshold: 200,
                    shrink_threshold_factor: 85,
                };
                let query_poset = hyperast_benchmark_search::synth::synth(
                    &timeout, &inst, sss, meta_gen, &meta_simp, &config,
                );
                let elapsed_synth = start.elapsed();
                dbg!(query_poset.count());
                let graphs =
                    hyperast_benchmark_search::synth::Prep::extract_and_group(&query_poset);
                let elapsed_group_reduce = start.elapsed();
                for i in 0..graphs.g.len() {
                    log::info!("wcc 1 := {}", graphs.wcc_description(i));
                }

                // graphs.describe();

                // let majors: Vec<_> = graphs
                //     .majors()
                //     .filter(|(q, _)| {
                //         let lang = hyperast_gen_ts_java::language();
                //         !q.is_empty()
                //             // && q.lines().count() < 50 // ignore patterns not fitting on screen
                //             && hyperast_tsquery::Query::new(&q, lang).is_ok()
                //     })
                //     .collect();
                // log::warn!(
                //     "Length of major queries:{:?}",
                //     majors
                //         .iter()
                //         .map(|q| q.0.lines().count())
                //         .collect::<Vec<_>>()
                // );
                let tops: Vec<_> = graphs
                    .tops()
                    .filter(|(q, _)| {
                        let lang = hyperast_gen_ts_java::language();
                        !q.is_empty()
                            // && q.lines().count() < 50 // ignore patterns not fitting on screen
                            && hyperast_tsquery::Query::new(&q, lang).is_ok()
                    })
                    .collect();
                log::warn!(
                    "Length of top queries:{:?}",
                    tops.iter().map(|q| q.0.lines().count()).collect::<Vec<_>>()
                );
                // tops.sort_by_key(|x| x.1.len());
                // log::warn!(
                //     "Length of top queries:\n{}",
                //     tops.iter()
                //         .map(|q| format!(
                //             "---tops---{}---{:?}----\n{}\n",
                //             q.0.lines().count(),
                //             q.1,
                //             q.0.lines().collect::<String>()
                //         ))
                //         .collect::<String>()
                // );
                let mut file = match std::fs::OpenOptions::new()
                    .write(true)
                    .create(true)
                    .truncate(true)
                    .open({
                        let mut output = output.clone();
                        output.set_extension("md");
                        output
                    }) {
                    Ok(file) => file,
                    Err(e) => {
                        println!("Error opening file: {}", e);
                        panic!();
                    }
                };
                file.set_len(0).unwrap();
                // let grouped = GroupedLattices::<&_>::new(&query_poset);
                let mut graphs: Vec<_> = graphs
                    .g
                    .into_iter()
                    .map(|graph| {
                        (
                            hyperast_gen_ts_tsquery::lattice_graph::lattice_stats(
                                &query_poset,
                                &graph,
                                |x| {
                                    x.contains(hyperast_gen_ts_tsquery::code2query::TrMarker::Uniqs)
                                },
                            ),
                            graph,
                        )
                    })
                    .collect();
                graphs.sort_by(|a, b| a.0.cmp(&b.0).reverse());
                let grouped = GroupedLattices { graphs };
                if let Err(e) = writeln!(
                    file,
                    "{}",
                    hyperast_gen_ts_tsquery::synth_display::markdown(
                        &grouped,
                        &query_poset,
                        stores
                    )
                ) {
                    eprintln!("{}", e);
                }
                let mut file = match std::fs::OpenOptions::new()
                    .write(true)
                    .create(true)
                    .truncate(true)
                    .open({
                        let mut output = output.clone();
                        output.set_extension("csv");
                        output
                    }) {
                    Ok(file) => file,
                    Err(e) => {
                        println!("Error opening file: {}", e);
                        panic!();
                    }
                };
                use std::io::Write;
                file.set_len(0).unwrap();
                macro_rules! write_data {
                    ($f:expr, $val:ident) => {
                        let name = stringify!($val);
                        let val = $val;
                        if let Err(e) = writeln!(file, "{name} {val}") {
                            eprintln!("{}", e);
                        }
                    };
                    ($f:expr, t=$val:ident) => {
                        let name = stringify!($val);
                        let val = $val;
                        let val = val.as_secs_f64();
                        if let Err(e) = writeln!(file, "{name} {val}") {
                            eprintln!("{}", e);
                        }
                    };
                }
                let input = input.to_str().unwrap_or("");
                let examples = inst.len();
                let wcc = grouped.graphs.len();
                let timeout = timeout.to_string();
                let hyperast_benchmark_search::synth::SynthConfig {
                    size_threshold,
                    shrink_threshold_factor,
                } = config;
                let shrink_threshold_factor = shrink_threshold_factor as f32 / 100.0;
                let kind = "value";
                write_data!(file, kind); // the header
                write_data!(file, user);
                write_data!(file, name);
                write_data!(file, commit);
                write_data!(file, depth);
                write_data!(file, input);
                write_data!(file, examples);
                write_data!(file, wcc);
                write_data!(file, timeout);
                write_data!(file, size_threshold);
                write_data!(file, shrink_threshold_factor);
                write_data!(file, t = elapsed_meta_simp_comp);
                write_data!(file, t = elapsed_load_repo);
                write_data!(file, t = elapsed_compute_examples);
                write_data!(file, t = elapsed_compute_examples_post);
                write_data!(file, t = elapsed_synth);
                write_data!(file, t = elapsed_group_reduce);
            } else if language == "Cpp" {
                todo!("import cpp gen")
                // per_blob::<hyperast_gen_ts_cpp::types::TStore>(
                //     repo, &sub, commit, depth, &lang, queries, timeout,
                // );
            }
        }
        Bench::DETECTION {
            sub,
            s,
            language,
            cached,
            nospace,
            input,
        } => {
            if cached {
                todo!();
            }
            if nospace {
                todo!();
            }
            let queries = hyperast_benchmark_search::ReadSearches::new(input);
            let sub = sub
                .map(|path| read_subpatterns_file(&path))
                .unwrap_or_default();
            assert!(s.len() <= sub.len());
            // only select wanted subpatterns
            let sub = s.iter().map(|i| sub[*i].as_str()).collect::<Vec<_>>();

            let lang = hyperast_vcs_git::resolve_language(&language).unwrap();
            if language == "Java" {
                per_blob::<hyperast_gen_ts_java::types::TStore>(
                    repo, &sub, commit, depth, &lang, queries, timeout,
                );
            } else if language == "Cpp" {
                todo!("import cpp gen")
                // per_blob::<hyperast_gen_ts_cpp::types::TStore>(
                //     repo, &sub, commit, depth, &lang, queries, timeout,
                // );
            }
        } // TODO also synth based on deletes
          // TODO also synth on intersection of search results and deletes -> equivalent to find_uniques except for ability to map before after
    }
}

#[test]
fn debug_malloc_free_issue() {
    let meta_simp = hyperast_benchmark_search::meta_queries::META_SIMP;
    dbg!(&meta_simp);
    for i in 0..100 {
        dbg!(i);
        hyperast_tsquery::Query::new(meta_simp, hyperast_gen_ts_tsquery::language()).unwrap();
        dbg!()
    }
}
