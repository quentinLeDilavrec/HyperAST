//! Compute stats provided with a directory of search benchmark results, print a table in csv or latex

use std::fs::{File, ReadDir};
use std::io::BufRead;
use std::path::{Path, PathBuf};

use clap::Parser as _;
use hyperast_benchmark_search::enable_logging;
use hyperast_benchmark_search::search::{Bench, Cli};

#[cfg(not(target_env = "msvc"))]
use jemallocator::Jemalloc;
use polars_core::frame::DataFrame;

#[cfg(not(target_env = "msvc"))]
#[global_allocator]
static GLOBAL: Jemalloc = Jemalloc;

fn main() {
    // let layer = tracing_subscriber::EnvFilter::try_from_default_env();
    // tracing_subscriber::fmt()
    //     .with_env_filter(layer.unwrap_or_else(|_| "off".into()))
    //     .init();
    enable_logging();

    let argv = std::env::args().collect::<Vec<_>>();

    let path = PathBuf::from(argv.get(1).unwrap());
    // let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("search_results/");

    if !std::fs::exists(&path).unwrap() {
        println!("No search results found.");
        return;
    }

    let d = extract_from_results_dir(std::fs::read_dir(&path).unwrap()).unwrap();
    let d = d
        .into_iter()
        .filter_map(|BenchResult { data, config, dir }| {
            let config = handle_log(config).unwrap();

            let df = handle_csv(&data).unwrap();

            let data = process_df(df).unwrap();
            Some(BenchResult { data, config, dir })
        })
        .collect::<Vec<_>>();

    let d = d
        .into_iter()
        .fold(std::collections::HashMap::new(), |mut acc, d| {
            let key = (
                d.config.input.clone(),
                d.config.language.clone().unwrap(),
                d.config.depth,
                d.config.bench.clone(),
            );
            let r = acc.entry(key).or_insert(vec![]);
            r.push(d);
            acc
        });
    // dbg!(&d);

    let mut d = d.into_iter().collect::<Vec<_>>();
    d.sort_by_key(|(_, v)| v.len());
    for (k, v) in &mut d {
        let Some(input) = &k.0 else {
            dbg!();
            continue;
        };
        let Some(bench) = &k.3 else {
            dbg!(&k.3);
            continue;
        };
        let Some(_) = show_config(input, bench, &k.1, k.2) else {
            continue;
        };
        for x in v {
            eprint!("\t{:>10} ", x.config.name);
            let df = search_delta_stats(&mut x.data).unwrap();
            eprintln!("{}", df);
            eprintln!("\t{}", x.dir.to_string_lossy());
        }
    }
}

fn show_config(input: &PathBuf, bench: &Bench, language: &str, depth: usize) -> Option<()> {
    eprintln!("--input {}", input.to_string_lossy());
    match bench {
        Bench::OURS { sub: Some(sub), .. } => {
            eprintln!("--sub {}\n", sub.to_string_lossy(),);
        }
        _ => (),
    }

    eprint!("--language {}\n--depth {}\n", language, depth);
    match bench {
        Bench::OURS { .. } => Some(()),
        Bench::TSQ2 {
            prepare,
            cache,
            tree,
            blob,
            ..
        }
        | Bench::TS {
            blob,
            tree,
            prepare,
            cache,
            ..
        } => {
            if *blob {
                eprintln!("--blob {}", blob);
            }
            if *tree {
                eprintln!("--tree {}", tree);
            }
            if *prepare {
                eprintln!("--prepare {}", prepare);
            }
            if *cache {
                eprintln!("--cache {}", cache);
            }
            Some(())
        }
        _ => return None,
    }
}

fn process_df(mut df: DataFrame) -> Result<DataFrame, polars_core::prelude::PolarsError> {
    use polars_core::datatypes::StringChunked;
    use polars_core::prelude::*;

    let names = df.get_column_names();
    let names = names
        .into_iter()
        .map(|x| x.trim().trim_end_matches("(us)").to_owned())
        .collect::<Vec<_>>();
    df.set_column_names(&names)?;
    let mut df = df.drop_many(&["since_first_commit", "goto_count"]);

    let s = df.column("task")?.str()?;
    let trimmed: StringChunked = s.into_iter().map(|opt| opt.map(|v| v.trim())).collect();
    df.replace("task", trimmed.into_series())?;

    Ok(df)
}

fn search_delta_stats(df: &mut DataFrame) -> Result<DataFrame, polars_core::prelude::PolarsError> {
    use polars_core::prelude::*;
    let tasks = df.column("task")?.str()?;
    if tasks.is_empty() {
        eprintln!();
        return Ok(df.clone());
    };

    let execs = tasks.equal("exec on commit");
    let mut prev = 0_u32;
    let fairer = tasks
        .iter()
        .zip(df.column("cumulative")?.u32()?.iter())
        .map(|(t, c)| {
            let mut x = c.unwrap();
            if t.unwrap().eq("compile query") {
                prev = 0;
                None
            } else if t.unwrap().eq("exec on commit") {
                if prev == 0 {
                    // ignores the first exec on commit to be fairer to baseline,
                    // where we cannot separate commit preparation from exec
                    prev = x;
                    return None;
                }
                // recompute a delta including all processing between exec events
                x = x - prev;
                prev = c.unwrap();
                Some(x)
            } else {
                None
            }
        })
        .collect::<Vec<_>>();
    let fairer = Series::new("fairer".into(), fairer);
    let mut prev = 0;
    let queries: Int32Chunked = tasks.apply_values_generic(|v| {
        let mut x = prev;
        if v.eq("compile query") {
            x += 1;
        }
        prev = x;
        x
    });
    let column = queries.with_name("queries");
    df.with_column(column)?;
    df.with_column(fairer)?;
    let df = df.filter(&execs)?;
    let gr = df.group_by(["queries"])?;
    let df = gr
        .apply(|df| stats(&df))?
        .sort(["query"], Default::default())?;
    Ok(df)
}

fn stats(df: &DataFrame) -> Result<DataFrame, polars_core::prelude::PolarsError> {
    use polars_core::prelude::*;
    let queries = df.column("queries")?;
    let query = if queries.i32()?.n_unique()? == 1 {
        queries.get(0)?
    } else {
        dbg!(&df);
        return Ok(DataFrame::default());
    };
    // let delta = "delta";
    let delta = "fairer";
    let delta = df.column(delta)?.u32()?;
    let count = delta.len();
    let ms = 1_000f64;
    let Some(min) = delta.min() else {
        eprintln!();
        return Ok(DataFrame::default());
    };
    let min = min as f64 / ms;
    let max = delta.max().unwrap() as f64 / ms;
    let mean = delta.mean().unwrap() / ms;
    let median = delta.median().unwrap() / ms;
    let std = delta.std(1).unwrap() / ms;
    let var = delta.var(1).unwrap() / ms;

    Ok(DataFrame::new(vec![
        Series::new("query", &[query]),
        Series::new("count", &[count as u32]),
        Series::new("min", &[min]),
        Series::new("max", &[max]),
        Series::new("mean", &[mean]),
        Series::new("median", &[median]),
        Series::new("std", &[std]),
        Series::new("var", &[var]),
    ])?)
}

fn handle_csv(path: &Path) -> polars_core::error::PolarsResult<DataFrame> {
    use polars_core::prelude::*;
    use polars_io::*;

    let mut schema = polars_core::schema::Schema::new();
    schema.with_column("task".into(), DataType::String);
    schema.with_column("cumulative(us)".into(), DataType::UInt32);
    schema.with_column("delta(us)".into(), DataType::UInt32);
    schema.with_column("since_first_commit(us)".into(), DataType::UInt32);
    schema.with_column("mem".into(), DataType::String);
    schema.with_column("value".into(), DataType::UInt32);
    schema.with_column("status_count".into(), DataType::UInt32);
    schema.with_column("goto_count".into(), DataType::UInt32);
    schema.with_column("node_count".into(), DataType::UInt32);
    schema.with_column("size".into(), DataType::UInt32);

    let o = csv::read::CsvParseOptions::default().with_separator(b";"[0]);
    // Prefer `from_path` over `new` as it is faster.
    csv::read::CsvReadOptions::default()
        .with_parse_options(o)
        .with_has_header(true)
        .with_schema(Some(schema.into()))
        .try_into_reader_with_file_path(Some(path.into()))?
        .finish()
}

#[derive(Debug)]
struct BenchResult<D, C = D> {
    data: D,
    config: C,
    dir: PathBuf,
}

fn extract_from_results_dir(dir: ReadDir) -> Result<Vec<BenchResult<PathBuf>>, std::io::Error> {
    let mut r = vec![];
    let mut data = None;
    let mut config = None;
    for p in dir {
        let p = p.unwrap();
        let path = p.path();
        if path
            .extension()
            .map_or(false, |e| e.eq_ignore_ascii_case("stderr"))
        {
            config = Some(path);
            continue;
        } else if path
            .extension()
            .map_or(false, |e| e.eq_ignore_ascii_case("csv"))
        {
            data = Some(path);
            continue;
        }
        let dir = match std::fs::read_dir(&path) {
            Ok(dir) => dir,
            Err(err) => {
                dbg!(err.kind());
                continue;
            }
        };
        r.extend(extract_from_results_dir(dir)?);
    }
    if let (Some(data), Some(config)) = (data, config) {
        let path = data.parent().unwrap().to_owned();
        r.push(BenchResult {
            data,
            config,
            dir: path,
        });
    }
    Ok(r)
}

fn handle_log(config: PathBuf) -> Result<Cli, Box<dyn std::error::Error>> {
    let config = std::io::BufReader::new(File::open(config)?);
    let mut lines = config.lines();
    let first_line = lines.next().unwrap()?;
    let itr = first_line.split(" "); // will produce invalid results with spaces in quotes
    let config = Cli::try_parse_from(itr)?;
    // TODO handle remaining lines, try to log parsable stuff
    // e.g. for queries, block with special start line giving length`
    Ok(config)
}
