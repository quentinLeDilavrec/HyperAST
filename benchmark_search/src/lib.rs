//! Benchmark of code search using the HyperAST and Tree-sitter queries.
//!
//! validity: baseline tree-sitter, same number of matches (tree-sitter and our query syntax have slightly diverged, so it can only be done on a subset)
//!
//! performances: baseline tree-sitter, time/memory show perf issues when not using our approach
//!
//! code: repository (reuse known repositories) (but tree-sitter does not work on entire commits) ) / files (reuse tsg dataset)
//!
//! Priorities:
//! The main objective of this benchmark suite is to measure performances (mem, latency, wall time).
//! The validity will first be limited to the capabilities of the baseline.

pub mod no_hyperast;
pub mod with_hyperast;

mod data;
pub use data::DATASET;
pub mod queries;

pub mod synth;
pub mod synth_init_inst;

pub mod search;

pub use hyperast_gen_ts_tsquery::meta_queries;

use std::fmt::Display;
use std::time::{Duration, Instant};
use std::{env, fs, io, path};

pub fn tempfile() -> io::Result<(path::PathBuf, fs::File)> {
    let mut path = env::temp_dir();
    let file_name = std::time::SystemTime::UNIX_EPOCH;
    path.push(file_name.elapsed().unwrap().as_nanos().to_string());
    let file = fs::File::create(&path)?;
    Ok((path, file))
}

pub fn commit_rw(
    commit: &str,
    limit: Option<usize>,
    repository: &git2::Repository,
) -> Result<impl Iterator<Item = git2::Oid>, git2::Error> {
    Ok(hyperast_vcs_git::git::Builder::new(&repository)?
        .after(commit)?
        .first_parents()?
        .walk()?
        .take(limit.unwrap_or(1))
        .map(|x| x.expect("a valid commit oid")))
}

pub fn enable_logging() {
    use tracing_subscriber::layer::SubscriberExt as _;
    use tracing_subscriber::util::SubscriberInitExt;
    let layer = tracing_subscriber::EnvFilter::try_from_default_env();
    tracing_subscriber::registry()
        .with(layer.unwrap_or_else(|_| "off".into()))
        .with(tracing_subscriber::fmt::layer())
        .init();
}

pub fn read_subpatterns_file(sub: &std::path::Path) -> Vec<String> {
    fs::read_to_string(sub)
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

#[derive(Debug, Clone, Copy)]
pub struct Config {
    pub config: hyperast_vcs_git::processing::RepoConfig,
    pub first_chunk: usize,
    pub chunk_interval: usize,
    pub depth: usize,
}

impl Config {
    pub fn freq1(config: hyperast_vcs_git::processing::RepoConfig, depth: usize) -> Self {
        Self {
            config,
            first_chunk: 1,
            chunk_interval: 1,
            depth,
        }
    }
}

pub enum LogEntry<R> {
    PrepareRepository,
    PrepareCommits(usize),
    PrepareFiles(usize),
    CompileQuery,
    ExecuteQueryOnFile(R),
    ExecuteQueryOnCommit(R, usize),
}
impl<R> LogEntry<R> {
    fn name(&self) -> String {
        match self {
            LogEntry::PrepareCommits(x) => {
                format!(
                    "{:>20}",
                    if *x > 1 {
                        format!("prep {x} commits")
                    } else {
                        format!("prep {x} commit")
                    },
                )
            }
            LogEntry::PrepareFiles(x) => format!(
                "{:>20}",
                if *x > 1 {
                    format!("prep {x} files")
                } else {
                    format!("prep {x} file")
                },
            ),
            LogEntry::PrepareRepository => format!("{:>20}", "prep repo",),
            LogEntry::CompileQuery => format!("{:>20}", "compile query",),
            LogEntry::ExecuteQueryOnFile(_) => format!("{:>20}", "exec on file",),
            LogEntry::ExecuteQueryOnCommit(_, _) => {
                format!("{:>20}", "exec on commit",)
            }
        }
    }
}

pub(crate) trait ResultLogger<R> {
    fn log(&mut self, entry: LogEntry<R>) -> Result<(), TimeoutError>;
    // fn file_result(&mut self, r: R) -> Result<(), TimeoutError> {
    //     self.log(LogEntry::ExecuteQueryOnFile(r))
    // }
    fn commit_result(&mut self, r: R) -> Result<(), TimeoutError> {
        self.log(LogEntry::ExecuteQueryOnCommit(r, 0))
    }
    fn commit_result_with_size(&mut self, r: R, size: usize) -> Result<(), TimeoutError> {
        self.log(LogEntry::ExecuteQueryOnCommit(r, size))
    }

    fn commit_prepared(&mut self, count: usize) -> Result<(), TimeoutError> {
        self.log(LogEntry::PrepareCommits(count))
    }
    fn query_compiled(&mut self) -> Result<(), TimeoutError> {
        self.log(LogEntry::CompileQuery)
    }
    fn repo_prepared(&mut self) -> Result<(), TimeoutError> {
        self.log(LogEntry::PrepareRepository)
    }
}

pub struct Cumulative<R> {
    start_time: Instant,
    timeout: Timeout,
    cumulative: Vec<(LogEntry<R>, Duration)>,
}

impl<R> Default for Cumulative<R> {
    fn default() -> Self {
        Self::with_timeout(Timeout(Duration::MAX))
    }
}

impl<R> Cumulative<R> {
    fn with_timeout(timeout: Timeout) -> Self {
        Cumulative {
            start_time: Instant::now(),
            timeout,
            cumulative: Vec::with_capacity(100),
        }
    }
}

impl<R> ResultLogger<R> for Cumulative<R> {
    fn log(&mut self, entry: LogEntry<R>) -> Result<(), TimeoutError> {
        let duration = self.start_time.elapsed();
        self.cumulative.push((entry, duration));
        if duration > self.timeout.0 {
            Err(TimeoutError(duration))
        } else {
            Ok(())
        }
    }
}

#[derive(Clone, Debug)]
pub struct Timeout(std::time::Duration);

pub fn parse_timeout(s: &str) -> Result<Timeout, clap::Error> {
    s.parse()
        .map(|secs| {
            Timeout(
                std::time::Duration::try_from_secs_f64(secs).unwrap_or(std::time::Duration::MAX),
            )
        })
        .map_err(|e| clap::Error::raw(clap::error::ErrorKind::InvalidValue, e))
}

impl Timeout {
    pub const MAX: Timeout = Timeout(std::time::Duration::MAX);
}

impl Display for Timeout {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.as_secs_f64())
    }
}

pub struct TimeoutError(std::time::Duration);

impl std::fmt::Display for TimeoutError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Timeout error: {:?}", self.0)
    }
}

impl<R: Display> Cumulative<R>
where
    CsvHeader<R>: Display,
{
    fn finish(&self) {
        self.write_to(std::io::stdout());
    }
    fn write_to<T: io::Write + Send + 'static>(&self, mut writer: T) {
        writeln!(writer, "{}", self).unwrap();
    }
}

impl<R: Display> Display for Cumulative<R>
where
    CsvHeader<R>: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // log::info!("Finished in {:?}", start_instant.elapsed());
        let cumulative = &self.cumulative;
        let mut first_commit_prep = Duration::ZERO;
        let mut prev = Duration::ZERO;
        writeln!(
            f,
            "{:>20};{:>14};{:>9};{:>22};{:>6};{:>10}",
            "task",
            "cumulative(us)",
            "delta(us)",
            "since_first_commit(us)",
            CsvHeader::<R>::default(),
            "size"
        )?;

        for (task, duration) in cumulative {
            writeln!(
                f,
                "{};{}",
                LogTimeTask::new(&mut first_commit_prep, &mut prev, task, duration),
                LogTask::new(task),
            )?;
        }
        Ok(())
    }
}

struct LogTimeTask<'a, R> {
    first_commit_prep: &'a Duration,
    delta: Duration,
    since: Duration,
    task: &'a LogEntry<R>,
    duration: &'a Duration,
}

impl<'a, R> LogTimeTask<'a, R> {
    fn new(
        first_commit_prep: &'a mut Duration,
        prev: &'a mut Duration,
        task: &'a LogEntry<R>,
        duration: &'a Duration,
    ) -> Self {
        let delta = *duration - *prev;
        let since = *duration - *first_commit_prep;
        *prev = *duration;
        match task {
            LogEntry::PrepareCommits(_) => {
                if *first_commit_prep == Duration::ZERO {
                    // *prev = *duration;
                    *first_commit_prep = *duration;
                }
            }
            // LogEntry::ExecuteQueryOnCommit(_, _) => {
            //     *prev = *duration;
            // }
            _ => {}
        }
        Self {
            first_commit_prep,
            delta,
            since,
            task,
            duration,
        }
    }
}

impl<'a, R> std::fmt::Display for LogTimeTask<'a, R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let duration = self.duration;
        let delta = &self.delta;
        let since = if let LogEntry::PrepareCommits(_) = self.task {
            *duration - *self.first_commit_prep
        } else {
            self.since
        };
        write!(
            f,
            "{:>20};{:>14};{:>9};{:>22}",
            self.task.name(),
            duration.as_micros(),
            delta.as_micros(),
            since.as_micros(),
        )
    }
}

struct LogTask<'a, R> {
    task: &'a LogEntry<R>,
}

impl<'a, R> LogTask<'a, R> {
    fn new(task: &'a LogEntry<R>) -> Self {
        Self { task }
    }
}

impl<'a, R: Display> std::fmt::Display for LogTask<'a, R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.task {
            LogEntry::PrepareRepository
            | LogEntry::PrepareCommits(_)
            | LogEntry::PrepareFiles(_)
            | LogEntry::CompileQuery => {
                write!(f, "{:>6}", "")
            }
            LogEntry::ExecuteQueryOnFile(v) => write!(f, "{:>6}", v),
            LogEntry::ExecuteQueryOnCommit(v, size) if *size == 0 => {
                write!(f, "{:>6};", v)
            }
            LogEntry::ExecuteQueryOnCommit(v, size) => {
                write!(f, "{:>6};{:>6}", v, size)
            }
        }
    }
}

/// Simply used to define how to display the csv header of something
pub struct CsvHeader<T>(std::marker::PhantomData<T>);

impl<R> Default for CsvHeader<R> {
    fn default() -> Self {
        CsvHeader(std::marker::PhantomData)
    }
}

impl Display for CsvHeader<usize> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "value")
    }
}

pub struct NonBlockingResLogger<R> {
    start_time: Instant,
    timeout: Timeout,
    writer: tracing_appender::non_blocking::NonBlocking,
    _guard: tracing_appender::non_blocking::WorkerGuard,
    first_commit_prep: Duration,
    prev: Duration,
    _phantom: std::marker::PhantomData<R>,
}

impl<R> NonBlockingResLogger<R>
where
    CsvHeader<R>: Display,
{
    fn with_timeout<T: io::Write + Send + 'static>(writer: T, timeout: Timeout) -> Self {
        // let (mut non_blocking, _guard) = tracing_appender::non_blocking(std::io::stdout());
        let (mut non_blocking, _guard) = tracing_appender::non_blocking(writer);
        use std::io::Write;
        writeln!(
            non_blocking,
            "{:>20};{:>14};{:>9};{:>14};{:>8};{:>6};{:>6}",
            "task",
            "cumulative(us)",
            "delta(us)",
            "since_first_commit(us)",
            "mem",
            CsvHeader::<R>::default(),
            "size"
        )
        .unwrap();
        NonBlockingResLogger {
            start_time: Instant::now(),
            timeout,
            first_commit_prep: Duration::ZERO,
            prev: Duration::ZERO,
            writer: non_blocking,
            _guard,
            _phantom: std::marker::PhantomData,
        }
    }

    fn finish(&self) {}
}

impl<R> Display for NonBlockingResLogger<R> {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // self.writer.flush().map_err(|e| std::fmt::Error)
        Ok(())
    }
}

impl<R: Display> ResultLogger<R> for NonBlockingResLogger<R> {
    fn log(&mut self, entry: LogEntry<R>) -> Result<(), TimeoutError> {
        let duration = self.start_time.elapsed();
        let mem = hyperast::utils::memusage();
        use std::io::Write;
        writeln!(
            &mut self.writer,
            "{};{:>8};{}",
            LogTimeTask::new(
                &mut self.first_commit_prep,
                &mut self.prev,
                &entry,
                &duration
            ),
            mem,
            LogTask::new(&entry),
        )
        .unwrap();

        if duration > self.timeout.0 {
            Err(TimeoutError(duration))
        } else {
            Ok(())
        }
    }
}

fn read_multilines(buffer: &mut String) -> &str {
    let len = buffer.len();
    while let Ok(line) = std::io::stdin().read_line(buffer) {
        if line <= 1 {
            break;
        }
    }
    &buffer[len..]
}

/// give a more helpful error message when instantiating a [`git2::Repository`]
pub fn help_fetch(repo: &hyperast_vcs_git::git::Repo) -> git2::Repository {
    use hyperast_vcs_git::git::FetchRepoError;
    match repo.try_nofetch() {
        Ok(repo) => repo,
        Err(FetchRepoError::NoRepoAndNoFetch) => {
            log::error!(
                "After failing to fetch the repository, attempted to just retrieve the local copy."
            );
            log::error!(
                "But no repository was found at {}.",
                hyperast_vcs_git::git::Repo::LOCAL_REPO_PATH
            );
            panic!(
                "no repository found at {}, either enable fetching or place the corresponding Git database there",
                hyperast_vcs_git::git::Repo::LOCAL_REPO_PATH
            );
        }
        Err(FetchRepoError::Other(err)) => {
            panic!("{}", err);
        }
    }
}
