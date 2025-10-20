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

use std::{
    env,
    fmt::Display,
    fs, io, path,
    time::{self, Duration},
};

pub fn tempfile() -> io::Result<(path::PathBuf, fs::File)> {
    let mut path = env::temp_dir();
    let file_name = time::SystemTime::UNIX_EPOCH;
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

#[derive(Debug, Clone, Copy)]
pub struct Config {
    pub config: hyperast_vcs_git::processing::RepoConfig,
    pub first_chunk: usize,
    pub chunk_interval: usize,
    pub depth: usize,
}

pub enum LogEntry<R> {
    PrepareRepository,
    PrepareCommits(usize),
    PrepareFiles(usize),
    CompileQuery,
    ExecuteQueryOnFile(R),
    ExecuteQueryOnCommit(R, usize),
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
    start_time: time::Instant,
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
            start_time: time::Instant::now(),
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

#[derive(Clone)]
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
            "{:>20};{:>12};{:>9};{:>14};{:>6};{:>6}",
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
                "{}",
                TimedLog::new(&mut first_commit_prep, &mut prev, task, duration)
            )?;
        }
        Ok(())
    }
}

struct TimedLog<'a, R> {
    first_commit_prep: &'a Duration,
    delta: Duration,
    since: Duration,
    task: &'a LogEntry<R>,
    duration: &'a Duration,
}

impl<'a, R: Display> TimedLog<'a, R> {
    fn new(
        first_commit_prep: &'a mut Duration,
        prev: &'a mut Duration,
        task: &'a LogEntry<R>,
        duration: &'a Duration,
    ) -> Self {
        let delta = *duration - *prev;
        let since = *duration - *first_commit_prep;
        match task {
            LogEntry::PrepareCommits(_) => {
                if *first_commit_prep == Duration::ZERO {
                    *prev = *duration;
                    *first_commit_prep = *duration;
                }
            }
            LogEntry::ExecuteQueryOnCommit(_, _) => {
                *prev = *duration;
            }
            _ => (),
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

impl<'a, R: Display> std::fmt::Display for TimedLog<'a, R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let duration = self.duration;
        let delta = &self.delta;
        let since = self.since;
        match self.task {
            LogEntry::PrepareRepository => write!(
                f,
                "{:>20};{:>12};{:>9};{:>14};{:>6}",
                "prep repo",
                duration.as_micros(),
                delta.as_micros(),
                since.as_micros(),
                ""
            ),
            LogEntry::PrepareCommits(x) => {
                let since = *duration - *self.first_commit_prep;
                write!(
                    f,
                    "{:>20};{:>12};{:>9};{:>14};{:>6}",
                    if *x > 1 {
                        format!("prep {x} commits")
                    } else {
                        format!("prep {x} commit")
                    },
                    duration.as_micros(),
                    delta.as_micros(),
                    since.as_micros(),
                    ""
                )
            }
            LogEntry::PrepareFiles(x) => write!(
                f,
                "{:>20};{:>12};{:>9};{:>14};{:>6}",
                if *x > 1 {
                    format!("prep {x} files")
                } else {
                    format!("prep {x} file")
                },
                duration.as_micros(),
                delta.as_micros(),
                since.as_micros(),
                ""
            ),
            LogEntry::CompileQuery => write!(
                f,
                "{:>20};{:>12};{:>9};{:>14};{:>6}",
                "compile query",
                duration.as_micros(),
                delta.as_micros(),
                since.as_micros(),
                ""
            ),
            LogEntry::ExecuteQueryOnFile(v) => write!(
                f,
                "{:>20};{:>12};{:>9};{:>14};{:>6}",
                "exec on file",
                duration.as_micros(),
                delta.as_micros(),
                since.as_micros(),
                v
            ),
            LogEntry::ExecuteQueryOnCommit(v, size) if *size == 0 => {
                write!(
                    f,
                    "{:>20};{:>12};{:>9};{:>14};{:>6};",
                    "exec on commit",
                    duration.as_micros(),
                    delta.as_micros(),
                    since.as_micros(),
                    v,
                )
            }
            LogEntry::ExecuteQueryOnCommit(v, size) => {
                write!(
                    f,
                    "{:>20};{:>12};{:>9};{:>14};{:>6};{:>6}",
                    "exec on commit",
                    duration.as_micros(),
                    delta.as_micros(),
                    since.as_micros(),
                    v,
                    size
                )
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
    start_time: time::Instant,
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
            "{:>20};{:>12};{:>9};{:>14};{:>6};{:>6}",
            "task",
            "cumulative(us)",
            "delta(us)",
            "since_first_commit(us)",
            CsvHeader::<R>::default(),
            "size"
        )
        .unwrap();
        NonBlockingResLogger {
            start_time: time::Instant::now(),
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

        use std::io::Write;
        writeln!(
            &mut self.writer,
            "{}",
            TimedLog::new(
                &mut self.first_commit_prep,
                &mut self.prev,
                &entry,
                &duration
            )
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

#[derive(Default)]
pub struct ReadSearches {
    buffer: String,
    finished: bool,
    file: Option<(usize, Vec<String>)>,
}

impl ReadSearches {
    pub fn new(file: std::path::PathBuf) -> Self {
        let mut b = false;
        let file = std::fs::read_to_string(file)
            .expect("Failed to read provided pattern file")
            .lines()
            .fold(vec![String::new()], |mut acc, line| {
                if line.trim().is_empty() {
                    if b {
                        b = false;
                        acc.push(String::new());
                    } else {
                        acc.last_mut().unwrap().push_str(line);
                        b = true;
                    }
                } else {
                    b = false;
                    acc.last_mut().unwrap().push_str(line);
                }
                acc
            });
        dbg!(file.len());
        dbg!(&file);
        Self {
            buffer: String::new(),
            finished: false,
            file: Some((0, file)),
        }
    }
}

impl Iterator for ReadSearches {
    type Item = String;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some((index, patterns)) = &mut self.file {
            if *index < patterns.len() {
                let pattern = patterns[*index].clone();
                *index += 1;
                return Some(pattern);
            }
            return None;
        }
        while !self.finished {
            let s = read_multilines(&mut self.buffer);
            if s.is_empty() {
                self.finished = true;
            } else if s != "\n" {
                continue;
            }
            return Some(self.buffer.drain(..).collect());
        }
        None
    }
}

pub fn read_searches() {
    let mut buffer = String::new();
    loop {
        let s = read_multilines(&mut buffer);

        let new_block = s == "\n";
        let empty = s.is_empty();

        if new_block || empty {
            eprintln!("----------");
            eprint!("{buffer}");
        }

        if new_block {
            buffer = String::new();
            continue;
        }
        if empty {
            break;
        }
    }
}
