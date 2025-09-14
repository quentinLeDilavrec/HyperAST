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

mod data;
pub use data::DATASET;
pub mod queries;

use std::{env, fs, io, path, time};

pub fn tempfile() -> io::Result<(path::PathBuf, fs::File)> {
    let mut path = env::temp_dir();
    let file_name = time::SystemTime::UNIX_EPOCH;
    path.push(file_name.elapsed().unwrap().as_nanos().to_string());
    let file = fs::File::create(&path)?;
    Ok((path, file))
}
