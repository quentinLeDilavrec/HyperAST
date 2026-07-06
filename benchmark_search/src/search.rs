use std::fs::read_to_string;

use crate::{Timeout, read_multilines};

#[derive(clap::clap_derive::Parser, Debug)]
pub struct Cli {
    pub user: String,
    pub name: String,
    pub commit: String,
    pub depth: usize,
    #[clap(long)]
    pub fetch: bool,
    #[clap(long)]
    pub language: Option<String>,
    #[clap(long, value_parser = crate::parse_timeout, default_value_t = Timeout::MAX)]
    pub timeout: Timeout,
    #[clap(subcommand)]
    pub bench: Option<Bench>,
    #[clap(long)]
    pub input: Option<std::path::PathBuf>,
}

#[derive(clap::clap_derive::Subcommand, Clone, Debug, Eq, PartialEq, Hash)]
// #[derive(clap::clap_derive::ValueEnum, Clone)]
// #[clap(rename_all = "SCREAMING_SNAKE_CASE")]
#[allow(non_camel_case_types)]
pub enum Bench {
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

#[derive(Default)]
pub struct ReadSearches {
    buffer: String,
    finished: bool,
    file: Option<(usize, Vec<String>)>,
}

impl ReadSearches {
    pub fn new(file: std::path::PathBuf) -> Self {
        let mut b = false;
        let file = read_to_string(file)
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
