use axum::{Json, response::IntoResponse};
use enumset::{EnumSet, EnumSetType};
use serde::{Deserialize, Serialize};
use serde_aux::prelude::deserialize_bool_from_anything;
use std::{fmt::Debug, thread::sleep, time::Duration};
use tokio::time::Instant;

use hyperast::PrimInt;
use hyperast::position::position_accessors::{self, WithOffsets, WithPreOrderPath};
use hyperast::position::{compute_position_with_no_spaces, compute_range, resolve_range};
use hyperast::store::defaults::NodeIdentifier;
use hyperast_vcs_git::git::Repo;
use hyperast_vcs_git::preprocessed::child_at_path_tracked;
use hyperast_vcs_git::processing::ConfiguredRepoTrait;
use hyperast_vcs_git::{TStore, multi_preprocessed};

use crate::SharedState;
use crate::changes::{self, DstChanges, SrcChanges};

mod compute;
use compute::do_tracking;
mod more;
use more::{TargetCodeElement, shift_piece};

#[cfg(feature = "experimental")]
mod my_dash;

#[derive(Deserialize, Clone, Debug)]
pub struct TrackingParam {
    pub user: String,
    pub name: String,
    pub commit: String,
    pub file: String,
}

#[derive(Deserialize, Clone, Debug)]
pub struct TrackingAtPathParam {
    user: String,
    name: String,
    commit: String,
    path: String,
}

#[derive(Deserialize, Clone, Debug)]
pub struct TrackingQuery {
    pub start: Option<usize>,
    pub end: Option<usize>,
    pub before: Option<String>,
    #[serde(flatten)]
    pub flags: Flags,
}

macro_rules! decl_flags { ($($(#[$m:meta])* $f:ident => $e:ident,)*) => {
#[derive(serde::Deserialize, serde::Serialize, Default, Clone, Debug)]
#[serde(default)]
pub struct Flags {$(
    #[serde(deserialize_with = "deserialize_bool_from_anything")]
    $(#[$m])*
    pub $f: bool,
)*}

impl Flags {
    fn some(&self) -> bool {$(
        self.$f
    )||*}
}

#[derive(EnumSetType, Debug)]
pub enum FlagsE {$(
    $(#[$m])*
    $e,
)*}

impl From<&Flags> for EnumSet<FlagsE> {
    fn from(val: &Flags) -> Self {
        let mut r = EnumSet::new();
        $(if val.$f {
            r.insert(FlagsE::$e);
        })*;
        r
    }
}

}}

decl_flags!(
    /// stops on label update
    upd => Upd,
    /// stops when any descendant changes (.i.e, transitively)
    child => Child,
    /// stops when any ancestor changes (.i.e, transitively)
    parent => Parent,
    /// stops when children change,
    /// only consider direct children kind and label
    exact_child => ExactChild,
    /// stops when parent changes,
    /// only consider direct parent kind and label
    exact_parent => ExactParent,
    /// stops when any descendant changes significantly,
    ///
    sim_child => SimChild,
    /// stops when any ancestor changes significantly,
    ///
    sim_parent => SimParent,
    // TODO add beavior_child/parent which allow to ignore non-behavioral changes like local renames
    meth => Meth,
    typ => Typ,
    top => Top,
    file => File,
    pack => Pack,
    dependency => Dependency,
    dependent => Dependent,
    references => References,
    declaration => Declaration,
);

#[derive(Deserialize, Serialize)]
pub struct TrackingResult<IdN, Idx> {
    pub compute_time: f64,
    commits_processed: usize,
    #[serde(bound(serialize = "IdN: Clone + Into<self::IdN>, Idx: Serialize"))]
    src: PieceOfCode<IdN, Idx>,
    #[serde(bound(serialize = "IdN: Clone + Into<self::IdN>, Idx: Serialize"))]
    intermediary: Option<PieceOfCode<IdN, Idx>>,
    #[serde(bound(serialize = "IdN: Clone + Into<self::IdN>, Idx: Serialize"))]
    fallback: Option<PieceOfCode<IdN, Idx>>,
    #[serde(bound(serialize = "IdN: Clone + Into<self::IdN>, Idx: Serialize"))]
    matched: Vec<PieceOfCode<IdN, Idx>>,
}

// set the type of offset used to index in children list
type Idx = u16;
type IdN = NodeIdentifier;

impl IntoResponse for TrackingResult<IdN, Idx> {
    fn into_response(self) -> axum::response::Response {
        let mut resp = serde_json::to_string(&self).unwrap().into_response();
        let headers = resp.headers_mut();
        headers.insert(
            "Server-Timing",
            format!("track;desc=\"Compute Time\";dur={}", self.compute_time)
                .parse()
                .unwrap(),
        );
        resp
    }
}

impl<IdN, Idx> TrackingResult<IdN, Idx> {
    pub(crate) fn with_changes(
        self,
        (src_changes, dst_changes): (SrcChanges, DstChanges),
    ) -> TrackingResultWithChanges<IdN, Idx> {
        TrackingResultWithChanges {
            track: self,
            src_changes,
            dst_changes,
        }
    }
}

#[derive(Deserialize, Serialize)]
pub struct TrackingResultWithChanges<IdN, Idx> {
    #[serde(bound(serialize = "IdN: Clone + Into<self::IdN>, Idx: Serialize"))]
    pub track: TrackingResult<IdN, Idx>,
    src_changes: SrcChanges,
    dst_changes: DstChanges,
}

impl IntoResponse for TrackingResultWithChanges<IdN, Idx> {
    fn into_response(self) -> axum::response::Response {
        let mut resp = serde_json::to_string(&self).unwrap().into_response();
        let headers = resp.headers_mut();
        headers.insert(
            "Server-Timing",
            format!(
                "track;desc=\"Compute Time\";dur={}",
                self.track.compute_time
            )
            .parse()
            .unwrap(),
        );
        resp
    }
}

#[derive(Deserialize, Serialize, Debug)]
pub struct PieceOfCode<IdN = self::IdN, Idx = usize> {
    user: String,
    name: String,
    commit: String,
    path: Vec<Idx>,
    file: String,
    start: usize,
    end: usize,
    #[serde(bound(serialize = "IdN: Clone + Into<self::IdN>"))]
    #[serde(serialize_with = "custom_ser")]
    #[serde(skip_serializing_if = "Vec::is_empty")]
    path_ids: Vec<IdN>, // WARN this is not fetched::NodeIdentifier
}

fn custom_ser<IdN: Clone + Into<self::IdN>, S>(
    x: &Vec<IdN>,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    use serde::ser::SerializeSeq;
    let mut seq = serializer.serialize_seq(Some(x.len()))?;
    for element in x {
        let element: self::IdN = element.clone().into();
        let id: u64 = unsafe { std::mem::transmute(element) };
        seq.serialize_element(&id)?;
    }
    seq.end()
}

const MAX_NODES: usize = 200 * 4_000_000;

#[derive(Deserialize, Serialize)]
pub struct TrackingError {
    pub compute_time: f64,
    commits_processed: usize,
    node_processed: usize,
    pub message: String,
}

impl IntoResponse for TrackingError {
    fn into_response(self) -> axum::response::Response {
        let mut resp = Json(self).into_response();
        *resp.status_mut() = http::StatusCode::FORBIDDEN;
        resp
    }
}

pub fn track_code(
    state: SharedState,
    path: TrackingParam,
    query: TrackingQuery,
) -> Result<TrackingResult<IdN, Idx>, TrackingError> {
    let now = Instant::now();
    let TrackingParam {
        user,
        name,
        commit,
        file,
    } = path;
    let TrackingQuery {
        start,
        end,
        before,
        flags,
    } = query;
    let repo_specifier = hyperast_vcs_git::git::Forge::Github.repo(user, name);
    let repo_handle = (state.repositories.write().unwrap())
        .get_config(repo_specifier)
        .ok_or_else(|| TrackingError {
            compute_time: now.elapsed().as_secs_f64(),
            commits_processed: 0,
            node_processed: 0,
            message: "missing config for repository".to_string(),
        })?;
    let mut repository = repo_handle.fetch();
    log::debug!("done cloning {}", repository.spec);
    let mut commit = commit.clone();
    let mut node_processed = 0;
    let mut commits_processed = 1;
    let mut file = file;
    let mut start = start;
    let mut end = end;
    let mut source = None;
    while node_processed < MAX_NODES {
        commits_processed += 1;
        let commits = (state.repositories.write().unwrap())
            .pre_process_with_limit(&mut repository, "", &commit, 2)
            .map_err(|e| TrackingError {
                compute_time: now.elapsed().as_secs_f64(),
                commits_processed: 0,
                node_processed: 0,
                message: e.to_string(),
            })?;
        log::debug!("done construction of {commits:?} in {}", repository.spec);
        let src_oid = commits[0];
        let dst_oid = commits[1];
        match track_aux(
            state.clone(),
            &repository,
            src_oid,
            dst_oid,
            &file,
            start,
            end,
            &flags,
        )
        .into()
        {
            MappingResult::Direct { src, matches } => {
                dbg!(&src_oid, &dst_oid, &commit);
                let (src, intermediary) = shift_piece(&commit, source, src, repository.spec);
                log::debug!("tracked {commits:?}");
                return Ok(TrackingResult {
                    compute_time: now.elapsed().as_secs_f64(),
                    commits_processed,
                    src,
                    intermediary,
                    fallback: None,
                    matched: matches,
                });
            }
            MappingResult::Missing { src, fallback } => {
                let (src, intermediary) = shift_piece(&commit, source, src, repository.spec);
                log::debug!("tracking miss {commits:?}");
                return Ok(TrackingResult {
                    compute_time: now.elapsed().as_secs_f64(),
                    commits_processed,
                    src,
                    intermediary,
                    fallback: Some(fallback),
                    matched: vec![],
                });
            }
            MappingResult::Error(err) => Err(TrackingError {
                compute_time: now.elapsed().as_secs_f64(),
                commits_processed,
                node_processed,
                message: err,
            })?,
            MappingResult::Skipped { nodes, src, next } => {
                node_processed += nodes;
                dbg!(src_oid, dst_oid);
                if before.as_ref() == Some(&dst_oid.to_string()) {
                    let (src, intermediary) = shift_piece(&commit, source, src, repository.spec);
                    log::debug!("tracking skip {commits:?}");
                    return Ok(TrackingResult {
                        compute_time: now.elapsed().as_secs_f64(),
                        commits_processed,
                        src,
                        intermediary,
                        fallback: None,
                        matched: next,
                    });
                }
                if source.is_none() {
                    source = Some(src.globalize(repository.spec.clone(), commit));
                }
                commit = dst_oid.to_string();
                if next.len() > 1 {
                    log::error!("multiple matches")
                }
                if next.is_empty() {
                    unreachable!()
                } else {
                    let next = &next[0];
                    dbg!(next);
                    file = next.file.to_string();
                    start = Some(next.start);
                    end = Some(next.end);
                }
            }
        }
    }
    Err(TrackingError {
        compute_time: now.elapsed().as_secs_f64(),
        commits_processed,
        node_processed,
        message: format!("reached max number of diffed nodes: (ie. {})", MAX_NODES),
    })
}

pub(crate) fn track_code_at_path(
    state: SharedState,
    path: TrackingAtPathParam,
    query: TrackingQuery,
) -> Result<TrackingResult<IdN, Idx>, TrackingError> {
    let now = Instant::now();
    let TrackingQuery {
        start,
        end,
        before,
        flags,
    } = query;
    let TrackingAtPathParam {
        user,
        name,
        commit,
        path,
    } = path;
    let repo_specifier = hyperast_vcs_git::git::Forge::Github.repo(user, name);
    let repository = (state.repositories.write().unwrap())
        .get_config(repo_specifier)
        .ok_or_else(|| TrackingError {
            compute_time: now.elapsed().as_secs_f64(),
            commits_processed: 0,
            node_processed: 0,
            message: "missing config for repository".to_string(),
        })?;
    let mut repository = repository.fetch();
    log::debug!("done cloning {}", repository.spec);
    let mut commit = commit.clone();
    let mut node_processed = 0;
    let mut commits_processed = 1;
    let mut path: Vec<Idx> = path.split("/").filter_map(|x| x.parse().ok()).collect();
    let mut source = None;
    while node_processed < MAX_NODES {
        commits_processed += 1;
        let commits = state
            .repositories
            .write()
            .unwrap()
            .pre_process_with_limit(&mut repository, "", &commit, 2)
            .map_err(|e| TrackingError {
                compute_time: now.elapsed().as_secs_f64(),
                commits_processed: 0,
                node_processed: 0,
                message: e.to_string(),
            })?;
        log::debug!("done construction of {commits:?} in {}", repository.spec);
        let src_oid = commits[0];
        let dst_oid = if let Some(before) = &before {
            let commits = state
                .repositories
                .write()
                .unwrap()
                .pre_process_with_limit(&mut repository, "", before, 2)
                .map_err(|e| TrackingError {
                    compute_time: now.elapsed().as_secs_f64(),
                    commits_processed: 0,
                    node_processed: 0,
                    message: e.to_string(),
                })?;
            commits[0]
        } else {
            commits[1]
        };
        match track_aux2(state.clone(), &repository, src_oid, dst_oid, &path, &flags).into() {
            MappingResult::Direct { src, matches } => {
                dbg!(&src_oid, &dst_oid, &commit);
                let (src, intermediary) = shift_piece(&commit, source, src, repository.spec);
                return Ok(TrackingResult {
                    compute_time: now.elapsed().as_secs_f64(),
                    commits_processed,
                    src,
                    intermediary,
                    fallback: None,
                    matched: matches,
                });
            }
            MappingResult::Missing { src, fallback } => {
                let (src, intermediary) = shift_piece(&commit, source, src, repository.spec);
                return Ok(TrackingResult {
                    compute_time: now.elapsed().as_secs_f64(),
                    commits_processed,
                    src,
                    intermediary,
                    fallback: Some(fallback),
                    matched: vec![],
                });
            }
            MappingResult::Error(err) => Err(TrackingError {
                compute_time: now.elapsed().as_secs_f64(),
                commits_processed,
                node_processed,
                message: err,
            })?,
            MappingResult::Skipped { nodes, src, next } => {
                // TODO handle cases where there is no more commits
                if before.is_some() {
                    let (src, intermediary) = shift_piece(&commit, source, src, repository.spec);
                    return Ok(TrackingResult {
                        compute_time: now.elapsed().as_secs_f64(),
                        commits_processed,
                        src,
                        intermediary,
                        fallback: None,
                        matched: next,
                    });
                }
                node_processed += nodes;
                dbg!(src_oid, dst_oid);
                if source.is_none() {
                    source = Some(src.globalize(repository.spec.clone(), commit));
                }
                // commit = dst_oid.to_string();

                if next.len() > 1 {
                    log::error!("multiple matches")
                }
                if next.is_empty() {
                    unreachable!()
                } else {
                    let next = &next[0];
                    dbg!(next);
                    path = next.path.clone();
                    commit = next.commit.clone();
                }
            }
        }
    }
    Err(TrackingError {
        compute_time: now.elapsed().as_secs_f64(),
        commits_processed,
        node_processed,
        message: format!("reached max number of diffed nodes: (ie. {})", MAX_NODES),
    })
}

/// track in past for now
pub(crate) fn track_code_at_path_with_changes(
    state: SharedState,
    path: TrackingAtPathParam,
    query: TrackingQuery,
) -> Result<TrackingResultWithChanges<IdN, Idx>, TrackingError> {
    let now = Instant::now();
    let TrackingQuery {
        start: _,
        end: _,
        before,
        flags,
    } = query;
    let TrackingAtPathParam {
        user,
        name,
        commit,
        path,
    } = path;
    let repo_spec = hyperast_vcs_git::git::Forge::Github.repo(user, name);
    let repo_handle = (state.repositories.write().unwrap())
        .get_config(repo_spec)
        .ok_or_else(|| TrackingError {
            compute_time: now.elapsed().as_secs_f64(),
            commits_processed: 0,
            node_processed: 0,
            message: "missing config for repository".to_string(),
        })?;
    let mut repository = repo_handle.fetch();
    log::debug!("done cloning {}", repository.spec);
    let mut ori_oid = None;
    let mut commit = commit.clone();
    let mut node_processed = 0;
    let mut commits_processed = 1;
    let mut path: Vec<_> = path.split("/").filter_map(|x| x.parse().ok()).collect();
    let mut source = None;
    while node_processed < MAX_NODES {
        commits_processed += 1;
        let commits = (state.repositories.write().unwrap())
            .pre_process_with_limit(&mut repository, "", &commit, 4)
            .map_err(|e| TrackingError {
                compute_time: now.elapsed().as_secs_f64(),
                commits_processed: 0,
                node_processed: 0,
                message: e.to_string(),
            })?;
        log::warn!(
            "done construction of {commits:?} in {}",
            repository.spec.user()
        );
        let src_oid = commits[0];
        if ori_oid.is_none() {
            ori_oid = Some(src_oid);
        }
        let Some(&dst_oid) = commits.get(1) else {
            return Err(TrackingError {
                compute_time: now.elapsed().as_secs_f64(),
                commits_processed,
                node_processed,
                message: "this commit has no parent".into(),
            });
        };
        match track_aux2(state.clone(), &repository, src_oid, dst_oid, &path, &flags).into() {
            MappingResult::Direct { src, matches } => {
                dbg!(&src_oid, &dst_oid, &commit, &ori_oid);
                let changes = changes::added_deleted(state, &repository, dst_oid, ori_oid.unwrap())
                    .map_err(|err| TrackingError {
                        compute_time: now.elapsed().as_secs_f64(),
                        commits_processed,
                        node_processed,
                        message: err,
                    })?;
                let (src, intermediary) = shift_piece(&commit, source, src, repository.spec);
                let tracking_result = TrackingResult {
                    compute_time: now.elapsed().as_secs_f64(),
                    commits_processed,
                    src,
                    intermediary,
                    fallback: None,
                    matched: matches,
                };
                return Ok(tracking_result.with_changes(changes));
            }
            MappingResult::Missing { src, fallback } => {
                dbg!();
                let changes = changes::added_deleted(state, &repository, dst_oid, ori_oid.unwrap())
                    .map_err(|err| TrackingError {
                        compute_time: now.elapsed().as_secs_f64(),
                        commits_processed,
                        node_processed,
                        message: err,
                    })?;
                let (src, intermediary) = shift_piece(&commit, source, src, repository.spec);
                let tracking_result = TrackingResult {
                    compute_time: now.elapsed().as_secs_f64(),
                    commits_processed,
                    src,
                    intermediary,
                    fallback: Some(fallback),
                    matched: vec![],
                };
                return Ok(tracking_result.with_changes(changes));
            }
            MappingResult::Error(err) => Err(TrackingError {
                compute_time: now.elapsed().as_secs_f64(),
                commits_processed,
                node_processed,
                message: err,
            })?,
            MappingResult::Skipped { nodes, src, next } => {
                dbg!(nodes);
                node_processed += nodes;
                dbg!(src_oid, dst_oid);
                // TODO fix issue of not stoping when failling to match accurately,
                // most likely related to miss use of fallback value ?
                if commits.len() < 3 || (node_processed >= MAX_NODES) {
                    // no commit remaining (first + second < 3)
                    // NOTE there is no parent commit to dst_commit, thus we should stop now
                    let changes =
                        changes::added_deleted(state, &repository, dst_oid, ori_oid.unwrap())
                            .map_err(|err| TrackingError {
                                compute_time: now.elapsed().as_secs_f64(),
                                commits_processed,
                                node_processed,
                                message: err,
                            })?;
                    let (src, intermediary) = shift_piece(&commit, source, src, repository.spec);
                    let tracking_result = TrackingResult {
                        compute_time: now.elapsed().as_secs_f64(),
                        commits_processed,
                        src,
                        intermediary,
                        fallback: None,
                        matched: next,
                    };
                    return Ok(tracking_result.with_changes(changes));
                }
                if source.is_none() {
                    source = Some(src.globalize(repository.spec.clone(), commit));
                }
                if next.len() > 1 {
                    log::error!("multiple matches")
                }
                if next.is_empty() {
                    unreachable!()
                } else {
                    let next = &next[0]; // TODO stop on branching ?
                    path = next.path.clone();
                    commit = next.commit.clone();
                }
            }
        }
    }
    // TODO should not look as an irrecoverable fail...
    Err(TrackingError {
        compute_time: now.elapsed().as_secs_f64(),
        commits_processed,
        node_processed,
        message: format!("reached max number of diffed nodes: (ie. {})", MAX_NODES),
    })
}

enum MappingResult<IdN, Idx, T = PieceOfCode<IdN, Idx>> {
    Direct {
        src: LocalPieceOfCode<IdN, Idx>,
        matches: Vec<T>,
    },
    Missing {
        src: LocalPieceOfCode<IdN, Idx>,
        fallback: T,
    },
    Error(String),
    Skipped {
        nodes: usize,
        src: LocalPieceOfCode<IdN, Idx>,
        next: Vec<T>,
    },
}

impl<IdN, Idx, T> Into<Result<MappingResult<IdN, Idx, T>, String>> for MappingResult<IdN, Idx, T> {
    fn into(self) -> Result<MappingResult<IdN, Idx, T>, String> {
        match self {
            MappingResult::Error(e) => Err(e),
            x => Ok(x),
        }
    }
}

impl<IdN, Idx, T> From<Result<MappingResult<IdN, Idx, T>, String>> for MappingResult<IdN, Idx, T> {
    fn from(value: Result<MappingResult<IdN, Idx, T>, String>) -> Self {
        match value {
            Ok(x) => x,
            Err(e) => MappingResult::Error(e),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
struct LocalPieceOfCode<IdN, Idx> {
    file: String,
    start: usize,
    end: usize,
    path: Vec<Idx>,
    path_ids: Vec<IdN>,
}

impl<'a, S, IdN: Clone, Idx: Clone> From<(&S, &'a LocalPieceOfCode<IdN, Idx>)>
    for LocalPieceOfCode<IdN, Idx>
{
    fn from((_, p): (&S, &'a LocalPieceOfCode<IdN, Idx>)) -> Self {
        p.clone()
    }
}

impl<IdN, Idx> LocalPieceOfCode<IdN, Idx> {
    pub(crate) fn from_position(
        pos: &hyperast::position::Position,
        path: Vec<Idx>,
        path_ids: Vec<IdN>,
    ) -> Self {
        let range = pos.range();
        let file = pos.file();
        Self::from_file_and_range(file, range, path, path_ids)
    }
    pub(crate) fn from_file_and_range(
        file: &std::path::Path,
        range: std::ops::Range<usize>,
        path: Vec<Idx>,
        path_ids: Vec<IdN>,
    ) -> Self {
        let std::ops::Range { start, end } = range;
        let file = file.to_str().unwrap().to_string();
        Self {
            file,
            start,
            end,
            path,
            path_ids,
        }
    }
    pub(crate) fn from_pos<P>(pos: &P) -> Self
    where
        P: WithOffsets<Idx = Idx>
            + WithPreOrderPath<IdN>
            + hyperast::position::position_accessors::FileAndOffsetPostionT<IdN, IdO = usize>,
    {
        let mut path = vec![];
        let mut path_ids = vec![];
        for (o, i) in pos.iter_offsets_and_nodes() {
            path.push(o);
            path_ids.push(i);
        }
        Self::from_file_and_range(&pos.file(), pos.start()..pos.end(), path, path_ids)
    }
    pub(crate) fn globalize(self, spec: Repo, commit: impl ToString) -> PieceOfCode<IdN, Idx> {
        let commit = commit.to_string();
        PieceOfCode {
            user: spec.user().to_string(),
            name: spec.name().to_string(),
            commit,
            path: self.path,
            path_ids: self.path_ids,
            file: self.file,
            start: self.start,
            end: self.end,
        }
    }
    fn map_path<Idx2, F: Fn(Idx) -> Idx2>(self, f: F) -> LocalPieceOfCode<IdN, Idx2> {
        let path = self.path.into_iter().map(f).collect();
        LocalPieceOfCode {
            path,
            path_ids: self.path_ids,
            file: self.file,
            start: self.start,
            end: self.end,
        }
    }
}

fn track_aux(
    state: std::sync::Arc<crate::AppState>,
    repo_handle: &impl ConfiguredRepoTrait<
        Config = hyperast_vcs_git::processing::ParametrizedCommitProcessorHandle,
    >,
    src_oid: hyperast_vcs_git::git::Oid,
    dst_oid: hyperast_vcs_git::git::Oid,
    file: &String,
    start: Option<usize>,
    end: Option<usize>,
    flags: &Flags,
) -> Result<MappingResult<IdN, Idx>, String> {
    dbg!(src_oid, dst_oid);
    let repositories = state.repositories.read().unwrap();
    let commit_src = repositories
        .get_commit(repo_handle.config(), &src_oid)
        .ok_or_else(|| format!("{src_oid} is missing"))?;
    let src_tr = commit_src.ast_root;
    let commit_dst = repositories
        .get_commit(repo_handle.config(), &dst_oid)
        .ok_or_else(|| format!("{dst_oid} is missing"))?;
    let dst_tr = commit_dst.ast_root;
    let stores = &repositories.processor.main_stores;

    // let size = node_store.resolve(src_tr).size();
    log::debug!("tracking {file}");
    let file_node =
        child_at_path_tracked(&repositories.processor.main_stores, src_tr, file.split("/"));

    let Some((file_node, offsets_to_file)) = file_node else {
        return Err("not found".into());
    };

    dbg!(&offsets_to_file);
    let mut path_to_target = vec![];
    let (node, offsets_in_file) = resolve_range(file_node, start.unwrap_or(0), end, stores);
    path_to_target.extend(offsets_to_file.iter().map(|x| *x as Idx));
    dbg!(&node);
    dbg!(&offsets_in_file);
    path_to_target.extend(offsets_in_file.iter().map(|x| *x as Idx));

    let computed_range = compute_range(file_node, &mut offsets_in_file.into_iter(), stores);
    dbg!(computed_range.0, computed_range.1);
    dbg!(&computed_range.2);
    let (_, _, no_spaces_path_to_target) =
        compute_position_with_no_spaces(src_tr, &mut path_to_target.iter().copied(), stores);
    let no_spaces_path_to_target = no_spaces_path_to_target;
    let dst_oid = dst_oid; // WARN not sure what I was doing there commit_dst.clone();
    let target = TargetCodeElement::<IdN, Idx> {
        start: computed_range.0,
        end: computed_range.1,
        path: path_to_target.clone(),
        path_no_spaces: no_spaces_path_to_target,
        node: computed_range.2,
        root: src_tr,
    };
    let postprocess_matching = |p: LocalPieceOfCode<IdN, Idx>| {
        p.globalize(repo_handle.spec().clone(), dst_oid.to_string())
    };
    do_tracking(
        &repositories,
        &state.partial_decomps,
        &state.mappings_alone,
        flags,
        &target,
        dst_tr,
        &postprocess_matching,
    )
    .into()
}

fn track_aux2(
    state: std::sync::Arc<crate::AppState>,
    repo_handle: &impl ConfiguredRepoTrait<
        Config = hyperast_vcs_git::processing::ParametrizedCommitProcessorHandle,
    >,
    src_oid: hyperast_vcs_git::git::Oid,
    dst_oid: hyperast_vcs_git::git::Oid,
    path: &[Idx],
    flags: &Flags,
) -> Result<MappingResult<IdN, Idx>, String> {
    let repositories = state.repositories.read().unwrap();
    let commit_src = repositories
        .get_commit(repo_handle.config(), &src_oid)
        .ok_or_else(|| format!("{src_oid} is missing"))?;
    let src_tr = commit_src.ast_root;
    let commit_dst = repositories
        .get_commit(repo_handle.config(), &dst_oid)
        .ok_or_else(|| format!("{dst_oid} is missing"))?;
    let dst_tr = commit_dst.ast_root;
    let stores = &repositories.processor.main_stores;

    let path_to_target: Vec<_> = path.to_vec();
    dbg!(&path_to_target);
    let (pos, target_node, no_spaces_path_to_target) =
        compute_position_with_no_spaces(src_tr, &mut path_to_target.iter().copied(), stores);
    let range = pos.range();
    let target = TargetCodeElement::<IdN, Idx> {
        start: range.start,
        end: range.end,
        path: path_to_target.clone(),
        path_no_spaces: no_spaces_path_to_target,
        node: target_node,
        root: src_tr,
    };
    let postprocess_matching = |p: LocalPieceOfCode<IdN, Idx>| {
        p.globalize(repo_handle.spec().clone(), dst_oid.to_string())
    };
    do_tracking(
        &repositories,
        &state.partial_decomps,
        &state.mappings_alone,
        flags,
        &target,
        dst_tr,
        &postprocess_matching,
    )
    .into()
}
