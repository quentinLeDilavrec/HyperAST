use axum::{Json, response::IntoResponse};
use serde_aux::prelude::deserialize_bool_from_anything;
use std::{fmt::Debug, thread::sleep, time::Duration};
use tokio::time::Instant;

use hyperast::position::position_accessors;
use hyperast::position::{compute_position_with_no_spaces, compute_range, resolve_range};
use hyperast::store::SimpleStores;
use hyperast::store::defaults::NodeIdentifier;
use hyperast_vcs_git::git::{Oid, Repo};
use hyperast_vcs_git::preprocessed::child_at_path_tracked;
use hyperast_vcs_git::processing::ConfiguredRepo2 as ConfiguredRepo;
use hyperast_vcs_git::processing::ParametrizedCommitProcessorHandle;
use hyperast_vcs_git::{TStore, multi_preprocessed};

use crate::changes::{DstChanges, SrcChanges, added_deleted};
use crate::{SharedState, track};

mod compute;
use compute::do_tracking;
mod more;
use more::{TargetCodeElement, repo_config_error, shift_piece};

#[cfg(feature = "experimental")]
mod my_dash;

#[derive(serde::Deserialize, Clone, Debug)]
pub struct TrackingParam {
    pub user: String,
    pub name: String,
    #[serde(deserialize_with = "string_to_oid")]
    #[serde(serialize_with = "oid_to_string")]
    pub commit: Oid,
    pub file: String,
}

impl TrackingParam {
    pub fn repo(&self) -> Repo {
        hyperast_vcs_git::git::Forge::Github.repo(&self.user, &self.name)
    }
}

#[derive(serde::Deserialize, Clone, Debug)]
pub struct TrackingAtPathParam {
    user: String,
    name: String,
    #[serde(deserialize_with = "string_to_oid")]
    #[serde(serialize_with = "oid_to_string")]
    commit: Oid,
    path: String,
}

impl TrackingAtPathParam {
    pub fn repo(&self) -> Repo {
        hyperast_vcs_git::git::Forge::Github.repo(&self.user, &self.name)
    }
    pub fn path<T: std::str::FromStr>(&self) -> Vec<T> {
        (self.path.split("/"))
            .filter_map(|x| T::from_str(x).ok())
            .collect()
    }
}

#[derive(serde::Deserialize, Clone, Debug)]
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

#[derive(enumset::EnumSetType, Debug)]
pub enum FlagsE {$(
    $(#[$m])*
    $e,
)*}

impl From<&Flags> for enumset::EnumSet<FlagsE> {
    fn from(val: &Flags) -> Self {
        let mut r = enumset::EnumSet::new();
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

#[derive(serde::Deserialize, serde::Serialize)]
pub struct TrackingResult<IdN, Idx> {
    pub compute_time: f64,
    commits_processed: usize,
    #[serde(bound(serialize = "IdN: Clone + Into<self::IdN>, Idx: serde::Serialize"))]
    src: PieceOfCode<IdN, Idx>,
    #[serde(bound(serialize = "IdN: Clone + Into<self::IdN>, Idx: serde::Serialize"))]
    intermediary: Option<PieceOfCode<IdN, Idx>>,
    #[serde(bound(serialize = "IdN: Clone + Into<self::IdN>, Idx: serde::Serialize"))]
    fallback: Option<PieceOfCode<IdN, Idx>>,
    #[serde(bound(serialize = "IdN: Clone + Into<self::IdN>, Idx: serde::Serialize"))]
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

#[derive(serde::Deserialize, serde::Serialize)]
pub struct TrackingResultWithChanges<IdN, Idx> {
    #[serde(bound(serialize = "IdN: Clone + Into<self::IdN>, Idx: serde::Serialize"))]
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

#[derive(serde::Deserialize, serde::Serialize, Debug)]
pub struct PieceOfCode<IdN = self::IdN, Idx = usize> {
    user: String,
    name: String,
    #[serde(deserialize_with = "string_to_oid")]
    #[serde(serialize_with = "oid_to_string")]
    commit: Oid,
    path: Vec<Idx>,
    file: String,
    start: usize,
    end: usize,
    #[serde(bound(serialize = "IdN: Clone + Into<self::IdN>"))]
    #[serde(serialize_with = "custom_ser")]
    #[serde(skip_serializing_if = "Vec::is_empty")]
    path_ids: Vec<IdN>, // WARN this is not fetched::NodeIdentifier
}

fn oid_to_string<S: serde::Serializer>(x: &Oid, serializer: S) -> Result<S::Ok, S::Error> {
    serializer.collect_str(x)
}
fn string_to_oid<'de, D: serde::Deserializer<'de>>(deserializer: D) -> Result<Oid, D::Error> {
    let s = <&str as serde::Deserialize>::deserialize(deserializer)?;
    Oid::from_str(s).map_err(|e| serde::de::Error::custom(e))
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
        if id > u32::MAX as u64 {
            log::error!("node ids are too big, it will lead to bugs when used")
        }
        seq.serialize_element(&id)?;
    }
    seq.end()
}

const MAX_NODES: usize = 200 * 4_000_000;

#[derive(serde::Deserialize, serde::Serialize)]
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
    let repo_handle = (state.repositories.write().unwrap())
        .get_config(path.repo())
        .ok_or_else(|| repo_config_error(now))?;
    let mut repository = repo_handle.fetch();
    log::debug!("done cloning {}", repository.spec);

    let mut tracking = TrackingImpl::new(&state, now, query, path);
    while tracking.node_processed < MAX_NODES {
        tracking.commits_processed += 1;
        let commits = (state.repositories.write().unwrap())
            .pre_process_with_limit(&mut repository, "", &tracking.commit.to_string(), 4)
            .map_err(|e| tracking.error(e))?;
        log::warn!(
            "done construction of {commits:?} in {}",
            repository.spec.user()
        );
        let src_oid = commits[0];
        let Some(&dst_oid) = commits.get(1) else {
            return Err(tracking.error("this commit has no parent"));
        };
        let track_res = if tracking.file.is_none() {
            track_aux(&mut tracking, &repository, src_oid, dst_oid)
        } else {
            track_at_path_aux(&mut tracking, &repository, src_oid, dst_oid)
        }
        .into();
        let repo = &repository.spec;
        let r = handle_tracked(&mut tracking, repo, &commits, track_res, dst_oid)?;
        if let Some(res) = r {
            return Ok(res);
        }
    }
    Err(tracking.max_diffed_error())
}

pub(crate) fn track_code_at_path(
    state: SharedState,
    path: TrackingAtPathParam,
    query: TrackingQuery,
) -> Result<TrackingResult<IdN, Idx>, TrackingError> {
    let now = Instant::now();
    let repository = (state.repositories.write().unwrap())
        .get_config(path.repo())
        .ok_or_else(|| repo_config_error(now))?;
    let mut repository = repository.fetch();
    log::debug!("done cloning {}", repository.spec);
    let mut tracking = TrackingImpl::at_path(&state, now, query, path);
    let mut ori_oid = None;
    while tracking.node_processed < MAX_NODES {
        let commit = &tracking.commit;
        tracking.commits_processed += 1;
        let commits = (state.repositories.write().unwrap())
            .pre_process_with_limit(&mut repository, "", &tracking.commit.to_string(), 4)
            .map_err(|e| tracking.error(e))?;
        log::warn!(
            "done construction of {commits:?} in {}",
            repository.spec.user()
        );
        let src_oid = commits[0];
        if ori_oid.is_none() {
            ori_oid = Some(src_oid);
        }
        let Some(&dst_oid) = commits.get(1) else {
            return Err(tracking.error("this commit has no parent"));
        };
        let track_res = track_at_path_aux(&mut tracking, &repository, src_oid, dst_oid).into();
        let repo = &repository.spec;
        if let Some(res) = handle_tracked(&mut tracking, repo, &commits, track_res, dst_oid)? {
            return Ok(res);
        }
    }
    // TODO make sure it does not look as an irrecoverable fail
    Err(tracking.max_diffed_error())
}

/// track in past for now
pub(crate) fn track_code_at_path_with_changes(
    state: SharedState,
    path: TrackingAtPathParam,
    query: TrackingQuery,
) -> Result<TrackingResultWithChanges<IdN, Idx>, TrackingError> {
    let now = Instant::now();
    let repo_handle = (state.repositories.write().unwrap())
        .get_config(path.repo())
        .ok_or_else(|| repo_config_error(now))?;
    let mut repository = repo_handle.fetch();
    log::debug!("done cloning {}", repository.spec);
    let mut tracking = TrackingImpl::at_path(&state, now, query, path);
    let mut ori_oid = None;

    while tracking.node_processed < MAX_NODES {
        let commit = &tracking.commit;
        tracking.commits_processed += 1;
        let commits = (state.repositories.write().unwrap())
            .pre_process_with_limit(&mut repository, "", &tracking.commit.to_string(), 4)
            .map_err(|e| tracking.error(e))?;
        log::warn!(
            "done construction of {commits:?} in {}",
            repository.spec.user()
        );
        let src_oid = commits[0];
        if ori_oid.is_none() {
            ori_oid = Some(src_oid);
        }
        let Some(&dst_oid) = commits.get(1) else {
            return Err(tracking.error("this commit has no parent"));
        };
        dbg!(dst_oid);
        dbg!(src_oid);
        dbg!(ori_oid);
        let track_res = track_at_path_aux(&mut tracking, &repository, src_oid, dst_oid).into();
        let repo = &repository.spec;
        if let Some(res) = handle_tracked(&mut tracking, repo, &commits, track_res, dst_oid)? {
            let changes = added_deleted(state.clone(), &repository, dst_oid, ori_oid.unwrap())
                .map_err(|err| tracking.error(err))?;
            log::info!(
                "deletions {} {} additions {} {}",
                changes.0.commit,
                changes.0.deletions.len(),
                changes.1.commit,
                changes.1.additions.len()
            );
            return Ok(res.with_changes(changes));
        }
    }
    // TODO make sure it does not look as an irrecoverable fail
    Err(tracking.max_diffed_error())
}

struct TrackingImpl {
    state: SharedState,
    now: Instant,
    commit: Oid,
    node_processed: usize,
    commits_processed: usize,
    file: Option<String>,
    path: Option<Vec<Idx>>,
    query: TrackingQuery,
    source: Option<PieceOfCode<IdN, Idx>>,
}

impl TrackingImpl {
    fn new(
        state: &std::sync::Arc<crate::AppState>,
        now: Instant,
        query: TrackingQuery,
        path: TrackingParam,
    ) -> Self {
        TrackingImpl {
            state: state.clone(),
            now,
            commit: path.commit,
            file: Some(path.file),
            node_processed: 0,
            commits_processed: 1,
            path: None,
            query,
            source: None,
        }
    }
    fn at_path(
        state: &std::sync::Arc<crate::AppState>,
        now: Instant,
        query: TrackingQuery,
        path: TrackingAtPathParam,
    ) -> Self {
        let (path, commit) = (path.path(), path.commit);
        TrackingImpl {
            state: state.clone(),
            now,
            commit,
            file: None,
            node_processed: 0,
            commits_processed: 1,
            path: Some(path),
            query,
            source: None,
        }
    }

    fn max_diffed_error(self) -> TrackingError {
        self.error(format!(
            "reached max number of diffed nodes: (ie. {})",
            MAX_NODES
        ))
    }

    fn error(&self, message: impl ToString) -> TrackingError {
        TrackingError {
            compute_time: self.now.elapsed().as_secs_f64(),
            commits_processed: self.commits_processed,
            node_processed: self.node_processed,
            message: message.to_string(),
        }
    }
}

fn handle_tracked(
    tracking: &mut TrackingImpl,
    repo: &Repo,
    commits: &[Oid],
    track_res: MappingResult<IdN, Idx>,
    dst_oid: Oid,
) -> Result<Option<TrackingResult<IdN, Idx>>, TrackingError> {
    let commit = &tracking.commit;
    dbg!(&tracking.query.before);
    dbg!(tracking.query.before.as_ref() == Some(&dst_oid.to_string()));
    dbg!(commits.len() <= 3);
    dbg!(tracking.node_processed > MAX_NODES);
    let can_skip =
        tracking.query.before.as_ref() != Some(&dst_oid.to_string()) && commits.len() > 3;
    dbg!(can_skip);
    let (src, next) = match track_res {
        MappingResult::Skipped { nodes, src, next }
            if can_skip && tracking.node_processed + nodes < MAX_NODES =>
        {
            dbg!(nodes);
            tracking.node_processed += nodes;
            // TODO fix issue of not stoping when failling to match accurately,
            // most likely related to miss use of fallback value ?
            if tracking.source.is_none() {
                tracking.source = Some(src.globalize(repo, *commit));
            }
            if next.len() > 1 {
                log::warn!("multiple matches: {} matches", next.len());
            }
            let Some(next) = next.get(0) else {
                unreachable!("MappingResult::Missing should have been used if there is no match")
            };
            // TODO stop on branching ?
            tracking.path = Some(next.path.clone());
            tracking.commit = next.commit.clone();

            if tracking.file.is_some() {
                tracking.file = Some(next.file.to_string());
                tracking.query.start = Some(next.start);
                tracking.query.end = Some(next.end);
            }
            return Ok(None);
        }
        MappingResult::Error(err) => Err(tracking.error(err))?,
        MappingResult::Skipped { nodes, src, next } => {
            dbg!(&src);
            dbg!(&next);
            (src, Ok(next))
        }
        MappingResult::Direct { src, matches } => {
            dbg!(&src);
            dbg!(&matches);
            (src, Ok(matches))
        }
        MappingResult::Missing { src, fallback } => {
            dbg!();
            (src, Err(fallback))
        }
    };
    let (src, intermediary) = shift_piece(*commit, tracking.source.take(), src, repo);
    let compute_time = tracking.now.elapsed().as_secs_f64();
    let commits_processed = tracking.commits_processed;
    dbg!(&src);
    dbg!(&next);
    dbg!(next.is_ok());
    Ok(Some(match next {
        Ok(matched) => TrackingResult {
            compute_time,
            commits_processed,
            src,
            intermediary,
            fallback: None,
            matched,
        },
        Err(fallback) => TrackingResult {
            compute_time,
            commits_processed,
            src,
            intermediary,
            fallback: Some(fallback),
            matched: vec![],
        },
    }))
}

#[derive(Debug)]
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
        P: position_accessors::WithOffsets<Idx = Idx>
            + position_accessors::WithPreOrderPath<IdN>
            + position_accessors::FileAndOffsetPostionT<IdN, IdO = usize>,
    {
        let mut path = vec![];
        let mut path_ids = vec![];
        for (o, i) in pos.iter_offsets_and_nodes() {
            path.push(o);
            path_ids.push(i);
        }
        Self::from_file_and_range(&pos.file(), pos.start()..pos.end(), path, path_ids)
    }
    pub(crate) fn globalize(self, spec: &Repo, commit: Oid) -> PieceOfCode<IdN, Idx> {
        PieceOfCode {
            user: spec.user().to_string(),
            name: spec.name().to_string(),
            commit: commit,
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

type RepoConfig = hyperast_vcs_git::processing::ParametrizedCommitProcessorHandle;

fn track_aux(
    tracking: &mut TrackingImpl,
    repo_handle: &ConfiguredRepo,
    src_oid: Oid,
    dst_oid: Oid,
) -> Result<MappingResult<IdN, Idx>, String> {
    let src_tr = {
        let r = tracking.state.repositories.read().unwrap();
        get_commit_root(&r, &repo_handle.config, src_oid)?
    };
    let target = tracking.make_target(src_tr)?;
    let repositories = tracking.state.repositories.read().unwrap();
    let dst_tr = get_commit_root(&repositories, &repo_handle.config, dst_oid)?;
    let postprocess_matching =
        |p: LocalPieceOfCode<IdN, Idx>| p.globalize(&repo_handle.spec, dst_oid);

    do_tracking(
        &repositories,
        &tracking.state.partial_decomps,
        &tracking.state.mappings_alone,
        &tracking.query.flags,
        &target,
        dst_tr,
        &postprocess_matching,
    )
    .into()
}

impl TrackingImpl {
    fn make_target(&mut self, src_tr: IdN) -> Result<TargetCodeElement<IdN, Idx>, String> {
        let start = self.query.start;
        let end = self.query.end;
        const AT_PATH_ERROR: &str = "did you mean to execute at_path variant ?";
        let file = self.file.as_ref().expect(AT_PATH_ERROR);
        let repositories = self.state.repositories.read().unwrap();
        let stores = &repositories.processor.main_stores;
        log::debug!("tracking {}", file);
        let file_node = child_at_path_tracked(stores, src_tr, file.split("/"));
        let Some((file_node, offsets_to_file)) = file_node else {
            return Err("not found".into());
        };
        let mut path_to_target = vec![];
        let (node, offsets_in_file) = resolve_range(file_node, start.unwrap_or(0), end, stores);
        path_to_target.extend(offsets_to_file.iter().map(|x| *x as Idx));
        path_to_target.extend(offsets_in_file.iter().map(|x| *x as Idx));
        let computed_range = compute_range(file_node, &mut offsets_in_file.into_iter(), stores);
        let (_, _, no_spaces_path_to_target) =
            compute_position_with_no_spaces(src_tr, &mut path_to_target.iter().copied(), stores);
        Ok(TargetCodeElement::<IdN, Idx> {
            start: computed_range.0,
            end: computed_range.1,
            path: path_to_target.clone(),
            path_no_spaces: no_spaces_path_to_target,
            node: computed_range.2,
            root: src_tr,
        })
    }
}

fn target_code_elem(
    stores: &SimpleStores<TStore>,
    src_tr: IdN,
    path: &Vec<Idx>,
) -> TargetCodeElement<IdN, Idx> {
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
    target
}

fn get_commit_root(
    repositories: &std::sync::RwLockReadGuard<'_, multi_preprocessed::PreProcessedRepositories>,
    repo_handle: &ParametrizedCommitProcessorHandle,
    src_oid: Oid,
) -> Result<IdN, String> {
    let commit_src = repositories
        .get_commit(repo_handle, &src_oid)
        .ok_or_else(|| format!("{src_oid} is missing"))?;
    let src_tr = commit_src.ast_root;
    Ok(src_tr)
}

fn track_at_path_aux(
    tracking: &mut TrackingImpl,
    repo_handle: &ConfiguredRepo,
    src_oid: Oid,
    dst_oid: Oid,
) -> Result<MappingResult<IdN, Idx>, String> {
    let path = tracking.path.as_ref().unwrap();
    let flags = &tracking.query.flags;
    let state = &tracking.state;
    let repositories = state.repositories.read().unwrap();
    let src_tr = get_commit_root(&repositories, &repo_handle.config, src_oid)?;
    let stores = &repositories.processor.main_stores;

    let target = target_code_elem(stores, src_tr, path);

    let postprocess_matching =
        |p: LocalPieceOfCode<IdN, Idx>| p.globalize(&repo_handle.spec, dst_oid);
    let dst_tr = get_commit_root(&repositories, &repo_handle.config, dst_oid)?;
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
