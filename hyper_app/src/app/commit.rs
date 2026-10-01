use std::i64;

use poll_promise::Promise;
use re_ui::UiExt as _;
use serde::{Deserialize, Serialize};

use crate::utils_poll::Resource;

use super::types::{Commit, CommitId, Repo};

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct CommitMetadata {
    /// commit message
    pub(crate) message: Option<String>,
    /// parents commits
    /// if multiple parents, the first one should be where the merge happens
    pub(crate) parents: Vec<CommitId>,
    /// tree corresponding to version
    pub(crate) tree: Option<super::types::Oid>,
    /// offset in minutes
    pub(crate) timezone: i32,
    /// seconds
    pub(crate) time: i64,
    /// (opt) ancestors in powers of 2; [2,4,8,16,32]
    /// important to avoid linear loading time
    pub(crate) ancestors: Vec<CommitId>,
    pub(crate) forth_timestamp: i64,
}

impl CommitMetadata {
    pub(crate) fn show(&self, ui: &mut egui::Ui) {
        let tz = &chrono::FixedOffset::west_opt(self.timezone * 60).unwrap();
        let date = chrono::Duration::seconds(self.time);
        let date = chrono::DateTime::<chrono::FixedOffset>::default()
            .with_timezone(tz)
            .checked_add_signed(date);
        if let Some(date) = date {
            ui.label(format!("Date:\t{:?}", date));
        } else {
        }
        fn join<I: Iterator>(it: I, s: I::Item) -> I::Item
        where
            I::Item: ToString + Clone,
            I::Item: FromIterator<I::Item>,
        {
            use itertools::intersperse;
            intersperse(it, s).collect::<I::Item>()
        }
        if ui.available_width() > 300.0 {
            ui.label(format!(
                "Parents: {}",
                join(self.parents.iter().map(|x| x.as_str()), " + ".to_string())
            ));
        } else {
            let text = join(self.parents.iter().map(|x| x.prefix(8)), " + ".to_string());
            let label = ui.label(format!("Parents: {}", text));
            if label.hovered() {
                let text = join(self.parents.iter().map(|x| x.as_str()), " + ".to_string());
                egui::Tooltip::always_open(
                    ui.ctx().clone(),
                    ui.layer_id(),
                    label.id.with("tooltip"),
                    &label,
                )
                .show(|ui| {
                    ui.label(&text);
                    ui.label("CTRL+C to copy (and send in the debug console)");
                });
                const SC_COPY: egui::KeyboardShortcut =
                    egui::KeyboardShortcut::new(egui::Modifiers::CTRL, egui::Key::C);
                wasm_rs_dbg::dbg!(&text);
                if ui.input_mut(|mem| mem.consume_shortcut(&SC_COPY)) {
                    wasm_rs_dbg::dbg!(&text);
                    ui.ctx().copy_text(text.to_string());
                }
            }
        }
        if let Some(msg) = &self.message {
            if let Some(head) = msg.lines().next() {
                let label0 = ui.label("Commit message:");
                let head =
                    egui::RichText::new(head).background_color(ui.style().visuals.extreme_bg_color);
                let label = ui.label(head);
                if label0.hovered() || label.hovered() {
                    egui::Tooltip::always_open(
                        ui.ctx().clone(),
                        ui.layer_id(),
                        label.id.with("tooltip"),
                        label0.rect.union(label.rect),
                    )
                    .show(|ui| {
                        ui.text_edit_multiline(&mut msg.to_string());
                    });
                }
            }
        }
    }

    pub(crate) fn local_datetime(&self) -> Option<chrono::DateTime<chrono::FixedOffset>> {
        let seconds_since_epoch = self.time;
        let tz_offset_minutes = self.timezone;
        use chrono::prelude::*;
        let utc_datetime = DateTime::from_timestamp(seconds_since_epoch, 0)?;
        let offset = FixedOffset::east_opt(tz_offset_minutes * 60)?;
        let local_datetime = utc_datetime.with_timezone(&offset);

        Some(local_datetime)
    }
}

pub(super) fn fetch_commit(
    ctx: &egui::Context,
    api_addr: &str,
    commit: &Commit,
) -> Promise<Result<super::CommitMdPayload, String>> {
    let ctx = ctx.clone();
    let (sender, promise) = Promise::new();
    let url = format!(
        "http://{}/commit/github/{}/{}/{}",
        api_addr, &commit.repo.user, &commit.repo.name, &commit.id,
    );

    let request = ehttp::Request::get(&url);

    ehttp::fetch(request, move |response| {
        ctx.request_repaint(); // wake up UI thread
        let resource = response
            .and_then(|response| Resource::<CommitMetadata>::from_response(&ctx, response))
            .and_then(|x| x.content.ok_or("No content".into()))
            .map(|x| (x, None));
        sender.send(resource);
    });
    promise
}

pub(super) fn fetch_commit0(
    ctx: &egui::Context,
    api_addr: &str,
    commit: &Commit,
) -> Promise<Result<CommitMetadata, String>> {
    let ctx = ctx.clone();
    let (sender, promise) = Promise::new();
    let url = format!(
        "http://{}/commit/github/{}/{}/{}",
        api_addr, &commit.repo.user, &commit.repo.name, &commit.id,
    );

    let request = ehttp::Request::get(&url);
    // request
    //     .headers
    //     .insert("Content-Type".to_string(), "text".to_string());

    ehttp::fetch(request, move |response| {
        ctx.request_repaint(); // wake up UI thread
        let resource = response
            .and_then(|response| Resource::<CommitMetadata>::from_response(&ctx, response))
            .and_then(|x| x.content.ok_or("No content".into()));
        sender.send(resource);
    });
    promise
}

impl Resource<CommitMetadata> {
    fn from_response(_ctx: &egui::Context, response: ehttp::Response) -> Result<Self, String> {
        // let content_type = response.content_type().unwrap_or_default();

        let text = response.text();
        let text = text.ok_or("")?;
        let text = serde_json::from_str(text).map_err(|x| x.to_string())?;

        Ok(Self {
            response,
            content: text,
        })
    }
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct MergePr {
    pub(crate) merge_commit: Option<Commit>,
    pub(crate) head_commit: Commit,
    pub(crate) title: String,
    pub(crate) number: i64,
}

impl Resource<MergePr> {
    fn from_response(_ctx: &egui::Context, response: ehttp::Response) -> Result<Self, String> {
        let text = response.text();
        let text = text.ok_or("nothing in response")?.to_string();
        let text = serde_json::from_str(&text).map_err(|x| x.to_string())?;

        Ok(Self {
            response,
            content: text,
        })
    }
}

pub(super) fn fetch_merge_pr(
    ctx: &egui::Context,
    api_addr: &str,
    commit: &Commit,
    md: CommitMetadata,
    pid: ProjectId,
) -> Promise<Result<super::CommitMdPayload, String>> {
    let ctx = ctx.clone();
    let (sender, promise) = Promise::new();
    let url = format!(
        "http://{}/pr/github/{}/{}/{}",
        api_addr, &commit.repo.user, &commit.repo.name, &commit.id,
    );
    let url_fork = format!(
        "http://{}/fork/github/{}/{}",
        api_addr, &commit.repo.user, &commit.repo.name,
    );

    let request = ehttp::Request::get(&url);

    ehttp::fetch(request, move |response| {
        ctx.request_repaint(); // wake up UI thread
        let resource = response
            .and_then(|response| Resource::<MergePr>::from_response(&ctx, response))
            .and_then(|x| x.content.ok_or("No content".into()));

        let resource = resource.map(|x| {
            let request = ehttp::Request::post(
                &format!(
                    "{}/{}/{}/{}",
                    url_fork, x.head_commit.repo.user, x.head_commit.repo.name, x.head_commit.id,
                ),
                Default::default(),
            );
            ehttp::fetch(request, |x| log::info!("{:?}", x));

            log::error!("{:?}", x);
            // if !md.parents.contains(&x.head_commit.id) {
            //     md.parents.push(x.head_commit.id.clone());
            // }
            (md, Some((x.head_commit, pid)))
        });
        sender.send(resource);
    });
    promise
}

#[allow(unused)]
pub(super) fn fetch_commit_parents(
    ctx: &egui::Context,
    api_addr: &str,
    commit: &Commit,
    depth: usize,
) -> Promise<Result<Vec<CommitId>, String>> {
    let ctx = ctx.clone();
    let (sender, promise) = Promise::new();
    let url = format!(
        "http://{}/commit-parents/github/{}/{}/{}/{}",
        api_addr, &commit.repo.user, &commit.repo.name, &commit.id, depth
    );

    let request = ehttp::Request::get(&url);
    ehttp::fetch(request, move |response| {
        ctx.request_repaint(); // wake up UI thread
        let resource = response
            .and_then(|response| Resource::<Vec<CommitId>>::from_response(&ctx, response))
            .and_then(|x| x.content.ok_or("No content".into()));
        sender.send(resource);
    });
    promise
}

impl Resource<Vec<CommitId>> {
    #[allow(unused)]
    fn from_response(ctx: &egui::Context, response: ehttp::Response) -> Result<Self, String> {
        let text = response.text();
        let text = text.ok_or("")?;
        let text = serde_json::from_str(text).map_err(|x| x.to_string())?;

        Ok(Self {
            response,
            content: text,
        })
    }
}

pub(crate) fn validate_pasted_project_url(
    paste: &str,
) -> Result<(super::types::Repo, Vec<CommitId>), &'static str> {
    use std::str::FromStr;
    match hyperast::utils::Url::from_str(paste) {
        Ok(url) if &url.domain == "github.com" && &url.protocol == "https" => {
            let path = url.path.split_once('&').map_or(url.path.as_str(), |x| x.0);
            let path: Vec<_> = path.split('/').collect();
            if path.len() > 1 && !path[0].is_empty() && !path[1].is_empty() {
                let repo = super::types::Repo {
                    user: path[0].to_string(),
                    name: path[1].to_string(),
                };
                if let Some(after_repo) = path.get(2) {
                    if *after_repo == "commit" {
                        if let Some(after_repo) = path.get(3) {
                            if after_repo.chars().all(|x| x.is_alphanumeric()) {
                                Ok((repo, vec![after_repo.parse().unwrap()]))
                            } else if let Some(_) = after_repo.split_once("..") {
                                Err("range of commits are not handled (WIP)")
                            } else {
                                Err("url scheme not handled")
                            }
                        } else {
                            Err("commit id missing")
                        }
                    } else if after_repo.chars().all(|x| x.is_alphanumeric()) {
                        Ok((repo, vec![after_repo.parse().unwrap()]))
                    } else if let Some(_) = after_repo.split_once("..") {
                        Err("range of commits are not handled (WIP)")
                    } else {
                        Err("url scheme not handled")
                    }
                } else {
                    Ok((repo, vec![]))
                }
            } else {
                Err("url scheme not handled")
            }
        }
        Ok(url) if &url.protocol == "https" => Err("not a github.com domain "),
        Ok(_) => Err("must be https protocol"),
        _ => Err("not a valid url"),
    }
}

// TODO move to a dedicated crate (not egui_addon)
/// Selection of projects.
///     Each project is identified by main repository.
///     Each project contains a selection of other repositories considered as forks,
///     and a set of commits (not branches)
#[derive(Deserialize, Serialize, Debug)]
pub struct SelectedProjects {
    len: usize,
    repositories: Vec<Repo>,
    offsets: Vec<u32>,
    // TODO use inline commit ids
    commits: Vec<CommitId>,
    // TODO add forks
    // NOTE using git remote add on backend should be the best approach https://stackoverflow.com/questions/66621183/github-how-to-work-across-multiple-forks-of-the-same-repository
}

impl Default for SelectedProjects {
    fn default() -> Self {
        let mut s = Self::empty();
        s.add_with_commit_slice(
            ["INRIA", "spoon"].into(),
            &["56e12a0c0e0e69ea70863011b4f4ca3305e0542b"],
        );
        s.add_with_commit_slice(
            ["official-stockfish", "Stockfish"].into(),
            &["7f2eb10e93879bc569c7ddf6fb51d6f812cc477c"],
        );
        s.add_with_commit_slice(
            ["tree-sitter", "tree-sitter"].into(),
            &["800f2c41d0e35e4383172d7a67a16f3933b86039"],
        );
        s.add_with_commit_slice(
            ["rerun-io", "egui_tiles"].into(),
            &["0fe81768278678db4f66a297178c04f23452c682"],
        );
        s.add_with_commit_slice(
            ["Marcono1234", "gson"].into(),
            &["3d241ca0a6435cbf1fa1cdaed2af8480b99fecde"],
        );
        s.add_with_commit_slice(
            ["tree-sitter", "tree-sitter-cpp"].into(),
            &["ab1065fa23a43a447bd7e619a3af90253867af24"],
        );
        s.add_with_commit_slice(
            ["graphhopper", "graphhopper"].into(),
            &["90acd4972610ded0f1581143f043eb4653a4c691"],
        );
        s.add_with_commit_slice(
            ["apache", "dubbo"].into(),
            &["aaafad80bec93ddb167ec613eb930749f5ec90ec"],
        );
        s
        // Cpp:
        // https://github.com/tree-sitter/tree-sitter/commit/800f2c41d0e35e4383172d7a67a16f3933b86039

        // Rust (just for the commit history, I still don't have the proper parsing facilities setup for Rust):
        // https://github.com/rerun-io/egui_tiles/0fe81768278678db4f66a297178c04f23452c682

        // Java:
        // https://github.com/INRIA/spoon/commit/56e12a0c0e0e69ea70863011b4f4ca3305e0542b
        // https://github.com/graphhopper/graphhopper/commit/90acd4972610ded0f1581143f043eb4653a4c691
        // Java repos with merges
        // https://github.com/dubbo/dubbo/commit/aaafad80bec93ddb167ec613eb930749f5ec90ec
    }
}

/// Id of each project, ie. a repository and a selection of other repositories considered as forks
#[derive(Deserialize, Serialize, Copy, Clone, Debug, Hash, PartialEq, Eq)]
#[repr(transparent)]
pub struct ProjectId(usize);
impl ProjectId {
    pub const INVALID: Self = Self(usize::MAX);
}

impl SelectedProjects {
    fn empty() -> Self {
        Self {
            len: 0,
            repositories: vec![],
            offsets: vec![],
            commits: vec![],
        }
    }

    pub(crate) fn add_with_commit_slice(
        &mut self,
        repo: Repo,
        commits: &[impl Into<CommitId> + Clone],
    ) -> ProjectId {
        self.add(
            repo,
            commits.into_iter().map(|x| x.clone().into()).collect(),
        )
    }

    pub(crate) fn add(&mut self, repo: Repo, commits: Vec<CommitId>) -> ProjectId {
        if let Some(i) = self.repositories.iter().position(|x| x == &repo) {
            let i = ProjectId(i);
            let (_, mut cs) = self._get_mut(i);
            // TODO opti extend on empty set of commits
            for c in commits {
                cs.push(c);
            }
            i
        } else {
            self.len += 1;
            let i = ProjectId(self.repositories.len());
            self.repositories.push(repo);
            assert!(self.commits.len() <= u32::MAX as usize);
            self.offsets.push(self.commits.len() as u32);
            self.commits.extend(commits);
            i
        }
    }

    pub(crate) fn remove(&mut self, ProjectId(i): ProjectId) {
        self.len -= 1;
        let range = self.c_range(i);
        log::debug!("before proj({}) {:?} rm: {:?}", i, range, self.offsets);
        self.offsets[i + 1..]
            .iter_mut()
            .for_each(|x| *x -= range.len() as u32);
        self.commits.drain(range);
        // self.repositories.remove(i as usize);
        // self.offsets.remove(i as usize);
        log::debug!("after proj {} rm: {:?}", self.commits.len(), self.offsets);
        if i > 0 {
            assert!(self.offsets[i - 1] <= self.offsets[i])
        }
        for i in self.project_ids() {
            log::debug!("{:?}", self.get_mut(i));
        }
    }

    fn c_range(&self, i: usize) -> std::ops::Range<usize> {
        let end = if let Some(i) = self.offsets.get(i + 1) {
            *i as usize
        } else {
            self.commits.len()
        };
        self.offsets[i] as usize..end
    }

    pub(crate) fn len(&self) -> usize {
        self.len
    }

    pub(crate) fn commit_count(&self) -> usize {
        self.commits.len()
    }

    pub(crate) fn project_ids(&self) -> impl Iterator<Item = ProjectId> + use<> {
        (0..self.repositories.len()).map(ProjectId)
    }

    pub fn get(&mut self, ProjectId(i): ProjectId) -> Option<&Repo> {
        self.repositories.get(i)
    }

    pub(crate) fn get_mut<'a>(
        &'a mut self,
        ProjectId(i): ProjectId,
    ) -> Option<(&'a mut Repo, CommitSlice<'a>)> {
        if i >= self.repositories.len() {
            return None;
        }
        let c_range = self.c_range(i);
        if c_range.is_empty() {
            return None;
        };
        let end = c_range.end;
        Some((
            self.repositories.get_mut(i)?,
            CommitSlice {
                end,
                commits: &mut self.commits,
                offsets: &mut self.offsets,
                i,
            },
        ))
    }

    fn _get_mut<'a>(&'a mut self, ProjectId(i): ProjectId) -> (&'a mut Repo, CommitSlice<'a>) {
        let c_range = self.c_range(i);
        let end = c_range.end;
        (
            self.repositories.get_mut(i).unwrap(),
            CommitSlice {
                end,
                commits: &mut self.commits,
                offsets: &mut self.offsets,
                i,
            },
        )
    }
    pub fn repositories(&mut self) -> impl Iterator<Item = &mut Repo> {
        self.repositories.iter_mut()
    }

    pub(crate) fn find(&mut self, repo: &Repo) -> Option<ProjectId> {
        self.repositories
            .iter()
            .position(|r| r == repo)
            .map(ProjectId)
    }
}

#[derive(Debug)]
pub(crate) struct CommitSlice<'a> {
    offsets: &'a mut Vec<u32>,
    commits: &'a mut Vec<CommitId>,
    i: usize,
    end: usize,
}

impl<'a> CommitSlice<'a> {
    pub(crate) fn push(&mut self, c: CommitId) {
        let start = self.offsets[self.i] as usize;
        if self.commits[start..self.end].contains(&c) {
            return;
        }
        self.commits.insert(self.end, c);
        self.end += 1;
        self.offsets[self.i + 1..].iter_mut().for_each(|x| *x += 1);
    }

    pub(crate) fn last_mut(&mut self) -> Option<&mut CommitId> {
        self.commits.get_mut(self.end.checked_sub(1)?)
    }

    pub(crate) fn pop(&mut self) -> CommitId {
        self.end = (self.end.checked_sub(1))
            .expect("trying to remove a commit from a project without any");
        self.offsets[self.i + 1..].iter_mut().for_each(|x| *x -= 1);
        self.commits.remove(self.end)
    }

    pub(crate) fn iter_mut(&mut self) -> impl Iterator<Item = &mut CommitId> {
        self.commits[self.offsets[self.i] as usize..self.end].iter_mut()
    }

    pub(crate) fn remove(&mut self, j: usize) -> CommitId {
        self.end -= 1;
        self.offsets[self.i + 1..].iter_mut().for_each(|x| *x -= 1);
        self.commits.remove(self.offsets[self.i] as usize + j)
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub(crate) struct SubsTimed {
    // indexing in CommitsLayout.subs
    pub(crate) prev_sub: usize,
    // indexing in CommitsLayout.commits
    pub(crate) prev: usize,
    // indexing in CommitsLayout.commits
    pub(crate) start: usize,
    // indexing in CommitsLayout.commits
    pub(crate) end: usize,
    // indexing in CommitsLayout.subs
    pub(crate) succ_sub: usize,
    // indexing in CommitsLayout.commits
    pub(crate) succ: usize,
    pub(crate) delta_time: i64,
}
impl SubsTimed {
    pub(crate) fn range(&self) -> std::ops::Range<usize> {
        self.start..self.end
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub(crate) struct CommitsLayoutTimed {
    pub(crate) branch_names: Vec<String>,
    pub(crate) commits: Vec<CommitId>,
    // times and lines
    pub(crate) times: Vec<i64>,
    // indexing in subs
    pub(crate) branches: Vec<usize>,
    pub(crate) subs: Vec<SubsTimed>,
    pub(crate) max_time: i64,
    pub(crate) min_time: i64,
    pub(crate) max_delta: i64,
}

impl Default for CommitsLayoutTimed {
    fn default() -> Self {
        Self {
            branch_names: Default::default(),
            commits: Default::default(),
            times: Default::default(),
            branches: Default::default(),
            subs: Default::default(),
            max_time: 0,
            min_time: i64::MAX,
            // excluding subs with no prev AND succ
            max_delta: 0,
        }
    }
}

impl CommitsLayoutTimed {
    pub(crate) fn time(&self, id: usize) -> Option<i64> {
        let r = self.times[id];
        if r == -1 { None } else { Some(r) }
    }
}

pub(crate) fn compute_commit_layout_timed(
    commits: impl Fn(&CommitId) -> Option<CommitMetadata>,
    branches: impl Iterator<Item = (String, CommitId)>,
) -> CommitsLayoutTimed {
    use std::collections::HashMap;
    type TId = usize;
    type SId = usize;
    let mut r = CommitsLayoutTimed::default();
    let mut index = HashMap::<CommitId, (TId, SId)>::default();
    for (branch_name, target) in branches {
        // log::debug!("{} {}", branch_name, target);

        r.branch_names.push(branch_name);
        r.commits.push("branch".into());
        r.branches.push(r.subs.len());
        let branch_index = r.times.len();
        let mut waiting: Vec<(CommitId, TId, SId)> = vec![(target, branch_index, r.subs.len())];
        r.times.push(-1);
        loop {
            let Some((mut current, prev, prev_sub)) = waiting.pop() else {
                break;
            };
            let start = r.times.len();
            let end;
            let mut succ = None;
            loop {
                if let Some(fork) = index.get(&current) {
                    succ = Some(*fork);
                    end = r.times.len();
                    break;
                }
                index.insert(current, (r.times.len(), r.subs.len()));
                if let Some(commit) = commits(&current) {
                    // universal time then ?
                    let time = commit.time; // + commit.timezone as i64 * 60;
                    r.min_time = time.min(r.min_time);
                    r.max_time = time.max(r.max_time);
                    r.commits.push(current);
                    r.times.push(time);
                    if let Some(p) = commit.parents.get(0) {
                        current = *p;
                    } else {
                        end = r.times.len();
                        break;
                    }
                    if let Some(p) = commit.parents.get(1..) {
                        for p in p {
                            waiting.push((*p, r.times.len() - 1, r.subs.len()));
                        }
                    }
                } else {
                    r.commits.push(current);
                    r.times.push(-1);
                    end = r.times.len();
                    break;
                }
            }
            let delta_time;
            let (succ, succ_sub) = if let Some(succ) = succ {
                if r.times[prev] != -1 {
                    delta_time = (r.times[prev] - r.times[succ.0]).abs();
                    r.max_delta = r.max_delta.max(delta_time);
                } else {
                    delta_time = 100;
                }
                succ
            } else {
                if r.subs.is_empty() {
                    delta_time = 0;
                } else if r.times[prev] == -1 {
                    delta_time = 100;
                } else if r.times[end - 1] != -1 {
                    delta_time = (r.times[prev] - r.times[end - 1]).abs();
                    r.max_delta = r.max_delta.max(delta_time);
                } else if let Some(t) = r.times[start..end - 1].iter().rev().find(|x| **x != -1) {
                    delta_time = (r.times[prev] - t).abs();
                    r.max_delta = r.max_delta.max(delta_time);
                } else {
                    delta_time = 100;
                }
                (usize::MAX, usize::MAX)
            };
            r.subs.push(SubsTimed {
                prev,
                prev_sub,
                start,
                end,
                succ,
                succ_sub,
                delta_time,
            });
        }
        r.times[branch_index] = r.times[branch_index + 1];
    }
    r
}

#[inline(always)]
pub(crate) fn show_commits_selection<const BUTTON: bool>(
    ui: &mut egui::Ui,
    api_addr: &str,
    commit_md: &mut super::CommitMdStore,
    r: &mut Repo,
    mut c: CommitSlice<'_>,
    i: ProjectId,
    mut range: std::ops::Range<isize>,
) -> Option<CommitId> {
    let text_style = egui::TextStyle::Body;
    let row_height = ui.text_style_height(&text_style);
    let space = ui.style().spacing.item_spacing.y;
    let row_height = row_height + space;
    let mut clicked = None;
    if range.start == 0 {
        ui.allocate_ui((ui.available_width(), 30.0).into(), |ui| {
            ui.label(format!("{}/{}", r.user, r.name))
        });
    }
    let mut total = 1usize;
    range.start -= 1;
    range.end -= 1;

    commit_md.try_poll_all_waiting(|v| v.map(|x| x.0));
    for commit_oid in c.iter_mut().map(|x| &*x) {
        let mut to_fetch = std::collections::HashSet::default();
        const IT: bool = true;
        let _total = if IT {
            show_commits_as_tree_it::<true>(
                ui,
                commit_oid,
                commit_md,
                &mut to_fetch,
                0,
                &mut clicked,
                range.clone(),
                row_height,
            )
        } else {
            show_commits_as_tree::<true>(
                ui,
                commit_oid,
                commit_md,
                &mut to_fetch,
                0,
                &mut clicked,
                range.clone(),
                row_height,
            )
        };
        // let _total = show_commits_as_tree::<true>(

        range.start = range.start.saturating_sub(_total as isize);
        range.end = range.end.saturating_sub(_total as isize);
        total = total.saturating_add(_total);

        for id in to_fetch {
            let repo = r.clone();
            let commit = Commit { repo, id };
            let v = fetch_commit(ui.ctx(), api_addr, &commit);
            commit_md.insert(commit.id, v);
        }
    }
    total += 1;
    let id = egui::Id::new(("commit.sel", i));
    ui.data_mut(|d| {
        let r = d.get_temp_mut_or(id, total);
        *r = total.max(*r)
    });
    clicked
}

/// imperative version
fn show_commits_as_tree_it<'a, const BUTTON: bool>(
    ui: &mut egui::Ui,
    id: &'a CommitId,
    commit_md: &'a super::CommitMdStore,
    to_fetch: &mut std::collections::HashSet<CommitId>,
    d: usize,
    clicked: &mut Option<CommitId>,
    mut range: std::ops::Range<isize>,
    row_height: f32,
) -> usize {
    let mut shown = std::collections::HashSet::<&CommitId>::default();
    let mut queue = vec![(id, d)];
    let mut total = 0;
    while let Some((id, d)) = queue.pop() {
        let Some(res) = commit_md.get(id) else {
            let waiting = commit_md.len_waiting();
            if range.contains(&0) {
                ui.horizontal(|ui| {
                    ui.label("fetching");
                    ui.add_space(2.0);
                    ui.label(id.to_string());
                    ui.add_space(2.0);
                    ui.spinner();
                    ui.label(waiting.to_string());
                });
                total += 1;
                range.start -= 1;
                range.end -= 1;
            }
            if waiting < 30 && commit_md.is_absent(id) {
                to_fetch.insert(*id);
            }
            continue;
        };
        if let Err(err) = res {
            ui.horizontal(|ui| {
                ui.error_label(err);
                if ui.button("Retry").clicked() {
                    to_fetch.insert(*id);
                }
            });
            total += 2;
            range.start -= 2;
            range.end -= 2;
            continue;
        }
        let Ok(md) = res else { unreachable!() };
        if range.start <= 0 {
            let size = (ui.available_width(), row_height).into();
            ui.new_child(egui::UiBuilder::new())
                .allocate_ui(size, |ui| {
                    show_commit_row::<BUTTON>(ui, id, clicked, md);
                });
            let text_style = egui::TextStyle::Body;
            let row_height = ui.text_style_height(&text_style);
            let size = (ui.available_width(), row_height).into();
            ui.allocate_exact_size(size, egui::Sense::empty());
        }
        total += 1;
        range.start -= 1;
        range.end -= 1;
        shown.insert(id);
        if range.end >= -10 {
            for x in (&md.parents).into_iter().rev() {
                if !shown.contains(x) {
                    queue.push((x, d + 1));
                }
            }
        }
        let waiting = commit_md.len_waiting();
        for (i, a) in md.ancestors.iter().enumerate() {
            let i = i + 1;
            if d + i * i >= 100 {
                break;
            }
            if waiting < 30 && commit_md.is_absent(a) {
                to_fetch.insert(*a);
            }
        }
        if range.end < -10 {
            break;
        }
    }
    total
}

fn show_commits_as_tree<const BUTTON: bool>(
    ui: &mut egui::Ui,
    id: &CommitId,
    commit_md: &super::CommitMdStore,
    to_fetch: &mut std::collections::HashSet<CommitId>,
    d: usize,
    clicked: &mut Option<CommitId>,
    mut range: std::ops::Range<isize>,
    row_height: f32,
) -> usize {
    let mut total = 1;
    let limit = usize::MAX;
    if d >= limit {
        return 0;
    }
    let Some(res) = commit_md.get(id) else {
        let waiting = commit_md.len_waiting();
        if !range.contains(&0) {
            return 0;
        }
        ui.horizontal(|ui| {
            ui.label("fetching");
            ui.add_space(2.0);
            ui.label(id.to_string());
            ui.add_space(2.0);
            ui.spinner();
            ui.label(waiting.to_string());
        });
        if waiting < 30 && commit_md.is_absent(id) {
            to_fetch.insert(*id);
        }
        return total;
    };
    if let Err(err) = res {
        ui.horizontal(|ui| {
            ui.error_label(err);
            if ui.button("Retry").clicked() {
                to_fetch.insert(*id);
            }
        });

        return total;
    }
    let Ok(md) = res else { unreachable!() };
    if range.contains(&0) {
        ui.allocate_ui((ui.available_width(), row_height).into(), |ui| {
            show_commit_row::<BUTTON>(ui, id, clicked, md);
        });
    }
    range.start -= total as isize;
    range.end -= total as isize;
    for id in &md.parents {
        if range.end < -200 {
            break;
        }
        let _total = show_commits_as_tree::<BUTTON>(
            ui,
            id,
            commit_md,
            to_fetch,
            d + 1,
            clicked,
            range.clone(),
            row_height,
        );
        range.start = range.start.saturating_sub(_total as isize);
        range.end = range.end.saturating_sub(_total as isize);
        total = total.saturating_add(_total);
    }
    let waiting = commit_md.len_waiting();
    for (i, a) in md.ancestors.iter().enumerate() {
        let i = i + 2;
        if d + i * i >= limit {
            break;
        }
        if waiting < 30 && commit_md.is_absent(a) {
            to_fetch.insert(*a);
        }
    }
    total
}

fn show_commit_row<const BUTTON: bool>(
    ui: &mut egui::Ui,
    id: &CommitId,
    clicked: &mut Option<CommitId>,
    md: &CommitMetadata,
) {
    let text = egui::RichText::new(format!("{}", id)).monospace();
    let local_datetime = md.local_datetime();
    let button = ui.horizontal(|ui| {
        let button = if BUTTON {
            ui.button(text)
        } else {
            ui.label(text)
        };
        let p = md.parents.len();
        let local_datetime = if let Some(local_datetime) = local_datetime {
            format!("{local_datetime} ")
        } else {
            "".to_string()
        };
        let text = if p == 1 {
            format!("{local_datetime} 1 parent")
        } else {
            format!("{local_datetime} {p} parents")
        };
        ui.label(egui::RichText::new(text).monospace().weak());
        button
    });
    let button = button.inner.on_hover_ui_at_pointer(|ui| {
        ui.vertical(|ui| {
            if let Some(local_datetime) = local_datetime {
                ui.label(format!("time: {}", local_datetime));
            }
            ui.label("Parents:");
            for id in &md.parents {
                ui.monospace(id.to_string());
            }
        });
    });
    if BUTTON && button.clicked() {
        *clicked = Some(*id);
    }
}
