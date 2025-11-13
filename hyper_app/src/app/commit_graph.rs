use lazy_static::lazy_static;
use std::ops::{ControlFlow, Mul as _};

use super::commit;
use super::querying::{MatchingError, StreamedComputeResults};
use super::utils_results_batched::ComputeResultIdentified;
use super::{CommitMdStore, ProjectId, QResId, QueryId, poll_md_with_pr2};

type GuardedCache<'a, T, U> =
    std::sync::MutexGuard<'a, super::utils_commit::BorrowFrameCache<T, U>>;
type Cache<T, U> =
    std::sync::Arc<std::sync::Mutex<crate::app::utils_commit::BorrowFrameCache<T, U>>>;

lazy_static! {
    static ref RES_PER_COMMIT: Cache<super::ResultsPerCommit, ComputeResPerCommit> =
        Default::default();
}

lazy_static! {
    static ref LAYOUT: Cache<commit::CommitsLayoutTimed, ComputeLayout> = Default::default();
}

#[derive(Default)]
pub struct ComputeResPerCommit;

type ResHash = u64;

impl
    egui::util::cache::ComputerMut<
        ((ProjectId, QueryId, ResHash), &StreamedComputeResults),
        super::ResultsPerCommit,
    > for ComputeResPerCommit
{
    fn compute(
        &mut self,
        ((pid, qid, _), r): ((ProjectId, QueryId, ResHash), &StreamedComputeResults),
    ) -> super::ResultsPerCommit {
        let mut res = super::ResultsPerCommit::default();
        update_results_per_commit(&mut res, r);
        res
    }
}

#[derive(Default)]
pub struct ComputeLayout;

impl
    egui::util::cache::ComputerMut<
        ((&str, &str, usize), &super::CommitMdStore),
        commit::CommitsLayoutTimed,
    > for ComputeLayout
{
    fn compute(
        &mut self,
        ((name, target, _), fetched_commit_metadata): ((&str, &str, usize), &super::CommitMdStore),
    ) -> commit::CommitsLayoutTimed {
        commit::compute_commit_layout_timed(
            |id| fetched_commit_metadata.get(id)?.as_ref().ok().cloned(),
            Some((name.to_string(), target.to_string())).into_iter(),
        )
    }
}

impl crate::HyperApp {
    pub(crate) fn show_commit_graphs_timed(&mut self, ui: &mut egui::Ui) {
        let res_per_commit = &mut RES_PER_COMMIT.lock().unwrap();
        let layout_cache = &mut LAYOUT.lock().unwrap();
        let mut caches_to_clear = vec![];
        ui.add_space(20.0);
        let ready_count = self.data.fetched_commit_metadata.len_local();
        for repo_id in self.data.selected_code_data.project_ids() {
            let qrid = (self.data.queries_results)
                .find(|x| x.project == repo_id)
                .map(|x| x.0);
            self.show_commit_graph_timed(
                ui,
                res_per_commit,
                layout_cache,
                &mut caches_to_clear,
                ready_count,
                repo_id,
                qrid,
            );
        }
        for repo_id in caches_to_clear {
            let Some((r, mut c)) = self.data.selected_code_data.get_mut(repo_id) else {
                continue;
            };
            let Some(branch) = c.iter_mut().next() else {
                continue;
            };
            let branch = (format!("{}/{}", r.user, r.name), branch.clone());

            if let Some(r) = (self.data.queries_results)
                .iter()
                .find(|x| x.project == repo_id)
            {
                let qid = r.query;
                if let Some(Ok(r)) = r.content.get() {
                    let content_hash = r.rows.lock().unwrap().0;
                    let key = (repo_id, qid, content_hash);
                    res_per_commit.remove(key);
                }
            }
            layout_cache.remove((
                &branch.0,
                &branch.1,
                ready_count, // TODO find something more reliable
            ));
        }
        ui.add_space(20.0);
        res_per_commit.evice_cache();
        layout_cache.evice_cache();
    }

    fn show_commit_graph_timed(
        &mut self,
        ui: &mut egui::Ui,
        res_per_commit: &mut GuardedCache<'_, super::ResultsPerCommit, ComputeResPerCommit>,
        layout_cache: &mut GuardedCache<'_, commit::CommitsLayoutTimed, ComputeLayout>,
        caches_to_clear: &mut Vec<ProjectId>,
        ready_count: usize,
        repo_id: ProjectId,
        qrid: Option<QResId>,
    ) {
        let Some((r, mut commit_slice)) = self.data.selected_code_data.get_mut(repo_id) else {
            return;
        };
        let Some(branch) = commit_slice.iter_mut().next() else {
            return;
        };
        let branch = (format!("{}/{}", r.user, r.name), branch.clone());
        let id = ui.make_persistent_id("bottom_cache_layout");
        let r = r.clone();

        let results_per_commit = qrid
            .and_then(|qrid| self.data.queries_results.get(qrid))
            .and_then(|r| {
                let qid = r.query;
                let r = r.content.get()?.as_ref().ok()?;
                let content_hash = r.rows.lock().unwrap().0;
                let key = (repo_id, qid, content_hash);
                Some(res_per_commit.get2(key, r))
            });

        if self.data.fetched_commit_metadata.is_absent(&branch.1) {
            let commit = r.clone().with(&branch.1);
            let fetching = commit::fetch_commit(ui.ctx(), &self.data.api_addr, &commit);
            self.data
                .fetched_commit_metadata
                .insert(commit.id, fetching);
        }
        let cached = layout_cache.get2(
            (
                &branch.0,
                &branch.1,
                ready_count, // TODO find something more reliable
            ),
            &self.data.fetched_commit_metadata,
        );

        let mut to_fetch = vec![];
        let mut to_poll = vec![];

        let resp = egui::Frame::NONE
            .inner_margin(egui::vec2(50.0, 10.0))
            .show(ui, |ui| {
                show_commit_graph_timed_egui_plot(
                    ui,
                    self.data.max_fetch,
                    &self.data.fetched_commit_metadata,
                    results_per_commit,
                    cached,
                    repo_id,
                    &mut to_fetch,
                    &mut to_poll,
                )
            })
            .inner;

        match resp.inner {
            GraphInteration::ClickCommit(i)
                if resp.response.secondary_clicked() && ui.input(|i| i.modifiers.command) =>
            {
                let mut it = commit_slice.iter_mut();
                *it.next().unwrap() = cached.commits[i].clone();
                for _ in 0..it.count() {
                    commit_slice.pop();
                }
            }
            GraphInteration::ClickCommit(i) if resp.response.clicked() => {
                self.selected_commit = Some((repo_id, cached.commits[i].to_string()));
                self.selected_baseline = None;
                let qres = (self.data.queries_results)
                    .iter()
                    .find(|x| x.project == repo_id && x.query.to_usize() == 0)
                    .unwrap();
                if let super::Tab::QueryResults { id, format } = &mut self.tabs[qres.tab] {
                    *format = crate::app::ResultFormat::Table
                } else {
                    panic!()
                }
            }
            GraphInteration::ClickCommit(i) if resp.response.secondary_clicked() => {
                let commit = format!(
                    "https://github.com/{}/{}/commit/{}",
                    r.user, r.name, cached.commits[i]
                );
                ui.ctx().copy_text(commit.to_string());
                self.notifs.success(format!(
                    "Copied address of github commit to clipboard\n{}",
                    commit
                ));
                let id = &cached.commits[i];
                let repo = r.clone();
                let id = id.clone();
                let commit = crate::app::types::Commit { repo, id };
                let md = self.data.fetched_commit_metadata.remove(&commit.id);
                log::debug!("fetch_merge_pr");
                let waiting = commit::fetch_merge_pr(
                    ui.ctx(),
                    &self.data.api_addr,
                    &commit,
                    md.unwrap().unwrap().clone(),
                    repo_id,
                );
                self.data.fetched_commit_metadata.insert(commit.id, waiting);
            }
            GraphInteration::ClickErrorFetch(i) => {
                for i in i {
                    let id = &cached.commits[i];
                    let repo = r.clone();
                    let id = id.clone();
                    let commit = crate::app::types::Commit { repo, id };
                    let v = commit::fetch_commit(ui.ctx(), &self.data.api_addr, &commit);
                    self.data.fetched_commit_metadata.insert(commit.id, v);
                }
            }
            GraphInteration::ClickChange(i, after) => {
                let commit = format!(
                    "https://github.com/{}/{}/commit/{}",
                    r.user, r.name, cached.commits[after]
                );
                self.notifs.add_log(re_log::LogMsg {
                    level: log::Level::Info,
                    target: format!("graph/commits"),
                    msg: format!("Selected\n{} vs {}", commit, cached.commits[i]),
                });
                if resp.response.clicked() {
                    self.selected_baseline = Some(cached.commits[i].to_string());
                    self.selected_commit = Some((repo_id, cached.commits[after].to_string()));
                    // assert_eq!(self.data.queries.len(), 1); // need to retieve current query if multiple

                    let qres = (self.data.queries_results)
                        .iter()
                        .find(|x| x.project == repo_id && self.data.queries[x.query].lang == "Java")
                        .unwrap();
                    if let super::Tab::QueryResults { id, format } = &mut self.tabs[qres.tab] {
                        *format = crate::app::ResultFormat::Hunks
                    } else {
                        panic!()
                    }
                } else if resp.response.secondary_clicked() {
                    ui.ctx().copy_text(commit.to_string());
                    self.notifs.success(format!(
                        "Copied address of github commit to clipboard\n{}",
                        commit
                    ));
                }
            }
            _ => (),
        }
        for id in to_fetch {
            if !self.data.fetched_commit_metadata.is_absent(id) {
                continue;
            }
            let repo = r.clone();
            let id = id.clone();
            let commit = crate::app::types::Commit { repo, id };
            let v = commit::fetch_commit(ui.ctx(), &self.data.api_addr, &commit);
            self.data.fetched_commit_metadata.insert(commit.id, v);
        }
        let mut helper = ToPollHelper {
            ctx: ui.ctx(),
            repo_id,
            r: &r,
            md_fetch: &mut self.data.fetched_commit_metadata,
            commit_slice: &mut commit_slice,
            api_addr: &self.data.api_addr,
        };
        let max_time = cached.max_time;
        let max_fetch = self.data.max_fetch;
        for id in to_poll {
            to_poll_helper(&mut helper, id, max_time, caches_to_clear, max_fetch);
        }
    }
}

struct ToPollHelper<'a, 'b> {
    ctx: &'a egui::Context,
    repo_id: ProjectId,
    r: &'a super::Repo,
    api_addr: &'a str,
    md_fetch: &'a mut CommitMdStore,
    commit_slice: &'a mut super::CommitSlice<'b>,
}

fn to_poll_helper(
    helper: &mut ToPollHelper<'_, '_>,
    id: &str,
    max_time: i64,
    caches_to_clear: &mut Vec<ProjectId>,
    max_fetch: i64,
) {
    let repo_id = helper.repo_id;
    let was_err = helper.md_fetch.get(id).map_or(false, |x| x.is_err());
    if !helper.md_fetch.try_poll_with(id, |x| {
        x.map(|x| poll_md_with_pr2(x, repo_id, helper.commit_slice))
    }) {
        return;
    }
    let Some(Ok(md)) = helper.md_fetch.get(id) else {
        return;
    };
    let md = md.clone();
    if was_err {
        caches_to_clear.push(repo_id);
    }
    if md.forth_timestamp == i64::MAX {
        return;
    }
    if md.ancestors.is_empty() {
        return;
    }
    let id1 = &md.ancestors[0];
    let id2 = md.ancestors.get(1);
    let forth_timestamp = md.forth_timestamp;
    if max_time - forth_timestamp < max_fetch && helper.md_fetch.is_absent(id1) {
        to_poll_helper_aux(helper, id1, &md);
    }
    let Some(id2) = id2 else {
        return;
    };
    if max_time - forth_timestamp < max_fetch && helper.md_fetch.is_absent(id2) {
        to_poll_helper_aux(helper, id2, &md);
    }
    let ToPollHelper { ctx, api_addr, .. } = helper;
    let repo = helper.r.clone();
    let commit = crate::app::types::Commit {
        repo,
        id: id.to_string(),
    };
    log::debug!("fetch_merge_pr");
    let waiting = commit::fetch_merge_pr(ctx, api_addr, &commit, md.clone(), repo_id);
    helper.md_fetch.insert(id.to_string(), waiting);
}

fn to_poll_helper_aux(helper: &mut ToPollHelper<'_, '_>, id: &str, md: &commit::CommitMetadata) {
    let repo_id = helper.repo_id;
    let ToPollHelper {
        ctx,
        commit_slice,
        md_fetch,
        api_addr,
        ..
    } = helper;
    if !md_fetch.is_waiting(id) {
        let repo = helper.r.clone();
        let commit = crate::app::types::Commit {
            repo,
            id: id.to_string(),
        };
        let v = commit::fetch_commit(ctx, api_addr, &commit);
        md_fetch.insert(id.to_string(), v);
        return;
    }
    if !md_fetch.try_poll_with(id, |x| {
        x.map(|x| poll_md_with_pr2(x, repo_id, commit_slice))
    }) {
        return;
    }
    let repo = helper.r.clone();
    let commit = crate::app::types::Commit {
        repo,
        id: id.to_string(),
    };
    log::debug!("fetch_merge_pr");
    let waiting = commit::fetch_merge_pr(ctx, api_addr, &commit, md.clone(), repo_id);
    md_fetch.insert(id.to_string(), waiting);
}

enum GraphInteration {
    None,
    ClickCommit(usize),
    ClickChange(usize, usize),
    ClickErrorFetch(Vec<usize>),
}

const DIFF_VALS: bool = true;
const LEFT_VALS: bool = false;
const RIGHT_VALS: bool = false;

fn show_commit_graph_timed_egui_plot<'a>(
    ui: &mut egui::Ui,
    max_fetch: i64,
    fetched_commit_metadata: &CommitMdStore,
    results_per_commit: Option<&super::ResultsPerCommit>,
    cached: &'a commit::CommitsLayoutTimed,
    repo_id: ProjectId,
    to_fetch: &mut Vec<&'a String>,
    to_poll: &mut Vec<&'a String>,
) -> egui_plot::PlotResponse<GraphInteration> {
    let diff_val_col = if ui.visuals().dark_mode {
        egui::Color32::YELLOW
    } else {
        egui::Color32::RED
    };
    use egui_plot::*;
    Plot::new(repo_id)
        .view_aspect(8.0)
        .show_axes([true, false])
        .allow_zoom([true, false])
        .x_axis_formatter(|m, _range| {
            let v = m.value as i64 - cached.max_time;
            if v == 0 {
                format!("0")
            } else if m.step_size as i64 > 60 * 60 * 24 * 364 {
                format!("{:+}y", v / (60 * 60 * 24 * 364))
            } else if m.step_size as i64 > 60 * 60 * 24 * 20 {
                format!("{:+}M", v / (60 * 60 * 24 * 30))
            } else if m.step_size as i64 > 60 * 60 * 24 * 6 {
                format!("{:+}w", v / (60 * 60 * 24 * 7))
            } else if m.step_size as i64 > 60 * 60 * 20 {
                format!("{:+}d", v / (60 * 60 * 24))
            } else {
                format!("{:+}h", v / (60 * 60))
            }
        })
        .x_grid_spacer(|i| with_egui_plot::compute_multi_x_marks(i, cached.max_time))
        .show_y(false)
        .y_axis_formatter(|m, _| Default::default())
        .show_grid([true, false])
        .set_margin_fraction(egui::vec2(0.1, 0.3))
        .allow_scroll([true, false])
        .label_formatter(|name, value| {
            label_formatter(
                fetched_commit_metadata,
                results_per_commit,
                cached,
                name,
                value,
            )
        })
        .show(ui, |plot_ui| {
            let mut ouput = GraphInteration::None;
            let mut offsets = vec![];
            let mut offsets2 = vec![];
            let mut points_with_data = vec![];
            let mut points = vec![];
            'subs: for sub @ commit::SubsTimed {
                prev,
                prev_sub,
                start,
                end,
                succ,
                succ_sub,
                delta_time,
            } in &cached.subs
            {
                const CORNER: bool = true;
                let mut line = vec![];

                let prev_p = [
                    if cached.times[*prev] == -1 {
                        -1
                    } else {
                        let t = cached.times[*prev];
                        if cached.max_time - t > max_fetch {
                            continue 'subs;
                        }
                        t
                    },
                    with_egui_plot::transform_y(cached.subs[*prev_sub].delta_time),
                ];
                line.push(prev_p.map(|x| x as f64));
                for i in *start..*end {
                    let t = cached.times[i];
                    if t == -1 {
                        if fetched_commit_metadata.is_absent(&cached.commits[i]) {
                            to_fetch.push(&cached.commits[i]);
                        } else if let Some(a) = fetched_commit_metadata.get(&cached.commits[i]) {
                            match a {
                                Err(e) => {
                                    to_poll.push(&cached.commits[i]);
                                    let plot_point = [
                                        cached.times[*prev] as f64,
                                        with_egui_plot::transform_y(*delta_time) as f64 + 30.0,
                                    ];
                                    if plot_ui.response().clicked() {
                                        let point = plot_ui.response().hover_pos().unwrap();
                                        let pos = plot_ui
                                            .transform()
                                            .position_from_point(&plot_point.into());
                                        let dist_sq = point.distance_sq(pos);
                                        if dist_sq < 100.0 {
                                            log::error!("should reload");
                                            if let GraphInteration::None = ouput {
                                                ouput = GraphInteration::ClickErrorFetch(vec![i]);
                                            } else if let GraphInteration::ClickErrorFetch(v) =
                                                &mut ouput
                                            {
                                                v.push(i)
                                            }
                                        }
                                    }
                                    let series: Vec<[f64; 2]> = vec![plot_point];
                                    let points = Points::new("error", series)
                                        .radius(4.0)
                                        .color(egui::Color32::RED)
                                        .name(format!("Error getting {}:\n{e}", cached.commits[i]));
                                    plot_ui.add(points);
                                }
                                _ => (),
                            }
                        } else {
                            to_poll.push(&cached.commits[i]);
                        }
                        break;
                    }
                    let commit = &cached.commits[i];

                    let before = if i != *start {
                        Some(cached.commits[i - 1].as_str())
                    } else if *prev != usize::MAX {
                        Some(cached.commits[*prev].as_str())
                    } else {
                        None
                    };
                    let after = if i + 1 < *end {
                        Some(cached.commits[i + 1].as_str())
                    } else if *succ != usize::MAX {
                        Some(cached.commits[*succ].as_str())
                    } else {
                        None
                    };
                    let diff = results_per_commit
                        .zip(before)
                        .and_then(|(x, c1)| x.try_diff_as_string(c1, commit));

                    let y = with_egui_plot::transform_y(*delta_time);
                    let mut p = [t, y];
                    if *start == i {
                        let corner = [
                            (p[0] as f64).max(
                                line.last().unwrap()[0]
                                    - plot_ui.transform().dvalue_dpos()[0] * 10.0,
                            ),
                            p[1] as f64,
                        ];
                        line.push(corner);
                        if let Some(text) = DIFF_VALS.then_some(()).and(diff) {
                            plot_ui.text(
                                Text::new("diff", corner.into(), text)
                                    .anchor(egui::Align2::RIGHT_BOTTOM)
                                    .color(diff_val_col),
                            );

                            if plot_ui.response().clicked() {
                                let point = plot_ui.response().hover_pos().unwrap();
                                let plot_point = PlotPoint::new(corner[0], corner[1]);
                                let pos = plot_ui.transform().position_from_point(&plot_point);
                                let dist_sq = point.distance_sq(pos);
                                if dist_sq < 100.0 {
                                    log::error!("clicked");
                                    ouput = GraphInteration::ClickChange(i, *prev);
                                }
                            }
                        }
                    } else {
                        if t > cached.times[i - 1] {
                            p[1] += 100;
                        }
                        let a = line.last().unwrap().clone();
                        let b = p.map(|x| x as f64);
                        let position = with_egui_plot::center(a, b);
                        if let Some(text) = DIFF_VALS.then_some(()).and(diff) {
                            plot_ui.text(
                                Text::new("diff val", position, text)
                                    .anchor(egui::Align2::RIGHT_BOTTOM)
                                    .color(diff_val_col),
                            );
                            if plot_ui.response().clicked() {
                                let point = plot_ui.response().hover_pos().unwrap();
                                let pos = plot_ui.transform().position_from_point(&position);
                                let dist_sq = point.distance_sq(pos);
                                if dist_sq < 100.0 {
                                    log::debug!("clicked");
                                    ouput = GraphInteration::ClickChange(i, i - 1);
                                }
                            }
                        }
                    }
                    line.push(p.map(|x| x as f64));

                    // stop rendering when reached limit
                    if cached.max_time - t > max_fetch {
                        let line = Line::new("last line", line).allow_hover(false);
                        plot_ui.line(line);
                        continue 'subs;
                    }

                    if i == 1 {
                        let text = results_per_commit
                            .and_then(|x| x.offset(commit).map(|offset| x.vals_to_string(offset)));
                        if let Some(text) = text {
                            plot_ui.text(
                                Text::new("text last", p.map(|x| x as f64).into(), text)
                                    .anchor(egui::Align2::RIGHT_BOTTOM)
                                    .color(egui::Color32::GRAY),
                            );
                        }
                    }

                    let vals_offset = results_per_commit.and_then(|x| {
                        x.offset_with_variation(commit.as_str(), before, Some(commit.as_str()))
                    });
                    if let Some(offset) = LEFT_VALS.then_some(()).and(vals_offset) {
                        let text = results_per_commit.unwrap().vals_to_string(offset);
                        plot_ui.text(
                            Text::new("left vals", p.map(|x| x as f64).into(), text)
                                .anchor(egui::Align2::RIGHT_BOTTOM)
                                .color(egui::Color32::GRAY),
                        );
                    }

                    if results_per_commit
                        .and_then(|x| x._get_offset(commit))
                        .is_some()
                    {
                        points_with_data.push(p.map(|x| x as f64));
                        offsets.push(i as u32);
                    } else {
                        points.push(p.map(|x| x as f64));
                        offsets2.push(i as u32);
                    }
                }

                if *succ < usize::MAX && cached.times[*succ] != -1 {
                    let y = cached.subs[*succ_sub].delta_time;
                    let y = with_egui_plot::transform_y(y);
                    let x = cached.times[*succ];
                    let position: PlotPoint;
                    let p = [x, y].map(|x| x as f64);
                    if CORNER {
                        let prev = line.last().unwrap();
                        let x = p[0];
                        let x = x + plot_ui.transform().dvalue_dpos()[0] * 10.0;
                        let x = prev[0].min(x);
                        let y = prev[1];
                        let y = if (y - p[1]).abs() < 1.0 {
                            y - 1000.0
                        } else if y < p[1] {
                            y - plot_ui.transform().dvalue_dpos()[1] * 5.0
                        } else {
                            y + plot_ui.transform().dvalue_dpos()[1] * 5.0
                        };
                        let corner = [x, y];
                        position = corner.into();
                        line.push(corner);
                        line.push(p);
                    } else {
                        position = with_egui_plot::center(*line.last().unwrap(), p);
                        line.push(p);
                    }

                    let c1 = if start == end { *prev } else { *end - 1 };
                    let diff = results_per_commit.and_then(|x| {
                        x.try_diff_as_string(&cached.commits[c1], &cached.commits[*succ])
                    });
                    if let Some(text) = DIFF_VALS.then_some(()).and(diff) {
                        plot_ui.text(
                            Text::new("diff vals", position, text)
                                .anchor(egui::Align2::RIGHT_BOTTOM)
                                .color(egui::Color32::RED),
                        );

                        if plot_ui.response().clicked() {
                            let point = plot_ui.response().hover_pos().unwrap();
                            let plot_point = position;
                            let pos = plot_ui.transform().position_from_point(&plot_point);
                            let dist_sq = point.distance_sq(pos);
                            if dist_sq < 100.0 {
                                log::error!("clicked");
                                log::error!(
                                    "{} {} {} {}\n{} {} {} {}",
                                    cached.commits[*prev],
                                    cached.commits[end - 1],
                                    cached.commits[*start],
                                    cached.commits[*succ],
                                    prev,
                                    end,
                                    start,
                                    succ,
                                );
                                ouput = GraphInteration::ClickChange(*succ, c1);
                            }
                        }
                    }
                }

                let line = Line::new("line", line).allow_hover(false);
                plot_ui.line(line);
            }

            let points = Points::new("commit", points)
                .radius(2.0)
                .color(egui::Color32::GREEN)
                .name("Commit");

            let item = with_egui_plot::CommitPoints {
                offsets: offsets2,
                points,
                with_data: false,
            };
            let has_any_click = plot_ui
                .response()
                .flags
                .contains(egui::response::Flags::CLICKED);
            if has_any_click {
                if let Some(x) =
                    item.find_closest(plot_ui.response().hover_pos().unwrap(), plot_ui.transform())
                {
                    if x.dist_sq < 10.0 {
                        let i = item.offsets[x.index] as usize;
                        ouput = GraphInteration::ClickCommit(i);
                    }
                }
            }
            plot_ui.add(item);
            let points = Points::new("commit with data", points_with_data)
                .radius(2.0)
                .color(egui::Color32::DARK_GREEN)
                .name("Commit with data");
            let item = with_egui_plot::CommitPoints {
                offsets,
                points,
                with_data: true,
            };
            if has_any_click {
                if let Some(x) =
                    item.find_closest(plot_ui.response().hover_pos().unwrap(), plot_ui.transform())
                {
                    if x.dist_sq < 10.0 {
                        let i = item.offsets[x.index] as usize;
                        ouput = GraphInteration::ClickCommit(i);
                    }
                }
            }
            plot_ui.add(item);

            for &b in &cached.branches {
                let b = cached.subs[b].prev;
                let y = cached.subs[b].delta_time;
                let y = with_egui_plot::transform_y(y);
                let position = [cached.times[b] as f64, y as f64].into();
                let text = &cached.commits[b];
                let text = Text::new("branch name", position, text).anchor(egui::Align2::LEFT_TOP);
                plot_ui.text(text);
            }
            ouput
        })
}

fn update_results_per_commit(
    results_per_commit: &mut super::ResultsPerCommit,
    r: &StreamedComputeResults,
) {
    let header = &r.head;
    let mut vals = vec![0; header.len()];
    results_per_commit.set_cols(header);
    for r in &r.rows.lock().unwrap().1 {
        let Ok(r) = r else { continue };
        for (i, r) in r.inner.result.as_array().unwrap().into_iter().enumerate() {
            if i > 2 {
                break;
            }
            vals[i] = r.as_i64().unwrap() as i32;
        }
        results_per_commit.insert(&r.commit, r.inner.compute_time as f32, &[], &vals);
    }
    log::debug!("{:?}", results_per_commit);
}

mod with_egui_plot {
    use egui_plot::*;

    pub(crate) fn transform_y(y: i64) -> i64 {
        assert_ne!(y, i64::MIN);
        assert_ne!(y, i64::MAX);
        assert!(y > -1);
        if y == 0 {
            0
        } else {
            -((y as f64).sqrt() as i64) - 1
        }
    }

    pub fn center(a: [f64; 2], b: [f64; 2]) -> PlotPoint {
        fn apply(a: [f64; 2], b: [f64; 2], f: impl Fn(f64, f64) -> f64) -> [f64; 2] {
            [f(a[0], b[0]), f(a[1], b[1])]
        }
        let position = apply(a, b, |a, b| (a + b) / 2.0).into();
        position
    }

    /// from egui_plot
    /// Fill in all values between [min, max] which are a multiple of `step_size`
    fn fill_marks_between(
        step_size: f64,
        (min, max): (f64, f64),
        ori: i64,
    ) -> impl Iterator<Item = GridMark> {
        debug_assert!(min <= max, "Bad plot bounds: min: {min}, max: {max}");
        let (min, max) = (min - ori as f64, max - ori as f64);
        let first = (min / step_size).ceil() as i64;
        let last = (max / step_size).ceil() as i64;

        (first..last).map(move |i| {
            let value = (i as f64) * step_size + ori as f64;
            GridMark { value, step_size }
        })
    }

    pub(crate) fn compute_multi_x_marks(i: GridInput, ori: i64) -> Vec<GridMark> {
        // TODO use proper rounded year convention
        let year = 60 * 60 * 24 * 365 + 60 * 60 * 6;
        let years = fill_marks_between(year as f64, i.bounds, ori);
        let month = 60 * 60 * 24 * 30;
        let months = fill_marks_between(month as f64, i.bounds, ori);
        let week = 60 * 60 * 24 * 7;
        let weeks = fill_marks_between(week as f64, i.bounds, ori);
        let day = 60 * 60 * 24;
        let days = fill_marks_between(day as f64, i.bounds, ori);
        years.chain(months).chain(weeks).chain(days).collect()
    }

    pub struct CommitPoints<'a> {
        pub offsets: Vec<u32>,
        pub points: Points<'a>,
        pub with_data: bool,
    }

    impl<'a> PlotItem for CommitPoints<'a> {
        fn shapes(&self, ui: &egui::Ui, transform: &PlotTransform, shapes: &mut Vec<egui::Shape>) {
            self.points.shapes(ui, transform, shapes)
        }

        fn initialize(&mut self, x_range: std::ops::RangeInclusive<f64>) {
            self.points.initialize(x_range)
        }

        fn name(&self) -> &str {
            PlotItem::name(&self.points)
        }

        fn color(&self) -> egui::Color32 {
            PlotItem::color(&self.points)
        }

        fn highlight(&mut self) {
            PlotItem::highlight(&mut self.points)
        }

        fn highlighted(&self) -> bool {
            self.points.highlighted()
        }

        fn allow_hover(&self) -> bool {
            PlotItem::allow_hover(&self.points)
        }

        fn geometry(&self) -> PlotGeometry<'_> {
            self.points.geometry()
        }

        fn bounds(&self) -> PlotBounds {
            self.points.bounds()
        }

        fn id(&self) -> egui::Id {
            PlotItem::id(&self.points)
        }

        fn on_hover(
            &self,
            elem: ClosestElem,
            shapes: &mut Vec<egui::Shape>,
            cursors: &mut Vec<Cursor>,
            plot: &PlotConfig<'_>,
            label_formatter: &LabelFormatter<'_>,
        ) {
            let points = match self.geometry() {
                PlotGeometry::Points(points) => points,
                PlotGeometry::None => {
                    panic!("If the PlotItem has no geometry, on_hover() must not be called")
                }
                PlotGeometry::Rects => {
                    panic!("If the PlotItem is made of rects, it should implement on_hover()")
                }
            };

            // this method is only called, if the value is in the result set of find_closest()
            let value = points[elem.index];
            let pointer = plot.transform.position_from_point(&value);

            let offset = self.offsets[elem.index];

            let font_id = egui::TextStyle::Body.resolve(plot.ui.style());
            // WARN big hack passing an index as a f64...
            let mark = if self.with_data {
                super::CUSTOM_LABEL_FORMAT_MARK_WITH_DATA
            } else {
                super::CUSTOM_LABEL_FORMAT_MARK_NO_DATA
            };
            let text = label_formatter.as_ref().unwrap()(
                mark,
                &PlotPoint {
                    x: f64::from_bits(offset as u64),
                    y: 0.0,
                },
            );
            plot.ui.painter().text(
                pointer + egui::vec2(3.0, -2.0),
                egui::Align2::LEFT_BOTTOM,
                text,
                font_id,
                plot.ui.visuals().text_color(),
            );
            log::debug!("{}", label_formatter.is_some());
        }

        fn base(&self) -> &egui_plot::PlotItemBase {
            self.points.base()
        }

        fn base_mut(&mut self) -> &mut egui_plot::PlotItemBase {
            self.points.base_mut()
        }
    }
}

const CUSTOM_LABEL_FORMAT_MARK_WITH_DATA: &str = "_d";
const CUSTOM_LABEL_FORMAT_MARK_NO_DATA: &str = "_";

fn label_formatter(
    fetched_commit_metadata: &CommitMdStore,
    results_per_commit: Option<&super::ResultsPerCommit>,
    cached: &commit::CommitsLayoutTimed,
    name: &str,
    value: &egui_plot::PlotPoint,
) -> String {
    fn msg(x: Option<&Result<commit::CommitMetadata, String>>) -> Option<&str> {
        x?.as_ref().ok()?.message.as_ref().map(|s| s.as_str())
    }
    if name == CUSTOM_LABEL_FORMAT_MARK_WITH_DATA {
        let i = value.x.to_bits() as usize;
        let c = &cached.commits[i];
        let s = results_per_commit.and_then(|x| x.offset(c.as_str()).map(|o| x.vals_to_string(o)));

        format!(
            "{}\n{}\n\n{}",
            &c[..6],
            s.unwrap_or_default(),
            msg(fetched_commit_metadata.get(c)).unwrap_or_default()
        )
    } else if name == CUSTOM_LABEL_FORMAT_MARK_NO_DATA {
        let i = value.x.to_bits() as usize;
        let c = &cached.commits[i];
        format!(
            "{}\n\n{}",
            &c[..6],
            msg(fetched_commit_metadata.get(c)).unwrap_or_default()
        )
    } else {
        name.to_string()
    }
}
