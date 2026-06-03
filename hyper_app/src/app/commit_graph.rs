use crate::app::types::CommitId;

use super::querying::StreamedComputeResults;
use super::{CommitMdStore, ProjectId, QResId, poll_md_with_pr};
use super::{QueryResults, commit};

mod graph_caching;

type CommitComputeCache<'a> =
    graph_caching::GuardedCache<'a, super::ResultsPerCommit, graph_caching::ComputeResPerCommit>;
type CommitLayoutCache<'a> =
    graph_caching::GuardedCache<'a, commit::CommitsLayoutTimed, graph_caching::ComputeLayout>;

impl crate::HyperApp {
    pub(crate) fn show_commit_graphs_timed(&mut self, ui: &mut egui::Ui) {
        let res_per_commit = &mut graph_caching::RES_PER_COMMIT.lock().unwrap();
        let layout_cache = &mut graph_caching::LAYOUT.lock().unwrap();
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
            let branch = (format!("{}/{}", r.user, r.name), *branch);

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
        res_per_commit: &mut CommitComputeCache<'_>,
        layout_cache: &mut CommitLayoutCache<'_>,
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
        let branch = (format!("{}/{}", r.user, r.name), *branch);
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
            let commit = r.clone().with(branch.1);
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

        let widget = CommitGraphWidget {
            max_fetch: self.data.max_fetch,
            fetched_commit_metadata: &self.data.fetched_commit_metadata,
            results_per_commit,
            cached,
            repo_id,
        };
        let resp = egui::Frame::NONE
            .inner_margin(egui::vec2(50.0, 10.0))
            .show(ui, |ui| widget.show(ui, &mut to_fetch, &mut to_poll))
            .inner;

        self.handle_graph_interactions(ui, repo_id, &r, cached, resp);

        for id in to_fetch {
            if !self.data.fetched_commit_metadata.is_absent(id) {
                continue;
            }
            let commit = r.clone().with(*id);
            let v = commit::fetch_commit(ui.ctx(), &self.data.api_addr, &commit);
            self.data.fetched_commit_metadata.insert(commit.id, v);
        }
        let Some((r, commit_slice)) = self.data.selected_code_data.get_mut(repo_id) else {
            return;
        };
        let mut helper = ToPollHelper {
            ctx: ui.ctx(),
            repo_id,
            r: &r,
            md_fetch: &mut self.data.fetched_commit_metadata,
            commit_slice,
            api_addr: &self.data.api_addr,
        };
        let max_time = cached.max_time;
        let max_fetch = self.data.max_fetch;
        for id in to_poll {
            to_poll_helper(&mut helper, *id, max_time, caches_to_clear, max_fetch);
        }
    }

    fn handle_graph_interactions(
        &mut self,
        ui: &mut egui::Ui,
        repo_id: ProjectId,
        r: &super::Repo,
        cached: &commit::CommitsLayoutTimed,
        resp: egui_plot::PlotResponse<GraphInteraction>,
    ) {
        let is_target_repo = |x: &QueryResults| x.project == repo_id;
        let is_target_query = |x: &QueryResults| x.query.to_usize() == 0;
        let is_target_lang = |x: &QueryResults| self.data.queries[x.query].lang == "Java";

        let effect = to_effect(resp);
        if let InteractionEffect::ClickCommitPlusCmdMod(i) = effect {
            if let Some((_, mut commit_slice)) = self.data.selected_code_data.get_mut(repo_id) {
                let mut it = commit_slice.iter_mut();
                *it.next().unwrap() = cached.commits[i];
                for _ in 0..it.count() {
                    commit_slice.pop();
                }
            }
        } else if let InteractionEffect::ClickErrorFetch(i) = effect {
            let api_addr = &self.data.api_addr;
            for i in i {
                let id = &cached.commits[i];
                let commit = r.clone().with(*id);
                let v = commit::fetch_commit(ui.ctx(), api_addr, &commit);
                self.data.fetched_commit_metadata.insert(commit.id, v);
            }
        } else if let InteractionEffect::ClickCommitPrimary(i) = effect {
            let pred = |x: &_| is_target_repo(x) && is_target_query(x);
            if let Some((_qrid, qres)) = self.data.queries_results.find(pred) {
                self.selected_commit = Some((repo_id, cached.commits[i]));
                self.selected_baseline = None;
                if let super::Tab::QueryResults { format, .. } = &mut self.tabs[qres.tab] {
                    *format = crate::app::ResultFormat::Table
                } else {
                    panic!()
                }
            }
        } else if let InteractionEffect::ClickCommitSecondary(i) = effect {
            let md_fetch = &mut self.data.fetched_commit_metadata;

            let ctx = ui.ctx();
            let commit = r.clone().with(cached.commits[i]);
            commit.commit_url_to_clipboard(ctx, &mut self.notifs);

            let md = md_fetch.remove(&commit.id);
            log::debug!("fetch_merge_pr");
            let waiting = commit::fetch_merge_pr(
                ctx,
                &self.data.api_addr,
                &commit,
                md.unwrap().unwrap().clone(),
                repo_id,
            );
            md_fetch.insert(commit.id, waiting);
        } else if let InteractionEffect::ClickChangeSecondary(i, after) = effect {
            let url = r.clone().url();
            self.notifs.add_log(re_log::LogMsg {
                level: log::Level::Info,
                target: "graph/commits".to_string(),
                msg: format!(
                    "Selected\n{url}/compare/{}..{}",
                    &cached.commits[after], cached.commits[i]
                ),
            });
            let commit = r.clone().with(cached.commits[i]);
            commit.commit_url_to_clipboard(ui.ctx(), &mut self.notifs);
        } else if let InteractionEffect::ClickChangePrimary(i, after) = effect {
            let commit = r.clone().with(cached.commits[after]).url();
            self.notifs.add_log(re_log::LogMsg {
                level: log::Level::Info,
                target: "graph/commits".to_string(),
                msg: format!("Selected\n{} vs {}", commit, cached.commits[i]),
            });

            self.selected_baseline = Some(cached.commits[i]);
            self.selected_commit = Some((repo_id, cached.commits[after]));
            // assert_eq!(self.data.queries.len(), 1); // need to retrieve current query if multiple

            let pred = |x: &_| is_target_repo(x) && is_target_lang(x);
            let Some((_qrid, qres)) = self.data.queries_results.find(pred) else {
                log::warn!("No Java query results found");
                return;
            };
            if let super::Tab::QueryResults { id: _, format } = &mut self.tabs[qres.tab] {
                *format = crate::app::ResultFormat::Hunks
            } else {
                panic!()
            }
        }
    }
}

/// Declares the different effects of graph interactions on the underlying app state
enum InteractionEffect {
    None,
    ClickCommitPlusCmdMod(usize),
    ClickCommitPrimary(usize),
    ClickCommitSecondary(usize),
    ClickErrorFetch(Vec<usize>),
    ClickChangePrimary(usize, usize),
    ClickChangeSecondary(usize, usize),
}

/// Helper to easily control the interactions with the graph, without thinking about how they are handled
fn to_effect(resp: egui_plot::PlotResponse<GraphInteraction>) -> InteractionEffect {
    let secondary = resp.response.secondary_clicked();
    let primary = resp.response.clicked();
    let ctx = &resp.response.ctx;
    let cmd_mod = ctx.input(|i| i.modifiers.command);
    use GraphInteraction::*;
    use InteractionEffect as Effect;
    // NOTE be cautious with order of guards
    match resp.inner {
        ClickCommit(i) if secondary && cmd_mod => Effect::ClickCommitPlusCmdMod(i),
        ClickCommit(i) if primary => Effect::ClickCommitPrimary(i),
        ClickCommit(i) if secondary => Effect::ClickCommitSecondary(i),
        ClickErrorFetch(v) => Effect::ClickErrorFetch(v),
        ClickChange(i, after) if primary => Effect::ClickChangePrimary(i, after),
        ClickChange(i, after) if secondary => Effect::ClickChangeSecondary(i, after),
        _ => Effect::None,
    }
}

struct ToPollHelper<'a, 'b> {
    ctx: &'a egui::Context,
    repo_id: ProjectId,
    r: &'a super::Repo,
    api_addr: &'a str,
    md_fetch: &'a mut CommitMdStore,
    commit_slice: super::CommitSlice<'b>,
}

fn to_poll_helper(
    helper: &mut ToPollHelper<'_, '_>,
    id: CommitId,
    max_time: i64,
    caches_to_clear: &mut Vec<ProjectId>,
    max_fetch: i64,
) {
    let repo_id = helper.repo_id;
    let was_err = helper.md_fetch.get(&id).map_or(false, |x| x.is_err());
    if !helper.md_fetch.try_poll_with(&id, |x| {
        x.map(|x| poll_md_with_pr(x, repo_id, &mut helper.commit_slice))
    }) {
        return;
    }
    let Some(Ok(md)) = helper.md_fetch.get(&id) else {
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
        to_poll_helper_aux(helper, *id1, &md);
    }
    let Some(id2) = id2 else {
        return;
    };
    if max_time - forth_timestamp < max_fetch && helper.md_fetch.is_absent(id2) {
        to_poll_helper_aux(helper, *id2, &md);
    }
    let ToPollHelper { ctx, api_addr, .. } = helper;
    let commit = helper.r.clone().with(id);
    log::debug!("fetch_merge_pr");
    let waiting = commit::fetch_merge_pr(ctx, api_addr, &commit, md.clone(), repo_id);
    helper.md_fetch.insert(id, waiting);
}

fn to_poll_helper_aux(
    helper: &mut ToPollHelper<'_, '_>,
    id: CommitId,
    md: &commit::CommitMetadata,
) {
    let repo_id = helper.repo_id;
    let ToPollHelper {
        ctx,
        commit_slice,
        md_fetch,
        api_addr,
        ..
    } = helper;
    if !md_fetch.is_waiting(&id) {
        let commit = helper.r.clone().with(id);
        let v = commit::fetch_commit(ctx, api_addr, &commit);
        md_fetch.insert(id, v);
        return;
    }
    if !md_fetch.try_poll_with(&id, |x| {
        x.map(|x| poll_md_with_pr(x, repo_id, commit_slice))
    }) {
        return;
    }
    let commit = helper.r.clone().with(id);
    log::debug!("fetch_merge_pr");
    let waiting = commit::fetch_merge_pr(ctx, api_addr, &commit, md.clone(), repo_id);
    md_fetch.insert(id, waiting);
}

enum GraphInteraction {
    None,
    ClickCommit(usize),
    ClickChange(usize, usize),
    ClickErrorFetch(Vec<usize>),
}

const CORNER: bool = true;

const DIFF_VALS: bool = true;
const LEFT_VALS: bool = false;

struct CommitGraphWidget<'a, 'b> {
    fetched_commit_metadata: &'b CommitMdStore,
    results_per_commit: Option<&'b super::ResultsPerCommit>,
    repo_id: ProjectId,
    max_fetch: i64,
    cached: &'a commit::CommitsLayoutTimed,
}

impl<'a> CommitGraphWidget<'a, '_> {
    fn show(
        self,
        ui: &mut egui::Ui,
        to_fetch: &mut Vec<&'a CommitId>,
        to_poll: &mut Vec<&'a CommitId>,
    ) -> egui_plot::PlotResponse<GraphInteraction> {
        use egui_plot::*;
        let max_time = self.cached.max_time;
        let plot = Plot::new(self.repo_id)
            .view_aspect(8.0)
            .show_axes([true, false])
            .allow_zoom([true, false])
            .x_axis_formatter(|m, _range| {
                let v = m.value as i64 - max_time;
                let step_size = m.step_size as i64;
                time_axis_format(v, step_size)
            })
            .x_grid_spacer(|i| with_egui_plot::compute_multi_x_marks(i, max_time))
            .show_y(false)
            .y_axis_formatter(|_, _| Default::default())
            .show_grid([true, false])
            .set_margin_fraction(egui::vec2(0.1, 0.3))
            .allow_scroll([true, false]);

        show_commit_graph_timed_egui_plot(self, ui, to_fetch, to_poll, plot)
    }
}

fn show_commit_graph_timed_egui_plot<'a>(
    widget: CommitGraphWidget<'a, '_>,
    ui: &mut egui::Ui,
    to_fetch: &mut Vec<&'a CommitId>,
    to_poll: &mut Vec<&'a CommitId>,
    plot: egui_plot::Plot<'_>,
) -> egui_plot::PlotResponse<GraphInteraction> {
    let dark_mode = ui.visuals().dark_mode;
    let results_per_commit = widget.results_per_commit;
    let md_fetch = widget.fetched_commit_metadata;
    let cached = widget.cached;
    let style = ui.ctx().style();
    ui.ctx().style_mut(|style| {
        style.visuals.window_fill = egui::Color32::TRANSPARENT;
        style.visuals.window_stroke = egui::Stroke::NONE;
        style.visuals.popup_shadow = egui::Shadow::NONE;
    });

    let resp = plot
        .label_formatter(|name, value| {
            label_formatter(md_fetch, results_per_commit, cached, name, value)
        })
        .show(ui, |plot_ui| {
            plot_graph_aux(widget, plot_ui, dark_mode, to_fetch, to_poll)
        });
    ui.ctx().set_style(style);
    resp
}

fn plot_graph_aux<'a>(
    widget: CommitGraphWidget<'a, '_>,
    plot_ui: &mut egui_plot::PlotUi<'_>,
    dark_mode: bool,
    to_fetch: &mut Vec<&'a CommitId>,
    to_poll: &mut Vec<&'a CommitId>,
) -> GraphInteraction {
    let diff_val_col = if dark_mode {
        egui::Color32::YELLOW
    } else {
        egui::Color32::RED
    };
    let CommitGraphWidget {
        max_fetch,
        fetched_commit_metadata,
        results_per_commit,
        cached,
        ..
    } = widget;
    use egui_plot::*;
    let mut output = GraphInteraction::None;
    let mut offsets = vec![];
    let mut offsets2 = vec![];
    let mut points_with_data = vec![];
    let mut points = vec![];
    'subs: for sub in &cached.subs {
        let mut line = vec![];

        let prev_p = {
            let t = cached.times[sub.prev];
            if t != 1 && cached.max_time - t > max_fetch {
                continue;
            }
            let delta = cached.subs[sub.prev_sub].delta_time;
            let y = with_egui_plot::transform_y(delta);
            [t, y]
        };

        line.push(prev_p.map(|x| x as f64));
        for i in sub.range() {
            let Some(t) = cached.time(i) else {
                if fetched_commit_metadata.is_absent(&cached.commits[i]) {
                    to_fetch.push(&cached.commits[i]);
                    break;
                }
                let Some(a) = fetched_commit_metadata.get(&cached.commits[i]) else {
                    to_poll.push(&cached.commits[i]);
                    break;
                };
                let Err(e) = a else { break };

                to_poll.push(&cached.commits[i]);
                let plot_point = [
                    cached.times[sub.prev] as f64,
                    with_egui_plot::transform_y(sub.delta_time) as f64 + 30.0,
                ];
                if plot_ui.response().clicked() {
                    let point = plot_ui.response().hover_pos().unwrap();
                    let pos = plot_ui.transform().position_from_point(&plot_point.into());
                    let dist_sq = point.distance_sq(pos);
                    if dist_sq < 100.0 {
                        log::error!("should reload");
                        if let GraphInteraction::None = output {
                            output = GraphInteraction::ClickErrorFetch(vec![i]);
                        } else if let GraphInteraction::ClickErrorFetch(v) = &mut output {
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
                break;
            };
            let commit = &cached.commits[i];

            let (before, _after) = before_after(cached, sub, i);
            let diff = results_per_commit
                .zip(before)
                .and_then(|(x, c1)| x.try_diff_as_string(c1, commit));

            let y = with_egui_plot::transform_y(sub.delta_time);
            let mut p = [t, y];
            if sub.start == i {
                let corner = [
                    (p[0] as f64)
                        .max(line.last().unwrap()[0] - plot_ui.transform().dvalue_dpos()[0] * 10.0),
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
                            output = GraphInteraction::ClickChange(i, sub.prev);
                        }
                    }
                }
            } else {
                if t > cached.times[i - 1] {
                    p[1] += 100;
                }
                let a = *line.last().unwrap();
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
                            output = GraphInteraction::ClickChange(i, i - 1);
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
                    .and_then(|x| x.offset(&commit).map(|offset| x.vals_to_string(offset)));
                if let Some(text) = text {
                    plot_ui.text(
                        Text::new("text last", p.map(|x| x as f64).into(), text)
                            .anchor(egui::Align2::RIGHT_BOTTOM)
                            .color(egui::Color32::GRAY),
                    );
                }
            }

            let vals_offset = results_per_commit
                .and_then(|x| x.offset_with_variation(&commit, before, Some(&commit)));
            if let Some(offset) = LEFT_VALS.then_some(()).and(vals_offset) {
                let text = results_per_commit.unwrap().vals_to_string(offset);
                plot_ui.text(
                    Text::new("left vals", p.map(|x| x as f64).into(), text)
                        .anchor(egui::Align2::RIGHT_BOTTOM)
                        .color(egui::Color32::GRAY),
                );
            }

            if results_per_commit
                .and_then(|x| x._get_offset(&commit))
                .is_some()
            {
                points_with_data.push(p.map(|x| x as f64));
                offsets.push(i as u32);
            } else {
                points.push(p.map(|x| x as f64));
                offsets2.push(i as u32);
            }
        }

        if sub.succ < usize::MAX && cached.times[sub.succ] != -1 {
            let y = cached.subs[sub.succ_sub].delta_time;
            let y = with_egui_plot::transform_y(y);
            let x = cached.times[sub.succ];
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

            let c1 = if sub.start == sub.end {
                sub.prev
            } else {
                sub.end - 1
            };
            let diff = results_per_commit
                .and_then(|x| x.try_diff_as_string(&cached.commits[c1], &cached.commits[sub.succ]));
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
                            cached.commits[sub.prev],
                            cached.commits[sub.end - 1],
                            cached.commits[sub.start],
                            cached.commits[sub.succ],
                            sub.prev,
                            sub.end,
                            sub.start,
                            sub.succ,
                        );
                        output = GraphInteraction::ClickChange(sub.succ, c1);
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
        // can unwrap as click must be inside the area
        let point = plot_ui.response().hover_pos().unwrap();
        if let Some(x) = item.find_closest(point, plot_ui.transform()) {
            if x.dist_sq < 10.0 {
                let i = item.offsets[x.index] as usize;
                output = GraphInteraction::ClickCommit(i);
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
        // can unwrap as click must be inside the area
        let hovered = plot_ui.response().hover_pos().unwrap();
        if let Some(x) = item.find_closest(hovered, plot_ui.transform()) {
            if x.dist_sq < 10.0 {
                let i = item.offsets[x.index] as usize;
                output = GraphInteraction::ClickCommit(i);
            }
        }
    }
    plot_ui.add(item);

    for (&b, name) in cached.branches.iter().zip(cached.branch_names.iter()) {
        let b = cached.subs[b].prev;
        let y = cached.subs[b].delta_time;
        let y = with_egui_plot::transform_y(y);
        let position = [cached.times[b] as f64, y as f64].into();
        let text = name.to_string();
        let text = Text::new("branch name", position, text).anchor(egui::Align2::LEFT_TOP);
        plot_ui.text(text);
    }
    output
}

fn before_after<'a>(
    cached: &'a commit::CommitsLayoutTimed,
    sub: &commit::SubsTimed,
    i: usize,
) -> (Option<&'a CommitId>, Option<&'a CommitId>) {
    let before = if i != sub.start {
        Some(&cached.commits[i - 1])
    } else if sub.prev != usize::MAX {
        Some(&cached.commits[sub.prev])
    } else {
        None
    };
    let after = if i + 1 < sub.end {
        Some(&cached.commits[i + 1])
    } else if sub.succ != usize::MAX {
        Some(&cached.commits[sub.succ])
    } else {
        None
    };
    (before, after)
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
        let values = r.inner.result.as_array().unwrap();
        for (i, r) in values.into_iter().enumerate() {
            if i > 2 {
                break;
            }
            vals[i] = r.as_i64().unwrap() as i32;
        }
        let comp_time = r.inner.compute_time as f32;
        results_per_commit.insert(&r.commit, comp_time, &[], &vals);
    }
    log::debug!("{:?}", results_per_commit);
}

fn time_axis_format(v: i64, step_size: i64) -> String {
    if v == 0 {
        "0".to_string()
    } else if step_size > 60 * 60 * 24 * 364 {
        format!("{:+}y", v / (60 * 60 * 24 * 364))
    } else if step_size > 60 * 60 * 24 * 20 {
        format!("{:+}M", v / (60 * 60 * 24 * 30))
    } else if step_size > 60 * 60 * 24 * 6 {
        format!("{:+}w", v / (60 * 60 * 24 * 7))
    } else if step_size > 60 * 60 * 20 {
        format!("{:+}d", v / (60 * 60 * 24))
    } else {
        format!("{:+}h", v / (60 * 60))
    }
}

mod with_egui_plot;

// ###########################################################
// #### Now some hacks to render complex labels in the plot###
// ###########################################################

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
        x?.as_ref().ok()?.message.as_deref()
    }
    if name == CUSTOM_LABEL_FORMAT_MARK_WITH_DATA {
        let i = value.x.to_bits() as usize;
        let c = &cached.commits[i];
        let s = results_per_commit.and_then(|x| x.offset(c).map(|o| x.vals_to_string(o)));
        format!(
            "{}\n{}\n\n{}",
            &c.prefix(6),
            s.unwrap_or_default(),
            msg(fetched_commit_metadata.get(c)).unwrap_or_default()
        )
    } else if name == CUSTOM_LABEL_FORMAT_MARK_NO_DATA {
        let i = value.x.to_bits() as usize;
        let c = &cached.commits[i];
        format!(
            "{}\n\n{}",
            &c.prefix(6),
            msg(fetched_commit_metadata.get(c)).unwrap_or_default()
        )
    } else {
        name.to_string()
    }
}

impl with_egui_plot::CommitPoints<'_> {
    fn label_formatter(
        &self,
        elem: egui_plot::ClosestElem,
        label_formatter: &egui_plot::LabelFormatter<'_>,
    ) -> String {
        // WARN big hack passing an index as a f64...
        let with_data = self.with_data;
        let offset = self.offsets[elem.index];
        let mark = if with_data {
            CUSTOM_LABEL_FORMAT_MARK_WITH_DATA
        } else {
            CUSTOM_LABEL_FORMAT_MARK_NO_DATA
        };
        label_formatter.as_ref().unwrap()(
            mark,
            &egui_plot::PlotPoint {
                x: f64::from_bits(offset as u64),
                y: 0.0,
            },
        )
    }
}
