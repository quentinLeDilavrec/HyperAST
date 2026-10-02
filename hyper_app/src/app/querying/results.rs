use std::sync::Arc;

use re_ui::UiExt as _;

use crate::app::store::FetchedHyperAST;
use crate::app::{
    AppData, CommitMdStore, ProjectId, QResId, QueriesDifferentialResults, QueryData, QueryId,
    QueryResults, ResultFormat, TabId,
};
use crate::app::{commit, tracking};
use crate::types::{self, Commit, CommitId};
use crate::utils_results_batched;

pub(crate) fn show_results(
    ui: &mut egui::Ui,
    id: &mut QResId,
    format: &mut ResultFormat,
    pane: &mut TabId,
    selected_commit: &mut Option<(ProjectId, CommitId)>,
    selected_baseline: &mut Option<CommitId>,
    data: &mut AppData,
) {
    let qres = data.queries_results.get_mut(*id);
    let Some((proj_id, qid, res)) = extract_qres(ui, qres) else {
        ui.error_label(format!("problem with query result {id:?}"));
        return;
    };
    if let ResultFormat::Table = format {
        let mut sel_commit = None;
        if let Some(selected) = selected_commit {
            if selected.0 == *proj_id {
                let id = egui::Id::new(proj_id);
                let i = ui.data_mut(|w| {
                    let m: &mut (Option<(ProjectId, CommitId)>, usize) =
                        w.get_temp_mut_or_default(id);
                    if m.0.as_ref() != Some(selected) {
                        m.0 = Some(*selected);
                        m.1 = (res.rows.lock().unwrap().1)
                            .iter()
                            .position(|x| x.as_ref().map_or(false, |r| r.commit == selected.1))
                            .unwrap_or(usize::MAX);
                        log::debug!("{:?}", m);
                        Some(m.1)
                    } else {
                        None
                    }
                });
                sel_commit = i;
            }
        }
        ui.push_id("table", |ui| {
            utils_results_batched::show_long_result_table(
                ui,
                (&res.head, None, res.rows.lock().unwrap().1.as_slice()),
                &mut sel_commit,
                |cid| {
                    let md_fetch = &data.fetched_commit_metadata;
                    let commit_metadata = md_fetch.get(&cid.parse().unwrap())?;
                    (commit_metadata.as_ref()).ok()?.message.clone()
                },
            )
        });
    } else if let ResultFormat::Hunks = format {
        if show_hunks_header(
            ui,
            format,
            &mut data.fetched_commit_metadata,
            selected_baseline,
            selected_commit,
        ) {
            return;
        }

        let Some(selected_baseline) = selected_baseline else {
            unreachable!()
        };
        let Some(selected_commit) = selected_commit else {
            unreachable!()
        };
        let Some(differential) = &mut data.queries_differential_results else {
            compute_queries_differential_results(
                ui,
                *pane,
                *proj_id,
                *qid,
                data,
                selected_baseline,
                selected_commit,
            );
            return;
        };
        let (absent, new) = update_queries_differential_results(
            ui,
            &data.queries,
            selected_baseline,
            *qid,
            differential,
        );
        if absent {
            wasm_rs_dbg::dbg!(new);
            data.queries_differential_results = None;
            return;
        }
        let x = match differential.2.get() {
            Some(Ok(x)) => x,
            Some(Err(err)) => {
                ui.error_label(format!("Error on Differential: {:?}", err));
                if ui.button("retry").clicked() {
                    data.queries_differential_results = None;
                }
                return;
            }
            None => {
                return;
            }
        };

        if new {
            wasm_rs_dbg::dbg!(x.results.len());
            let store = &data.store;
            store.demand_nodes(x.iter_nodes_ids());
        }
        let fetched_files = &mut data.fetched_files;
        let api_addr = &data.api_addr;
        show_hunks(ui, fetched_files, api_addr, x, selected_commit);
    } else if let ResultFormat::Tree = format {
        let Some(selected_baseline) = selected_baseline else {
            unreachable!()
        };
        let Some(selected_commit) = selected_commit else {
            unreachable!()
        };

        let Some(differential) = &mut data.queries_differential_results else {
            compute_queries_differential_results(
                ui,
                *pane,
                *proj_id,
                *qid,
                data,
                selected_baseline,
                selected_commit,
            );
            return;
        };
        let (absent, new) = update_queries_differential_results(
            ui,
            &data.queries,
            selected_baseline,
            *qid,
            differential,
        );
        if absent {
            wasm_rs_dbg::dbg!(new);
            data.queries_differential_results = None;
            return;
        }
        let Some(Ok(x)) = differential.2.get_mut() else {
            return;
        };
        if new {
            wasm_rs_dbg::dbg!(x.results.len());
            let store = &data.store;
            store.demand_nodes(x.iter_nodes_ids());
        }
        show_tree_view_pair(
            ui,
            selected_baseline,
            selected_commit,
            &data.api_addr,
            data.store.clone(),
            x,
            &mut data.aspects,
            &mut data.selected_code_data,
            &mut data.long_tracking,
        );
    } else {
        if let ResultFormat::List = format {
            // utils_results_batched::show_long_result_list(ui, res);
        } else if let ResultFormat::Json = format {
        }
        todo!()
    }
}

type ComputeRes = Result<utils_results_batched::ComputeResultIdentified, super::MatchingError>;
type StreamedComputeTable = super::StreamedDataTable<Vec<String>, ComputeRes>;
fn extract_qres<'a>(
    ui: &mut egui::Ui,
    qres: Option<&'a mut QueryResults>,
) -> Option<(
    &'a mut ProjectId,
    &'a mut QueryId,
    &'a mut StreamedComputeTable,
)> {
    let Some(QueryResults {
        project: pid,
        query: qid,
        content: res,
        tab: _,
    }) = qres
    else {
        log::error!("query result not in list");
        return None;
    };
    let res = res.get_mut()?;
    if let Err(err) = res {
        ui.error_label(&format!("error {:?}", err));
        return None;
    }
    let Ok(res) = res else { unreachable!() };
    Some((pid, qid, res))
}

fn show_hunks_header(
    ui: &mut egui::Ui,
    format: &mut ResultFormat,
    fetched_commit_metadata: &mut CommitMdStore,
    selected_baseline: &mut Option<CommitId>,
    selected_commit: &mut Option<(ProjectId, CommitId)>,
) -> bool {
    let oid = &selected_commit.as_ref().unwrap().1;
    ui.label(oid.as_str());
    let Some(selected_baseline) = &selected_baseline else {
        *format = ResultFormat::Table;
        return true;
    };
    ui.label(format!("baseline: {}", selected_baseline));
    if let Some(msg) = fetched_commit_metadata
        .get(oid)
        .and_then(|x| x.as_ref().ok())
        .and_then(|x| x.message.as_ref())
    {
        ui.label("message: ");
        egui::Frame::group(ui.style()).show(ui, |ui| {
            let mut msg_lines = msg.lines();
            let mut i = 0;
            while let Some(t) = msg_lines.next() {
                ui.label(t);
                i += 1;
                if i == 3 {
                    let rem = msg_lines.count();
                    if rem > 0 {
                        ui.weak(format!("... ({rem} rem. lines)"));
                    }
                    break;
                }
            }
        });
    }
    false
}

fn compute_queries_differential_results(
    ui: &mut egui::Ui,
    pane: TabId,
    proj_id: ProjectId,
    qid: QueryId,
    data: &mut AppData,
    selected_baseline: &CommitId,
    selected_commit: &(ProjectId, CommitId),
) {
    let pid = selected_commit.0;
    if pid != proj_id {
        return;
    }
    let (repo, _c) = data.selected_code_data.get_mut(pid).unwrap();
    let language = &data.queries[qid].lang;
    let query = data.queries[qid].query.as_ref().to_string();
    wasm_rs_dbg::dbg!(&query);
    let config = match language.parse() {
        Ok(config) => config,
        Err(()) => {
            log::warn!("{} is not supported defaulting to Java", &language);
            types::Config::MavenJava
        }
    };
    let language = language.to_string();
    let commits = 2;
    let baseline = Commit {
        repo: repo.clone(),
        id: *selected_baseline,
    };
    let commit = Commit {
        repo: repo.clone(),
        id: selected_commit.1,
    };
    let max_matches = data.queries[qid].max_matches;
    let timeout = data.queries[qid].timeout;
    let precomp = data.queries[qid].precomp;
    wasm_rs_dbg::dbg!(qid, &data.queries, precomp);
    let precomp = precomp.map(|qid| &data.queries[qid]);
    let precomp = precomp.map(|p| p.query.as_ref().to_string());
    let hash = egui::util::hash((&query, *selected_baseline));
    let prom = super::remote_compute_query_differential(
        ui.ctx(),
        &data.api_addr,
        &super::ComputeConfigQueryDifferential {
            commit,
            config,
            baseline,
        },
        super::QueryContent {
            language,
            query,
            precomp,
            commits,
            max_matches,
            timeout,
        },
    );
    data.queries_differential_results = Some((pid, qid, Default::default(), pane, hash));
    let res = data.queries_differential_results.as_mut().unwrap();
    res.2.buffer(prom);
}

const B: f32 = 15.;
const H: f32 = 800.;

fn show_hunks(
    ui: &mut egui::Ui,
    fetched_files: &mut types::FetchedFiles,
    api_addr: &String,
    x: &super::DetailsResults,
    selected_commit: &(ProjectId, CommitId),
) {
    let len = x.results.len();
    egui::ScrollArea::vertical()
        .scroll_bar_visibility(egui::scroll_area::ScrollBarVisibility::AlwaysVisible)
        .show_rows(ui, H, len, |ui, cols| {
            let (mut rect, _) = ui.allocate_exact_size(
                egui::Vec2::new(ui.available_width(), H * (cols.end - cols.start) as f32),
                egui::Sense::hover(),
            );
            let top = rect.top();
            for i in cols.clone() {
                let (t, b) = rect.split_top_bottom_at_y(top + H * (i - cols.start + 1) as f32);
                rect = b;
                show_hunk(ui, fetched_files, api_addr, x, selected_commit, t, i);
            }
        });
}

fn show_hunk(
    ui: &mut egui::Ui,
    fetched_files: &mut types::FetchedFiles,
    api_addr: &String,
    x: &super::DetailsResults,
    selected_commit: &(ProjectId, CommitId),
    mut rect: egui::Rect,
    i: usize,
) {
    use std::ops::SubAssign;
    rect.bottom_mut().sub_assign(B);
    let line_pos_1 = egui::emath::GuiRounding::round_to_pixels(
        rect.left_bottom(),
        ui.painter().pixels_per_point(),
    );
    let line_pos_2 = egui::emath::GuiRounding::round_to_pixels(
        rect.right_bottom(),
        ui.painter().pixels_per_point(),
    );
    ui.painter()
        .line_segment([line_pos_1, line_pos_2], ui.visuals().window_stroke());
    rect.bottom_mut().sub_assign(B);
    let mut ui = ui.new_child(
        egui::UiBuilder::new()
            .id_salt((i, &x.results[i]))
            .max_rect(rect)
            .layout(egui::Layout::top_down(egui::Align::Min)),
    );
    ui.set_clip_rect(rect.intersect(ui.clip_rect()));
    ui.label(format!(
        "{}:{}..{}",
        x.results[i].0.file.file_path,
        x.results[i].0.range.as_ref().unwrap().start,
        x.results[i].0.range.as_ref().unwrap().end
    ));
    let after = x.results[i].1.clone();
    assert_eq!(after.file.commit.id, selected_commit.1);
    use crate::app::smells;
    smells::show_diff(
        &mut ui,
        api_addr,
        &smells::ExamplesValue {
            before: x.results[i].0.clone(),
            after,
            inserts: Default::default(),
            deletes: Default::default(),
            moves: Default::default(),
        },
        fetched_files,
    );
}

fn update_queries_differential_results(
    ui: &mut egui::Ui,
    queries: impl std::ops::Index<QueryId, Output = QueryData>,
    selected_baseline: &CommitId,
    qid: QueryId,
    differential: &mut QueriesDifferentialResults,
) -> (bool, bool) {
    if differential.2.is_waiting() {
        ui.spinner();
    }
    let new = differential.2.try_poll_with(|x| {
        x.map_err(|e| super::QueryingError::NetworkError(e))
            .and_then(|x| x.content.unwrap())
    });
    let absent = if !differential.2.is_present() && !differential.2.is_waiting() {
        true
    } else if let Some(Err(_)) = differential.2.get() {
        false
    } else {
        egui::util::hash((queries[qid].query.as_ref(), selected_baseline)) != differential.4
    };
    (absent, new)
}

fn show_tree_view_pair(
    ui: &mut egui::Ui,
    selected_baseline: &types::Oid,
    selected_commit: &(ProjectId, types::Oid),
    api_addr: &String,
    store: Arc<FetchedHyperAST>,
    x: &mut super::DetailsResults,
    aspects: &mut types::ComputeConfigAspectViews,
    selected_projects: &mut commit::SelectedProjects,
    long_tacking: &mut tracking::long_tracking::LongTracking,
) {
    use egui_addon::InteractiveSplitter;
    use tracking::long_tracking::ColView;
    use tracking::long_tracking::PlacedCode;

    let commit = selected_commit;
    let bl = &(selected_commit.0, *selected_baseline);

    let mut f = |ui: &mut egui::Ui, id_salt: (&_, &_), left_side, commit: &_| {
        let mut curr_view = ColView::default();
        (curr_view.matcheds).extend(x.results.iter_mut().enumerate().map(|(i, x)| {
            let code = if left_side { &mut x.0 } else { &mut x.1 };
            PlacedCode::new(code, i)
        }));

        ui.push_id(id_salt, |ui| {
            show_tree_view(
                ui,
                aspects,
                selected_projects,
                long_tacking,
                store.clone(),
                commit,
                api_addr,
                &mut curr_view,
            );
            ui.separator();
        })
    };

    let rect = ui.clip_rect();
    InteractiveSplitter::vertical().show(ui, |ui1, ui2| {
        ui1.set_clip_rect(ui1.max_rect().intersect(rect));
        ui2.set_clip_rect(ui2.max_rect().intersect(rect));
        f(ui2, (commit, bl), true, commit);
        f(ui1, (bl, commit), false, bl);
    });
}

fn show_tree_view(
    ui: &mut egui::Ui,
    aspects: &mut types::ComputeConfigAspectViews,
    selected_projects: &mut commit::SelectedProjects,
    long_tacking: &mut tracking::long_tracking::LongTracking,
    store: Arc<FetchedHyperAST>,
    commit: &(ProjectId, CommitId),
    api_addr: &String,
    curr_view: &mut tracking::long_tracking::ColView<'_>,
) {
    let (repo, _c) = selected_projects.get_mut(commit.0).unwrap();

    let curr_commit = Commit {
        repo: repo.clone(),
        id: commit.1,
    };
    let tree_viewer = long_tacking.tree_viewer.entry(curr_commit.clone());
    let tree_viewer = tree_viewer.or_default();
    tree_viewer.try_poll();
    let trigger = true;
    let Some(tree_viewer) = tree_viewer.get_mut() else {
        if !tree_viewer.is_waiting() {
            use crate::app::code_aspects;
            tree_viewer.buffer(code_aspects::remote_fetch_node_old(
                ui.ctx(),
                &api_addr,
                store,
                &curr_commit,
                "",
            ));
        }
        return Default::default();
    };

    let Ok(tree_viewer) = tree_viewer else {
        return Default::default();
    };
    let col = 0;
    let min_col = 0;
    let mut attacheds = vec![];
    let mut deferred_focus_scroll = None;
    tracking::long_tracking::show_tree_view(
        ui,
        min_col,
        api_addr,
        col,
        trigger,
        tree_viewer,
        curr_view,
        aspects,
        &mut attacheds,
        &mut deferred_focus_scroll,
    );

    if let Some((o, _i, mut scroll)) = deferred_focus_scroll {
        let o: f32 = o;
        // let g_o = attacheds
        //     .get(i)
        //     .and_then(|a| a.0.get(&0))
        //     .and_then(|x| x.1)
        //     .map(|p| p.min.y)
        //     .unwrap_or(ui.max_rect().height() / 2000.0);
        let g_o: f32 = 50.0;
        wasm_rs_dbg::dbg!(o, g_o);
        if (scroll.state.offset.y - (o - g_o)).abs() < 20.0 {
            scroll.state.offset = (0.0, (o - g_o)).into();
        }
        scroll.state.store(ui.ctx(), scroll.id);
    }
}
