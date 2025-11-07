use std::ops::IndexMut;

use egui::Widget;
use re_ui::{DesignTokens, UiExt};

use crate::app::{
    ProjectId, QResId,
    querying::{self, ComputeConfigQuery},
    types::{self, Commit, Config, QueriedLang},
};

use super::{QueryDataVec, QueryId, TabId, utils_results_batched::ComputeError};

impl crate::HyperApp {
    pub(crate) fn show_left_panel(&mut self, ctx: &egui::Context) {
        egui::SidePanel::left("left_panel")
            .default_width(ctx.screen_rect().width() / 6.0)
            .frame(egui::Frame {
                fill: ctx.style().visuals.panel_fill,
                ..Default::default()
            })
            .show_animated(ctx, self.show_left_panel, |ui| {
                egui::ScrollArea::vertical().show(ui, |ui| {
                    self.show_left_panel_views_props(ui);
                    // self.show_left_panel_custom_contents(ui);
                })
            });
    }

    fn show_left_panel_views_props(&mut self, ui: &mut egui::Ui) {
        for tile_id in self.tree.active_tiles() {
            let Some(&pane) = self.tree.tiles.get_pane(&tile_id) else {
                continue;
            };
            let title = self.tabs[pane].title(&self.data);
            use super::re_ui_collapse::SectionCollapsingHeader;
            SectionCollapsingHeader::with_id(ui.id().with(pane), title)
                .default_open(false)
                .show(ui, |ui| {
                    if let super::Tab::ProjectSelection() = self.tabs[pane] {
                        self.data.show_actions(ui);
                        ui.indent("proj_list", |ui| {
                            let mut span = ui.full_span();
                            span.min += 10.0;
                            ui.full_span_scope(span, |ui| self.show_repositories(ui))
                        });
                    } else if let super::Tab::QueryResults { id, format } = &mut self.tabs[pane] {
                        selection_querying_result_format(ui, format);
                    } else if let super::Tab::LocalQuery(id) = self.tabs[pane] {
                        self.show_local_query_left_panel(ui, id);
                    } else if let super::Tab::TreeAspect = self.tabs[pane] {
                        use crate::app::code_aspects;
                        let (proj_resp, commit_resp, path_resp) =
                            code_aspects::show_config(ui, &mut self.data.aspects);
                        if proj_resp.clicked() {
                            let ctx = ui.ctx();
                            self.modal_handler_proj_or_commits
                                .open_projects(code_aspects::project_modal_handler);
                        }
                        if commit_resp.clicked() {
                            let repo = &self.data.aspects.commit.repo;
                            if let Some(proj) = self.data.selected_code_data.find(repo) {
                                self.modal_handler_proj_or_commits
                                    .open_commits(proj, |data, cid| data.aspects.commit.id = cid);
                            }
                        }
                        if path_resp.changed() && !self.data.aspects.commit.id.is_empty() {
                            use crate::app::code_aspects::remote_fetch_node_old as fetch;
                            self.data.aspects_result = Some(fetch(
                                ui.ctx(),
                                &self.data.api_addr,
                                self.data.store.clone(),
                                &self.data.aspects.commit,
                                &self.data.aspects.path,
                            ));
                        }
                    } else if let super::Tab::TSG = self.tabs[pane] {
                        use crate::app::tsg;
                        let (proj_resp, commit_resp) = tsg::show_config(ui, &mut self.data.tsg);
                        if proj_resp.clicked() {
                            let ctx = ui.ctx();
                            self.modal_handler_proj_or_commits
                                .open_projects(tsg::project_modal_handler);
                        }
                    } else if let super::Tab::Smells = self.tabs[pane] {
                        use crate::app::smells;
                        let (proj_resp, commit_resp) =
                            smells::show_config(ui, &mut self.data.smells);
                        if proj_resp.clicked() {
                            let ctx = ui.ctx();
                            self.modal_handler_proj_or_commits
                                .open_projects(smells::project_modal_handler);
                        }
                    } else if let super::Tab::LongTracking = self.tabs[pane] {
                        use crate::app::long_tracking;
                        let (proj_resp, commit_resp) =
                            long_tracking::show_config(ui, &mut self.data.long_tracking);
                        if proj_resp.clicked() {
                            let ctx = ui.ctx();
                            self.modal_handler_proj_or_commits
                                .open_projects(long_tracking::project_modal_handler);
                        }
                    }
                });
        }
    }
    fn show_local_query_left_panel(&mut self, ui: &mut egui::Ui, qid: QueryId) {
        let query = &mut self.data.queries[qid];
        ui.horizontal(|ui| {
            ui.label("name: ");
            ui.text_edit_singleline(&mut query.name);
        });

        show_lang_selector(ui, qid, &mut query.lang);

        let create_q =
            show_precomp_selector(ui, qid, query.precomp.clone(), &mut self.data.queries);

        if create_q.inner.map_or(false, |x| x.clicked()) {
            let qid = self.data.queries.push(crate::app::QueryData {
                name: "precomp".to_string(),
                lang: self.data.queries[qid].lang.to_string(),
                query: egui_addon::code_editor::CodeEditor::new(
                    egui_addon::code_editor::EditorInfo::default().into(),
                    r#"translation_unit"#.to_string(),
                ),
                ..Default::default()
            });
            let query = &mut self.data.queries[qid];
            query.precomp = Some(qid);
            let tid = self.tabs.push(crate::app::Tab::LocalQuery(qid));
            let child = self.tree.tiles.insert_pane(tid);
            match self.tree.tiles.get_mut(self.tree.root.unwrap()) {
                Some(egui_tiles::Tile::Container(c)) => c.add_child(child),
                _ => todo!(),
            };
        }
        let query = &mut self.data.queries[qid];
        egui::Slider::new(&mut query.commits, 1..=100)
            .text("#commits")
            .clamping(egui::SliderClamping::Never)
            .ui(ui)
            .on_hover_text("Maximum number of commits that will be processed.");
        egui::Slider::new(&mut query.max_matches, 1..=1000)
            .text("match limit")
            .clamping(egui::SliderClamping::Never)
            .ui(ui)
            .on_hover_text("Maximum number of match per commit\n, for any of the patterns.");
        egui::Slider::new(&mut query.timeout, 1..=5000)
            .text("commit timeout")
            .clamping(egui::SliderClamping::Never)
            .ui(ui)
            .on_hover_text("Maximum time to match each commit.");
        let compute_button = ui.add_enabled(false, egui::Button::new("Compute All"));
        let q_res_ids = &mut query.results;
        if self.data.selected_code_data.commit_count() != q_res_ids.len() {
            // TODO update on new commit
            // TODO update list instead of recreating it
            let mut l = self.data.selected_code_data.project_ids().filter_map(|i| {
                let (r, mut c) = self.data.selected_code_data.get_mut(i)?;
                let c = &mut c.iter_mut();
                c.next().map(|c| {
                    let commit = types::Commit {
                        repo: r.clone(),
                        id: c.clone(),
                    };
                    (i, commit)
                })
            });
            let mut it = q_res_ids.iter();
            let mut r = vec![];
            loop {
                match (l.next(), it.next()) {
                    (None, None) => break,
                    (Some((rid, c)), None) => {
                        let qrid = self.data.queries_results.push(super::QueryResults {
                            project: rid,
                            query: qid,
                            content: Default::default(),
                            tab: TabId::INVALID,
                        });
                        r.push(qrid);
                    }
                    (None, Some(&_id)) => {
                        // nothing to do ProjectIds are valid for the duration of the session
                        // self.data.queries_results[id as usize].0 = u16::MAX;
                    }
                    (Some((i, _)), Some(&id)) => {
                        self.data.queries_results[id].project = i;
                        r.push(id);
                    }
                }
            }
            *q_res_ids = r;
        }
        let query_data = &self.data.queries[qid];
        let q_res_ids = &query_data.results;
        ui.style_mut().spacing.item_spacing = egui::vec2(3.0, 2.0);
        for &q_res_id in q_res_ids {
            if q_res_id == QResId::INVALID {
                continue;
            }
            let q_res = &mut self.data.queries_results[q_res_id];
            let Some((repo, mut c)) = self.data.selected_code_data.get_mut(q_res.project) else {
                continue;
            };
            let Some(c) = c.iter_mut().next() else {
                continue;
            };

            fn update_tiles(
                tabs: &mut crate::app::Tabs,
                tree: &mut egui_tiles::Tree<crate::app::TabId>,
                q_res: &mut super::QueryResults,
                q_res_id: QResId,
            ) {
                let tid = tabs.push(crate::app::Tab::QueryResults {
                    id: q_res_id,
                    format: super::ResultFormat::Table,
                });
                q_res.tab = tid;
                let tid = tree.tiles.insert_new(egui_tiles::Tile::Pane(tid));
                tree.move_tile_to_container(tid, tree.root().unwrap(), usize::MAX, false);
            }
            let compute_button = ui
                .horizontal(|ui| {
                    ui.style_mut().spacing.button_padding = egui::vec2(3.0, 2.0);
                    let w = &mut ui.style_mut().visuals.widgets;
                    let d = egui::Color32::DARK_GREEN;
                    let _n = w.hovered.weak_bg_fill;
                    let n = egui::Color32::GREEN;
                    w.open.weak_bg_fill = d;
                    w.active.weak_bg_fill = d;
                    w.hovered.weak_bg_fill = n;
                    w.inactive.weak_bg_fill = d;
                    let q_res = &mut self.data.queries_results[q_res_id];
                    let compute_button;
                    if let Some(content) = q_res.content.get() {
                        match content {
                            Ok(content) => {
                                let rows = content.rows.lock().unwrap();
                                if rows.2 {
                                    if let [Err(err)] = rows.1.as_slice() {
                                        let d = egui::Color32::DARK_RED;
                                        let n = egui::Color32::RED;
                                        let w = &mut ui.style_mut().visuals.widgets;
                                        w.open.weak_bg_fill = d;
                                        w.active.weak_bg_fill = d;
                                        w.hovered.weak_bg_fill = n;
                                        w.inactive.weak_bg_fill = d;
                                        compute_button =
                                            ui.add(egui::Button::new("⚠")).on_hover_ui(|ui| {
                                                ui.label(format!(
                                                    "streamed results {}/{}",
                                                    rows.1.len(),
                                                    content.commits
                                                ));
                                                ui.label(format!("{:?}", err));
                                            });
                                    } else if rows.1.len() == content.commits {
                                        compute_button =
                                            ui.add(egui::Button::new("↺")).on_hover_ui(|ui| {
                                                ui.label(format!(
                                                    "streamed results {}/{}",
                                                    rows.1.len(),
                                                    content.commits
                                                ));
                                            });
                                    } else {
                                        let rect = ui.spinner().rect;
                                        compute_button = ui
                                            .interact(
                                                rect,
                                                ui.id().with(q_res.project),
                                                egui::Sense::click(),
                                            )
                                            .on_hover_text(format!(
                                                "waiting for the rest of the entries: {}/{}",
                                                rows.1.len(),
                                                content.commits
                                            ));
                                    }
                                } else if rows.1.len() == content.commits {
                                    compute_button =
                                        ui.add(egui::Button::new("↺")).on_hover_ui(|ui| {
                                            ui.label(format!(
                                                "streamed results {}/{}",
                                                rows.1.len(),
                                                content.commits
                                            ));
                                        });
                                } else {
                                    let d = egui::Color32::DARK_GRAY;
                                    let n = egui::Color32::GRAY;
                                    let w = &mut ui.style_mut().visuals.widgets;
                                    w.open.weak_bg_fill = d;
                                    w.active.weak_bg_fill = d;
                                    w.hovered.weak_bg_fill = n;
                                    w.inactive.weak_bg_fill = d;
                                    compute_button =
                                        ui.add(egui::Button::new("⚠")).on_hover_ui(|ui| {
                                            ui.label(format!(
                                                "interupted at {}/{}",
                                                rows.1.len(),
                                                content.commits
                                            ));
                                        });
                                }
                            }
                            Err(err) => {
                                let d = egui::Color32::DARK_RED;
                                let n = egui::Color32::RED;
                                let w = &mut ui.style_mut().visuals.widgets;
                                w.open.weak_bg_fill = d;
                                w.active.weak_bg_fill = d;
                                w.hovered.weak_bg_fill = n;
                                w.inactive.weak_bg_fill = d;
                                compute_button = ui
                                    .add(egui::Button::new("⚠"))
                                    .on_hover_text(format!("{}\n{}", err.head(), err.content()));
                            }
                        }
                        ui.label(&format!(
                            "{}/{}/{}",
                            repo.user,
                            repo.name,
                            &c[..6.min(c.len())]
                        ));
                        if q_res.content.is_waiting() {
                            ui.spinner();
                            let synced = Self::sync_query_results(q_res);
                            if let Ok(true) = synced {
                                self.save_interval = std::time::Duration::ZERO;
                                if q_res.tab == TabId::INVALID {
                                    update_tiles(&mut self.tabs, &mut self.tree, q_res, q_res_id);
                                }
                            }
                        }
                    } else {
                        let q_res = &mut self.data.queries_results[q_res_id];
                        let synced = Self::sync_query_results(q_res);
                        if let Err(Some(err)) = &synced {
                            let d = egui::Color32::DARK_RED;
                            let n = egui::Color32::RED;
                            let w = &mut ui.style_mut().visuals.widgets;
                            w.open.weak_bg_fill = d;
                            w.active.weak_bg_fill = d;
                            w.hovered.weak_bg_fill = n;
                            w.inactive.weak_bg_fill = d;
                            compute_button = ui.add(egui::Button::new("⚠")).on_hover_text(err);
                        } else if let Err(None) = &synced {
                            compute_button = ui.spinner();
                        } else if let Ok(false) = &synced {
                            compute_button = ui.add(egui::Button::new("⏵"));
                        }
                        // finally, if unassigned pane then add it
                        else {
                            self.save_interval = std::time::Duration::ZERO;
                            if q_res.tab == TabId::INVALID {
                                update_tiles(&mut self.tabs, &mut self.tree, q_res, q_res_id);
                            }
                            compute_button = ui.add(egui::Button::new("⏵"));
                        }
                        ui.label(&format!(
                            "{}/{}/{}",
                            repo.user,
                            repo.name,
                            &c[..6.min(c.len())]
                        ));
                        let query_data = &self.data.queries[q_res.query];
                        // let current_lang = &query_data.lang;
                        let w = &mut ui.style_mut().visuals.widgets;
                        w.hovered.weak_bg_fill = _n;
                        ui.selectable_label(false, "..").on_hover_ui(|ui| {
                            ui.horizontal(|ui| {
                                ui.label("name:");
                                ui.label(&query_data.name)
                            });
                            ui.horizontal(|ui| {
                                ui.label("lang:");
                                ui.label(&query_data.lang)
                            });
                        });
                        // egui::ComboBox::new(("Lang", q_res.query), "Lang")
                        //     .selected_text(query_data.lang.as_str())
                        //     .show_ui(ui, |ui| {
                        //         lang_selection(ui, current_lang, "Cpp", &mut new_lang, q_res.query);
                        //         lang_selection(
                        //             ui,
                        //             current_lang,
                        //             "Java",
                        //             &mut new_lang,
                        //             q_res.query,
                        //         );
                        //     });
                    };
                    compute_button
                })
                .inner;
            let q_res = &mut self.data.queries_results[q_res_id];

            if compute_button.clicked() {
                let (repo, mut commit_slice) =
                    self.data.selected_code_data.get_mut(q_res.project).unwrap();
                let query_data = &self.data.queries[q_res.query];
                let language = query_data.lang.to_string();
                let query = query_data.query.as_ref().to_string();
                wasm_rs_dbg::dbg!(&query);
                let commits = query_data.commits as usize;
                let commit = Commit {
                    repo: repo.clone(),
                    id: commit_slice.iter_mut().next().cloned().unwrap(),
                };
                let max_matches = query_data.max_matches;
                let timeout = query_data.timeout;
                let precomp = query_data.precomp.clone().map(|id| &self.data.queries[id]);
                let precomp = precomp.map(|p| p.query.as_ref().to_string());
                let prom = querying::remote_compute_query_aux(
                    ui.ctx(),
                    &self.data.api_addr,
                    &ComputeConfigQuery {
                        commit,
                        config: Config::MavenJava,
                        len: commits,
                    },
                    querying::QueryContent {
                        language,
                        query,
                        commits,
                        max_matches,
                        timeout,
                        precomp,
                    },
                    commit_slice.iter_mut().skip(1).map(|x| x.to_string()),
                );
                q_res.content.buffer(prom);
            }
        }
        if compute_button.clicked() {
            todo!()
            // *loc_rem = LocalOrRemote::Remote(querying::remote_compute_query_aux(
            //     ui.ctx(),
            //     &self.data.api_addr,
            //     &self.data.query,
            //     &mut self.data.querying_context,
            // ));
        }
    }

    /// returns Ok(true) if it is now local
    fn sync_query_results(
        q_res: &mut super::QueryResults,
        // results_per_commit: &mut super::ResultsPerCommit,
    ) -> Result<bool, Option<String>> {
        if q_res.content.is_waiting() {
            if q_res.content.try_poll_with(|x| {
                // x.map_err(|x| querying::QueryingError::NetworkError(x))?
                //     .content
                //     .ok_or(querying::QueryingError::NetworkError(
                //         "content was not deserialized or empty".to_string(),
                //     ))?
                //     .map(|x| x.into())
                x
            }) {
                Ok(true)
            } else {
                Err(None)
            }
        } else {
            Ok(false)
        }

        // let a = std::mem::take(&mut q_res.2);
        // match a {
        //     LocalOrRemote::Remote(prom) => {
        //         match prom.try_take() {
        //             Ok(Ok(r)) => {
        //                 if let Some(r) = r.content {
        //                     if let Ok(r) = &r {
        //                         // Self::update_results_per_commit(results_per_commit, r);
        //                     }
        //                     q_res.2 = LocalOrRemote::Local(r);
        //                     Ok(true)
        //                 } else {
        //                     Ok(false)
        //                 }
        //             }
        //             Ok(Err(err)) => {
        //                 log::error!("{}", err);
        //                 Err(Some(format!("error: {}", err)))
        //             }
        //             Err(prom) => {
        //                 q_res.2 = LocalOrRemote::Remote(prom);
        //                 Err(None)
        //             }
        //         }
        //     }
        //     LocalOrRemote::None => Ok(false),
        //     _ => Ok(false),
        // }
    }

    fn show_left_panel_custom_contents(&mut self, ui: &mut egui::Ui) {
        egui::TopBottomPanel::top("left_panel_top_bar")
            .min_height(3.0 * re_ui::DesignTokens::title_bar_height())
            .frame(egui::Frame {
                inner_margin: egui::Margin::symmetric(re_ui::DesignTokens::view_padding(), 0),
                ..Default::default()
            })
            .show_inside(ui, |ui| {
                ui.horizontal(|ui| {
                    ui.strong("Actions");
                    ui.horizontal_wrapped(|ui| {
                        self.data.show_actions(ui);
                    });
                })
            });

        // list_item::list_item_scope(ui, "testing stuff", |ui| {
        //     ui.list_item().show_hierarchical(
        //         ui,
        //         list_item::PropertyContent::new("Text (editable)")
        //             .value_text_mut(&mut self.latest_cmd),
        //     );
        //     ui.list_item().show_hierarchical(
        //         ui,
        //         list_item::PropertyContent::new("Color")
        //             .with_icon(&re_ui::icons::SPACE_VIEW_TEXT)
        //             .action_button(&re_ui::icons::ADD, || {
        //                 // re_log::warn!("Add button clicked");
        //             })
        //             .value_color_mut(&mut egui::Color32::RED.to_array()),
        //     );
        //     ui.list_item().show_hierarchical(
        //         ui,
        //         list_item::PropertyContent::new("Bool (editable)")
        //             .value_bool_mut(&mut self.dummy_bool),
        //     );
        // });

        egui::ScrollArea::vertical()
            .auto_shrink([false; 2])
            .show(ui, |ui| {
                egui::Frame {
                    inner_margin: egui::Margin::same(re_ui::DesignTokens::view_padding()),
                    ..Default::default()
                }
                .show(ui, |ui| self.left_panel_mid_section_ui(ui));
            });

        egui::ScrollArea::vertical()
            .auto_shrink([false; 2])
            .show(ui, |ui| {
                egui::Frame {
                    inner_margin: egui::Margin::same(re_ui::DesignTokens::view_padding()),
                    ..Default::default()
                }
                .show(ui, |ui| self.left_panel_bottom_section_ui(ui));
            });
    }

    pub(crate) fn bottom_panel(&mut self, ctx: &egui::Context) {
        let mut frame_style = DesignTokens::bottom_panel_frame();
        if !ctx.style().visuals.dark_mode {
            frame_style.fill = egui::Visuals::light().window_fill;
            frame_style.stroke = egui::Visuals::light().window_stroke;
            frame_style.shadow.color = egui::Visuals::light().window_shadow.color;
        }
        egui::TopBottomPanel::bottom("bottom_panel")
            .resizable(true)
            .frame(frame_style)
            .show_animated(ctx, self.show_bottom_panel, |ui| {
                let view = &mut self.bottom_view;
                ui.horizontal(|ui| {
                    ui.strong("Bottom panel");
                    ui.add_space(20.0);
                    egui::ComboBox::from_label("View")
                        .selected_text(view.as_ref())
                        .show_ui(ui, |ui| {
                            ui.selectable_value(view, super::BottomPanelConfig::Commits, "Commits");
                            ui.selectable_value(
                                view,
                                super::BottomPanelConfig::CommitsTime,
                                "Commits Time",
                            );
                            ui.add_enabled_ui(false, |ui| {
                                ui.selectable_value(
                                    view,
                                    super::BottomPanelConfig::Temporal,
                                    "Temporal",
                                );
                                ui.selectable_value(
                                    view,
                                    super::BottomPanelConfig::Temporal,
                                    "Commit Metadata",
                                );
                            })
                        });
                    ui.add_space(20.0);
                    const MAX: i64 = 60 * 60 * 24 * 365;
                    const MIN: i64 = 60 * 60 * 24 * 1;
                    let resp =
                        &egui::widgets::Slider::new(&mut self.data.offset_fetch, 0..=MAX).ui(ui);
                    if resp.drag_stopped() {
                        self.save_interval = std::time::Duration::ZERO;
                    }
                    let resp = &egui::widgets::Slider::new(&mut self.data.max_fetch, MIN..=MAX)
                        .clamping(egui::SliderClamping::Never)
                        .custom_formatter(|n, _| {
                            let n = n as i64;
                            let days = n / (60 * 60 * 24);
                            let hours = (n / (60 * 60)) % 24;
                            let mins = (n / 60) % 60;
                            let secs = n % 60;
                            format!("{days:02}:{hours:02}:{mins:02}:{secs:02}")
                        })
                        .custom_parser(|s| {
                            let parts: Vec<&str> = s.split(':').collect();
                            if parts.len() == 4 {
                                parts[0]
                                    .parse::<i64>()
                                    .and_then(|d| {
                                        parts[1].parse::<i64>().and_then(|h| {
                                            parts[2].parse::<i64>().and_then(|m| {
                                                parts[3].parse::<i64>().map(|s| {
                                                    ((d * 60 * 60 * 24)
                                                        + (h * 60 * 60)
                                                        + (m * 60)
                                                        + s)
                                                        as f64
                                                })
                                            })
                                        })
                                    })
                                    .ok()
                            } else {
                                None
                            }
                        })
                        .ui(ui);
                    if resp.drag_stopped() {
                        self.save_interval = std::time::Duration::ZERO;
                    }
                });
                if *view == super::BottomPanelConfig::Commits {
                    egui::Frame::menu(ui.style()).show(ui, |ui| {
                        egui::ScrollArea::both().show(ui, |ui| {
                            ui.add(egui::Label::new("---o--------".repeat(50)).extend());
                            ui.add(egui::Label::new("------------".repeat(50)).extend());
                            ui.add(egui::Label::new("-------o----".repeat(50)).extend());
                            ui.add(egui::Label::new("-o----------".repeat(50)).extend());
                        });
                    });
                } else if *view == super::BottomPanelConfig::CommitsTime {
                    egui::ScrollArea::both()
                        .auto_shrink([false; 2])
                        .show(ui, |ui| {
                            egui::Frame::menu(ui.style()).show(ui, |ui| {
                                let timed = true;
                                if timed {
                                    self.print_commit_graph_timed(ui);
                                } else {
                                    self.print_commit_graph(ui, ctx);
                                }
                            });
                        });
                }
            });
    }
}
fn show_precomp_selector(
    ui: &mut egui::Ui,
    qid: QueryId,
    precomp: Option<QueryId>,
    queries: &mut QueryDataVec,
) -> egui::InnerResponse<Option<egui::Response>> {
    let sel_precomp = if let Some(id) = precomp {
        queries[id].name.to_string()
    } else {
        "<none>".to_string()
    };

    let mut create_q = egui::ComboBox::new((ui.id(), "Precomp", qid), "Precomp")
        .selected_text(sel_precomp)
        .show_ui(ui, |ui| {
            let create_q = ui.button("new");
            let mut precomp = None;
            for (i, q) in queries.enumerate() {
                let v = &q.name;
                if ui.selectable_label(i == qid, v).clicked() {
                    if i == qid {
                        precomp = Some(i);
                    }
                }
            }
            let query = &mut queries[qid];
            if let Some(precomp) = precomp {
                query.precomp = Some(precomp as QueryId);
            }
            if ui
                .selectable_label(query.precomp.is_none(), "<none>")
                .clicked()
            {
                query.precomp = None;
            }
            create_q
        });
    create_q
}

fn show_lang_selector(ui: &mut egui::Ui, id: impl std::hash::Hash, lang: &mut String) {
    egui::ComboBox::new((ui.id(), "Lang", id), "Lang")
        .selected_text(lang.as_str())
        .show_ui(ui, |ui| {
            let v = "Cpp";
            if ui.selectable_label(v == lang, v).clicked() {
                if lang != v {
                    *lang = v.to_string()
                }
            }
            let v = "Java";
            if ui.selectable_label(v == lang, v).clicked() {
                if lang != v {
                    *lang = v.to_string()
                }
            }
        });
}

fn selection_querying_result_format(ui: &mut egui::Ui, format: &mut super::ResultFormat) {
    egui::ComboBox::from_label("Commits")
        .selected_text(format.as_ref())
        .show_ui(ui, |ui| {
            ui.selectable_value(format, super::ResultFormat::List, "List");
            ui.selectable_value(format, super::ResultFormat::Table, "Table");
            if format == &super::ResultFormat::Tree {
                ui.selectable_value(format, super::ResultFormat::Hunks, "Hunks");
            }
            if format == &super::ResultFormat::Hunks {
                ui.selectable_value(format, super::ResultFormat::Tree, "Tree");
            }
            ui.add_enabled_ui(false, |ui| {
                ui.selectable_value(format, super::ResultFormat::Json, "Json");
            });
        });
}

fn lang_selection<'a, T>(
    ui: &mut egui::Ui,
    current_lang: &str,
    selected_value: &'a str,
    new_lang: &mut Option<(&'a str, T)>,
    payload: T,
) {
    let same = current_lang == selected_value;
    if ui.selectable_label(same, selected_value).clicked() {
        if !same {
            *new_lang = Some((selected_value, payload));
        }
    }
}
