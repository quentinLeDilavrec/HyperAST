use crate::app::*;
use commit::CommitSlice;
use re_ui::list_item;
use utils_egui::MyUiExt as _;
mod bars;
mod panels;

// TODO show list of groups of actions, ok to type erase, just mut borrow AppData and an ui
trait AppActions {
    fn ui(self, data: &mut AppData, ui: &mut egui::Ui) -> egui::Response;
}
// impl<F> Widget for F
// where
//     F: FnOnce(&mut Ui) -> Response,
// {
//     fn ui(self, ui: &mut Ui) -> Response {
//         self(ui)
//     }
// }

impl<F> AppActions for F
where
    F: FnOnce(&mut AppData, &mut egui::Ui) -> egui::Response,
{
    fn ui(self, data: &mut AppData, ui: &mut egui::Ui) -> egui::Response {
        self(data, ui)
    }
}

impl super::AppData {
    pub(crate) fn show_actions(&mut self, ui: &mut egui::Ui) -> egui::Response {
        ACTIONS.into_iter().fold(
            ui.interact(egui::Rect::NOTHING, ui.next_auto_id(), egui::Sense::empty()),
            |acc, f| acc.union(f.ui(self, ui)),
        )
    }
}

impl super::HyperApp {
    fn show_repositories(&mut self, ui: &mut egui::Ui) {
        let label = "Repositories";

        let id = ui.make_persistent_id(label);
        let mut paste_trigered = false;
        let content = list_item::LabelContent::new(label)
            .truncate(true)
            .with_buttons(|ui| {
                ui.add_space(3.0);
                let button = egui::Button::new("📋");
                if ui.add_enabled(true, button).clicked() {
                    paste_trigered = true;
                }
                let button = egui::ImageButton::new(
                    re_ui::icons::EXTERNAL_LINK
                        .as_image()
                        .fit_to_exact_size(egui::Vec2::splat(12.0))
                        .tint(ui.visuals().widgets.inactive.fg_stroke.color),
                );
                let resp = ui.add_enabled(true, button);
                if resp.clicked() {
                    self.modal_handler_proj_or_commits
                        .open_projects(|_, pid| pid);
                }
                resp
            })
            .always_show_buttons(true);

        let force_background = if ui.visuals().dark_mode {
            re_ui::design_tokens_of(egui::Theme::Dark).section_header_color
        } else {
            ui.visuals().widgets.active.bg_fill
        };

        let resp = list_item::list_item_scope(ui, id, |ui| {
            list_item::ListItem::new()
                .interactive(true)
                .force_background(force_background)
                .show_hierarchical_with_children(ui, id, true, content, |ui| {
                    //TODO(ab): this space is not desirable when the content actually is list items
                    // ui.add_space(4.0); // Add space only if there is a body to make minimized headers stick together.
                    for i in self.data.selected_code_data.project_ids() {
                        let Some((r, commits)) = self.data.selected_code_data.get_mut(i) else {
                            continue;
                        };

                        let label = format!("github.com/{}/{}", r.user, r.name);

                        let label = egui::RichText::new(label)
                            .size(10.0)
                            .line_height(Some(10.0));
                        show_repo_item_buttons(commits, label, ui, ui.id().with(i));
                    }
                    // ui.add_space(4.0); // Same here
                })
        });

        if resp.item_response.clicked() {
            // `show_hierarchical_with_children_unindented` already toggles on double-click,
            // but we are _only_ a collapsing header, so we should also toggle on normal click:
            if let Some(mut state) = egui::collapsing_header::CollapsingState::load(ui.ctx(), id) {
                state.toggle(ui);
                state.store(ui.ctx());
            }
        };

        let paste = utils::prepare_paste(ui, paste_trigered, &mut self.capture_clip_into_repos);
        let Some(paste) = paste else {
            return;
        };
        self.capture_clip_into_repos = false;
        Self::parse_pasted_repositories(paste, &mut self.notifs, &mut self.data);
    }

    fn parse_pasted_repositories(paste: String, notifs: &mut NotificationUi, data: &mut AppData) {
        if !paste.contains("\n") {
            Self::parse_pasted_repository(paste, notifs, data);
            return;
        }
        let mut acc = vec![];
        let mut bad = 0;
        for paste in paste.split("\n") {
            match commit::validate_pasted_project_url(&paste) {
                Ok(x) => {
                    acc.push(Ok(x));
                }
                Err(err) => {
                    bad += 1;
                    log::warn!("Wrong input from clipboard: {}:\n{}", err, paste);
                    acc.push(Err(format!(
                        "Wrong input from clipboard: {}:\n{}",
                        err, paste
                    )));
                }
            }
        }
        if bad == 0 {
            for s in acc.chunks(5) {
                let f = |(r, cs): &(Repo, Vec<CommitId>)| {
                    if cs.is_empty() {
                        format!("github.com/{}/{}", r.user, r.name)
                    } else {
                        cs.iter()
                            .map(|c| format!("\n\tgithub.com/{}/{}/{}", r.user, r.name, c))
                            .collect()
                    }
                };
                let text: String = s
                    .into_iter()
                    .filter_map(|x| x.as_ref().ok())
                    .map(f)
                    .collect();
                notifs.success(format!("Succesfully added: {}", text));
            }
            for x in acc {
                let (repo, commits) = x.unwrap();
                data.selected_code_data.add(repo, commits);
            }
        } else if bad == acc.len() {
            notifs.add_log(re_log::LogMsg {
                level: log::Level::Error,
                target: format!("clipboard"),
                msg: format!("Wrong input from clipboard:\n{}", paste),
            });
        // } else if bad <= 2 && bad * 4 <= acc.len() { // TODO later if annoying
        } else {
            let good: Vec<_> = (acc.into_iter().enumerate())
                .filter_map(|(i, x)| x.ok().map(|_| i))
                .collect();
            notifs.add_log(re_log::LogMsg {
                level: log::Level::Error,
                target: format!("clipboard"),
                msg: format!(
                    "{bad} Wrong inputs from clipboard but {:?} could be accepted:\n{}",
                    good, paste
                ),
            });
        }
    }

    fn parse_pasted_repository(paste: String, notifs: &mut NotificationUi, data: &mut AppData) {
        let result = commit::validate_pasted_project_url(&paste);
        let msg = |repo: &Repo, commits: &[CommitId]| {
            let msg = format!("Successfully added github.com/{}/{}", repo.user, repo.name);
            if commits.is_empty() {
                msg
            } else if commits.len() == 1 {
                format!("{msg}/{}", commits[0])
            } else {
                let commits: String = commits.iter().map(|c| format!("\n{}", c)).collect();
                format!("{msg}{}", commits)
            }
        };
        match result {
            Ok((repo, commits)) => {
                notifs.success(msg(&repo, &commits));
                data.selected_code_data.add(repo, commits);
            }
            Err(err) => {
                log::warn!("Wrong input from clipboard: {}:\n{}", err, paste);
                notifs.add_log(re_log::LogMsg {
                    level: log::Level::Warn,
                    target: format!("clipboard"),
                    msg: format!("Wrong input from clipboard: {}:\n{}", err, paste),
                });
            }
        }
    }
}

pub(crate) fn show_repo_item_buttons(
    mut commits: CommitSlice<'_>,
    label: impl Into<egui::WidgetText>,
    ui: &mut egui::Ui,
    id: egui::Id,
) -> list_item::ShowCollapsingResponse<()> {
    let button_menu = |ui: &mut egui::Ui| {
        let button = egui::Button::new("commit");
        let button = ui.add(button);
        if button.clicked() {
            commits.push("".into());
        }
        let popup_contents = |ui: &mut egui::Ui| {
            let commit_id = commits.last_mut().unwrap();
            let text = &mut commit_id.to_string();
            let singleline = &ui.text_edit_singleline(text);
            if button.clicked() {
                singleline.request_focus()
            }
            if singleline.changed() && singleline.lost_focus() {
                if text.is_empty() {
                    commits.pop();
                } else {
                    *commit_id = text.parse().unwrap();
                }
            } else if singleline.changed() {
                *commit_id = text.parse().unwrap();
            }
            if singleline.lost_focus() {
                ui.close();
            }
        };
        egui::Popup::from_toggle_button_response(&button).show(popup_contents);
        let button = egui::Button::new("branch");
        if ui.add_enabled(false, button).clicked() {
            wasm_rs_dbg::dbg!("TODO add branch");
        }
        let button = egui::Button::new("commit range");
        if ui.add_enabled(false, button).clicked() {
            wasm_rs_dbg::dbg!("TODO add commit range");
        }
    };
    let add_button = egui::Button::image(
        re_ui::icons::ADD
            .as_image()
            .fit_to_exact_size(egui::Vec2::splat(12.0)),
    );
    let content = list_item::LabelContent::new(label)
        .with_buttons(|ui| {
            ui.add_space(4.0);
            ui.spacing_mut().item_spacing = egui::Vec2::ZERO;

            let add_button = ui.add(add_button);
            egui::Popup::from_toggle_button_response(&add_button).show(button_menu);
            add_button
        })
        .always_show_buttons(true);

    let height: f32 = 16.0;
    let response = _show_repo_item_header(ui, content, id, height);
    show_commits_items(ui, response, commits, id, true)
}

pub(crate) fn _show_repo_item_header(
    ui: &mut egui::Ui,
    content: impl list_item::ListItemContent,
    id: egui::Id,
    height: f32,
) -> list_item::ShowCollapsingResponse<()> {
    let list = list_item::ListItem::new()
        .interactive(true)
        .with_height(height);
    list_item::list_item_scope(ui, id, |ui| {
        list.show_hierarchical_with_children(ui, id, true, content, |_| ())
    })
}

pub(crate) fn show_commits_items(
    ui: &mut egui::Ui,
    response: list_item::ShowCollapsingResponse<()>,
    commits: CommitSlice<'_>,
    id: egui::Id,
    indented: bool,
) -> list_item::ShowCollapsingResponse<()> {
    let height = 10.0;
    let mut state = egui::collapsing_header::CollapsingState::load(ui.ctx(), id).unwrap();
    let mut span = ui.full_span().shrink(height * 0.8);
    span.min = span.max.min(span.min + height);
    ui.full_span_scope(span, |ui| {
        if indented {
            ui.spacing_mut().indent = re_ui::design_tokens_of(egui::Theme::Dark).small_icon_size.x
                + re_ui::design_tokens_of(egui::Theme::Dark).text_to_icon_padding();
            state.show_body_indented(&response.item_response, ui, |ui| {
                show_commit_list(ui, commits, height)
            })
        } else {
            state.show_body_unindented(ui, |ui| show_commit_list(ui, commits, height))
        }
    });

    if response.item_response.clicked() {
        // `show_hierarchical_with_children_unindented` already toggles on double-click,
        // but we are _only_ a collapsing header, so we should also toggle on normal click:
        if let Some(mut state) = egui::collapsing_header::CollapsingState::load(ui.ctx(), id) {
            state.toggle(ui);
            state.store(ui.ctx());
        }
    }
    response
}

fn show_commit_list(ui: &mut egui::Ui, mut commits: CommitSlice<'_>, height: f32) {
    ui.add_space(height * 0.5);
    let button_size = egui::Vec2::splat(height);
    let button_tint = ui.visuals().widgets.inactive.fg_stroke.color;
    let remove_button = egui::ImageButton::new(
        re_ui::icons::REMOVE
            .as_image()
            .fit_to_exact_size(button_size),
    )
    .tint(button_tint);
    list_item::list_item_scope(ui, ui.id().with("commits"), |ui| {
        let mut rm = None;
        for (i, oid) in commits.iter_mut().enumerate() {
            let text = egui::RichText::new(oid.as_str())
                .size(height * 0.8)
                .line_height(Some(height * 0.8));
            let buttons = |ui: &mut egui::Ui| {
                let resp = ui.add(remove_button.clone());
                if resp.clicked() {
                    rm = Some(i);
                }
                resp
            };
            let content = list_item::LabelContent::new(text).with_buttons(buttons);
            let resp = list_item::ListItem::new()
                .with_height(height)
                .show_flat(ui, content);
            let popup_contents = |ui: &mut egui::Ui| {
                let singleline = &ui.text_edit_singleline(&mut oid.tb());
                if resp.clicked() {
                    singleline.request_focus()
                }
                if singleline.lost_focus() {
                    ui.close();
                }
            };
            egui::Popup::from_toggle_button_response(&resp).show(popup_contents);
        }
        if let Some(j) = rm {
            egui::Popup::close_all(ui.ctx());
            commits.remove(j);
        }
    });
    ui.add_space(height * 0.5);
}

impl super::HyperApp {
    /// Show recent text log messages to the user as toast notifications.
    pub fn show_text_logs_as_notifications(&mut self) {
        // while let Ok(re_log::LogMsg {
        //     level,
        //     target: _,
        //     msg,
        // }) = self.text_log_rx.try_recv()
        // {
        //     let kind = match level {
        //         re_log::Level::Error => toasts::ToastKind::Error,
        //         re_log::Level::Warn => toasts::ToastKind::Warning,
        //         re_log::Level::Info => toasts::ToastKind::Info,
        //         re_log::Level::Debug | re_log::Level::Trace => {
        //             continue; // too spammy
        //         }
        //     };

        //     self.toasts.add(re_log::LogMsg {
        //         kind,
        //         text: msg,
        //         options: toasts::ToastOptions::with_ttl_in_seconds(4.0),
        //     });
        // }
    }
}

use super::AppData;

impl super::HyperApp {
    pub(crate) fn old_ui(&mut self, ctx: &egui::Context) {
        if false {
            let mut trigger_compute = false;
            let Self {
                selected,
                data:
                    AppData {
                        api_addr,
                        scripting_context,
                        querying_context,
                        tsg_context,
                        smells_context: _,
                        languages: _,
                        single,
                        query,
                        tsg,
                        smells,
                        multi,
                        diff,
                        tracking,
                        aspects,
                        compute_single_result,
                        querying_result,
                        tsg_result,
                        smells_result,
                        smells_diffs_result,
                        fetched_files,
                        tracking_result,
                        aspects_result,
                        store,
                        long_tracking,
                        ..
                    },
                ..
            } = self;
            egui::SidePanel::left("side_panel")
                .width_range(
                    ctx.available_rect().width() * 0.1..=ctx.available_rect().width() * 0.9,
                )
                .show(ctx, |ui| {
                    ui.vertical_centered(|ui| {
                        ui.label(
                            egui::RichText::new("Processing API for HyperAST")
                                .heading()
                                .size(25.0),
                        );
                    });
                    egui::widgets::global_theme_preference_switch(ui);

                    egui::ScrollArea::vertical().show(ui, |ui| {
                        ui.add_space(20.0);
                        show_menu(ui, selected, &single_repo::WANTED, |ui| {
                            single_repo::show_config(ui, single)
                        });
                        ui.separator();
                        show_menu(ui, selected, &querying::WANTED, |ui| {
                            querying::show_config(ui, query)
                        });
                        ui.separator();
                        show_menu(ui, selected, &tsg::WANTED, |ui| tsg::show_config(ui, tsg));
                        ui.separator();
                        show_menu(ui, selected, &smells::WANTED, |ui| {
                            smells::show_config(ui, smells)
                        });

                        ui.separator();
                        ui.add_enabled_ui(false, |ui| {
                            show_multi_repo(ui, selected, multi);
                            ui.wip(Some(" soon available"));
                        });
                        ui.separator();
                        ui.add_enabled_ui(false, |ui| {
                            show_diff_menu(ui, selected, diff);
                            ui.wip(Some(" soon available"));
                        });
                        ui.separator();
                        // ui.add_enabled_ui(false, |ui| {
                        show_menu(ui, selected, &code_tracking::WANTED, |ui| {
                            code_tracking::show_config(ui, tracking, tracking_result)
                        });
                        // ui.wip(Some(" soon available"));
                        // });
                        ui.separator();
                        show_menu(ui, selected, &long_tracking::WANTED, |ui| {
                            long_tracking::show_config(ui, long_tracking)
                        });
                        ui.separator();
                        show_menu(ui, selected, &code_aspects::WANTED, |ui| {
                            code_aspects::show_config(ui, aspects)
                        });
                    });

                    ui.with_layout(egui::Layout::bottom_up(egui::Align::LEFT), |ui| {
                        ui.horizontal(|ui| {
                            ui.spacing_mut().item_spacing.x = 0.0;
                            ui.label("powered by ");
                            ui.hyperlink_to("egui", "https://github.com/emilk/egui");
                            ui.label(" and ");
                            ui.hyperlink_to(
                                "eframe",
                                "https://github.com/emilk/egui/tree/master/crates/eframe",
                            );
                            ui.label(".");
                        });
                    });
                });
            // TODO change layout with window ratio
            // ui.ctx().screen_rect()
            if *selected == types::SelectedConfig::Single {
                egui::CentralPanel::default().show(ctx, |ui| {
                    single_repo::show_single_repo(
                        ui,
                        api_addr,
                        single,
                        scripting_context,
                        &mut trigger_compute,
                        compute_single_result,
                    );
                });
            } else if *selected == types::SelectedConfig::Querying {
                egui::CentralPanel::default().show(ctx, |ui| {
                    querying::show_querying(
                        ui,
                        api_addr,
                        query,
                        querying_context,
                        &mut trigger_compute,
                        querying_result,
                    );
                });
            } else if *selected == types::SelectedConfig::Tsg {
                egui::CentralPanel::default().show(ctx, |ui| {
                    tsg::show_querying(
                        ui,
                        api_addr,
                        tsg,
                        tsg_context,
                        &mut trigger_compute,
                        tsg_result,
                    );
                });
            } else if *selected == types::SelectedConfig::Smells {
                egui::CentralPanel::default().show(ctx, |ui| {
                    smells::show_central_panel(
                        ui,
                        api_addr,
                        smells,
                        smells_result,
                        smells_diffs_result,
                        fetched_files,
                    );
                });
            } else if *selected == types::SelectedConfig::Tracking {
                egui::CentralPanel::default().show(ctx, |ui| {
                    code_tracking::show_code_tracking_results(
                        ui,
                        &api_addr,
                        tracking,
                        tracking_result,
                        fetched_files,
                        ctx,
                    );
                });
            } else if *selected == types::SelectedConfig::LongTracking {
                egui::CentralPanel::default()
                    .frame(egui::Frame::central_panel(&ctx.style()).inner_margin(2.0))
                    .show(ctx, |ui| {
                        long_tracking::show_results(
                            ui,
                            &api_addr,
                            aspects,
                            store.clone(),
                            long_tracking,
                            fetched_files,
                        );
                    });
            } else if *selected == types::SelectedConfig::Aspects {
                egui::CentralPanel::default().show(ctx, |ui| {
                    if let Some(aspects_result) = aspects_result {
                        code_aspects::show(aspects_result, ui, api_addr, aspects);
                    } else {
                        // *aspects_result = Some(code_aspects::remote_fetch_tree(
                        //     ctx,
                        //     &aspects.commit,
                        //     &aspects.path,
                        // ));
                        *aspects_result = Some(code_aspects::remote_fetch_node_old(
                            ctx,
                            &api_addr,
                            store.clone(),
                            &aspects.commit,
                            &aspects.path,
                        ));
                    }
                });
            }

            if trigger_compute {
                if *selected == types::SelectedConfig::Single {
                    self.data.compute_single_result = Some(single_repo::remote_compute_single(
                        ctx,
                        api_addr,
                        &mut single.content,
                        scripting_context,
                    ));
                } else if *selected == types::SelectedConfig::Querying {
                    self.data.querying_result = Some(querying::remote_compute_query(
                        ctx,
                        api_addr,
                        query,
                        querying_context,
                    ));
                } else if *selected == types::SelectedConfig::Tsg {
                    self.data.tsg_result =
                        Some(tsg::remote_compute_query(ctx, api_addr, tsg, tsg_context));
                }
            }
        }
    }
}

fn show_multi_repo(
    ui: &mut egui::Ui,
    selected: &mut types::SelectedConfig,
    _multi: &mut types::ComputeConfigMulti,
) {
    let wanted = types::SelectedConfig::Multi;
    let add_body = |ui: &mut egui::Ui| {
        ui.text_edit_singleline(&mut "github.com/INRIA/spoon");
    };
    show_menu(ui, selected, &wanted, add_body);
}

fn show_diff_menu(
    ui: &mut egui::Ui,
    selected: &mut types::SelectedConfig,
    _diff: &mut types::ComputeConfigDiff,
) {
    let wanted = types::SelectedConfig::Diff;
    let add_body = |ui: &mut egui::Ui| {
        ui.text_edit_singleline(&mut "github.com/INRIA/spoon");
    };
    show_menu(ui, selected, &wanted, add_body);
}

pub(crate) fn show_repo_menu(ui: &mut egui::Ui, repo: &mut Repo) -> bool {
    let mut changed = false;
    ui.horizontal(|ui| {
        ui.spacing_mut().item_spacing.x = 0.0;
        let user_id = ui.next_auto_id().with("user");
        let name_id = ui.next_auto_id().with("name");
        ui.push_id("user", |ui| {
            ui.label("github.com/"); // efwserfwefwe/fewefwse
            let events = ui.input(|i| i.events.clone()); // avoid dead-lock by cloning. TODO(emilk): optimize
            for event in &events {
                match event {
                    egui::Event::Paste(text_to_insert) => {
                        if !text_to_insert.is_empty() {
                            // let mut ccursor = delete_selected(text, &cursor_range);
                            // insert_text(&mut ccursor, text, text_to_insert);
                            // Some(CCursorRange::one(ccursor))
                        }
                    }
                    _ => (),
                };
            }
            if egui::TextEdit::singleline(&mut repo.user)
                .margin(egui::Vec2::new(0.0, 0.0))
                .desired_width(40.0)
                .id(user_id)
                .show(ui)
                .response
                .changed()
            {
                let mut user = None;
                let mut name = None;
                match repo.user.split_once("/") {
                    Some((a, "")) => {
                        user = Some(a.to_string());
                    }
                    Some((a, b)) => {
                        user = Some(a.to_string());
                        name = Some(b.to_string());
                    }
                    None => (),
                }
                if let Some(user) = user {
                    changed |= repo.user != user;
                    repo.user = user;
                    ui.memory_mut(|mem| {
                        mem.surrender_focus(user_id);
                        mem.request_focus(name_id)
                    });
                }
                if let Some(name) = name {
                    changed |= repo.name != name;
                    repo.name = name;
                }
            }
        });
        // 62a2b556c26f0f42a2ae791a86dc39dd36d35392
        if ui
            .push_id("name", |ui| {
                ui.label("/");
                egui::TextEdit::singleline(&mut repo.name)
                    .clip_text(true)
                    .desired_width(40.0)
                    .desired_rows(1)
                    .hint_text("name")
                    .id(name_id)
                    .interactive(true)
                    .show(ui)
            })
            .inner
            .response
            .changed()
        {
            changed |= true;
        };
    });
    changed
}

pub fn show_menu<R>(
    ui: &mut egui::Ui,
    selected: &mut types::SelectedConfig,
    wanted: &types::SelectedConfig,
    add_body: impl FnOnce(&mut egui::Ui) -> R,
) -> egui::CollapsingResponse<R> {
    let title = selected.title();
    let id = ui.make_persistent_id(title.as_ref());
    radio_collapsing(ui, id, title, selected, &wanted, add_body)
}
