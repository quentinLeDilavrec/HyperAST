use re_ui::{ContextExt as _, UiExt as _};

use crate::{app::types, command::UICommand};

use super::CommandSender;

impl crate::HyperApp {
    // #[cfg(target_arch = "wasm32")]
    // fn top_bar(&mut self, egui_ctx: &egui::Context) {}

    // #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn top_bar(&mut self, egui_ctx: &egui::Context) {
        let mut top_bar_style = egui_ctx.top_bar_style(cfg!(not(target_arch = "wasm32")));

        let mut frame_style = re_ui::design_tokens_of(egui::Theme::Dark).top_panel_frame();
        top_bar_style.height = 21.0;
        if !egui_ctx.style().visuals.dark_mode {
            frame_style.fill = egui::Color32::WHITE;
            frame_style.stroke = egui::Visuals::light().window_stroke;
            frame_style.stroke.color = egui::Color32::WHITE;
            frame_style.shadow.color = egui::Color32::WHITE;
        }
        egui::TopBottomPanel::top("top_bar")
            .frame(frame_style)
            .exact_height(top_bar_style.height)
            .show(egui_ctx, |ui| {
                ui.set_max_height(top_bar_style.height);
                top_bar(self, ui, frame_style)
            });
    }
}

fn top_bar(app: &mut crate::HyperApp, ui: &mut egui::Ui, frame_style: egui::Frame) {
    #[cfg(not(target_arch = "wasm32"))]
    if !re_ui::NATIVE_WINDOW_BAR {
        // Interact with background first, so that buttons in the top bar gets input priority
        // (last added widget has priority for input).
        let title_bar_response = ui.interact(
            ui.max_rect(),
            ui.id().with("background"),
            egui::Sense::click(),
        );
        if title_bar_response.double_clicked() {
            let maximized = ui.input(|i| i.viewport().maximized.unwrap_or(false));
            ui.ctx()
                .send_viewport_cmd(egui::ViewportCommand::Maximized(!maximized));
        } else if title_bar_response.is_pointer_button_down_on() {
            // TODO(emilk): This should probably only run on `title_bar_response.drag_started_by(PointerButton::Primary)`,
            // see https://github.com/emilk/egui/pull/4656
            ui.ctx().send_viewport_cmd(egui::ViewportCommand::StartDrag);
        }
    }

    egui::MenuBar::new().ui(ui, |ui| {
        // ui.add_space(top_bar_style.indent);
        let rect = ui.max_rect();

        #[cfg(target_os = "macos")]
        ui.add_space(60.0);

        let title = egui::RichText::new("HyperAST").heading().size(15.0);
        let title = egui::Label::new(title).sense(egui::Sense::click());
        if ui.add(title).clicked() {
            handle_app_title_click(app);
        }
        ui.menu_button("File", |ui| file_menu(ui, &app.data.command_sender));
        egui::warn_if_debug_build(ui);

        ui.with_layout(egui::Layout::left_to_right(egui::Align::Center), |ui| {
            let desired_size = egui::Vec2::new(rect.width() / 3.0, rect.height());
            let max_rect = egui::Rect::from_center_size(rect.center(), desired_size);
            hori_rot_container(ui, frame_style, max_rect, |ui, rect| {
                hori_menu_workspaces(app, ui, rect)
            });
        });
        use crate::command::UICommandSender;
        // if ui
        //     .add(ui.small_icon_button_widget(&re_ui::icons::ADD))
        //     .on_hover_text("new blank layout")
        //     .clicked()
        // {
        //     // TODO
        // }

        let button = egui::RichText::new("💾").size(14.0);
        let button = egui::Button::new(button)
            .small()
            .min_size((15.0, 15.0).into());
        if ui.add(button).on_hover_text("save layout").clicked() {
            app.data.command_sender.send_ui(UICommand::SaveLayout);
        }
        ui.add_space(1.0);
        if ui.button("↺").on_hover_text("reset layout").clicked() {
            app.data.command_sender.send_ui(UICommand::ResetLayout);
        }

        ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
            top_bar_right_ui(app, ui);
        });
    });
}

fn handle_app_title_click(app: &mut crate::HyperApp) {
    let app_info_tab = super::Tab::MarkdownStatic(0);
    let root = app.tree.root.unwrap();
    let tid = app.tabs.enumerate().find(|(_, x)| *x == &app_info_tab);
    let tid = if let Some((tid, _)) = tid {
        tid
    } else {
        if app.maximized.is_some() {
            return;
        }
        app.tabs.push(app_info_tab)
    };
    let tree = &mut app.tree;
    let Some(child) = tree.tiles.find_pane(&tid) else {
        // add tile to tile tree
        let child = tree.tiles.insert_pane(tid);
        match tree.tiles.get_mut(root) {
            Some(egui_tiles::Tile::Container(c)) => c.add_child(child),
            _ => todo!(),
        };
        return;
    };
    // if already there, might have to fix state
    if !tree.is_visible(child) || !tree.active_tiles().contains(&child) {
        tree.set_visible(child, true);
        tree.move_tile_to_container(child, root, 0, true);
    }
}

/// menu to select the different workspaces
fn hori_menu_workspaces(app: &mut crate::HyperApp, ui: &mut egui::Ui, _rect: egui::Rect) {
    ui.add_space(50.0);
    ui.visuals_mut().selection.bg_fill = ui.visuals().widgets.active.bg_fill;
    ui.visuals_mut().selection.stroke.color = ui.visuals().widgets.active.fg_stroke.color;
    ui.visuals_mut().selection.stroke.width *= 4.0;
    ui.spacing_mut().button_padding = egui::Vec2::new(3.0, 3.0);
    for s in <types::SelectedConfig as strum::IntoEnumIterator>::iter() {
        let text = s.title();
        let text = egui::RichText::new(text.as_ref()).size(12.0);
        let button = egui::Button::new(text).selected(s == app.selected);
        if ui
            .add_enabled(s.enabled(), button)
            .on_disabled_hover_text("WIP layout")
            .on_hover_ui(|ui| s.on_hover_show(ui))
            .clicked()
            && app.selected != s
        {
            let (tabs, tree) = app.layouts.remove(s.title().as_ref()).unwrap_or_else(|| {
                let tabs = s.default_layout();
                let tree = egui_tiles::Tree::new_grid(
                    "my_tree",
                    tabs.enumerate().map(|(i, _)| i).collect(),
                );
                (tabs, tree)
            });
            let tabs = std::mem::replace(&mut app.tabs, tabs);
            let tree = std::mem::replace(&mut app.tree, tree);
            app.layouts
                .insert(app.selected.title().into(), (tabs, tree));
            app.selected = s;
        }
    }
    ui.add_space(50.0);
}

/// container that rotates horizontally,
/// with shading on the left and right sides,
/// so it looks like there is depth.
fn hori_rot_container(
    ui: &mut egui::Ui,
    frame_style: egui::Frame,
    mut max_rect: egui::Rect,
    add_contents: impl FnOnce(&mut egui::Ui, egui::Rect),
) {
    ui.visuals_mut().clip_rect_margin = 0.0;
    ui.scope_builder(egui::UiBuilder::new().max_rect(max_rect), |ui| {
        egui::ScrollArea::horizontal()
            .auto_shrink(false)
            .hscroll(true)
            .scroll_bar_visibility(egui::scroll_area::ScrollBarVisibility::AlwaysHidden)
            .show_viewport(ui, add_contents);
    });
    max_rect.max.y += 5.0;
    max_rect.min.y -= 2.0;
    // left shading
    let (rect, _) = max_rect.split_left_right_at_fraction(0.15);
    let mut mesh = egui::Mesh::default();
    mesh.colored_vertex(rect.left_bottom(), frame_style.fill);
    mesh.colored_vertex(rect.left_top(), frame_style.fill);
    mesh.colored_vertex(rect.right_bottom(), egui::Color32::TRANSPARENT);
    mesh.colored_vertex(rect.right_top(), egui::Color32::TRANSPARENT);
    mesh.add_triangle(0, 1, 2);
    mesh.add_triangle(1, 2, 3);
    ui.painter().add(mesh);
    // right shading
    let rect = egui::Rect::from_min_size(max_rect.right_bottom() - rect.size(), rect.size());
    let mut mesh = egui::Mesh::default();
    mesh.colored_vertex(rect.left_bottom(), egui::Color32::TRANSPARENT);
    mesh.colored_vertex(rect.left_top(), egui::Color32::TRANSPARENT);
    mesh.colored_vertex(rect.right_bottom(), frame_style.fill);
    mesh.colored_vertex(rect.right_top(), frame_style.fill);
    mesh.add_triangle(0, 1, 2);
    mesh.add_triangle(1, 2, 3);
    ui.painter().add(mesh);
}

fn top_bar_right_ui(app: &mut crate::HyperApp, ui: &mut egui::Ui) {
    // From right-to-left:
    if re_ui::CUSTOM_WINDOW_DECORATIONS {
        ui.add_space(8.0);
        #[cfg(not(target_arch = "wasm32"))]
        if true {
            ui.native_window_buttons_ui();
        }
        ui.separator();
    }

    app.notifs.notification_toggle_button(ui);
    ui.medium_icon_toggle_button(&re_ui::icons::RIGHT_PANEL_TOGGLE, &mut app.show_right_panel);
    ui.medium_icon_toggle_button(
        &re_ui::icons::BOTTOM_PANEL_TOGGLE,
        &mut app.show_bottom_panel,
    );
    ui.medium_icon_toggle_button(&re_ui::icons::LEFT_PANEL_TOGGLE, &mut app.show_left_panel);
    // TODO find if we need the container config buttons
    // ui.add_enabled_ui(false, |ui| {
    //     let mut grid = false;
    //     ui.medium_icon_toggle_button(&re_ui::icons::CONTAINER_GRID, &mut grid);
    //     ui.medium_icon_toggle_button(&re_ui::icons::CONTAINER_TABS, &mut grid);
    //     ui.medium_icon_toggle_button(&re_ui::icons::CONTAINER_HORIZONTAL, &mut grid);
    //     ui.medium_icon_toggle_button(&re_ui::icons::CONTAINER_VERTICAL, &mut grid);
    // });
    egui::global_theme_preference_switch(ui);

    let resp = ui.toggle_value(&mut app.persistence, "persistance");
    if resp.changed() {
        app.save_interval = std::time::Duration::from_secs(0);
    }
}

fn file_menu(ui: &mut egui::Ui, command_sender: &CommandSender) {
    UICommand::SaveResults.menu_button_ui(ui, command_sender);
    UICommand::SaveLayout.menu_button_ui(ui, command_sender);
    #[cfg(not(target_arch = "wasm32"))]
    UICommand::Open.menu_button_ui(ui, command_sender);
    #[cfg(not(target_arch = "wasm32"))]
    UICommand::Quit.menu_button_ui(ui, command_sender);
}
