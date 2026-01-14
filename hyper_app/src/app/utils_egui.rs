use super::{code_tracking, types};
use egui::{CollapsingResponse, Id};
use egui_addon::{
    egui_utils::{radio_collapsing, show_wip},
    syntax_highlighting,
};
use re_ui::UiExt;
use std::collections::hash_map;

#[allow(unused)] // TODO move to egui_addon
pub trait MyUiExt: UiExt {
    fn radio_collapsing<R, S: PartialEq + Clone>(
        &mut self,
        id: Id,
        title: impl Into<String>,
        selected: &mut S,
        wanted: &S,
        add_body: impl FnOnce(&mut egui::Ui) -> R,
    ) -> CollapsingResponse<R> {
        radio_collapsing(self.ui_mut(), id, title, selected, wanted, add_body)
    }

    fn wip(&mut self, short: Option<&str>) {
        show_wip(self.ui_mut(), short)
    }

    fn show_remote_code(
        &mut self,
        api_addr: &str,
        commit: &types::Commit,
        file_path: &mut String,
        file_result: hash_map::Entry<'_, types::FileIdentifier, code_tracking::RemoteFile>,
    ) -> (
        egui::Response,
        egui::InnerResponse<()>,
        std::option::Option<
            egui::InnerResponse<
                Option<egui::scroll_area::ScrollAreaOutput<egui::text_edit::TextEditOutput>>,
            >,
        >,
    ) {
        egui::ScrollArea::horizontal()
            .show(self.ui_mut(), |ui| {
                ui.show_remote_code1(
                    api_addr,
                    commit,
                    file_path,
                    file_result,
                    f32::INFINITY,
                    false,
                )
            })
            .inner
    }

    fn show_remote_code1(
        &mut self,
        api_addr: &str,
        commit: &types::Commit,
        file_path: &mut String,
        file_result: hash_map::Entry<'_, types::FileIdentifier, code_tracking::RemoteFile>,
        desired_width: f32,
        wrap: bool,
    ) -> (
        egui::Response,
        egui::InnerResponse<()>,
        std::option::Option<
            egui::InnerResponse<
                Option<egui::scroll_area::ScrollAreaOutput<egui::text_edit::TextEditOutput>>,
            >,
        >,
    ) {
        let mut upd_src = false;
        egui::collapsing_header::CollapsingState::load_with_default_open(
            self.ui().ctx(),
            self.ui().id().with("file view"),
            true,
        )
        .show_header(self.ui_mut(), |ui| {
            upd_src = ui.text_edit_singleline(file_path).lost_focus();
            let url = format!(
                "{}/{}/{}/blob/{}/{}",
                "https://github.com", &commit.repo.user, &commit.repo.name, &commit.id, &file_path,
            );
            ui.hyperlink_to("see in github", url);
        })
        .body_unindented(|ui| {
            ui.add_space(4.0);
            let r = code_tracking::try_fetch_remote_file(&file_result, |file| {
                let code: &str = &file.content;
                let language = "java";
                // show_code_scrolled(ui, language, wrap, code, desired_width)
                ui.show_code_scrolled(language, wrap, code, desired_width)
            });
            if upd_src || r.is_none() {
                if let std::collections::hash_map::Entry::Vacant(_) = file_result {
                    file_result.insert_entry(code_tracking::remote_fetch_file(
                        ui.ctx(),
                        &api_addr,
                        commit,
                        file_path,
                    ));
                }
            }
            match r {
                None => None,
                Some(Ok(r)) => r,
                Some(Err(error)) => {
                    ui.colored_label(
                        ui.visuals().error_fg_color,
                        if error.is_empty() { "Error" } else { &error },
                    );
                    None
                }
            }
        })
    }

    fn show_code_scrolled(
        &mut self,
        language: &str,
        wrap: bool,
        code: &str,
        desired_width: f32,
    ) -> Option<egui::scroll_area::ScrollAreaOutput<egui::text_edit::TextEditOutput>> {
        use syntax_highlighting::syntax_highlighting_async as syntax_highlighter;
        let theme = syntax_highlighting::syntect::CodeTheme::from_memory(self.ui().ctx());

        let mut layouter = |ui: &egui::Ui, code: &dyn egui::TextBuffer, wrap_width: f32| {
            let mut layout_job = syntax_highlighter::highlight(ui.ctx(), &theme, code, language);
            if wrap {
                layout_job.wrap.max_width = wrap_width;
            }
            ui.fonts(|f| f.layout_job(layout_job))
        };
        Some(egui::ScrollArea::both().show(self.ui_mut(), |ui| {
            egui::TextEdit::multiline(&mut code.to_string())
                .font(egui::FontId::new(10.0, egui::FontFamily::Monospace)) // for cursor height
                .code_editor()
                .desired_rows(10)
                .desired_width(desired_width)
                .clip_text(true)
                .lock_focus(true)
                .layouter(&mut layouter)
                .show(ui)
        }))
    }

    fn show_remote_code2(
        &mut self,
        api_addr: &str,
        commit: &mut types::Commit,
        file_path: &mut String,
        file_result: hash_map::Entry<'_, types::FileIdentifier, code_tracking::RemoteFile>,
        desired_width: f32,
        wrap: bool,
    ) -> (
        egui::Response,
        egui::InnerResponse<()>,
        std::option::Option<
            egui::InnerResponse<
                Option<
                    egui::scroll_area::ScrollAreaOutput<(
                        SkipedBytes,
                        egui::text_edit::TextEditOutput,
                    )>,
                >,
            >,
        >,
    ) {
        let mut upd_src = false;
        egui::collapsing_header::CollapsingState::load_with_default_open(
            self.ui().ctx(),
            self.ui().id().with("file view"),
            true,
        )
        .show_header(self.ui_mut(), |ui| {
            upd_src = ui.text_edit_singleline(file_path).lost_focus();
            let url = format!(
                "{}/{}/{}/blob/{}/{}",
                "https://github.com", &commit.repo.user, &commit.repo.name, &commit.id, &file_path,
            );
            ui.hyperlink_to("see in github", url);
        })
        .body_unindented(|ui| {
            ui.add_space(4.0);
            if let hash_map::Entry::Occupied(promise) = &file_result {
                let promise = promise.get();
                let resp = if let Some(result) = promise.ready() {
                    match result {
                        Ok(resource) => {
                            // ui_resource(ui, resource);
                            if let Some(text) = &resource.content {
                                let code: &str = &text.content;
                                let language = "java";
                                // show_code_scrolled(ui, language, wrap, code, desired_width)
                                ui.show_code_scrolled2(
                                    language,
                                    wrap,
                                    code,
                                    &text.line_breaks,
                                    desired_width,
                                )
                            } else {
                                None
                            }
                        }
                        Err(error) => {
                            // This should only happen if the fetch API isn't available or something similar.
                            ui.colored_label(
                                ui.visuals().error_fg_color,
                                if error.is_empty() { "Error" } else { error },
                            );
                            None
                        }
                    }
                } else {
                    ui.spinner();
                    None
                };
                if upd_src {
                    file_result.insert_entry(code_tracking::remote_fetch_file(
                        ui.ctx(),
                        &api_addr,
                        commit,
                        file_path,
                    ));
                }
                resp
            } else {
                file_result.insert_entry(code_tracking::remote_fetch_file(
                    ui.ctx(),
                    &api_addr,
                    commit,
                    file_path,
                ));
                None
            }
        })
    }

    fn show_code_scrolled2(
        &mut self,
        language: &str,
        wrap: bool,
        code: &str,
        line_breaks: &[usize],
        desired_width: f32,
    ) -> Option<egui::scroll_area::ScrollAreaOutput<(SkipedBytes, egui::text_edit::TextEditOutput)>>
    {
        let theme = egui_extras::syntax_highlighting::CodeTheme::from_memory(
            self.ui().ctx(),
            self.ui().style(),
        );
        let mut layouter = |ui: &egui::Ui, code: &dyn egui::TextBuffer, _wrap_width: f32| {
            let code: &str = code.as_str();
            let layout_job = egui_extras::syntax_highlighting::highlight(
                ui.ctx(),
                ui.style(),
                &theme,
                &code,
                language,
            );
            if wrap {
                panic!();
                // layout_job.wrap.max_width = wrap_width;
            }
            ui.fonts(|f| f.layout_job(layout_job))
        };
        let font_id = egui::FontId::new(10.0, egui::FontFamily::Monospace);

        let total_rows = line_breaks.len();
        Some(if false {
            // by rows highlight
            let row_height_sans_spacing = self.ui().fonts(|f| f.row_height(&font_id)) - 0.9; //text_style_height(&text_style);
            egui::ScrollArea::vertical().show_rows(
                self.ui_mut(),
                row_height_sans_spacing,
                total_rows,
                |ui, rows| {
                    let start = if rows.start == 0 {
                        0
                    } else {
                        line_breaks[rows.start - 1]
                    };
                    ui.painter()
                        .debug_rect(ui.max_rect(), egui::Color32::RED, "text");
                    let _desired_height_rows = ui.available_height() / row_height_sans_spacing
                        * (rows.end - rows.start) as f32;
                    let te = egui::TextEdit::multiline(
                        &mut code[start..line_breaks[(rows.end).min(total_rows - 1)]].to_string(),
                    )
                    .font(font_id) // for cursor height
                    .code_editor()
                    // .desired_rows(desired_height_rows as usize)
                    // .desired_width(100.0)
                    .desired_width(desired_width)
                    .clip_text(true)
                    .lock_focus(true)
                    .layouter(&mut layouter)
                    .show(ui);
                    (start, te)
                },
            )
        } else {
            egui::ScrollArea::vertical().show(self.ui_mut(), |ui| {
                let te = egui::TextEdit::multiline(&mut code.to_string())
                    .font(font_id) // for cursor height
                    .code_editor()
                    // .desired_rows(desired_height_rows as usize)
                    // .desired_width(100.0)
                    .desired_width(desired_width)
                    .clip_text(true)
                    .lock_focus(true)
                    .layouter(&mut layouter)
                    .show(ui);
                (0, te)
            })
        })
    }
    fn double_ended_slider(
        &mut self,
        low: &mut usize,
        high: &mut usize,
        range: std::ops::RangeInclusive<usize>,
    ) -> egui::Response {
        self.ui_mut()
            .horizontal(|ui| {
                let mut lower_value = *low as f32;
                let mut upper_value = *high as f32;
                let range =
                    std::ops::RangeInclusive::new(*range.start() as f32, *range.end() as f32);
                let slider = egui_double_slider::DoubleSlider::new(
                    &mut lower_value,
                    &mut upper_value,
                    range.clone(),
                )
                .separation_distance(1f32);
                let mut resp = ui.add(slider);
                *low = lower_value as usize;
                *high = upper_value as usize;
                ui.spacing_mut().button_padding = Default::default();
                ui.spacing_mut().interact_size = Default::default();
                resp |= ui.add(egui::DragValue::new(low).range(range.clone()));
                ui.label("..");
                resp |= ui.add(egui::DragValue::new(high).range(range.clone()));
                resp
            })
            .inner
    }

    fn aux<R>(&mut self, txt: &str, f: impl FnOnce(&mut egui::Ui) -> R) -> R {
        let (frame, area) = framed_scroll_area_aux();
        area.id_salt(txt)
            .show(self.ui_mut(), |ui| frame.show(ui, f).inner)
            .inner
    }

    fn responsive_width_slider(&mut self, mut slider: egui::Slider, text: &str) -> egui::Response {
        let ui = self.ui_mut();
        if ui.spacing().slider_width + 100. < ui.available_width() {
            slider = slider.text(text);
        } else {
            slider = slider.text(egui::WidgetText::default());
            ui.label(format!("{}:", text));
        }
        ui.add(slider)
    }
    fn grouped_wrapped_list<T, It: IntoIterator<Item = Option<T>>>(
        &mut self,
        mut it: It,
        mut add_content: impl FnMut(&mut egui::Ui, usize, T),
    ) {
        let ui = self.ui_mut();
        egui::Frame::group(ui.style()).show(ui, |ui| {
            ui.horizontal_wrapped(|ui| {
                let mut rest = false;
                for (i, x) in it.into_iter().enumerate() {
                    let Some(x) = x else { continue };
                    if rest {
                        ui.separator();
                    }
                    rest = true;
                    add_content(ui, i, x);
                }
            })
        });
    }
}

impl MyUiExt for egui::Ui {}

type SkipedBytes = usize;

pub fn framed_scroll_area_aux() -> (egui::Frame, egui::ScrollArea) {
    let inner_margin =
        egui::Margin::same(re_ui::design_tokens_of(egui::Theme::Dark).view_padding());
    let frame = egui::Frame {
        inner_margin,
        ..Default::default()
    };
    let area = egui::ScrollArea::both()
        .auto_shrink([false; 2])
        .stick_to_bottom(true);
    (frame, area)
}
