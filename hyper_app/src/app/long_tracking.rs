use epaint::{Pos2, ahash::HashSet};
use poll_promise::Promise;
use std::collections::{HashMap, VecDeque};
use std::ops::{ControlFlow, Range};
use std::sync::Arc;

use egui_addon::MultiSplitter;
use egui_addon::code_editor::generic_text_buffer::byte_index_from_char_index;
use egui_addon::egui_utils::highlight_byte_range;

use hyperast::store::nodes::fetched::NodeIdentifier;
use hyperast::types::{AnyType, HyperType, Labeled, TypeStore};

use super::code_aspects::{FetchedView, HightLightHandle, remote_fetch_node_old};
use super::code_tracking::{
    FetchedFiles, RemoteFile, TrackingResult, TrackingResultWithChanges, TrackingResultsWithChanges,
};
use super::commit::{CommitMetadata, fetch_commit0};
use super::show_commit_menu;
use super::tree_view::{Action, store::FetchedHyperAST};
use super::types::{
    CodeRange, Commit, ComputeConfigAspectViews, FileIdentifier, Resource, SelectedConfig,
};
use super::utils_egui::MyUiExt as _;
use super::utils_poll::{AccumulableResult, Buffered, MultiBuffered};

use super::detached_view::DetatchedViewOptions;

type AccumulableTrackingResults = AccumulableResult<TrackingResultsWithChanges, Vec<String>>;
type LongTrackingResults = VecDeque<(
    Buffered<Result<CommitMetadata, String>>,
    MultiBuffered<AccumulableTrackingResults, Result<TrackingResultWithChanges, String>>,
)>;

type BufferedPerCommit<T> = HashMap<Commit, Buffered<T>>;

#[derive(serde::Deserialize, serde::Serialize)]
#[serde(default)]
pub(crate) struct LongTacking {
    pub(crate) flags: Flags,
    pub(crate) ser_view: bool,
    pub(crate) tree_view: bool,
    pub(crate) detatched_view: bool,
    pub(crate) detatched_view_options: DetatchedViewOptions,
    pub(crate) origins: Vec<CodeRange>,
    pub(crate) origin_index: usize,
    #[serde(skip)] // TODO remove that
    pub(crate) results: LongTrackingResults,
    #[serde(skip)]
    pub(crate) tree_viewer: BufferedPerCommit<Result<Resource<FetchedView>, String>>,
    #[serde(skip)]
    pub(crate) additionnal_links: Vec<[CodeRange; 2]>,
}

impl Default for LongTacking {
    fn default() -> Self {
        Self {
            flags: Default::default(),
            ser_view: false,
            tree_view: true,
            detatched_view: false,
            detatched_view_options: Default::default(),
            origins: vec![Default::default()],
            origin_index: Default::default(),
            results: VecDeque::from(vec![Default::default()]),
            tree_viewer: Default::default(),
            additionnal_links: Default::default(),
        }
    }
}

#[derive(serde::Deserialize, serde::Serialize, Default)]
#[serde(default)]
pub(crate) struct Flags {
    pub(crate) upd: bool,
    pub(crate) child: bool,
    pub(crate) parent: bool,
    pub(crate) exact_child: bool,
    pub(crate) exact_parent: bool,
    pub(crate) sim_child: bool,
    pub(crate) sim_parent: bool,
    pub(crate) meth: bool,
    pub(crate) typ: bool,
    pub(crate) top: bool,
    pub(crate) file: bool,
    pub(crate) pack: bool,
    pub(crate) dependency: bool,
    pub(crate) dependent: bool,
    pub(crate) references: bool,
    pub(crate) declaration: bool,
}
impl Flags {
    fn ui(&mut self, ui: &mut egui::Ui) -> egui::Response {
        macro_rules! show {
            ($($f:ident: $s:expr),+ $(; $($df:ident: $ds:expr),+)?) => {
                $(ui.checkbox(&mut self.$f, $s))|+
                $(| ui.add_enabled_ui(false, |ui| {
                    let r = $(ui.checkbox(&mut self.$df, $ds))|+;
                    ui.wip(Some("need more parameters ?"));
                    r
                }).inner)?
            }
        }
        show!(
            upd: "updated",
            child: "children changed",
            parent: "parent changed";
            exact_child: "children changed formatting",
            exact_parent: "parent changed formatting",
            sim_child: "children changed structure",
            sim_parent: "parent changed structure",
            meth: "method changed",
            typ: "type changed",
            top: "top-level type changed",
            file: "file changed",
            pack: "package changed",
            dependency: "dependency changed",
            dependent: "dependent changed",
            references: "references changed",
            declaration: "declaration changed"
        )
    }
}

pub(crate) const WANTED: SelectedConfig = SelectedConfig::LongTracking;

pub(crate) fn show_config(ui: &mut egui::Ui, tracking: &mut LongTacking) {
    show_commit_menu(ui, &mut tracking.origins[0].file.commit);
    // let repo_changed = show_repo_menu(ui, &mut tracking.origins[0].file.commit.repo);
    // let old = tracking.origins[0].file.commit.id.clone();
    // let commit_te = egui::TextEdit::singleline(&mut tracking.origins[0].file.commit.id)
    //     .clip_text(true)
    //     .desired_width(150.0)
    //     .desired_rows(1)
    //     .hint_text("commit")
    //     .id(ui.id().with("commit"))
    //     .interactive(true)
    //     .show(ui);
    // if repo_changed || commit_te.response.changed() {
    //     // todo!()
    // } else {
    //     assert_eq!(old, tracking.origins[0].file.commit.id.clone());
    // };
    ui.checkbox(&mut tracking.tree_view, "tree view");
    ui.checkbox(&mut tracking.ser_view, "serialized view");
    ui.checkbox(&mut tracking.detatched_view, "detatched view");
    if tracking.detatched_view {
        ui.indent("detached_options", |ui| {
            tracking.detatched_view_options.ui(ui);
        });
    }
    ui.add(egui::Label::new(
        egui::RichText::from("Triggers").font(egui::FontId::proportional(16.0)),
    ));
    tracking.flags.ui(ui);
}

#[derive(serde::Deserialize, serde::Serialize, Clone, Copy, Debug)]
pub struct State {
    pub(crate) offset: f32,
    pub(crate) width: f32,
}

impl Default for State {
    fn default() -> Self {
        Self {
            offset: 0.0,
            width: 1.0,
        }
    }
}

impl State {
    pub fn load(ctx: &egui::Context, id: egui::Id) -> Option<Self> {
        ctx.data_mut(|d| d.get_persisted(id))
    }

    pub fn store(self, ctx: &egui::Context, id: egui::Id) {
        ctx.data_mut(|d| d.insert_persisted(id, self));
    }
}
type PortId = egui::Id;

pub(crate) type Attacheds = Vec<(
    HashMap<usize, (PortId, Option<egui::Rect>)>,
    HashMap<usize, (PortId, Option<egui::Rect>)>,
)>;

struct LongTrackingResultsImpl<'a> {
    viewport_x: egui::Rangef,
    timeline_window: egui::Rect,
    max_col: usize,
    min_col: usize,
    total_cols: usize,
    w_state: Option<State>,
    spacing: egui::Vec2,
    w_id: egui::Id,
    col_width: f32,
    viewport_width: f32,
    api_addr: &'a str,
}

impl<'a> LongTrackingResultsImpl<'a> {
    fn new(
        ui: &mut egui::Ui,
        api_addr: &'a str,
        aspects: &mut ComputeConfigAspectViews,
        long_tracking: &mut LongTacking,
        fetched_files: &mut FetchedFiles,
    ) -> Self {
        let w_id = ui.id().with("Tracking Timeline");
        let timeline_window = ui.available_rect_before_wrap();
        let spacing: egui::Vec2 = (0.0, 0.0).into();
        let mut w_state = State::load(ui.ctx(), w_id);
        let col_width = if long_tracking.results.len() <= 2 {
            let width = ui.available_width() / long_tracking.results.len() as f32;
            width
        } else if let Some(w_state) = w_state {
            w_state.width
        } else {
            let width = timeline_window.width() * 0.4;
            w_state = Some(State { offset: 0.0, width });
            width
        };
        let total_cols = long_tracking.results.len();
        let col_width_with_spacing = col_width + spacing.x;
        use egui::NumExt;
        let viewport_width = (col_width_with_spacing * total_cols as f32 - spacing.x).at_least(0.0);

        let viewport_left = timeline_window.left() - w_state.map_or(0.0, |x| x.offset);
        let viewport_x = egui::Rangef::new(viewport_left, viewport_left + viewport_width);

        let mut min_col = (viewport_x.min / col_width_with_spacing).floor() as usize;
        let offset = w_state.map_or(0.0, |x| x.offset);
        let mut min_col = (offset / col_width_with_spacing).floor() as usize;
        let offset = offset + timeline_window.width();
        let mut max_col = (offset / col_width_with_spacing).ceil() as usize;
        if max_col > total_cols {
            let diff = max_col.saturating_sub(min_col);
            max_col = total_cols;
            min_col = total_cols.saturating_sub(diff);
        }

        Self {
            api_addr,
            viewport_x,
            timeline_window,
            max_col,
            min_col,
            total_cols,
            w_state,
            w_id,
            viewport_width,
            spacing,
            col_width,
        }
    }

    fn col_range(&self) -> Range<usize> {
        self.min_col..self.max_col
    }

    fn make_main_ui(&self, ui: &mut egui::Ui) -> egui::Ui {
        let LongTrackingResultsImpl {
            viewport_x,
            timeline_window,
            ..
        } = *self;
        use egui::NumExt;
        let viewport =
            egui::Rect::from_x_y_ranges(viewport_x, ui.available_rect_before_wrap().y_range());
        let layout = egui::Layout::left_to_right(egui::Align::BOTTOM);
        let mut ui = ui.new_child(egui::UiBuilder::new().layout(layout).max_rect(viewport));
        ui.set_clip_rect(timeline_window);
        ui
    }

    fn make_tracking_ui(&self, ui: &mut egui::Ui) -> egui::Ui {
        let LongTrackingResultsImpl {
            max_col,
            min_col,
            spacing,
            col_width,
            ..
        } = *self;
        let scale = |x| ui.max_rect().left() + min_col as f32 * (col_width + spacing.x);
        let [x_min, x_max] = [min_col, max_col].map(scale);
        let x_min = x_min + spacing.x / 3.0;
        let x_max = x_max - spacing.x * 2.0 / 3.0;
        let rect = egui::Rect::from_x_y_ranges(x_min..=x_max, ui.max_rect().y_range());
        let layout = egui::Layout::left_to_right(egui::Align::BOTTOM);
        ui.new_child(egui::UiBuilder::new().layout(layout).max_rect(rect))
    }
}

fn show_commit(
    ui: &mut egui::Ui,
    col: usize,
    store: &Arc<FetchedHyperAST>,
    aspects: &mut ComputeConfigAspectViews,
    long_tracking: &mut LongTacking,
    fetched_files: &mut FetchedFiles,
    res_impl: &LongTrackingResultsImpl<'_>,
) {
    let mut tracking_result = (Buffered::Empty, MultiBuffered::default());
    let (md, tracking_result) = if long_tracking.results.is_empty() {
        &mut tracking_result
    } else {
        let res = &mut long_tracking.results[col];
        res.1.try_poll();
        res.0.try_poll();
        res
    };
    let tracked;
    let (code_ranges, md) = if col == long_tracking.origin_index {
        tracked = None;
        if let Some(md) = md.get_mut() {
            (long_tracking.origins.iter_mut().collect::<Vec<_>>(), md)
        } else {
            if !md.is_waiting() {
                let code_range = &mut long_tracking.origins[0];
                md.buffer(fetch_commit0(
                    ui.ctx(),
                    res_impl.api_addr,
                    &code_range.file.commit,
                ));
            }
            return;
        }
    } else if let (Some(tracking_result), Some(md)) = (tracking_result.get_mut(), md.get_mut()) {
        if tracking_result.content.track.results.is_empty() {
            panic!("{:?}", tracking_result.errors)
        } else {
            let track = &tracking_result.content.track.results[0];

            tracked = Some(TrackingResultWithChanges {
                track: TrackingResult {
                    matched: vec![],
                    ..track.clone()
                },
                src_changes: tracking_result.content.src_changes.clone(),
                dst_changes: tracking_result.content.dst_changes.clone(),
            });
            let track = &mut tracking_result.content.track.results;
            let track = track.iter_mut().map(|track| {
                ((track.matched).get_mut(0)).unwrap_or_else(|| track.fallback.as_mut().unwrap())
            });
            (track.collect(), md)
        }
    } else if let Some(tracking_result) = tracking_result.get_mut() {
        if tracking_result.content.track.results.is_empty() && !tracking_result.errors.is_empty() {
            ui.colored_label(
                ui.visuals().error_fg_color,
                tracking_result.errors.join("\n"),
            );
            return;
        } else if !md.is_waiting() {
            let track = &tracking_result.content.track.results[0];
            let api_addr = res_impl.api_addr;
            if let Some(code_range) = &track.intermediary {
                md.buffer(fetch_commit0(ui.ctx(), api_addr, &code_range.file.commit));
            } else if let Some(code_range) = track.matched.get(0) {
                md.buffer(fetch_commit0(ui.ctx(), api_addr, &code_range.file.commit));
            } else if let Some(code_range) = &track.fallback {
                md.buffer(fetch_commit0(ui.ctx(), api_addr, &code_range.file.commit));
            } else {
                unreachable!("should have been matched or been given a fallback")
            }
            ui.spinner();
            return;
        } else {
            ui.spinner();
            return;
        }
    } else {
        ui.spinner();
        return;
    };
    show_commitid_info(tracked, ui, code_ranges);
    match md {
        Ok(md) => {
            md.show(ui);
        }
        Err(err) => {
            ui.colored_label(ui.visuals().error_fg_color, err);
        }
    }
}
fn show_timeline(
    ui: &mut egui::Ui,
    api_addr: &str,
    aspects: &mut ComputeConfigAspectViews,
    store: &Arc<FetchedHyperAST>,
    long_tracking: &mut LongTacking,
    fetched_files: &mut FetchedFiles,
    res_impl: &LongTrackingResultsImpl<'_>,
) {
    ui.set_clip_rect(ui.max_rect().expand2((1.0, 0.0).into()));

    let mut show_c = |ui: &mut _, col| {
        show_commit(
            ui,
            col,
            store,
            aspects,
            long_tracking,
            fetched_files,
            res_impl,
        )
    };
    if res_impl.total_cols == 0 {
        ui.spinner();
    } else if res_impl.total_cols == 1 {
        show_c(ui, 0);
    } else {
        let total_cols = res_impl.total_cols;
        let ratios = (0..total_cols - 1)
            .map(|_| 1.0 / (total_cols) as f32)
            .collect();
        MultiSplitter::vertical()
            .ratios(ratios)
            .show(ui, move |uis| {
                for (col, ui) in uis.into_iter().enumerate() {
                    show_c(ui, col);
                }
            });
    }
    let Some(mut w_state) = res_impl.w_state else {
        return;
    };
    let map_left = egui::remap_clamp(
        w_state.offset,
        0.0..=res_impl.viewport_width,
        ui.max_rect().x_range(),
    );
    let map_right = egui::remap_clamp(
        w_state.offset + res_impl.timeline_window.width(),
        0.0..=res_impl.viewport_width,
        ui.max_rect().x_range(),
    );
    let rect = {
        let map_width =
            res_impl.timeline_window.width() / res_impl.viewport_width * ui.max_rect().width();
        egui::Rect::from_x_y_ranges(map_left..=map_left + map_width, ui.max_rect().y_range())
    };
    timeline_drag_box(ui, &mut w_state, rect, res_impl);
    // left vertical handle
    if let Some(s) = timeline_drag_box_vertical_handle(
        ui,
        rect,
        map_right,
        res_impl,
        rect.left(),
        ui.id().with("__resize_l"),
        |x| rect.max.x - x,
    ) {
        w_state = s;
        w_state.offset -= res_impl.timeline_window.width();
    }
    // right vertical handle
    if let Some(s) = timeline_drag_box_vertical_handle(
        ui,
        rect,
        map_left,
        res_impl,
        rect.right(),
        ui.id().with("__resize_r"),
        |x| x - rect.min.x,
    ) {
        w_state = s;
    }
    w_state.store(ui.ctx(), res_impl.w_id);
}

fn timeline_drag_box_vertical_handle(
    ui: &mut egui::Ui,
    rect: egui::Rect,
    map_offset: f32,
    res_impl: &LongTrackingResultsImpl<'_>,
    line_x: f32,
    resize_id: egui::Id,
    resize: impl Fn(f32) -> f32,
) -> Option<State> {
    let LongTrackingResultsImpl {
        timeline_window,
        total_cols,
        viewport_width,
        spacing,
        ..
    } = *res_impl;
    use egui::NumExt;
    let mut res = None;
    let mut resize_hover = false;
    let mut is_resizing = false;
    if let Some(pointer) = ui.ctx().pointer_latest_pos() {
        let we_are_on_top = (ui.ctx())
            .layer_id_at(pointer)
            .map_or(true, |top_layer_id| top_layer_id == ui.layer_id());
        let mouse_over_resize_line = we_are_on_top
            && rect.y_range().contains(pointer.y)
            && (line_x - pointer.x).abs() <= ui.style().interaction.resize_grab_radius_side;

        if ui.input(|i| i.pointer.any_pressed() && i.pointer.any_down()) && mouse_over_resize_line {
            ui.ctx().set_dragged_id(resize_id);
        }
        is_resizing = ui.ctx().is_being_dragged(resize_id);
        if is_resizing {
            let x = timeline_window.x_range().clamp(pointer.x); // or ui.max_rect()
            // let x = x.clamp(timeline_window.min.x, timeline_window.max.x);
            let col_x = resize(x).at_least(0.0);
            let col_ratio = ui.max_rect().width() / total_cols as f32 / col_x;
            let width = timeline_window.width() * col_ratio;

            let with_spacing = width + spacing.x;
            let viewport_width = (with_spacing * total_cols as f32 - spacing.x).at_least(0.0);
            let from = ui.max_rect().x_range();
            let to = 0.0..=viewport_width;
            let offset = egui::remap_clamp(map_offset, from, to);
            res = Some(State { offset, width })
        }

        let dragging_something_else = ui.input(|i| i.pointer.any_down() || i.pointer.any_pressed());
        resize_hover = mouse_over_resize_line && !dragging_something_else;

        if resize_hover || is_resizing {
            ui.ctx().set_cursor_icon(egui::CursorIcon::ResizeHorizontal);
        }
    }
    let stroke = if is_resizing {
        ui.style().visuals.widgets.active.fg_stroke // highly visible
    } else if resize_hover {
        ui.style().visuals.widgets.hovered.fg_stroke // highly visible
    } else {
        ui.style().visuals.widgets.noninteractive.bg_stroke // dim
        // egui::Stroke::NONE
    };
    let painter = ui.painter_at(ui.max_rect());
    painter.vline(line_x, rect.y_range(), stroke);
    res
}

fn timeline_drag_box(
    ui: &mut egui::Ui,
    w_state: &mut State,
    rect: egui::Rect,
    res_impl: &LongTrackingResultsImpl<'_>,
) {
    let LongTrackingResultsImpl {
        timeline_window,
        viewport_width,
        ..
    } = *res_impl;
    let interactive_rect = egui::Rect::from_center_size(rect.center(), (40., 30.).into());
    let id = ui.id().with("map_drag");
    let layer_id = egui::LayerId::new(egui::Order::Foreground, ui.id().with("drag_handle"));
    let map_drag = ui
        .scope_builder(
            egui::UiBuilder::new()
                .layer_id(layer_id)
                .max_rect(ui.max_rect()),
            |ui| ui.interact(interactive_rect, id, egui::Sense::drag()),
        )
        .inner;
    if map_drag.hovered() {
        ui.ctx().set_cursor_icon(egui::CursorIcon::ResizeHorizontal);
    }
    let mult = if map_drag.dragged() {
        let delta = map_drag.drag_delta();
        if delta.x != 0.0 {
            let x = delta.x / ui.max_rect().width() * viewport_width;
            let max = viewport_width - timeline_window.width();
            w_state.offset = (w_state.offset + x).clamp(0.0, max);
        }
        0.8
    } else {
        0.4
    };
    let fill_color = egui::Color32::DARK_GRAY.linear_multiply(mult);
    let painter = ui.painter_at(ui.max_rect());
    painter.rect(
        rect,
        egui::CornerRadius::ZERO,
        fill_color,
        egui::Stroke::new(1.0, egui::Color32::DARK_GRAY),
        egui::StrokeKind::Inside,
    );
    painter.text(
        rect.center(),
        egui::Align2::CENTER_CENTER,
        "↔",
        egui::FontId::monospace(50.0),
        egui::Color32::BLACK,
    );
}

fn show_trackings(
    ui: &mut egui::Ui,
    api_addr: &str,
    aspects: &mut ComputeConfigAspectViews,
    store: &Arc<FetchedHyperAST>,
    long_tracking: &mut LongTacking,
    fetched_files: &mut FetchedFiles,
    res_impl: &LongTrackingResultsImpl<'_>,
) {
    let mut cui = res_impl.make_tracking_ui(ui);

    let mut attached = AttachedImpl::new(res_impl, long_tracking.origin_index);
    attached.ui(&mut cui, long_tracking, store, aspects, fetched_files);

    let LongTrackingResultsImpl {
        timeline_window,
        total_cols,
        w_id,
        spacing,
        col_width,
        ..
    } = *res_impl;

    // handle the scrolling
    if let Some((o, i, mut scroll)) = attached.differed_focus_scroll {
        let o: f32 = o;
        let g_o = (attached.attacheds.get(i))
            .and_then(|a| a.0.get(&0))
            .and_then(|x| x.1)
            .map(|p| p.min.y)
            .unwrap_or(timeline_window.height() / 2000.0);
        let g_o: f32 = 50.0;
        scroll.state.offset = (0.0, (o - g_o).max(0.0)).into();
        scroll.state.store(ui.ctx(), scroll.id);
    }

    long_tracking.origins.extend(attached.new_origins);

    // add new tracking being computed on backend
    for (col, waiting) in attached.waiting {
        if col == 0 {
            long_tracking.results.push_front(Default::default());
            long_tracking.origin_index += 1;
            long_tracking.results[col].1.buffer(waiting);
        } else {
            // TODO handle more than going back in time
            long_tracking.results[col - 1].1.buffer(waiting);
        }
    }

    // render the attached color boxes
    ui.set_clip_rect(timeline_window);
    for i in 0..attached.attacheds.len() - 1 {
        let (left, right) = attached.attacheds.split_at(i + 1);
        let (greens, blues) = (&left.last().unwrap().1, &right.first().unwrap().0);
        let mut done = HashSet::default();
        let cable = false;
        let mut min_right_x = 0.0;
        let mut min_left_x = 0.0;
        let l_bound = res_impl.viewport_x.min + (i + 1) as f32 * (col_width + spacing.x) - 15.0;
        let r_bound = l_bound + 25.0;
        let mut f = |&(green, g_rect), &(blue, b_rect)| {
            if cable {
                let green: egui::Id = green;
                let blue: egui::Id = blue;
            }

            if let (Some(m_rect), Some(src_rect)) = (g_rect, b_rect) {
                let m_rect: egui::Rect = m_rect;
                let src_rect: egui::Rect = src_rect;
                let mut m_pos = m_rect.right_center();
                let mut src_pos = src_rect.left_center();
                let mut ctrl = (m_pos, src_pos);
                ctrl.0.x = l_bound;
                ctrl.1.x = r_bound;
                use egui::NumExt;
                m_pos.x = m_pos.x.at_most(l_bound);
                src_pos.x = src_pos.x.at_least(r_bound);
                let color = ui.style().visuals.text_color();
                let link =
                    epaint::PathShape::line(vec![m_pos, ctrl.0, ctrl.1, src_pos], (2.0, color));
                ui.painter().add(link);
            }
        };
        for (k, g) in greens {
            done.insert(k);
            if let Some(b) = blues.get(&k) {
                f(g, b)
            }
        }
        for (k, b) in blues {
            if done.contains(&k) {
                continue;
            }
            if let Some(g) = greens.get(&k) {
                f(g, b)
            }
        }
    }
}

struct AttachedImpl<'a> {
    attacheds: Attacheds,
    differed_focus_scroll: Option<DeferedFocusScroll>,
    waiting: Vec<(usize, TrackingResultWithChangesProm)>,
    new_origins: Vec<CodeRange>,
    res_impl: &'a LongTrackingResultsImpl<'a>,
    origin_index: usize,
}
impl<'a> AttachedImpl<'a> {
    fn new(res_impl: &'a LongTrackingResultsImpl<'a>, origin_index: usize) -> Self {
        Self {
            attacheds: Default::default(),
            differed_focus_scroll: Default::default(),
            waiting: Default::default(),
            new_origins: Default::default(),
            res_impl,
            origin_index,
        }
    }

    // show attacheds
    fn ui(
        &mut self,
        ui: &mut egui::Ui,
        long_tracking: &mut LongTacking,
        store: &Arc<FetchedHyperAST>,
        aspects: &mut ComputeConfigAspectViews,
        fetched_files: &mut FetchedFiles,
    ) -> () {
        let res_impl = self.res_impl;
        for col in res_impl.col_range() {
            self.attacheds.push(Default::default());

            let ui = &mut self.prep_ui(ui, col);

            let mut curr_view = match init_col_view(
                ui,
                self,
                col,
                &mut long_tracking.results,
                &mut long_tracking.origins,
            ) {
                Ok(curr_view) => curr_view,
                Err(err) => {
                    ui.colored_label(ui.visuals().error_fg_color, err);
                    continue;
                }
            };

            if long_tracking.tree_view {
                show_tree_view_of_tracking(
                    ui,
                    store,
                    col,
                    &long_tracking.flags,
                    aspects,
                    &mut long_tracking.tree_viewer,
                    self,
                    &mut curr_view,
                );
            } else if long_tracking.ser_view {
                show_ser_view_of_tracking(
                    ui,
                    &long_tracking.flags,
                    col,
                    fetched_files,
                    self,
                    &mut curr_view,
                );
            }
        }
    }

    fn prep_ui(&mut self, ui: &mut egui::Ui, col: usize) -> egui::Ui {
        let relative = col as isize - self.origin_index as isize;
        let LongTrackingResultsImpl {
            timeline_window,
            w_id,
            spacing,
            col_width,
            min_col,
            ..
        } = *self.res_impl;
        let x_range = ui.available_rect_before_wrap().x_range();
        let x_start = x_range.min + (col_width + spacing.x) * (col - min_col) as f32;
        let x_end = x_start + col_width;
        let max_rect = egui::Rect::from_x_y_ranges(x_start..=x_end, ui.max_rect().y_range());
        let x_start = timeline_window.x_range().min.max(x_start - spacing.x);
        let x_end = timeline_window.x_range().max.min(x_end);
        let clip_rect = egui::Rect::from_x_y_ranges(x_start..=x_end, ui.max_rect().y_range());
        let mut ui = egui::Ui::new(
            ui.ctx().clone(),
            w_id.with(relative),
            egui::UiBuilder {
                ui_stack_info: ui.stack().info.clone(),
                max_rect: Some(max_rect),
                ..Default::default()
            },
        );
        ui.set_clip_rect(clip_rect);
        ui
    }

    fn is_origin(&self, col: usize) -> bool {
        let relative = col as isize - self.origin_index as isize;
        relative == 0
    }

    fn has_future(&self, col: usize) -> bool {
        col + 1 < self.res_impl.total_cols
    }
    fn has_past(&self, col: usize) -> bool {
        col != 0
    }
}

fn init_col_view<'a>(
    ui: &mut egui::Ui,
    attached: &AttachedImpl<'_>,
    col: usize,
    tracking_results: &'a mut LongTrackingResults,
    tracking_origins: &'a mut Vec<CodeRange>,
) -> Result<ColView<'a>, String> {
    let mut curr_view = ColView::default();
    let original_targets = &mut curr_view.original_targets;
    if attached.is_origin(col) {
        match (attached.has_past(col), attached.has_future(col)) {
            (true, true) => {
                let result = tracking_results
                    .get_mut(col - 1)
                    .and_then(|x| x.1.get_mut());
                let Some(result) = result else {
                    original_targets.push((&mut tracking_origins[0], 0));
                    return Ok(curr_view);
                };
                if let Some(x) = &mut result.content.dst_changes {
                    curr_view.additions = Some(&mut x.additions);
                }
                if let Some(x) = &mut result.content.src_changes {
                    assert_ne!(tracking_origins[0].file.commit, x.commit);
                    curr_view.left_commit = Some(&mut x.commit);
                }
                let mut origins = tracking_origins.iter_mut();
                for (i, result) in result.content.track.results.iter_mut().enumerate() {
                    curr_view.effective_targets.push((&mut result.src, i));
                    (curr_view.effective_targets).push((origins.next().unwrap(), i));
                }
            }
            (true, false) => {
                let result = tracking_results
                    .get_mut(col - 1)
                    .and_then(|x| x.1.get_mut());
                let Some(result) = result else {
                    original_targets.push((&mut tracking_origins[0], 0));
                    return Ok(curr_view);
                };
                if let Some(changes) = &mut result.content.dst_changes {
                    curr_view.additions = Some(&mut changes.additions);
                }
                if let Some(changes) = &mut result.content.src_changes {
                    assert_ne!(tracking_origins[0].file.commit, changes.commit);
                    curr_view.left_commit = Some(&mut changes.commit);
                }
                let mut origins = tracking_origins.iter_mut();
                for (i, result) in result.content.track.results.iter_mut().enumerate() {
                    curr_view.effective_targets.push((&mut result.src, i));
                    if let Some(origins) = origins.next() {
                        curr_view.original_targets.push((origins, i));
                    }
                }
            }
            (false, true) => todo!(),
            (false, false) => {
                // nothing to do
                original_targets.push((&mut tracking_origins[0], 0));
            }
        }
    } else if attached.has_past(col) {
        let mut it = tracking_results.range_mut(col - 1..=col);
        let past = it.next();

        if let Some(result) = past.and_then(|x| x.1.get_mut()) {
            if let Some(changes) = &mut result.content.dst_changes {
                curr_view.additions = Some(&mut changes.additions);
            }
            if let Some(changes) = &mut result.content.src_changes {
                curr_view.left_commit = Some(&mut changes.commit);
            }
            for (i, result) in result.content.track.results.iter_mut().enumerate() {
                assert_ne!(
                    result.src.file.commit,
                    **curr_view.left_commit.as_ref().unwrap()
                );
                curr_view.effective_targets.push((&mut result.src, i));
            }
        }
        let curr = it.next();
        if let Some(result) = curr.and_then(|x| x.1.get_mut()) {
            for (i, result) in result.content.track.results.iter_mut().enumerate() {
                let result = result.matched.get_mut(0).or(result.fallback.as_mut());
                (curr_view.matcheds).push((result.unwrap(), i));
            }
            if let Some(changes) = &mut result.content.src_changes {
                curr_view.deletions = Some(&mut changes.deletions);
            }
        }
        if original_targets.is_empty() {
            original_targets.push((&mut tracking_origins[0], 0));
        }
        assert!(it.next().is_none());
    } else {
        let result = tracking_results.get_mut(col).and_then(|x| x.1.get_mut());
        let Some(result) = result else {
            original_targets.push((&mut tracking_origins[0], 0));
            return Ok(curr_view);
        };
        if result.content.track.results.is_empty() {
            return Err(result.errors.join("\n"));
        }
        if let Some(x) = &mut result.content.src_changes {
            curr_view.deletions = Some(&mut x.deletions);
        }
        for (i, result) in result.content.track.results.iter_mut().enumerate() {
            let result = result.matched.get_mut(0).or(result.fallback.as_mut());
            curr_view.matcheds.push((result.unwrap(), i));
        }
    }
    Ok(curr_view)
}

fn show_tree_view_of_tracking(
    ui: &mut egui::Ui,
    store: &Arc<FetchedHyperAST>,
    col: usize,
    flags: &Flags,
    aspects: &mut ComputeConfigAspectViews,
    tree_viewer: &mut BufferedPerCommit<Result<Resource<FetchedView>, String>>,
    attached: &mut AttachedImpl<'_>,
    curr_view: &mut ColView<'_>,
) {
    let res_impl = &attached.res_impl;
    let curr_commit = {
        let curr = if curr_view.matcheds.get(0).is_some() {
            curr_view.matcheds.get_mut(0)
        } else {
            curr_view.original_targets.get_mut(0)
        };
        let Some((curr, _)) = curr else {
            return;
        };
        &curr.file.commit
    };
    let tree_viewer = tree_viewer.entry(curr_commit.clone());
    let tree_viewer = tree_viewer.or_insert_with(|| Buffered::default());
    let trigger = tree_viewer.try_poll();
    let Some(tree_viewer) = tree_viewer.get_mut() else {
        if !tree_viewer.is_waiting() {
            tree_viewer.buffer(remote_fetch_node_old(
                ui.ctx(),
                res_impl.api_addr,
                store.clone(),
                &curr_commit,
                "",
            ));
        }
        return;
    };
    let Ok(tree_viewer) = tree_viewer.as_mut().map_err(|err| {
        log::error!("{}", err);
        ui.colored_label(ui.visuals().error_fg_color, err);
    }) else {
        return;
    };
    let Some(p) = show_tree_view(
        ui,
        res_impl.min_col,
        res_impl.api_addr,
        col,
        trigger,
        tree_viewer,
        curr_view,
        aspects,
        &mut attached.attacheds,
        &mut attached.differed_focus_scroll,
    ) else {
        return;
    };

    let curr = if !curr_view.matcheds.is_empty() {
        curr_view.matcheds.get_mut(0)
    } else {
        curr_view.original_targets.get_mut(0)
    };
    let Some((curr, _)) = curr else { return };
    if attached.is_origin(col) {
        curr.path = p;
        if col == 0 {
            // TODO only request changes when we have none
            let track_at_path = track_at_path_with_changes(
                ui.ctx(),
                res_impl.api_addr,
                &curr.file.commit,
                &curr.path,
                flags,
            );
            attached.waiting.push((col, track_at_path));
        } else {
            // TODO allow to reset tracking
            attached.new_origins.push(CodeRange {
                file: curr.file.clone(),
                range: None,
                path: curr.path.clone(),
                path_ids: vec![],
            });
            let past_commit = curr_view.left_commit.as_ref().unwrap();
            assert_ne!(&curr.file.commit, *past_commit);
            let track_at_path = track_at_path(
                ui.ctx(),
                res_impl.api_addr,
                &curr.file.commit,
                Some(past_commit),
                &curr.path,
                &Default::default(),
            );
            attached.waiting.push((col, track_at_path));
        }
    } else {
        if col == 0 {
            let track_at_path = track_at_path_with_changes(
                ui.ctx(),
                res_impl.api_addr,
                &curr.file.commit,
                &p,
                flags,
            );
            attached.waiting.push((col, track_at_path));
        } else {
            let past_commit = curr_view.left_commit.as_ref().unwrap();
            let present_commit = &curr.file.commit;
            // TODO allow to reset tracking
            assert_ne!(&present_commit, past_commit);
            let track_at_path = track_at_path(
                ui.ctx(),
                res_impl.api_addr,
                &present_commit,
                Some(past_commit),
                &p,
                &Default::default(),
            );
            attached.waiting.push((col, track_at_path));
        }
    }
}

type TrackingResultWithChangesProm = Promise<Result<TrackingResultWithChanges, String>>;

fn show_ser_view_of_tracking(
    ui: &mut egui::Ui,
    flags: &Flags,
    col: usize,
    fetched_files: &mut FetchedFiles,
    attached: &mut AttachedImpl<'_>,
    curr_view: &mut ColView<'_>,
) {
    let relative = col as isize - attached.origin_index as isize;
    let is_origin = relative == 0;
    let res_impl = &attached.res_impl;
    let api_addr = res_impl.api_addr;
    let Some(te) = show_code_view(ui, api_addr, curr_view, fetched_files) else {
        return;
    };
    let offset = 0;
    if te.response.is_pointer_button_down_on() {
        return;
    }
    let Some(bb) = &te.cursor_range else { return };
    let s = te.galley.text();
    let r = bb.as_sorted_char_range();
    let r = Range {
        start: offset + byte_index_from_char_index(s, r.start),
        end: offset + byte_index_from_char_index(s, r.end),
    };

    let curr = if curr_view.matcheds.get(0).is_some() {
        curr_view.matcheds.get_mut(0)
    } else {
        curr_view.original_targets.get_mut(0)
    };
    let Some((curr, _)) = curr else {
        return;
    };

    if curr.range == Some(r.clone()) {
        return;
    }
    if is_origin {
        curr.range = Some(r.clone());
    }
    if col == 0 {
        attached.waiting.push((
            col,
            track(
                ui.ctx(),
                api_addr,
                &curr.file.commit,
                &curr.file.file_path,
                &Some(r),
                flags,
            ),
        ));
    } else {
        // TODO allow to reset tracking
        attached.waiting.push((
            col,
            track(
                ui.ctx(),
                api_addr,
                &curr.file.commit,
                &curr.file.file_path,
                &Some(r),
                flags,
            ),
        ));
    }
}

pub(crate) fn show_results(
    ui: &mut egui::Ui,
    api_addr: &str,
    aspects: &mut ComputeConfigAspectViews,
    store: Arc<FetchedHyperAST>,
    long_tracking: &mut LongTacking,
    fetched_files: &mut FetchedFiles,
) {
    let mut res_impl =
        LongTrackingResultsImpl::new(ui, api_addr, aspects, long_tracking, fetched_files);

    egui::panel::TopBottomPanel::bottom("Timeline Map")
        .frame(egui::Frame::side_top_panel(ui.style()).inner_margin(0.0))
        .height_range(0.0..=ui.available_height() / 3.0)
        .default_height(ui.available_height() / 5.0)
        .resizable(true)
        .show_inside(ui, |ui| {
            show_timeline(
                ui,
                api_addr,
                aspects,
                &store,
                long_tracking,
                fetched_files,
                &res_impl,
            )
        });

    let mut ui = &mut res_impl.make_main_ui(ui);

    show_trackings(
        ui,
        api_addr,
        aspects,
        &store,
        long_tracking,
        fetched_files,
        &res_impl,
    );

    let LongTrackingResultsImpl {
        timeline_window,
        total_cols,
        ..
    } = res_impl;

    if long_tracking.detatched_view {
        let tracking_results = long_tracking.results.iter_mut().enumerate();
        let tracking_results = tracking_results.filter_map(|(col, (_, res))| {
            res.try_poll();
            res.get_mut()
                .map(|res| (col, res.content.track.results.as_mut()))
        });
        crate::app::detached_view::ui_detached(
            ui,
            store,
            timeline_window,
            total_cols,
            &long_tracking.detatched_view_options,
            &mut long_tracking.additionnal_links,
            tracking_results,
        );
    }
}

#[derive(Default, Debug)]
pub(crate) struct ColView<'a> {
    pub left_commit: Option<&'a mut Commit>,
    pub effective_targets: Vec<(&'a mut CodeRange, usize)>,
    pub original_targets: Vec<(&'a mut CodeRange, usize)>,
    pub matcheds: Vec<(&'a mut CodeRange, usize)>,
    pub additions: Option<&'a [u32]>,
    pub deletions: Option<&'a [u32]>,
}

#[allow(unused)]
fn show_code_view(
    ui: &mut egui::Ui,
    api_addr: &str,
    curr_view: &mut ColView<'_>,
    fetched_files: &mut FetchedFiles,
) -> Option<egui::text_edit::TextEditOutput> {
    let curr_file = {
        let curr = if curr_view.matcheds.get(0).is_some() {
            curr_view.matcheds.get_mut(0)
        } else {
            curr_view.original_targets.get_mut(0)
        };
        &mut curr?.0.file
    };

    let file_result = fetched_files.entry(curr_file.clone());
    let te = ui
        .show_remote_code1(
            api_addr,
            &mut curr_file.commit,
            &mut curr_file.file_path,
            file_result,
            f32::INFINITY,
            false,
        )
        .2;
    if let Some(egui::InnerResponse {
        inner: Some(aa), ..
    }) = te
    {
        // ui.painter().debug_rect(
        //     ui.max_rect(),
        //     egui::Color32::RED,
        //     format!("{:?}", curr.range),
        // );
        if let Some(range) = curr_view
            .original_targets
            .get(0)
            .as_ref()
            .and_then(|(x, _)| x.range.as_ref())
        {
            let te = &aa.inner; //&aa.inner.1;
            let offset = 0; //aa.inner.0;
            let range = range.start.saturating_sub(offset)..range.end.saturating_sub(offset);
            let color = egui::Color32::RED.linear_multiply(0.1);
            let rect = highlight_byte_range(ui, te, &range, color);
            // if result_changed {
            //         aa.content_size,
            //         aa.state.offset.y,
            //         aa.inner_rect.height(),
            //         rect.top(),
            //     );
            //     pos_ratio = Some((rect.top() - aa.state.offset.y) / aa.inner_rect.height());
            // }
        }
        if let Some(
            CodeRange {
                range: Some(range), ..
            },
            ..,
        ) = &curr_view.effective_targets.get(0).map(|x| &x.0)
        {
            let te = &aa.inner; //&aa.inner.1;
            let offset = 0; //aa.inner.0;
            let range = range.start.saturating_sub(offset)..range.end.saturating_sub(offset);
            let color = egui::Color32::BLUE.linear_multiply(0.1);
            // let rect = highlight_byte_range(ui, te, &range, color);
            // if result_changed {
            //         aa.content_size,
            //         aa.state.offset.y,
            //         aa.inner_rect.height(),
            //         rect.top(),
            //     );
            //     pos_ratio = Some((rect.top() - aa.state.offset.y) / aa.inner_rect.height());
            // }
        }
        if let Some(
            CodeRange {
                range: Some(range), ..
            },
            ..,
        ) = &curr_view.matcheds.get(0).map(|x| &x.0)
        {
            let te = &aa.inner; //&aa.inner.1;
            let offset = 0; //aa.inner.0;
            let range = range.start.saturating_sub(offset)..range.end.saturating_sub(offset);
            let color = egui::Color32::GREEN.linear_multiply(0.1);
            let rect = highlight_byte_range(ui, te, &range, color);
            // if result_changed {
            //         aa.content_size,
            //         aa.state.offset.y,
            //         aa.inner_rect.height(),
            //         rect.top(),
            //     );
            //     pos_ratio = Some((rect.top() - aa.state.offset.y) / aa.inner_rect.height());
            // }
        }

        let te = aa.inner; //&aa.inner.1;
        Some(te)
    } else {
        None
    }
}

type DeferedFocusScroll = (
    f32,
    usize,
    egui::scroll_area::ScrollAreaOutput<Option<Vec<usize>>>,
);

pub(crate) fn show_tree_view(
    ui: &mut egui::Ui,
    min_col: usize,
    api_addr: &str,
    col: usize,
    trigger: bool,
    tree_viewer: &mut Resource<FetchedView>,
    curr_view: &mut ColView<'_>,
    aspects: &mut ComputeConfigAspectViews,
    ports: &mut Attacheds,
    defered_focus_scroll: &mut Option<DeferedFocusScroll>,
) -> Option<Vec<usize>> {
    let mut scroll_focus = None;
    let mut scroll = egui::ScrollArea::both()
        .auto_shrink([false, false])
        .show_viewport(ui, |ui, viewport| {
            ui.set_height(3_000.0);
            ui.set_width(ui.available_width() - 15.0);
            // ui.set_clip_rect(ui.ctx().screen_rect());

            let Some(content) = &mut tree_viewer.content else {
                return None;
            };
            let mut hightlights = vec![];
            let mut focus = None;
            let mut blue_pos = HashMap::<usize, std::option::Option<egui::Rect>>::default();
            let mut green_pos = HashMap::<usize, std::option::Option<egui::Rect>>::default();
            for (_, i) in curr_view.effective_targets.iter() {
                blue_pos.insert(*i, None);
            }
            for (i, b_p) in blue_pos.iter_mut() {
                hightlights.push(HightLightHandle {
                    path: &curr_view
                        .effective_targets
                        .iter()
                        .find(|x| x.1 == *i)
                        .unwrap()
                        .0
                        .path[..],
                    color: &egui::Color32::BLUE,
                    id: *i,
                    screen_pos: b_p,
                });
            }
            let a = if curr_view.matcheds.len() == 1 {
                let Some((foc, i)) = curr_view.matcheds.get(0) else {
                    unreachable!()
                };
                green_pos.insert(*i, None);
                hightlights.push(HightLightHandle {
                    path: &foc.path[..],
                    color: &TARGET_COLOR,
                    id: *i,
                    screen_pos: green_pos.get_mut(i).unwrap(),
                });
                if trigger {
                    let mut pi = foc.path_ids.clone();
                    pi.reverse();
                    focus = Some((&foc.path[..], &pi[..]).into());
                    let id = ui.id();
                    let a = content.show(
                        ui,
                        api_addr,
                        aspects,
                        focus,
                        hightlights,
                        curr_view.additions,
                        curr_view.deletions,
                        "",
                    );
                    let bool = match a {
                        Action::Focused(_) => false,
                        Action::PartialFocused(_) => true,
                        Action::Keep => true,
                        x => panic!("{:?}", x),
                    };
                    if bool {
                        ui.ctx().memory_mut(|mem| {
                            *mem.data.get_temp_mut_or_default::<bool>(id) = true;
                        });
                    }
                    a
                } else {
                    let id = ui.id();
                    let bool = ui
                        .ctx()
                        .memory_mut(|mem| mem.data.get_temp::<bool>(id).unwrap_or(false));
                    let mut pi = foc.path_ids.clone();
                    pi.reverse();
                    if bool {
                        focus = Some((&foc.path[..], &pi[..]).into());
                    }
                    let a = content.show(
                        ui,
                        api_addr,
                        aspects,
                        focus,
                        hightlights,
                        curr_view.additions,
                        curr_view.deletions,
                        "",
                    );
                    let bool = match a {
                        Action::Focused(_) => false,
                        Action::PartialFocused(_) => true,
                        _ => false,
                    };
                    if !bool {
                        ui.ctx().memory_mut(|mem| {
                            mem.data.remove::<bool>(id);
                        });
                    }
                    a
                }
            } else {
                for (_, i) in curr_view.matcheds.iter() {
                    green_pos.insert(*i, None);
                }
                for (i, g_p) in green_pos.iter_mut() {
                    let matched = curr_view.matcheds.iter().find(|x| x.1 == *i).unwrap();
                    hightlights.push(HightLightHandle {
                        path: &matched.0.path[..],
                        color: &TARGET_COLOR,
                        id: *i,
                        screen_pos: g_p,
                    });
                }
                content.show(
                    ui,
                    api_addr,
                    aspects,
                    focus,
                    hightlights,
                    curr_view.additions,
                    curr_view.deletions,
                    "",
                )
            };
            // let a = content.show(ui, aspects, focus, hightlights, "");
            for (k, blue_pos) in blue_pos {
                if let Some(port) = ports.get_mut(col - min_col) {
                    let v = (ui.id().with("blue_highlight").with(k), blue_pos);
                    port.0.insert(k, v);
                }
            }
            for (k, green_pos) in green_pos {
                if let Some(port) = ports.get_mut(col - min_col) {
                    let v = (ui.id().with("green_highlight").with(k), green_pos);
                    port.1.insert(k, v);
                }
            }
            match a {
                Action::Focused(p) => {
                    scroll_focus = Some(p);
                    None
                }
                Action::Clicked(p) => Some(p),
                Action::SerializeKind(k) => {
                    let k = &k.as_any();
                    if let Some(k) = k.downcast_ref::<hyperast_gen_ts_cpp::types::Type>() {
                        aspects.ser_opt_cpp.insert(*k);
                    } else if let Some(k) = k.downcast_ref::<hyperast_gen_ts_java::types::Type>() {
                        aspects.ser_opt_java.insert(*k);
                    }
                    None
                }
                Action::HideKind(k) => {
                    let k = &k.as_any();
                    if let Some(k) = k.downcast_ref::<hyperast_gen_ts_cpp::types::Type>() {
                        aspects.hide_opt_cpp.insert(*k);
                    } else if let Some(k) = k.downcast_ref::<hyperast_gen_ts_java::types::Type>() {
                        aspects.hide_opt_java.insert(*k);
                    }
                    None
                }
                _ => None,
            }
        });
    if let Some(o) = scroll_focus {
        *defered_focus_scroll = Some((o, col - min_col + 1, scroll));
        None
    } else {
        scroll.inner
    }
}

const SC_COPY: egui::KeyboardShortcut =
    egui::KeyboardShortcut::new(egui::Modifiers::CTRL, egui::Key::C);

fn show_commitid_info(
    tracked: Option<TrackingResultWithChanges>,
    ui: &mut egui::Ui,
    code_ranges: Vec<&mut CodeRange>,
) {
    let f_commit = |ui: &mut egui::Ui, id: &str| {
        if ui.available_width() > 300.0 {
            ui.label(format!("commit {}", id));
        } else {
            let label = ui.label(format!("commit {}", &id[..8]));
            if label.hovered() {
                egui::show_tooltip(ui.ctx(), ui.layer_id(), label.id.with("tooltip"), |ui| {
                    ui.label(id);
                    ui.label("CTRL+C to copy (and send in the debug console)");
                });
                if ui.input_mut(|mem| mem.consume_shortcut(&SC_COPY)) {
                    ui.ctx().copy_text(id.to_string());
                }
            }
        }
    };
    let Some(tracked) = tracked else {
        let id = &code_ranges[0].file.commit.id;
        f_commit(ui, id);
        return;
    };
    if let Some(cr) = tracked
        .track
        .intermediary
        .as_ref()
        .or(tracked.track.matched.get(0).as_ref().copied())
        .or(tracked.track.fallback.as_ref())
    {
        let id = &cr.file.commit.id;
        f_commit(ui, id);
    } else {
        let id = &code_ranges[0].file.commit.id;
        f_commit(ui, id);
    }
    let commits_processed = tracked.track.commits_processed - 1;
    if commits_processed > 1 {
        ui.label(format!("skipped {} commits", commits_processed));
    }
}
pub(crate) const TARGET_COLOR: egui::Color32 = egui::Color32::from_rgb(255, 100, 0);

pub(super) fn track(
    ctx: &egui::Context,
    api_addr: &str,
    commit: &Commit,
    file_path: &String,
    range: &Option<Range<usize>>,
    flags: &Flags,
) -> Promise<ehttp::Result<TrackingResultWithChanges>> {
    let ctx = ctx.clone();
    let (sender, promise) = Promise::new();
    // TODO flags should not need the "=true"
    let flags = serde_qs::to_string(flags).unwrap(); //.replace("=true", "1").replace("=false", "0");
    let url = if let Some(range) = range {
        let flags = if flags.is_empty() {
            format!("")
        } else {
            format!("&{}", flags)
        };
        format!(
            "http://{}/track/github/{}/{}/{}/{}?start={}&end={}{}",
            api_addr,
            &commit.repo.user,
            &commit.repo.name,
            &commit.id,
            &file_path,
            &range.start,
            &range.end,
            flags
        )
    } else {
        let flags = if flags.is_empty() {
            format!("")
        } else {
            format!("?{}", flags)
        };
        format!(
            "http://{}/track/github/{}/{}/{}/{}{}",
            api_addr, &commit.repo.user, &commit.repo.name, &commit.id, &file_path, flags
        )
    };

    let mut request = ehttp::Request::get(&url);
    // request
    //     .headers
    //     .insert("Content-Type".to_string(), "text".to_string());

    ehttp::fetch(request, move |response| {
        ctx.request_repaint(); // wake up UI thread
        let resource = response
            .and_then(|response| {
                Resource::<TrackingResult>::from_response(&ctx, response)
                    .map(|x| x.map(|x| x.into()))
            })
            .and_then(|x| x.content.ok_or("Empty body".into()));
        sender.send(resource);
    });
    promise
}

pub(super) fn track_at_path(
    ctx: &egui::Context,
    api_addr: &str,
    commit: &Commit,
    exact_commit: Option<&Commit>,
    path: &[usize],
    flags: &Flags,
) -> Promise<ehttp::Result<TrackingResultWithChanges>> {
    let ctx = ctx.clone();
    let (sender, promise) = Promise::new();
    // TODO flags should not need the "=true"
    let flags = serde_qs::to_string(flags).unwrap(); //.replace("=true", "1").replace("=false", "0");
    let url = {
        format!(
            "http://{}/track_at_path/github/{}/{}/{}/{}?{}{}",
            api_addr,
            &commit.repo.user,
            &commit.repo.name,
            &commit.id,
            path.into_iter()
                .map(|x| x.to_string())
                .collect::<Vec<_>>()
                .join("/"),
            if let Some(exact_commit) = exact_commit {
                format!("before={}&", exact_commit.id)
            } else {
                "".to_string()
            },
            flags
        )
    };

    let mut request = ehttp::Request::get(&url);
    // request
    //     .headers
    //     .insert("Content-Type".to_string(), "text".to_string());

    ehttp::fetch(request, move |response| {
        ctx.request_repaint(); // wake up UI thread
        let resource = response
            .and_then(|response| {
                Resource::<TrackingResult>::from_response(&ctx, response)
                    .map(|x| x.map(|x| x.into()))
            })
            .and_then(|x| x.content.ok_or("Empty body".into()));
        sender.send(resource);
    });
    promise
}

pub(super) fn track_at_path_with_changes(
    ctx: &egui::Context,
    api_addr: &str,
    commit: &Commit,
    path: &[usize],
    flags: &Flags,
) -> Promise<ehttp::Result<TrackingResultWithChanges>> {
    let ctx = ctx.clone();
    let (sender, promise) = Promise::new();
    // TODO flags should not need the "=true"
    let flags = serde_qs::to_string(flags).unwrap(); //.replace("=true", "1").replace("=false", "0");
    let url = {
        format!(
            "http://{}/track_at_path_with_changes/github/{}/{}/{}/{}?{}",
            api_addr,
            &commit.repo.user,
            &commit.repo.name,
            &commit.id,
            path.into_iter()
                .map(|x| x.to_string())
                .collect::<Vec<_>>()
                .join("/"),
            flags
        )
    };

    let mut request = ehttp::Request::get(&url);
    // request
    //     .headers
    //     .insert("Content-Type".to_string(), "text".to_string());

    ehttp::fetch(request, move |response| {
        ctx.request_repaint(); // wake up UI thread
        let resource = response
            .and_then(|response| {
                Resource::<TrackingResultWithChanges>::from_response(&ctx, response)
            })
            .and_then(|x| x.content.ok_or("Empty body".into()));
        sender.send(resource);
    });
    promise
}
