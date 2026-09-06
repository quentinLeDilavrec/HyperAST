use epaint::ahash::HashSet;
use poll_promise::Promise;
use std::collections::{BTreeMap, HashMap, VecDeque};
use std::ops::Range;
use std::sync::Arc;
use std::usize;

use egui_addon::MultiSplitter;
use egui_addon::code_editor::generic_text_buffer::byte_index_from_char_index;
use egui_addon::egui_utils::highlight_byte_range;

use hyperast::store::nodes::fetched::NodeIdentifier;

use super::FetchedFiles;
use super::code_tracking::{TrackingResult, TrackingResultWithChanges, TrackingResultsWithChanges};

use crate::app::code_aspects::remote_fetch_node_old;
use crate::app::code_aspects::{FetchedView, Focus, HighLightHandle};
use crate::app::commit::{CommitMetadata, fetch_commit0};
use crate::app::store::FetchedHyperAST;
use crate::app::tree_view::Action;
use crate::app::types::{CodeRange, ComputeConfigAspectViews, SelectedConfig};
use crate::app::types::{Commit, CommitId};
use crate::app::utils_egui::MyUiExt as _;
use crate::utils_poll::{AccumulableResult, Buffered, MultiBuffered, Resource};

use super::detached_view::LinkConfig;

type AccumulableTrackingResults = AccumulableResult<TrackingResultsWithChanges, Vec<String>>;
type LongTrackingResults = VecDeque<(
    Buffered<Result<CommitMetadata, String>>,
    MultiBuffered<AccumulableTrackingResults, Result<TrackingResultWithChanges, String>>,
)>;
type BufferedPerCommit<T> = HashMap<Commit, Buffered<T>>;

const DEBUG: bool = false;

#[derive(serde::Deserialize, serde::Serialize)]
#[serde(default)]
pub(crate) struct LongTracking {
    pub(crate) flags: Flags,
    pub(crate) ser_view: bool,
    pub(crate) tree_view: bool,
    pub(crate) detached_view: bool,
    pub(crate) detached_view_link_config: LinkConfig,
    pub(crate) origins: Vec<CodeRange>,
    pub(crate) origin_index: usize,
    #[serde(skip)] // TODO remove that
    pub(crate) results: LongTrackingResults,
    #[serde(skip)]
    pub(crate) tree_viewer: BufferedPerCommit<Result<Resource<FetchedView>, String>>,
    pub(crate) manual_links: ManualLinks,
    pub(crate) query_enabled_extras: QueryEnabledExtras,
}

#[derive(serde::Deserialize, serde::Serialize, Default)]
pub(crate) struct ManualLinks {
    pub(crate) links: Vec<[CodeRange; 2]>,
    pub(crate) rm_links: HashSet<[CodeRange; 2]>,
}

#[derive(serde::Deserialize, serde::Serialize, Default)]
pub(crate) struct QueryEnabledExtras {
    pub(crate) query: String,
    #[serde(skip)]
    pub(crate) results:
        HashMap<NodeIdentifier, poll_promise::Promise<super::detached_view::ExtraQueryResult>>,
}

impl QueryEnabledExtras {
    pub(crate) fn result(
        &self,
        id: &NodeIdentifier,
    ) -> Option<crate::app::querying::DetailedResult> {
        let prom = self.results.get(id)?;
        if let Some(Ok(v)) = prom.ready()
            && let Some(Ok(v)) = &v.content
        {
            return Some(v.clone());
        }
        None
    }
}

impl LongTracking {
    pub(crate) fn repo(&self) -> &crate::app::Repo {
        let code = self.origins.get(0).unwrap();
        &code.file.commit.repo
    }
}

impl Default for LongTracking {
    fn default() -> Self {
        Self {
            flags: Default::default(),
            ser_view: false,
            tree_view: true,
            detached_view: false,
            detached_view_link_config: Default::default(),
            origins: vec![Default::default()],
            origin_index: Default::default(),
            results: VecDeque::from(vec![Default::default()]),
            tree_viewer: Default::default(),
            manual_links: Default::default(),
            query_enabled_extras: Default::default(),
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

pub(crate) fn show_config(
    ui: &mut egui::Ui,
    tracking: &mut LongTracking,
) -> (egui::Response, egui::Response) {
    let commit = &tracking.origins[0].file.commit;
    let (resp_repo, resp_commit) = commit.show_clickable(ui);

    ui.checkbox(&mut tracking.tree_view, "tree view");
    ui.checkbox(&mut tracking.ser_view, "serialized view");
    ui.checkbox(&mut tracking.detached_view, "detached view");
    if tracking.detached_view {
        ui.indent("detached_options", |ui| {
            tracking.detached_view_link_config.ui(ui);
        });
    }
    ui.add(egui::Label::new(
        egui::RichText::from("Triggers").font(egui::FontId::proportional(16.0)),
    ));
    tracking.flags.ui(ui);

    // TODO make it more general and use it to choose how to pp/hide
    if tracking.detached_view {
        ui.label("query for detached nodes extras");
        let resp = ui.text_edit_multiline(&mut tracking.query_enabled_extras.query);
        if resp.lost_focus() {
            tracking.query_enabled_extras.results.clear();
        }
    }

    (resp_repo, resp_commit)
}

pub(crate) fn project_modal_handler(
    data: &mut crate::app::AppData,
    pid: crate::app::ProjectId,
) -> crate::app::ProjectId {
    let projects = &mut data.selected_code_data;
    let commit = data.long_tracking.origins.get(0);
    let commit = commit.map(|x| &x.file.commit);
    use crate::app::utils_commit::project_modal_handler;
    let (repo, mut commits) = match project_modal_handler(pid, projects, commit) {
        Ok(value) => value,
        Err(value) => return value,
    };
    data.long_tracking = Default::default();
    let code = data.long_tracking.origins.get_mut(0).unwrap();
    let commit = &mut code.file.commit;
    commit.repo = repo.clone();
    commit.id = *commits.iter_mut().next().unwrap();
    crate::app::ProjectId::INVALID
}

pub(crate) fn commit_modal_handler(
    data: &mut crate::app::AppData,
    cid: crate::app::types::CommitId,
) {
    let code = data.long_tracking.origins.get_mut(0).unwrap();
    let mut commit = code.file.commit.clone();
    commit.id = cid;
    data.long_tracking = Default::default();
    let code = data.long_tracking.origins.get_mut(0).unwrap();
    code.file.commit = commit;
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

pub(crate) type Attacheds = Vec<Attached>;

#[derive(Clone, Debug, Default)]
pub(crate) struct Attached {
    pub left: BTreeMap<usize, AttachedVals>,
    pub right: BTreeMap<usize, AttachedVals>,
}

#[derive(Clone, Debug)]
pub(crate) struct AttachedVals {
    pub id: PortId,
    pub rect: Vec<egui::Rect>,
}

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
    fn new(ui: &mut egui::Ui, api_addr: &'a str, long_tracking: &mut LongTracking) -> Self {
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

        // let min_col = (viewport_x.min / col_width_with_spacing).floor() as usize;
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
        let viewport =
            egui::Rect::from_x_y_ranges(self.viewport_x, ui.available_rect_before_wrap().y_range());
        let layout = egui::Layout::left_to_right(egui::Align::BOTTOM);
        let mut ui = ui.new_child(egui::UiBuilder::new().layout(layout).max_rect(viewport));
        ui.set_clip_rect(self.timeline_window);
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
        let scale = |_x| ui.max_rect().left() + min_col as f32 * (col_width + spacing.x);
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
    long_tracking: &mut LongTracking,
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
            if let Some(track) = track.get(0)
                && DEBUG
            {
                let fallback = track.fallback.as_ref().map(|x| x.file.commit.id.prefix(6));
                let _src = track.src.file.commit.id.prefix(6);
                let _1trm = track
                    .intermediary
                    .as_ref()
                    .map(|x| x.file.commit.id.prefix(6));
                let _mtch = track
                    .matched
                    .iter()
                    .map(|x| x.file.commit.id.prefix(6))
                    .collect::<Vec<_>>();
                ui.painter().debug_rect(
                    ui.available_rect_before_wrap(),
                    egui::Color32::RED,
                    format!(
                        "{:?}\nsrc:{:?}\n1trm:{:?}\nmtch:{:?}",
                        fallback, _src, _1trm, _mtch
                    ),
                );
            }
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
            if let Some(code_range) = track.matched.get(0) {
                md.buffer(fetch_commit0(ui.ctx(), api_addr, &code_range.file.commit));
            } else if let Some(code_range) = &track.fallback {
                md.buffer(fetch_commit0(ui.ctx(), api_addr, &code_range.file.commit));
            // } else if let Some(code_range) = &track.intermediary {
            //     // TODO check
            //     md.buffer(fetch_commit0(ui.ctx(), api_addr, &code_range.file.commit));
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
            ui.probable_fetch_error(err.as_str());
        }
    }
}
fn show_timeline(
    ui: &mut egui::Ui,
    long_tracking: &mut LongTracking,
    res_impl: &LongTrackingResultsImpl<'_>,
) {
    ui.set_clip_rect(ui.max_rect().expand2((1.0, 0.0).into()));

    let mut show_c = |ui: &mut _, col| show_commit(ui, col, long_tracking, res_impl);
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
            let x = delta.x / ui.max_rect().width() * res_impl.viewport_width;
            let max = res_impl.viewport_width - res_impl.timeline_window.width();
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
    aspects: &mut ComputeConfigAspectViews,
    store: &Arc<FetchedHyperAST>,
    long_tracking: &mut LongTracking,
    fetched_files: &mut FetchedFiles,
    res_impl: &LongTrackingResultsImpl<'_>,
) {
    let mut cui = res_impl.make_tracking_ui(ui);

    let mut attached = AttachedImpl::new(res_impl, long_tracking.origin_index);
    attached.ui(&mut cui, long_tracking, store, aspects, fetched_files);

    let LongTrackingResultsImpl {
        timeline_window,
        spacing,
        col_width,
        ..
    } = *res_impl;

    // handle the scrolling
    if let Some((o, _i, mut scroll)) = attached.differed_focus_scroll {
        let o: f32 = o;
        // let g_o = (attached.attacheds.get(i))
        //     .and_then(|a| a.0.get(&0))
        //     .and_then(|x| x.1)
        //     .map(|p| p.min.y)
        //     .unwrap_or(timeline_window.height() / 2000.0);
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
    if DEBUG {
        ui.painter().debug_rect(
            ui.available_rect_before_wrap(),
            egui::Color32::ORANGE,
            format!(
                "{:?} {:?}",
                attached.attacheds.len(),
                attached
                    .attacheds
                    .iter()
                    .map(|x| (x.left.len(), x.right.len()))
                    .collect::<Vec<_>>()
            ),
        );
    }
    for i in 0..attached.attacheds.len() - 1 {
        let (left, right) = attached.attacheds.split_at(i + 1);
        let (greens, blues) = (&left.last().unwrap().right, &right.first().unwrap().left);
        let mut done = HashSet::default();
        // let cable = false;
        // let mut min_right_x = 0.0;
        // let mut min_left_x = 0.0;
        let l_bound = res_impl.viewport_x.min + (i + 1) as f32 * (col_width + spacing.x) - 15.0;
        let r_bound = l_bound + 25.0;
        let render = |g_rect, b_rect| {
            let m_rect: &egui::Rect = g_rect;
            let src_rect: &egui::Rect = b_rect;
            let mut m_pos = m_rect.right_center();
            let mut src_pos = src_rect.left_center();
            let mut ctrl = (m_pos, src_pos);
            ctrl.0.x = l_bound;
            ctrl.1.x = r_bound;
            use egui::NumExt;
            m_pos.x = m_pos.x.at_most(l_bound);
            src_pos.x = src_pos.x.at_least(r_bound);
            let color = ui.style().visuals.text_color();
            let link = epaint::PathShape::line(vec![m_pos, ctrl.0, ctrl.1, src_pos], (2.0, color));
            ui.painter().add(link);
        };
        for (k, g) in greens {
            done.insert(k);
            if let Some(b) = blues.get(&k) {
                for g in &g.rect {
                    for b in &b.rect {
                        render(g, b)
                    }
                }
            }
        }
        for (k, b) in blues {
            if done.contains(&k) {
                continue;
            }
            if let Some(g) = greens.get(&k) {
                for g in &g.rect {
                    for b in &b.rect {
                        render(g, b)
                    }
                }
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
        long_tracking: &mut LongTracking,
        store: &Arc<FetchedHyperAST>,
        aspects: &mut ComputeConfigAspectViews,
        fetched_files: &mut FetchedFiles,
    ) -> () {
        let res_impl = self.res_impl;
        for col in res_impl.col_range() {
            self.attacheds.push(Default::default());

            let ui = &mut self.prep_ui(ui, col);

            let mut curr_view = match init_col_view(
                self,
                col,
                &mut long_tracking.results,
                &mut long_tracking.origins,
            ) {
                Ok(curr_view) => curr_view,
                Err(err) => {
                    ui.probable_fetch_error(err);
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
                todo!();
            }
            (true, false) => {
                let past = tracking_results
                    .get_mut(col - 1)
                    .and_then(|x| x.1.get_mut());
                let Some(past) = past else {
                    original_targets.push(PlacedCode::new(&mut tracking_origins[0], 0));
                    return Ok(curr_view);
                };
                if let Some(changes) = &mut past.content.dst_changes {
                    curr_view.additions = Some(&mut changes.additions);
                }
                if let Some(changes) = &mut past.content.src_changes {
                    assert_ne!(tracking_origins[0].file.commit, changes.commit);
                    curr_view.left_commit = Some(&mut changes.commit);
                }
                let mut origins = tracking_origins.iter_mut();
                for (i, result) in past.content.track.results.iter_mut().enumerate() {
                    curr_view
                        .effective_targets
                        .push(PlacedCode::new(&mut result.src, i));
                    if let Some(origins) = origins.next() {
                        original_targets.push(PlacedCode::new(origins, i));
                    }
                }
            }
            (false, true) => todo!(),
            (false, false) => {
                // nothing to do
                original_targets.push(PlacedCode::new(&mut tracking_origins[0], 0));
            }
        }
    } else if attached.has_past(col) {
        let mut it = tracking_results.range_mut(col - 1..=col);
        let past = it.next();

        if let Some(past) = past.and_then(|x| x.1.get_mut()) {
            if let Some(changes) = &mut past.content.dst_changes {
                curr_view.additions = Some(&mut changes.additions);
            }
            if let Some(changes) = &mut past.content.src_changes {
                curr_view.left_commit = Some(&mut changes.commit);
            }
            for (i, result) in past.content.track.results.iter_mut().enumerate() {
                assert_ne!(
                    result.src.file.commit,
                    **curr_view.left_commit.as_ref().unwrap()
                );
                curr_view
                    .effective_targets
                    .push(PlacedCode::new(&mut result.src, i));
            }
        }
        let curr = it.next();
        if let Some(curr) = curr.and_then(|x| x.1.get_mut()) {
            for (i, result) in curr.content.track.results.iter_mut().enumerate() {
                if result.matched.is_empty() {
                    let res = result.fallback.as_mut();
                    curr_view.matcheds.push(PlacedCode::new(res.unwrap(), i));
                }
                for res in &mut result.matched {
                    curr_view.matcheds.push(PlacedCode::new(res, i));
                }
            }
            if let Some(changes) = &mut curr.content.src_changes {
                curr_view.deletions = Some(&mut changes.deletions);
            }
        }
        if original_targets.is_empty() {
            original_targets.push(PlacedCode::new(&mut tracking_origins[0], 0));
        }
        assert!(it.next().is_none());
    } else {
        let result = tracking_results.get_mut(col).and_then(|x| x.1.get_mut());
        let Some(result) = result else {
            original_targets.push(PlacedCode::new(&mut tracking_origins[0], 0));
            return Ok(curr_view);
        };
        if result.content.track.results.is_empty() {
            return Err(result.errors.join("\n"));
        }
        if let Some(x) = &mut result.content.src_changes {
            curr_view.deletions = Some(&mut x.deletions);
        }
        for (i, result) in result.content.track.results.iter_mut().enumerate() {
            if result.matched.is_empty() {
                let res = result.fallback.as_mut();
                curr_view.matcheds.push(PlacedCode::new(res.unwrap(), i));
            }
            for res in &mut result.matched {
                curr_view.matcheds.push(PlacedCode::new(res, i));
            }
            if let Some(result) = &mut result.intermediary {
                curr_view.effective_targets.push(PlacedCode::new(result, i));
            }
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
        let Some(curr) = curr else { return };
        &curr.code.file.commit
    };
    let tree_viewer = tree_viewer.entry(curr_commit.clone());
    let tree_viewer = tree_viewer.or_default();
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
        ui.probable_fetch_error(err.as_str());
    }) else {
        return;
    };

    if DEBUG {
        let _past_commit = curr_view.left_commit.as_ref();
        let _ori = (curr_view.original_targets.iter())
            .map(|x| x.code.file.commit.id.as_str())
            .collect::<Vec<_>>();
        let _eff = (curr_view.effective_targets.iter())
            .map(|x| x.code.file.commit.id.as_str())
            .collect::<Vec<_>>();
        let _mtch = (curr_view.matcheds.iter())
            .map(|x| x.code.file.commit.id.as_str())
            .collect::<Vec<_>>();
        ui.painter().debug_rect(
            ui.available_rect_before_wrap(),
            egui::Color32::RED,
            format!(
                "{:?}\nori:{:?}\neff:{:?}\nmtch:{:?}",
                _past_commit.map(|x| x.id.as_str()),
                _ori,
                _eff,
                _mtch
            ),
        );
    }

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
    let Some(curr) = curr else { return };
    let curr = &mut *curr.code;
    if attached.is_origin(col) {
        curr.path = p;
        if col == 0 {
            // TODO only request changes when we have none
            log::info!(
                "track_at_path_with_changes {col} {:?} {}",
                curr.path,
                curr.file.commit.id.prefix(6)
            );
            let track_at_path = track_at_path_with_changes(
                ui.ctx(),
                res_impl.api_addr,
                &curr.file.commit,
                &curr.path,
                flags,
            );
            attached.waiting.push((col, track_at_path));
        } else if let Some(past_commit) = &curr_view.left_commit {
            // TODO allow to reset tracking
            attached.new_origins.push(CodeRange {
                file: curr.file.clone(),
                range: None,
                path: curr.path.clone(),
                path_ids: vec![],
            });
            if DEBUG {
                ui.painter().debug_rect(
                    ui.available_rect_before_wrap(),
                    egui::Color32::RED,
                    past_commit.id.as_str(),
                );
            }
            assert_ne!(&curr.file.commit, *past_commit);
            log::info!(
                "track_at_path {col} {:?} {} {}",
                curr.path,
                curr.file.commit.id.prefix(6),
                past_commit.id.prefix(6)
            );
            let track_at_path = track_at_path(
                ui.ctx(),
                res_impl.api_addr,
                &curr.file.commit,
                Some(past_commit),
                &curr.path,
                &flags,
            );
            attached.waiting.push((col, track_at_path));
        } else {
            todo!("{:?} {}", curr.path, curr.file.commit.id.prefix(6));
        }
    } else {
        if col == 0 {
            log::info!(
                "track_at_path_with_changes {col} {:?} {}",
                curr.path,
                curr.file.commit.id.prefix(6),
            );
            let track_at_path = track_at_path_with_changes(
                ui.ctx(),
                res_impl.api_addr,
                &curr.file.commit,
                &p,
                flags,
            );
            attached.waiting.push((col, track_at_path));
        } else if let Some(past_commit) = curr_view.left_commit.as_ref() {
            let present_commit = &curr.file.commit;
            // TODO allow to reset tracking
            assert_ne!(&present_commit, past_commit);
            log::info!(
                "track_at_path {col} {:?} {} {}",
                curr.path,
                curr.file.commit.id.prefix(6),
                past_commit.id.prefix(6)
            );
            let track_at_path = track_at_path(
                ui.ctx(),
                res_impl.api_addr,
                &present_commit,
                Some(past_commit),
                &p,
                flags,
            );
            attached.waiting.push((col, track_at_path));
        } else {
            // TODO handle case of multiple concurrent tracking requests
            log::info!("attempt tracking while still waiting for previous tracking request");
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
    let Some(curr) = curr else { return };
    let curr = &mut *curr.code;

    if curr.range == Some(r.clone()) {
        return;
    }
    if is_origin {
        curr.range = Some(r.clone());
    }
    if col == 0 {
        log::info!(
            "track {col} {:?} {}",
            curr.path,
            curr.file.commit.id.prefix(6),
        );
        attached.waiting.push((
            col,
            track(
                ui.ctx(),
                api_addr,
                &curr.file.commit,
                &curr.file.file_path,
                &Some(r),
                None,
                flags,
            ),
        ));
    } else if let Some(left_commit) = &curr_view.left_commit {
        // TODO allow to reset tracking
        log::info!(
            "track {col} {:?} {} {}",
            curr.path,
            curr.file.commit.id.prefix(6),
            left_commit.id.prefix(6)
        );
        attached.waiting.push((
            col,
            track(
                ui.ctx(),
                api_addr,
                &curr.file.commit,
                &curr.file.file_path,
                &Some(r),
                Some(*left_commit),
                flags,
            ),
        ));
    } else {
        // TODO allow to reset tracking
        todo!("check behavior");
    }
}

pub(crate) fn show_results(
    ui: &mut egui::Ui,
    api_addr: &str,
    aspects: &mut ComputeConfigAspectViews,
    store: Arc<FetchedHyperAST>,
    long_tracking: &mut LongTracking,
    fetched_files: &mut FetchedFiles,
) {
    let res_impl = LongTrackingResultsImpl::new(ui, api_addr, long_tracking);

    egui::panel::TopBottomPanel::bottom("Timeline Map")
        .frame(egui::Frame::side_top_panel(ui.style()).inner_margin(0.0))
        .height_range(0.0..=ui.available_height() / 3.0)
        .default_height(ui.available_height() / 5.0)
        .resizable(true)
        .show_inside(ui, |ui| show_timeline(ui, long_tracking, &res_impl));

    let ui = &mut res_impl.make_main_ui(ui);

    show_trackings(ui, aspects, &store, long_tracking, fetched_files, &res_impl);

    let LongTrackingResultsImpl {
        timeline_window,
        total_cols,
        ..
    } = res_impl;

    if long_tracking.detached_view {
        let tracking_results = long_tracking.results.iter_mut().enumerate();
        let tracking_results = tracking_results.filter_map(|(col, (_, res))| {
            res.try_poll();
            res.get_mut()
                .map(|res| (col, res.content.track.results.as_mut()))
        });
        super::detached_view::ui_detached(
            ui,
            store,
            timeline_window,
            total_cols,
            &long_tracking.detached_view_link_config,
            tracking_results,
            &mut long_tracking.manual_links,
            &mut long_tracking.query_enabled_extras,
        );
    }
}

#[derive(Default, Debug)]
pub(crate) struct ColView<'a> {
    pub left_commit: Option<&'a mut Commit>,
    pub effective_targets: Vec<PlacedCode<'a>>,
    pub original_targets: Vec<PlacedCode<'a>>,
    pub matcheds: Vec<PlacedCode<'a>>,
    pub additions: Option<&'a [u32]>,
    pub deletions: Option<&'a [u32]>,
}

#[derive(Debug)]
pub(crate) struct PlacedCode<'a> {
    pub code: &'a mut CodeRange,
    pub id: usize,
}

impl<'a> PlacedCode<'a> {
    pub fn new(code: &'a mut CodeRange, id: usize) -> Self {
        Self { code, id }
    }
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
        &mut curr?.code.file
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
    let Some(egui::InnerResponse {
        inner: Some(aa), ..
    }) = te
    else {
        return None;
    };
    let first_ori = curr_view.original_targets.get(0);
    if let Some(range) = first_ori.as_ref().and_then(|x| x.code.range.as_ref()) {
        let te = &aa.inner;
        let color = egui::Color32::RED.linear_multiply(0.1);
        let rect = highlight_byte_range(ui, te, &range, color);
    }
    // let first_targ = curr_view.effective_targets.get(0);
    // let first_targ = first_targ.and_then(|x| x.0.range.as_ref());
    // if let Some(range) = first_targ {
    //     let te = &aa.inner;
    //     let offset = 0;
    //     let color = egui::Color32::BLUE.linear_multiply(0.1);
    //     let rect = highlight_byte_range(ui, te, &range, color);
    // }
    let first_match = curr_view.matcheds.get(0);
    let first_match = first_match.and_then(|x| x.code.range.as_ref());
    if let Some(range) = first_match {
        let te = &aa.inner;
        let color = egui::Color32::GREEN.linear_multiply(0.1);
        let rect = highlight_byte_range(ui, te, &range, color);
    }

    let te = aa.inner;
    Some(te)
}

type ClickedNode = egui::scroll_area::ScrollAreaOutput<Option<crate::app::tree_view::Offsets>>;
type DeferedFocusScroll = (f32, usize, ClickedNode);

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
    deferred_focus_scroll: &mut Option<DeferedFocusScroll>,
) -> Option<crate::app::tree_view::Offsets> {
    use egui::scroll_area::ScrollBarVisibility as Vis;
    let mut scroll_focus = None;
    let scroll = egui::ScrollArea::both()
        .auto_shrink([false, false])
        .scroll_bar_visibility(if ui.max_rect().width() < 500. {
            Vis::AlwaysHidden
        } else {
            Vis::VisibleWhenNeeded
        })
        .show_viewport(ui, |ui, _viewport| {
            ui.set_height(3_000.0);
            ui.set_max_width(500.);
            ui.set_min_width(200.);

            let Some(content) = &mut tree_viewer.content else {
                return None;
            };

            let a = show_tree_view_aux(
                ui, min_col, api_addr, col, trigger, content, curr_view, aspects, ports,
            );
            match a {
                Action::Focused(p) => {
                    dbg!(p);
                    scroll_focus = Some(p);
                    None
                }
                Action::Clicked(p) => Some(p),
                a => {
                    aspects.on_action(a);
                    None
                }
            }
        });
    if let Some(o) = scroll_focus {
        *deferred_focus_scroll = Some((o, col - min_col + 1, scroll));
        None
    } else {
        scroll.inner
    }
}

fn show_tree_view_aux(
    ui: &mut egui::Ui,
    min_col: usize,
    api_addr: &str,
    col: usize,
    trigger: bool,
    content: &mut FetchedView,
    curr_view: &mut ColView<'_>,
    aspects: &mut ComputeConfigAspectViews,
    ports: &mut Attacheds,
) -> Action {
    let action;
    let mut hightlights = vec![];
    let mut focus = vec![];
    let mut blue_pos = Vec::<Option<egui::Rect>>::default();
    let mut green_pos = Vec::<Option<egui::Rect>>::default();
    for _ in curr_view.effective_targets.iter() {
        blue_pos.push(None);
    }
    for (i, b_p) in blue_pos.iter_mut().enumerate() {
        let x = &curr_view.effective_targets[i];
        hightlights.push(HighLightHandle {
            path: &x.code.path[..],
            color: &egui::Color32::BLUE,
            id: x.id,
            screen_pos: b_p,
        });
    }
    if curr_view.matcheds.len() == 1 {
        let Some(x) = curr_view.matcheds.get(0) else {
            unreachable!()
        };
        let foc = &*x.code;
        green_pos.push(None);
        hightlights.push(HighLightHandle {
            path: &foc.path[..],
            color: &TARGET_COLOR,
            id: x.id,
            screen_pos: green_pos.last_mut().unwrap(),
        });
        let mut pi = foc.path_ids.clone();
        pi.reverse();
        let id = ui.id();
        let bool = ui
            .ctx()
            .memory_mut(|mem| mem.data.get_temp::<bool>(id).unwrap_or(trigger));
        let bool = bool || trigger;
        if bool {
            focus.push(Focus::new(&foc.path[..], &pi[..]));
        }
        action = content.show(
            ui,
            api_addr,
            aspects,
            focus,
            hightlights,
            curr_view.additions,
            curr_view.deletions,
            "",
            false,
        );
        let bool = match action {
            Action::Focused(_) => false,
            Action::PartialFocused(_) => trigger,
            Action::Keep => trigger,
            _ if trigger => panic!("{:?}", x),
            _ => false,
        };
        if bool {
            ui.ctx().memory_mut(|mem| {
                *mem.data.get_temp_mut_or_default::<bool>(id) = true;
            });
        } else if !trigger {
            ui.ctx().memory_mut(|mem| {
                mem.data.remove_temp::<bool>(id);
            });
        }
    } else {
        for _ in curr_view.matcheds.iter() {
            green_pos.push(None);
        }
        for (i, g_p) in green_pos.iter_mut().enumerate() {
            let matched = &curr_view.matcheds[i];
            hightlights.push(HighLightHandle {
                path: &matched.code.path[..],
                color: &TARGET_COLOR,
                id: matched.id,
                screen_pos: g_p,
            });
        }
        if DEBUG {
            ui.painter().debug_rect(
                ui.clip_rect().shrink(10.0),
                egui::Color32::GOLD,
                format!(
                    "{:?}",
                    curr_view.matcheds.iter().map(|x| x.id).collect::<Vec<_>>()
                ),
            );
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
            false,
        );
        action = match a {
            Action::PartialFocused(_) => Action::Keep,
            Action::Focused(_) => Action::Keep,
            x => x,
        };
    }
    let Some(port) = ports.get_mut(col - min_col) else {
        return action;
    };
    for (i, pos) in blue_pos.into_iter().enumerate() {
        let Some(pos) = pos else {
            continue;
        };
        let k = curr_view.effective_targets[i].id;
        let id = ui.id().with("blue_highlight").with(k);
        let x = port
            .left
            .entry(k)
            .or_insert(AttachedVals { id, rect: vec![] });
        assert_eq!(x.id, id);
        x.rect.push(pos);
    }
    for (i, pos) in green_pos.into_iter().enumerate() {
        let Some(pos) = pos else {
            continue;
        };
        let k = curr_view.matcheds[i].id;
        let id = ui.id().with("green_highlight").with(k);
        let x = port
            .right
            .entry(k)
            .or_insert(AttachedVals { id, rect: vec![] });
        assert_eq!(x.id, id);
        x.rect.push(pos);
    }
    if DEBUG {
        ui.painter()
            .debug_rect(ui.clip_rect(), egui::Color32::WHITE, format!("{:#?}", port));
    }
    action
}

const SC_COPY: egui::KeyboardShortcut =
    egui::KeyboardShortcut::new(egui::Modifiers::CTRL, egui::Key::C);

fn show_commitid_info(
    tracked: Option<TrackingResultWithChanges>,
    ui: &mut egui::Ui,
    code_ranges: Vec<&mut CodeRange>,
) {
    let f_commit = |ui: &mut egui::Ui, id: &CommitId| {
        if ui.available_width() > 300.0 {
            ui.label(format!("commit {}", id));
        } else {
            let label = ui.label(format!("commit {}", id.prefix(8)));
            if label.hovered() {
                egui::Tooltip::always_open(
                    ui.ctx().clone(),
                    ui.layer_id(),
                    label.id.with("tooltip"),
                    &label,
                )
                .show(|ui| {
                    ui.label(id.as_str());
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
    if let Some(cr) = None
        .or(tracked.track.matched.get(0).as_ref().copied())
        .or(tracked.track.fallback.as_ref())
    // .or(tracked.track.intermediary.as_ref())
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
pub(crate) const TARGET_COLOR: egui::Color32 = egui::Color32::from_rgb(255, 165, 0);

pub(super) fn track(
    ctx: &egui::Context,
    api_addr: &str,
    commit: &Commit,
    file_path: &String,
    range: &Option<Range<usize>>,
    before: Option<&Commit>,
    flags: &Flags,
) -> Promise<ehttp::Result<TrackingResultWithChanges>> {
    let ctx = ctx.clone();
    let (sender, promise) = Promise::new();
    // TODO flags should not need the "=true"
    let flags = serde_qs::to_string(flags).unwrap(); //.replace("=true", "1").replace("=false", "0");
    let url = if let Some(range) = range {
        let flags = if flags.is_empty() {
            Default::default()
        } else {
            format!("&{}", flags)
        };
        let rest = if let Some(before) = before {
            format!("before={}&{}", before.id.prefix(6), flags)
        } else {
            flags
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
            rest
        )
    } else {
        let flags = if flags.is_empty() {
            Default::default()
        } else {
            format!("?{}", flags)
        };
        format!(
            "http://{}/track/github/{}/{}/{}/{}{}",
            api_addr, &commit.repo.user, &commit.repo.name, &commit.id, &file_path, flags
        )
    };

    let request = ehttp::Request::get(&url);
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

    let request = ehttp::Request::get(&url);

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

    let request = ehttp::Request::get(&url);

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

pub(crate) fn prepare_export(
    long_tracking: &mut LongTracking,
    mut pp: impl FnMut(NodeIdentifier) -> String,
) -> impl serde::Serialize {
    let tracking_results = long_tracking.results.iter_mut().enumerate();
    let tracking_results = tracking_results.filter_map(|(col, (_, res))| {
        res.try_poll();
        res.get_mut()
            .map(|res| (col, res.content.track.results.as_mut_slice()))
    });
    let manual_links = &mut long_tracking.manual_links;
    let manual_rm_links = &mut manual_links.rm_links;
    let manual_links = &mut manual_links.links;
    use super::code_tracking::TrackingResult;
    use crate::app::types::CodeRange;
    #[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
    struct Res {
        code_ranges: Vec<CodeRange>,
        extras: Vec<crate::app::querying::DetailedResult>,
        pp: HashMap<u32, String>,
        tracking_results: Vec<Vec<TrackingResult<usize>>>,
        manual_rm_links: Vec<[usize; 2]>,
        manual_links: Vec<[usize; 2]>,
    }
    let mut code_ranges: Vec<CodeRange> = vec![];
    let mut extras: Vec<crate::app::querying::DetailedResult> = vec![];
    let mut pp_map: HashMap<u32, String> = HashMap::new();
    let mut find_or_insert = |x: &_| {
        code_ranges.iter().position(|y| y == x).unwrap_or_else(|| {
            let e = long_tracking
                .query_enabled_extras
                .result(x.path_ids.first().unwrap());
            if let Some(mut e) = e {
                e.cached = None;
                for c in &e.captures {
                    let code = pp(*c);
                    pp_map.insert(c.to_u32(), code);
                }
                extras.push(e);
            }
            code_ranges.push(x.clone());
            code_ranges.len() - 1
        })
    };
    let mut tr = |x: &mut TrackingResult| TrackingResult {
        compute_time: x.compute_time,
        commits_processed: x.commits_processed,
        src: find_or_insert(&x.src),
        intermediary: x.intermediary.as_ref().map(&mut find_or_insert),
        fallback: x.fallback.as_ref().map(&mut find_or_insert),
        matched: x
            .matched
            .iter()
            .map(&mut find_or_insert)
            .collect::<Vec<_>>(),
    };
    let tracking_results = tracking_results
        .map(|x| x.1.into_iter().map(&mut tr).collect::<Vec<_>>())
        .collect::<Vec<_>>();

    let res = Res {
        tracking_results,
        manual_rm_links: manual_rm_links
            .iter()
            .map(|[a, b]| [find_or_insert(a), find_or_insert(b)])
            .collect::<Vec<_>>(),
        manual_links: manual_links
            .iter()
            .map(|[a, b]| [find_or_insert(a), find_or_insert(b)])
            .collect::<Vec<_>>(),
        code_ranges,
        extras,
        pp: pp_map,
    };
    res
}

#[cfg(feature = "process_mining")]
fn compute_event_log(
    long_tracking: &mut LongTracking,
    stores: &FetchedHyperAST,
    pp: impl Fn(NodeIdentifier) -> String,
) -> process_mining::EventLog {
    let tracking_results = long_tracking.results.iter_mut().enumerate();
    let tracking_results = tracking_results.filter_map(|(col, (_, res))| {
        res.try_poll();
        res.get_mut()
            .map(|res| (col, res.content.track.results.as_mut_slice()))
    });

    let mut event_log = empty_event_log();
    event_log.traces.push(example_trace());

    use process_mining::core::event_data::case_centric::Attribute;
    use process_mining::core::event_data::case_centric::AttributeValue;
    use process_mining::core::event_data::case_centric::Event;
    use process_mining::core::event_data::case_centric::Trace;

    macro_rules! str_attr {
        ($key:expr, $value:expr) => {
            Attribute::new($key.to_string(), AttributeValue::String($value.to_string()))
        };
    }
    macro_rules! attr {
        ("concept:name", $name:expr) => {
            str_attr!("concept:name", $name)
        };
    }

    let mut trace = Trace::default();
    trace
        .attributes
        .push(attr!("concept:name", "hand made example"));
    let mut event = Event::new("Modify".to_string());
    event.attributes.push(str_attr!("instance:kind", "method"));
    event.attributes.push(str_attr!("instance:name", "main"));
    event.attributes.push(Attribute::new(
        "time:timestamp".to_string(),
        AttributeValue::Date(
            chrono::DateTime::from_timestamp_nanos(1662921288_000_000_000).fixed_offset(),
        ),
    ));
    trace.events.push(event);

    let mut trace = Trace::default();
    trace.attributes.push(Attribute::new(
        "concept:name".to_string(),
        AttributeValue::String("actual trace".to_string()),
    ));
    for tr in tracking_results.flat_map(|x| x.1.into_iter()) {
        let mut event = if tr.matched.is_empty() {
            Event::new("Insert".to_string())
        } else if tr
            .matched
            .iter()
            .any(|x| tr.src.path_ids.first() == x.path_ids.first())
        {
            Event::new("Not Modified".to_string())
        } else {
            Event::new("Modified".to_string())
        };
        let src = &tr.src;
        let Some(id) = src.path_ids.first() else {
            continue;
        };

        let node_store = stores.node_store.read().unwrap();
        let Some(r) = node_store.try_resolve::<hyperast::types::AnyType>(*id) else {
            stores.demand_node(*id);
            continue;
        };
        let kind = stores.resolve_type(&id);
        event.attributes.push(str_attr!("instance:kind", kind));
        use hyperast::types::WithStats;
        event.attributes.push(str_attr!("instance:size", r.size()));

        let p = if let Some(r) = &tr.src.range {
            format!("{}:{}..{}", tr.src.file.file_path, r.start, r.end)
        } else {
            tr.src.file.file_path.clone()
        };
        event.attributes.push(Attribute::new(
            "instance:position".to_string(),
            AttributeValue::String(p),
        ));
        event.attributes.push(Attribute::new(
            "instance:id".to_string(),
            AttributeValue::Int(tr.src.path_ids.first().unwrap().to_u32() as i64),
        ));

        let Some(mut e) = long_tracking.query_enabled_extras.result(id) else {
            continue;
        };
        let mut attributes = vec![];

        e.cached = None;
        for (c_n, _, cs) in e.captures() {
            for c in cs {
                let code = pp(*c);
                attributes.push(str_attr!(format!("capture:{}", c_n), code));
            }
        }
        event.attributes.push(Attribute::new(
            "instance:more".to_string(),
            AttributeValue::Container(attributes),
        ));
        trace.events.push(event);
    }

    event_log.traces.push(trace);

    event_log
}

#[cfg(feature = "process_mining")]
fn empty_event_log() -> process_mining::EventLog {
    use process_mining::core::event_data::case_centric::Attribute;
    use process_mining::core::event_data::case_centric::AttributeValue;
    use process_mining::core::event_data::case_centric::EventLogExtension;
    let mut event_log = process_mining::core::EventLog::default();

    let extensions = event_log.extensions.get_or_insert_default();
    extensions.push(EventLogExtension {
        name: "Concept".to_string(),
        prefix: "concept".to_string(),
        uri: "http://www.xes-standard.org/concept.xesext".to_string(),
    });
    extensions.push(EventLogExtension {
        name: "Time".to_string(),
        prefix: "time".to_string(),
        uri: "http://www.xes-standard.org/time.xesext".to_string(),
    });
    event_log
        .global_trace_attrs
        .get_or_insert_default()
        .push(Attribute::new(
            "concept:name".to_string(),
            AttributeValue::String("trace".to_string()),
        ));
    event_log
        .global_event_attrs
        .get_or_insert_default()
        .push(Attribute::new(
            "concept:name".to_string(),
            AttributeValue::String("event".to_string()),
        ));
    event_log
}

#[cfg(feature = "process_mining")]
fn example_trace() -> process_mining::core::event_data::case_centric::Trace {
    use process_mining::core::event_data::case_centric::Attribute;
    use process_mining::core::event_data::case_centric::AttributeValue;
    use process_mining::core::event_data::case_centric::Event;
    use process_mining::core::event_data::case_centric::Trace;
    let mut trace = Trace::default();
    trace.attributes.push(Attribute::new(
        "concept:name".to_string(),
        AttributeValue::String("example_trace".to_string()),
    ));
    let mut event = Event::new("Insert".to_string());
    event.attributes.push(Attribute::new(
        "time:timestamp".to_string(),
        AttributeValue::Date(
            chrono::DateTime::from_timestamp_nanos(1662921288_000_000_000).fixed_offset(),
        ),
    ));
    trace.events.push(event);
    let mut event = Event::new("Delete".to_string());
    event.attributes.push(Attribute::new(
        "time:timestamp".to_string(),
        AttributeValue::Date(
            chrono::DateTime::from_timestamp_nanos(1662931288_000_000_000).fixed_offset(),
        ),
    ));
    trace.events.push(event);
    trace
}

#[cfg(feature = "process_mining")]
pub(crate) fn compute_compressed_xes(
    long_tracking: &mut LongTracking,
    stores: &FetchedHyperAST,
    pp: impl Fn(hyperast::store::nodes::fetched::NodeIdentifier) -> String,
) -> Result<Result<Vec<u8>, std::io::Error>, impl std::error::Error + 'static> {
    let event_log = compute_event_log(long_tracking, stores, pp);

    let buffer = Vec::new();
    let mut writer = flate2::write::GzEncoder::new(buffer, flate2::Compression::default());

    let res = process_mining::core::event_data::case_centric::xes::export_xes_event_log(
        &mut writer,
        &event_log,
    )
    .map(|_| writer.finish());
    res
}

#[cfg(feature = "process_mining")]
pub(crate) fn compute_xes(
    long_tracking: &mut LongTracking,
    stores: &FetchedHyperAST,
    pp: impl Fn(hyperast::store::nodes::fetched::NodeIdentifier) -> String,
) -> Result<String, impl std::error::Error + 'static> {
    let event_log = compute_event_log(long_tracking, stores, pp);

    struct StringBuffer(String);
    impl std::io::Write for StringBuffer {
        fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
            self.0.push_str(&String::from_utf8_lossy(buf));
            Ok(buf.len())
        }

        fn flush(&mut self) -> std::io::Result<()> {
            Ok(())
        }
    }
    let mut buffer = StringBuffer(String::new());
    process_mining::core::event_data::case_centric::xes::export_xes_event_log(
        &mut buffer,
        &event_log,
    )
    .map(|_| buffer.0)
}
