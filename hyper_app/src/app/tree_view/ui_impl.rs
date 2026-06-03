use std::ops::ControlFlow;

use egui::Widget;
use egui_addon::syntax_highlighting as syntax_highlighter;

use crate::app::code_aspects::Focus;
use crate::app::code_aspects::HighLightHandle;
use crate::app::long_tracking::TARGET_COLOR;

use super::{Action, FetchedViewImpl, PrefillCache};
use super::{CLIP_LEN, DEBUG_LAYOUT};

use hyperast::types::{AnyType, HyperType, Labeled};
use hyperast::types::{WithChildren as _, WithStats as _};

pub use super::store::{LabelIdentifier, NodeIdentifier};

struct FoldRet<U, V> {
    #[allow(unused)]
    toggle_response: egui::Response,
    header_response: egui::Response,
    header_returned: U,
    body_response: Option<egui::Response>,
    body_returned: Option<V>,
}

impl<U, V>
    From<(
        egui::Response,
        egui::InnerResponse<U>,
        Option<egui::InnerResponse<V>>,
    )> for FoldRet<U, V>
{
    fn from(
        value: (
            egui::Response,
            egui::InnerResponse<U>,
            Option<egui::InnerResponse<V>>,
        ),
    ) -> Self {
        let (resp, ret) = value
            .2
            .map_or((None, None), |x| (Some(x.response), Some(x.inner)));
        Self {
            toggle_response: value.0,
            header_response: value.1.response,
            header_returned: value.1.inner,
            body_response: resp,
            body_returned: ret,
        }
    }
}

struct ChangeColor {
    add: egui::Color32,
    del: egui::Color32,
    both: egui::Color32,
}

impl ChangeColor {
    fn select(&self, is_add: bool, is_del: bool) -> Option<egui::Color32> {
        if is_add && is_del {
            Some(self.both)
        } else if is_add {
            Some(self.add)
        } else if is_del {
            Some(self.del)
        } else {
            None
        }
    }
}

const CC: ChangeColor = ChangeColor {
    add: egui::Color32::GREEN,
    del: egui::Color32::RED,
    both: egui::Color32::from_rgb(200, 200, 0),
};

impl<'a> FetchedViewImpl<'a> {
    pub(super) fn ui_both_impl(
        &mut self,
        ui: &mut egui::Ui,
        kind: AnyType,
        size: u32,
        _nid: NodeIdentifier,
        label: LabelIdentifier,
        cs: &[NodeIdentifier],
    ) -> Action {
        if self.is_hidden(kind) {
            self.prefill_cache.get_or_insert_default();
            return Action::Keep;
        }
        let min = ui.available_rect_before_wrap().min;
        if min.y < 0.0 {
            self.min_before_count += 1;
        }

        self.draw_count += 1;
        let id = ui.id().with(&self.path);
        let mut load_with_default_open =
            egui::collapsing_header::CollapsingState::load_with_default_open(ui.ctx(), id, false);
        self.additions_deletions_compute(size);
        let no_change = self.additions.is_none() && self.deletions.is_none();
        let add = self.additions.unwrap_or_default();
        let del = self.deletions.unwrap_or_default();
        if self.focus.is_some() {
            load_with_default_open.set_open(true)
        } else if self.open_changed
            && (!add.is_empty() || !del.is_empty() || !self.hightlights.is_empty())
        {
            load_with_default_open.set_open(true)
        }

        ui.spacing_mut().icon_spacing /= 2.0;
        let show: FoldRet<_, _> = load_with_default_open
            .show_header(ui, |ui| {
                let label_store = self.store.label_store.read().unwrap();
                let label = if let Some(label) = label_store.try_resolve(&label) {
                    label
                } else {
                    if !(self.store.labels_pending.lock().unwrap())
                        .iter()
                        .any(|x| x.contains(&label))
                    {
                        (self.store.labels_waiting.lock().unwrap())
                            .get_or_insert(Default::default())
                            .insert(label);
                    }
                    "..."
                };
                let mut label = egui::RichText::new(label);
                let text = format!("{} ", kind);
                let rt = egui::RichText::new(text).monospace();
                let gp = self.global_pos.as_ref();
                let rt = if no_change || gp.is_none() {
                    rt
                } else if let Some(c) = CC.select(add.last() == gp, del.last() == gp) {
                    let tc = &ui.style().visuals.code_bg_color;
                    rt.color(tc.lerp_to_gamma(c, 0.7))
                } else if let Some(c) = CC.select(!add.is_empty(), !del.is_empty()) {
                    let tc = &ui.style().visuals.code_bg_color;
                    rt.color(tc.lerp_to_gamma(c, 0.6))
                } else {
                    label = label.size(8.0).color(egui::Color32::GRAY);
                    rt.size(8.0).color(egui::Color32::GRAY)
                };

                let r = ui.label(rt);

                if !add.is_empty() {
                    let tc = &ui.style().visuals.code_bg_color;
                    let c = tc.lerp_to_gamma(CC.add, 0.5);
                    let rt_adds = egui::RichText::new(format!("+{}", add.len())).color(c);
                    ui.label(rt_adds.monospace());
                }
                if !del.is_empty() {
                    let tc = &ui.style().visuals.code_bg_color;
                    let c = tc.lerp_to_gamma(CC.del, 0.5);
                    let rt_dels = egui::RichText::new(format!("-{} ", del.len())).color(c);
                    ui.label(rt_dels.monospace());
                }
                ui.label(label);
                r
            })
            .body(|ui| self.children_ui(ui, cs, self.global_pos.map(|x| x - size)))
            .into();

        let mut prefill = self.prefill_cache.take().unwrap_or_default();
        prefill.head = show.header_response.rect.height();
        if DEBUG_LAYOUT {
            ui.painter().debug_rect(
                show.header_response.rect.union(
                    show.body_response
                        .as_ref()
                        .map_or(egui::Rect::NOTHING, |x| x.rect),
                ),
                egui::Color32::BLUE,
                format!("\t\t\t\t\t\t\t\t{:?}", show.header_response.rect),
            );
        }
        let mut rect = show.header_response.rect.union(
            show.body_response
                .as_ref()
                .map_or(egui::Rect::NOTHING, |x| x.rect),
        );
        rect.max.x += 10.0;

        for handle in &mut self.hightlights {
            selection_highlight(ui, handle, min, rect, self.root_ui_id);
        }
        self.prefill_cache = Some(prefill);

        let interact = show.header_returned.interact(egui::Sense::click_and_drag());

        let f = |ui: &mut egui::Ui| {
            ui.button("hide kind")
                .clicked()
                .then(|| Action::HideKind(kind))
        };
        if interact.drag_stopped() {
            show_node_menu_aux(ui, interact, f)
                .or(show.body_returned)
                .unwrap_or_default()
        } else if interact.clicked() {
            Action::Clicked(self.path.to_vec())
        } else if self.focus.iter().any(|x| x.is_empty()) {
            Action::Focused(min.y)
        } else {
            show_node_menu_aux(ui, interact, f)
                .or(show.body_returned)
                .unwrap_or_default()
        }
    }

    pub(super) fn ui_children_impl2(
        &mut self,
        ui: &mut egui::Ui,
        kind: AnyType,
        size: u32,
        nid: NodeIdentifier,
        cs: &[NodeIdentifier],
    ) -> Action {
        if self.is_hidden(kind) {
            self.prefill_cache.get_or_insert_default();
            return Action::Keep;
        }
        let min = ui.available_rect_before_wrap().min;
        if min.y < 0.0 {
            self.min_before_count += 1;
        }
        self.draw_count += 1;
        let id = ui.id().with(&self.path);

        self.additions_deletions_compute(size);
        if self.is_pp(kind) {
            let action = self.show_pp(ui, nid, size);
            return action;
        }

        let mut load_with_default_open =
            egui::collapsing_header::CollapsingState::load_with_default_open(ui.ctx(), id, false);
        let no_change = self.additions.is_none() && self.deletions.is_none();
        let add = self.additions.unwrap_or_default();
        let del = self.deletions.unwrap_or_default();
        if self.focus.is_some() {
            load_with_default_open.set_open(true)
        } else if self.open_changed
            && (!add.is_empty() || !del.is_empty() || !self.hightlights.is_empty())
        {
            load_with_default_open.set_open(true)
        }
        let show: FoldRet<_, _> = load_with_default_open
            .show_header(ui, |ui| {
                let text = format!("{}:", kind);
                let rt = egui::RichText::new(text).monospace();
                let gp = self.global_pos.as_ref();
                let rt = if no_change || gp.is_none() {
                    rt
                } else if let Some(c) = CC.select(add.last() == gp, del.last() == gp) {
                    let tc = &ui.style().visuals.code_bg_color;
                    rt.color(tc.lerp_to_gamma(c, 0.7))
                } else if let Some(c) = CC.select(!add.is_empty(), !del.is_empty()) {
                    let tc = &ui.style().visuals.code_bg_color;
                    rt.color(tc.lerp_to_gamma(c, 0.6))
                } else {
                    rt.size(8.0).color(egui::Color32::GRAY)
                };
                ui.label(rt)
            })
            .body(|ui| self.children_ui(ui, cs, self.global_pos.map(|x| x - size)))
            .into();
        let mut prefill = self.prefill_cache.take().unwrap_or_default();
        let mut rect = show.header_response.rect.union(
            show.body_response
                .as_ref()
                .map_or(egui::Rect::NOTHING, |x| x.rect),
        );
        rect.max.x += 10.0;
        prefill.head = show.header_response.rect.height();
        if DEBUG_LAYOUT {
            ui.painter().debug_rect(
                rect,
                egui::Color32::BLUE,
                format!("\t\t\t\t\t\t\t\t{:?}", show.header_response.rect),
            );
        }

        for handle in &mut self.hightlights {
            selection_highlight(ui, handle, min, rect, self.root_ui_id);
        }

        self.prefill_cache = Some(prefill);

        let interact = show.header_returned.interact(egui::Sense::click_and_drag());

        if interact.drag_stopped() {
            show_node_menu(ui, interact, kind)
                .or(show.body_returned)
                .unwrap_or_default()
        } else if interact.clicked() {
            Action::Clicked(self.path.to_vec())
        } else if self.focus.iter().any(|x| x.is_empty()) {
            Action::Focused(min.y)
        } else {
            show_node_menu(ui, interact, kind)
                .or(show.body_returned)
                .unwrap_or_default()
        }
    }

    fn additions_deletions_compute(&mut self, size: u32) {
        self.additions = if let (Some(add), Some(gp)) = (self.additions, self.global_pos) {
            let lld = gp - size;
            // ldd <=    <= pos
            let start: usize;
            let end: usize;
            let mut i = 0;
            loop {
                if i >= add.len() {
                    start = i;
                    break;
                }
                if lld <= add[i] {
                    start = i;
                    break;
                }
                i += 1;
            }
            loop {
                if i >= add.len() {
                    end = i;
                    break;
                }
                if add[i] == gp {
                    end = i;
                    break;
                }
                if add[i] > gp {
                    end = i;
                    break;
                }
                i += 1;
            }
            Some(&add[start..end])
        } else {
            None
        };
        self.deletions = if let (Some(del), Some(gp)) = (self.deletions, self.global_pos) {
            let lld = gp - size;
            // ldd <=    <= pos
            let start: usize;
            let end: usize;
            let mut i = 0;
            loop {
                if i >= del.len() {
                    start = i;
                    break;
                }
                if lld <= del[i] {
                    start = i;
                    break;
                }
                i += 1;
            }
            loop {
                if i >= del.len() {
                    end = i;
                    break;
                }
                if del[i] == gp {
                    end = i;
                    break;
                }
                if del[i] > gp {
                    end = i;
                    break;
                }
                i += 1;
            }
            Some(&del[start..end])
        } else {
            None
        };
    }

    pub(super) fn ui_non_loaded(
        &mut self,
        ui: &mut egui::Ui,
        nid: NodeIdentifier,
        offset: usize,
        child: NodeIdentifier,
    ) -> Action {
        let min = ui.available_rect_before_wrap().min;
        if min.y < 0.0 {
            self.min_before_count += 1;
        }
        self.draw_count += 1;
        let id = ui.id().with(&self.path);
        let mut load_with_default_open =
            egui::collapsing_header::CollapsingState::load_with_default_open(ui.ctx(), id, false);
        if self.focus.is_some() {
            load_with_default_open.set_open(true)
        }
        let show: FoldRet<_, _> = load_with_default_open
            .show_header(ui, |ui| ui.monospace(format!("waiting: {}", nid)))
            .body(|ui| {
                let mut act = Action::Keep;
                let mut prefill_old = self.prefill_cache.take().unwrap_or_default();
                let mut prefill = PrefillCache {
                    head: prefill_old.head,
                    ..Default::default()
                };
                let mut path = self.path.clone();
                path.push(offset);
                let _ = self.children_ui_aux(
                    ui,
                    offset,
                    &child,
                    &mut act,
                    &mut prefill_old,
                    &mut prefill,
                    None,
                    None,
                    &mut self.global_pos.clone(),
                    path,
                );
                act
            })
            .into();

        let mut prefill = self.prefill_cache.take().unwrap_or_default();
        let mut rect = show.header_response.rect.union(
            show.body_response
                .as_ref()
                .map(|x| x.rect)
                .unwrap_or(egui::Rect::NOTHING),
        );
        rect.max.x += 10.0;
        prefill.head = show.header_response.rect.height();
        if DEBUG_LAYOUT {
            ui.painter().debug_rect(
                rect,
                egui::Color32::BLUE,
                format!("\t\t\t\t\t\t\t\t{:?}", show.header_response.rect),
            );
        }

        for handle in &mut self.hightlights {
            selection_highlight(ui, handle, min, rect, self.root_ui_id);
        }

        self.prefill_cache = Some(prefill);
        if show
            .header_returned
            .interact(egui::Sense::click())
            .clicked()
        {
            Action::Clicked(self.path.to_vec())
        } else if self.focus.iter().any(|x| x.is_empty()) {
            Action::Focused(min.y)
        } else {
            show.body_returned.unwrap_or(Action::Keep)
        }
    }

    pub(super) fn ui_labeled_impl2(
        &mut self,
        ui: &mut egui::Ui,
        kind: AnyType,
        size: u32,
        nid: NodeIdentifier,
        label: LabelIdentifier,
    ) -> Action {
        if self.is_hidden(kind) {
            self.prefill_cache.get_or_insert_default();
            return Action::Keep;
        }
        let min = ui.available_rect_before_wrap().min;
        self.draw_count += 1;
        let id = ui.id().with(&self.path);
        self.additions_deletions_compute(size);
        if self.is_pp(kind) {
            let action = self.show_pp(ui, nid, size);
            return action;
        }
        let label = if let Some(get) = self.store.label_store.read().unwrap().try_resolve(&label) {
            get.replace("\n", "\\n")
                .replace("\t", "\\t")
                .replace(" ", "·")
        } else {
            if !(self.store.labels_pending.lock().unwrap())
                .iter()
                .any(|x| x.contains(&label))
            {
                (self.store.labels_waiting.lock().unwrap())
                    .get_or_insert(Default::default())
                    .insert(label);
            }
            "...".to_string()
        };
        let no_change = self.additions.is_none() && self.deletions.is_none();
        let add = self.additions.unwrap_or_default();
        let del = self.deletions.unwrap_or_default();
        let gp = self.global_pos.as_ref();
        let (color, fsize) = if no_change || gp.is_none() {
            (None, None)
        } else if let Some(c) = CC.select(add.last() == gp, del.last() == gp) {
            let tc = &ui.style().visuals.code_bg_color;
            (Some(tc.lerp_to_gamma(c, 0.7)), None)
        } else if let Some(c) = gp.and_then(|gp| {
            CC.select(
                *add.first().unwrap_or(&u32::MAX) <= *gp,
                *del.first().unwrap_or(&u32::MAX) <= *gp,
            )
        }) {
            let tc = &ui.style().visuals.code_bg_color;
            (Some(tc.lerp_to_gamma(c, 0.6)), None)
        } else {
            (Some(egui::Color32::GRAY), Some(8.0))
        };

        let action;
        let rect = if label.len() > 50 {
            if kind.is_spaces() {
                let text = format!("{}: ", kind);
                let mut rt = egui::RichText::new(text).monospace();
                if let Some(color) = color {
                    rt = rt.color(color);
                }
                if let Some(size) = fsize {
                    rt = rt.size(size);
                }

                let interact = ui.label(rt).interact(egui::Sense::click_and_drag());
                let rect1 = interact.rect;
                action = if interact.drag_stopped() {
                    show_node_menu(ui, interact, kind).unwrap_or_default()
                } else if interact.clicked() {
                    Action::Clicked(self.path.to_vec())
                } else {
                    show_node_menu(ui, interact, kind).unwrap_or_default()
                };
                let rect2 = ui.label(label.to_string()).rect;
                rect1.union(rect2)
            } else {
                let mut label = egui::RichText::new(label).monospace();
                let monospace = {
                    let text = format!("{}: ", kind,);
                    let mut rt = egui::RichText::new(text).monospace();

                    if let Some(color) = color {
                        label = label.color(color);
                        rt = rt.color(color);
                    }
                    if let Some(size) = fsize {
                        label = label.size(size);
                        rt = rt.size(size);
                    }
                    ui.label(rt).interact(egui::Sense::click_and_drag())
                };
                let rect1 = monospace.rect;
                let indent = ui.indent(id, |ui| ui.label(label).interact(egui::Sense::click()));
                action = if monospace.drag_stopped() {
                    show_node_menu(ui, monospace, kind).unwrap_or_default()
                } else if monospace.clicked() {
                    Action::Clicked(self.path.to_vec())
                } else {
                    show_node_menu(ui, monospace, kind).unwrap_or_default()
                };

                let rect2 = indent.response.rect;
                rect1.union(rect2)
            }
        } else {
            let add_contents = |ui: &mut egui::Ui| {
                let action = {
                    let text = format!("{}: ", kind,);
                    let mut label = egui::RichText::new(label);
                    let mut rt = egui::RichText::new(text).monospace();
                    if let Some(color) = color {
                        label = label.color(color);
                        rt = rt.color(color);
                    }
                    if let Some(size) = fsize {
                        label = label.size(size);
                        rt = rt.size(size);
                    }
                    let interact =
                        ui.add(egui::Label::new(rt).sense(egui::Sense::click_and_drag()));
                    ui.label(label);
                    if interact.drag_stopped() {
                        show_node_menu(ui, interact, kind).unwrap_or_default()
                    } else if interact.clicked() {
                        Action::Clicked(self.path.to_vec())
                    } else {
                        show_node_menu(ui, interact, kind).unwrap_or_default()
                    }
                };
                action
            };
            if kind.is_spaces() {
                action = Action::Keep;
                if self.aspects.spacing {
                    ui.horizontal(add_contents).response.rect
                } else {
                    egui::Rect::from_min_max(min, min)
                }
            } else if kind.is_syntax() {
                action = Action::Keep;
                if self.aspects.syntax {
                    ui.horizontal(add_contents).response.rect
                } else {
                    egui::Rect::from_min_max(min, min)
                }
            } else {
                let h = ui.horizontal(add_contents);
                action = h.inner;
                h.response.rect
            }
        };
        let mut prefill = self.prefill_cache.take().unwrap_or_default();
        prefill.head = ui.available_rect_before_wrap().min.y - min.y;

        for handle in &mut self.hightlights {
            if handle.path.is_empty() {
                selection_highlight(ui, handle, min, rect, self.root_ui_id);
            }
        }
        self.prefill_cache = Some(prefill);
        action
    }

    fn is_pp(&mut self, kind: AnyType) -> bool {
        if let Some(x) = kind.as_any().downcast_ref() {
            if self.aspects.ser_opt_cpp.contains(x) {
                return true;
            }
        };
        if let Some(x) = kind.as_any().downcast_ref() {
            if self.aspects.ser_opt_java.contains(x) {
                return true;
            }
        };
        false
    }

    fn is_hidden(&mut self, kind: AnyType) -> bool {
        if let Some(x) = kind.as_any().downcast_ref() {
            if self.aspects.hide_opt_cpp.contains(x) {
                return true;
            }
        };
        if let Some(x) = kind.as_any().downcast_ref() {
            if self.aspects.hide_opt_java.contains(x) {
                return true;
            }
        };
        false
    }

    fn show_pp(&mut self, ui: &mut egui::Ui, nid: NodeIdentifier, _size: u32) -> Action {
        let mut font = 12.0;

        let tc = ui.style().visuals.code_bg_color;
        let no_change = self.additions.is_none() && self.deletions.is_none();
        let add = self.additions.unwrap_or_default();
        let del = self.deletions.unwrap_or_default();
        let gp = self.global_pos.as_ref();
        let fg;
        let bg;
        if no_change || gp.is_none() {
            fg = tc.gamma_multiply(0.0);
            bg = egui::Color32::TRANSPARENT;
        } else if let Some(c) = CC.select(add.last() == gp, del.last() == gp) {
            fg = c; //.gamma_multiply(0.1).to_opaque();
            bg = c.gamma_multiply(0.1);
        } else if let Some(c) = gp.and_then(|gp| {
            CC.select(
                *add.first().unwrap_or(&u32::MAX) <= *gp,
                *del.first().unwrap_or(&u32::MAX) <= *gp,
            )
        }) {
            fg = c;
            bg = c.gamma_multiply(0.05);
        } else {
            font = 8.0;
            fg = tc.gamma_multiply(0.27);
            bg = egui::Color32::TRANSPARENT;
        };

        let mut prefill = self.prefill_cache.take().unwrap_or_default();
        let min = ui.available_rect_before_wrap().min;
        let theme = syntax_highlighter::simple::CodeTheme::from_memory(ui.ctx());
        // TODO fetch entire subtree, line breaks would also be useful
        let adv_theme = super::hyperast_layouter::AdvTheme::from(theme);
        let theme = adv_theme.fg(fg).bg(bg).size(font);
        let ppbuilder = super::pp::PPBuilder::new(self.store.clone(), nid).theme(theme);
        let layout_job = ppbuilder.compute_incr(ui.ctx());
        let galley = ui.fonts(|f| f.layout_job(layout_job));

        let size = galley.size();
        let resp = ui.allocate_exact_size(size, egui::Sense::click_and_drag());

        let rect = egui::Rect::from_min_size(min, size);
        ui.painter_at(rect.expand(1.0))
            .galley(min, galley, egui::Color32::RED);

        prefill.head = ui.available_rect_before_wrap().min.y - min.y;

        for handle in &mut self.hightlights {
            selection_highlight(ui, handle, min, rect, self.root_ui_id);
        }
        self.prefill_cache = Some(prefill);

        let action = if resp.1.drag_stopped() {
            Action::Keep
        } else if resp.1.clicked() {
            Action::Clicked(self.path.to_vec())
        } else if self.focus.iter().any(|x| x.is_empty()) {
            Action::Focused(min.y)
        } else {
            show_node_menu_aux(ui, resp.1, |ui| {
                let kind = self.store.resolve_type(&nid);
                ui.button("deserialize kind")
                    .clicked()
                    .then(|| Action::SerializeKind(kind))
            })
            .unwrap_or_default()
        };
        action
    }

    pub(super) fn ui_typed_impl2(
        &mut self,
        ui: &mut egui::Ui,
        kind: AnyType,
        _size: u32,
    ) -> Action {
        if self.is_hidden(kind) {
            return Action::Keep;
        }
        let min = ui.available_rect_before_wrap().min;
        self.draw_count += 1;

        let no_change = self.additions.is_none() && self.deletions.is_none();
        let add = self.additions.unwrap_or_default();
        let del = self.deletions.unwrap_or_default();
        let gp = self.global_pos.as_ref();
        let (color, fsize) = if no_change || gp.is_none() {
            (None, None)
        } else if let Some(c) = CC.select(add.last() == gp, del.last() == gp) {
            let tc = &ui.style().visuals.code_bg_color;
            (Some(tc.lerp_to_gamma(c, 0.7)), None)
        } else if let Some(c) = gp.and_then(|gp| {
            CC.select(
                *add.first().unwrap_or(&u32::MAX) <= *gp,
                *del.first().unwrap_or(&u32::MAX) <= *gp,
            )
        }) {
            let tc = &ui.style().visuals.code_bg_color;
            (Some(tc.lerp_to_gamma(c, 0.6)), None)
        } else {
            (Some(egui::Color32::GRAY), Some(8.0))
        };

        let mut rt = None;
        if kind.is_spaces() {
            if self.aspects.spacing {
                let text = format!("{}", kind);
                rt = Some(egui::RichText::new(text));
            }
        } else if kind.is_syntax() {
            if self.aspects.syntax {
                let text = format!("{}", kind);
                rt = Some(egui::RichText::new(text));
            }
        } else {
            let text = format!("{}", kind);
            rt = Some(egui::RichText::new(text));
        };
        if let Some(color) = color {
            rt = rt.map(|rt| rt.color(color));
        }
        if let Some(size) = fsize {
            rt = rt.map(|rt| rt.size(size));
        }
        let action = rt
            .map(|rt| ui.monospace(rt))
            .and_then(|interact| {
                let interact = interact.interact(egui::Sense::click_and_drag());
                show_node_menu_aux(ui, interact, |ui: &mut egui::Ui| {
                    ui.button("hide kind")
                        .clicked()
                        .then(|| Action::HideKind(kind))
                })
            })
            .unwrap_or_default();

        self.prefill_cache.get_or_insert_default().head =
            ui.available_rect_before_wrap().min.y - min.y;
        // TODO selection_highlight
        if self.focus.iter().any(|x| x.is_empty()) {
            Action::Focused(min.y)
        } else {
            action
        }
    }

    pub(crate) fn children_ui(
        &mut self,
        ui: &mut egui::Ui,
        cs: &[NodeIdentifier],
        mut global_pos: Option<u32>,
    ) -> Action {
        let mut action = Action::Keep;
        let additions = self.additions.as_ref().map(|x| &x[..]);
        let deletions = self.deletions.as_ref().map(|x| &x[..]);
        let mut prefill_old = self.prefill_cache.take().unwrap_or_default();

        let mut prefill = PrefillCache {
            head: prefill_old.head,
            ..PrefillCache::default()
        };
        for (i, c) in cs.iter().enumerate() {
            let mut path = self.path.clone();
            path.push(i);
            match self.children_ui_aux(
                ui,
                i,
                c,
                &mut action,
                &mut prefill_old,
                &mut prefill,
                additions,
                deletions,
                &mut global_pos,
                path,
            ) {
                ControlFlow::Continue(_) => continue,
                ControlFlow::Break(_) => break,
            }
        }
        self.prefill_cache = Some(prefill);
        action
    }

    fn children_ui_aux(
        &mut self,
        ui: &mut egui::Ui,
        i: usize,
        c: &NodeIdentifier,
        action: &mut Action,
        prefill_old: &mut PrefillCache,
        prefill: &mut PrefillCache,
        additions: Option<&[u32]>,
        deletions: Option<&[u32]>,
        mut global_pos: &mut Option<u32>,
        path: super::Offsets,
    ) -> ControlFlow<()> {
        let rect = ui.available_rect_before_wrap();
        let focus = (self.focus.as_ref())
            .filter(|x| x.offsets.get(0) == Some(&i))
            .map(|x| {
                let offsets = &x.offsets[1..];
                let ids = x.ids.get(1..).unwrap_or(&[]);
                Focus { offsets, ids }
            });
        if self.focus.is_none()
            && rect.min.y > 0.0
            && ui.ctx().screen_rect().height() - CLIP_LEN < rect.min.y
        {
            return ControlFlow::Break(());
        }
        let hightlights: Vec<_> =
            vec_extract_if_polyfill::MakeExtractIf::extract_if(&mut self.hightlights, |handle| {
                !handle.path.is_empty() && handle.path[0] == i
            })
            .map(|handle| HighLightHandle {
                path: &handle.path[1..],
                color: handle.color,
                screen_pos: handle.screen_pos,
                id: handle.id,
            })
            .collect();
        let mut imp = if let Some(child) = prefill_old.children.get(i) {
            let child_size = prefill_old.children_sizes.get(i).unwrap(); // children and children_sizes should be the same sizes
            let exact_max_y = rect.min.y + *child;
            if focus.is_none() && exact_max_y < CLIP_LEN {
                prefill.children.push(*child);
                prefill.children_sizes.push(*child_size);
                if let (Some(child_size), Some(gp)) = (child_size, &mut global_pos) {
                    *gp += child_size.get();
                } else {
                    *global_pos = None;
                }
                if DEBUG_LAYOUT {
                    ui.painter().debug_rect(
                        egui::Rect::from_min_max(rect.min, (rect.max.x, exact_max_y).into()),
                        egui::Color32::RED,
                        format!(
                            "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t{:?}\t{:?}\t{:?}",
                            exact_max_y, i, child
                        ),
                    );
                }
                ui.allocate_space((ui.min_size().x, *child).into());
                return ControlFlow::Continue(());
            } else {
                if DEBUG_LAYOUT {
                    ui.painter().debug_rect(
                        egui::Rect::from_min_max(rect.min, (rect.max.x, exact_max_y).into()),
                        egui::Color32::BLUE,
                        format!(
                            "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t{:?}\t{:?}\t{:?}",
                            exact_max_y, i, child
                        ),
                    );
                }
                FetchedViewImpl {
                    store: self.store.clone(), // TODO perfs, might be better to pass cloned store between children
                    aspects: self.aspects,
                    prefill_cache: None,
                    min_before_count: 0,
                    draw_count: 0,
                    hightlights,
                    focus,
                    path,
                    root_ui_id: self.root_ui_id,
                    additions,
                    deletions,
                    global_pos: None,
                    open_changed: true,
                }
            }
        } else if i == prefill_old.children.len() {
            if DEBUG_LAYOUT {
                ui.painter().debug_rect(
                    egui::Rect::from_min_max(rect.min, (rect.max.x, 200.0).into()),
                    egui::Color32::LIGHT_RED,
                    format!("\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t{:?}", i),
                );
            }
            FetchedViewImpl {
                store: self.store.clone(),
                aspects: self.aspects,
                prefill_cache: prefill_old.next.take().map(|b| *b),
                min_before_count: 0,
                draw_count: 0,
                hightlights,
                focus,
                path,
                root_ui_id: self.root_ui_id,
                additions,
                deletions,
                global_pos: None,
                open_changed: true,
            }
        } else {
            FetchedViewImpl {
                store: self.store.clone(),
                aspects: self.aspects,
                prefill_cache: None,
                min_before_count: 0,
                draw_count: 0,
                hightlights,
                focus,
                path,
                root_ui_id: self.root_ui_id,
                additions,
                deletions,
                global_pos: None,
                open_changed: true,
            }
        };
        let _size;
        let ret = if let Some(r) =
            (self.store.node_store.read().unwrap()).try_resolve::<NodeIdentifier>(*c)
        {
            let kind = self.store.resolve_type(c); //r.get_type();
            let l = r.try_get_label().copied();
            let cs = r.children();
            let size = r.size();

            if let Some(gp) = global_pos {
                *gp += size as u32;
            }
            _size = Some(size as u32);
            imp.global_pos = *global_pos;

            if let Some(cs) = cs {
                if let Some(label) = l {
                    imp.ui_both_impl(ui, kind, size as u32, *c, label, cs.0.to_vec().as_ref())
                } else {
                    imp.ui_children_impl2(ui, kind, size as u32, *c, cs.0.to_vec().as_ref())
                }
            } else if let Some(label) = l {
                imp.ui_labeled_impl2(ui, kind, size as u32, *c, label)
            } else {
                imp.ui_typed_impl2(ui, kind, size as u32)
            }
        // let ret = if let Some(c) = self.store.both.ids.iter().position(|x| x == c) {
        //     imp.ui_both_impl(ui, depth + 1, c)
        // } else if let Some(c) = self.store.labeled.ids.iter().position(|x| x == c) {
        //     imp.ui_labeled_impl(ui, depth + 1, c)
        // } else if let Some(c) = self.store.children.ids.iter().position(|x| x == c) {
        //     imp.ui_children_impl(ui, depth + 1, c)
        // } else if let Some(c) = self.store.typed.ids.iter().position(|x| x == c) {
        //     imp.ui_typed_impl(ui, depth + 1, c)
        } else {
            let min = ui.available_rect_before_wrap().min;
            let head = ui.available_rect_before_wrap().min.y - min.y;
            imp.prefill_cache = Some(PrefillCache {
                head,
                ..Default::default()
            });
            _size = None;
            if !(self.store.nodes_pending.lock().unwrap())
                .iter()
                .any(|x| x.contains(c))
            {
                (self.store.nodes_waiting.lock().unwrap())
                    .get_or_insert(Default::default())
                    .insert(*c);
            }
            if let Some(focus) = &imp.focus {
                wasm_rs_dbg::dbg!(&focus);
                imp.draw_count += 1;
                if let Some(x) = self.focus.as_ref().unwrap().ids.first() {
                    imp.additions = None;
                    imp.deletions = None;
                    let offset = focus.offsets.first().copied();
                    let a = imp.ui_non_loaded(ui, *c, offset.unwrap_or(0), *x);
                    match a {
                        Action::PartialFocused(x) => Action::PartialFocused(x),
                        Action::Focused(x) => Action::PartialFocused(x),
                        Action::Keep => {
                            Action::PartialFocused(ui.available_rect_before_wrap().min.y)
                        } // TODO find why it is not focused
                        x => panic!("{:?}", x),
                    }
                } else {
                    let kind: &'static dyn HyperType = &hyperast_gen_ts_cpp::types::Type::ERROR;
                    imp.additions = None;
                    imp.deletions = None;
                    let a = imp.ui_typed_impl2(ui, AnyType::from(kind), 0);
                    match a {
                        Action::PartialFocused(x) => Action::PartialFocused(x),
                        Action::Focused(x) => Action::PartialFocused(x),
                        Action::Keep => {
                            Action::PartialFocused(ui.available_rect_before_wrap().min.y)
                        }
                        x => panic!("{:?}", x),
                    }
                }
            } else if self.open_changed
                && (!self.additions.unwrap_or_default().is_empty()
                    || !self.deletions.unwrap_or_default().is_empty()
                    || !self.hightlights.is_empty())
            {
                let mut prefill = imp.prefill_cache.take().unwrap_or_default();
                imp.draw_count += 1;
                ui.horizontal(|ui| {
                    egui::Spinner::new().color(egui::Color32::LIGHT_BLUE).ui(ui);
                    ui.label(c.to_string());
                });
                prefill.head = ui.available_rect_before_wrap().min.y - min.y;
                imp.prefill_cache = Some(prefill);
                Action::PartialFocused(ui.available_rect_before_wrap().min.y)
            } else {
                let min = ui.available_rect_before_wrap().min;
                imp.draw_count += 1;
                ui.horizontal(|ui| {
                    egui::Spinner::new().ui(ui);
                    ui.label(c.to_string());
                });
                let mut prefill = imp.prefill_cache.take().unwrap_or_default();
                prefill.head = ui.available_rect_before_wrap().min.y - min.y;
                imp.prefill_cache = Some(prefill);
                Action::PartialFocused(ui.available_rect_before_wrap().min.y)
            }
        };
        match ret {
            Action::Clicked(_)
            | Action::Focused(_)
            | Action::PartialFocused(_)
            | Action::SerializeKind(_)
            | Action::HideKind(_) => {
                *action = ret;
            }
            _ => (),
        };
        let c_cache = imp.prefill_cache.unwrap_or_default();
        let h = c_cache.height();

        self.min_before_count += imp.min_before_count;
        self.draw_count += imp.draw_count;
        let mut color = egui::Color32::GOLD;
        if rect.min.y < CLIP_LEN && rect.min.y + h > CLIP_LEN {
            if c_cache.next.is_some() || !c_cache.children.is_empty() {
                color = egui::Color32::BROWN;
                prefill.next = Some(Box::new(c_cache))
            } else {
                color = egui::Color32::DARK_RED;
                prefill.children.push(h);
                prefill
                    .children_sizes
                    .push(_size.map(|x| x.try_into().unwrap()));
            }
        } else if prefill.next.is_none() {
            if rect.min.y > CLIP_LEN {
                color = egui::Color32::LIGHT_GREEN;
            }
            prefill.children.push(h);
            prefill
                .children_sizes
                .push(_size.map(|x| x.try_into().unwrap()));
        }
        if DEBUG_LAYOUT {
            ui.painter().debug_rect(
                egui::Rect::from_min_size(rect.min, (500.0-i as f32 * 3.0, h).into()),
                color,
                format!(
                    "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t{}\t{}\t{}",
                    h,rect.min.y, rect.min.y + h
                ),
            );
        }
        return ControlFlow::Continue(());
    }
}

fn selection_highlight(
    ui: &mut egui::Ui,
    handle: &mut HighLightHandle<'_>,
    min: epaint::Pos2,
    rect: epaint::Rect,
    root_ui_id: egui::Id,
) {
    if !handle.path.is_empty() {
        return;
    }
    let color = *handle.color;
    let id = handle.id;
    let ret_pos = &mut *handle.screen_pos;
    let clip = ui.clip_rect();
    let min_elem = clip.size().min_elem();
    if clip.intersects(rect) {
        ui.painter().debug_rect(rect, color, "");
    }
    let clip = if min_elem < 1.0 {
        clip
    } else {
        let mut clip = clip.shrink((min_elem / 2.0).min(4.0));
        clip.set_width((clip.width() - 14.0).max(0.0));
        clip
    };

    if !clip.intersects(rect) {
        return;
    }
    // TODO: Implement proper highlighting logic
    // this is definitely brittle
    if color == egui::Color32::BLUE {
        let _id = root_ui_id.with("blue_highlight").with(id);
        let pos = egui::pos2(min.x - 15.0, min.y - 10.0);
        let pos = clip.clamp(pos);
        if ui.clip_rect().contains(pos) {
            // show_port(ui, id, pos);
            *ret_pos = Some(rect);
        }
    } else if color == TARGET_COLOR {
        let _id = root_ui_id.with("green_highlight").with(id);
        let pos = egui::pos2(rect.max.x - 10.0, rect.min.y - 10.0);
        let pos = clip.clamp(pos);
        if ui.clip_rect().contains(pos) {
            // show_port(ui, id, pos);
            *ret_pos = Some(rect);
        }
    }
    ui.painter().debug_rect(rect, color, "");
}

fn show_node_menu(ui: &mut egui::Ui, interact: egui::Response, kind: AnyType) -> Option<Action> {
    show_node_menu_aux(ui, interact, |ui| {
        if ui.button("hide kind").clicked() {
            Some(Action::HideKind(kind))
        } else if ui.button("serialize kind").clicked() {
            Some(Action::SerializeKind(kind))
        } else {
            None
        }
    })
}

fn show_node_menu_aux(
    _ui: &mut egui::Ui,
    interact: egui::Response,
    add_content: impl Fn(&mut egui::Ui) -> Option<Action>,
) -> Option<Action> {
    let add_contents = |ui: &mut egui::Ui| {
        ui.set_width_range(40.0..=100.0);
        add_content(ui).or_else(|| {
            if ui.button("close menu").clicked() {
                ui.close();
            }
            None
        })
    };
    egui::Popup::context_menu(&interact)
        .show(add_contents)
        .and_then(|x| x.inner)
}
