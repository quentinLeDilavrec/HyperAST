use egui::Pos2;
use std::collections::{HashMap, VecDeque};
use std::sync::Arc;

use hyperast::store::nodes::fetched::NodeIdentifier;

use super::code_tracking::TrackingResult;
use super::tree_view::store::FetchedHyperAST;
use super::types::CodeRange;

const DEBUG: bool = false;

pub type LinkConfig = egui_addon::fancy_links::Config;

const D_LINE: &'static str = "drag line";

pub(crate) fn ui_detached<'a>(
    ui: &mut egui::Ui,
    store: Arc<FetchedHyperAST>,
    timeline_window: egui::Rect,
    total_cols: usize,
    link_config: &LinkConfig,
    additional_links: &mut Vec<[CodeRange; 2]>,
    it: impl Iterator<Item = (usize, &'a mut [TrackingResult])>,
) {
    let col_width = timeline_window.width() / total_cols as f32;
    let it = it.map(|(col, res)| {
        let default_x = timeline_window.left() + col as f32 * col_width;
        (default_x, res)
    });
    let DetachedElementResp {
        element: rendered,
        past: released_past,
        future: hovered_fut,
    } = ui_detached_nodes(ui, store, link_config, col_width, it);
    if let (Some(hovered_fut), Some(released_past)) = (hovered_fut, released_past) {
        additional_links.push([hovered_fut, released_past]);
    }
    for [m, src] in additional_links {
        let m_rect = *rendered.get(m).unwrap();
        let src_rect = *rendered.get(src).unwrap();
        link_config
            .source(src_rect)
            .sink(m_rect)
            .paint(ui.painter());
    }
}

fn ui_detached_nodes<'a>(
    ui: &mut egui::Ui,
    store: Arc<FetchedHyperAST>,
    options: &LinkConfig,
    col_width: f32,
    it: impl Iterator<Item = (f32, &'a mut [TrackingResult])>,
) -> DetachedElementResp<CodeRange, HashMap<CodeRange, egui::Rect>> {
    let mut result = DetachedElementResp::default();
    let tracking_results = it.flat_map(|(d, x)| x.iter_mut().enumerate().map(move |y| (d, y)));
    for (default_x, (i, r)) in tracking_results {
        let show = |ui: &mut _, x: &_, id, o: &_| show_element(ui, &store, options, x, id, o);
        let x = &mut r.src;
        let id = ui.id().with(&x);
        let default_pos = (default_x + col_width / 2.0, i as f32 * 50.0);
        let resp = show_detached_element(ui, x, id, default_pos, show);
        if DEBUG {
            ui.painter().debug_rect(
                resp.response.rect.expand(20.0),
                egui::Color32::RED,
                format!("{default_x} {i} {:?}", x.path_ids),
            );
        }
        interact_detached_element(ui, &mut result, x, id, &resp);
        let resp = resp.inner.element;
        let src_rect = resp.rect;
        for x in &mut r.matched {
            if let Some(m_pos) = result.element.get(&x) {
                options.source(src_rect).sink(*m_pos).paint(ui.painter());
                continue;
            }
            let id = ui.id().with(&x);
            let default_pos = (default_x, i as f32 * 50.0);
            let resp = show_detached_element(ui, x, id, default_pos, show);
            if DEBUG {
                ui.painter().debug_rect(
                    resp.response.rect.expand(20.0),
                    egui::Color32::BLUE,
                    format!(
                        "{default_x} {i} {:?}\n{:?}\n{}",
                        x.file,
                        x.path_ids,
                        all(&result, x)
                    ),
                );
            }
            interact_detached_element(ui, &mut result, x, id, &resp);
            let m_rect = resp.inner.element.rect;
            options.source(src_rect).sink(m_rect).paint(ui.painter());
        }
    }
    result
}

fn interact_detached_element(
    ui: &mut egui::Ui,
    result: &mut DetachedElementResp<CodeRange, HashMap<CodeRange, egui::Rect>>,
    x: &mut CodeRange,
    id: egui::Id,
    resp: &egui::InnerResponse<DetachedElementResp>,
) {
    let line_id = D_LINE.into();
    use egui::Color32;
    const COL: Color32 = Color32::BLUE;
    result.element.insert(x.clone(), resp.inner.element.rect);
    if resp.inner.future.is_some() {
        result.future = Some(x.clone());
    }
    if resp.inner.past.is_some() {
        result.past = Some(x.clone());
    }
    let past_interact = id.with("past_interact");
    if let Some(past) = &resp.inner.past {
        if past.double_clicked() {
        } else if past.is_pointer_button_down_on() {
            start_link_drag(ui, line_id, past_interact);
        }
    }
    let fut_interact = id.with("fut_interact");
    if let Some(fut) = &resp.inner.future {
        if fut.double_clicked() {
        } else if fut.is_pointer_button_down_on() {
            start_link_drag(ui, line_id, fut_interact);
        }
    }
    if ui.ctx().is_being_dragged(line_id) {
        link_dragged(ui, line_id, COL);
    } else if ui.memory_mut(|mem| mem.data.get_temp(line_id) == Some(past_interact)) {
        finish_link_drag(ui, line_id, COL);
        result.past = Some(x.clone());
    } else if ui.memory_mut(|mem| mem.data.get_temp(line_id) == Some(fut_interact)) {
        finish_link_drag(ui, line_id, COL);
        result.future = Some(x.clone());
    }
}

fn start_link_drag(ui: &mut egui::Ui, line_id: egui::Id, interact_id: egui::Id) {
    ui.memory_mut(|mem| {
        if let Some(i) = mem.data.get_temp(line_id) {
            if interact_id != i {
                panic!();
            }
        } else {
            mem.data.insert_temp(line_id, interact_id);
        }
    });
    ui.ctx().set_dragged_id(line_id);
}

fn link_dragged(ui: &mut egui::Ui, line_id: egui::Id, col: egui::Color32) {
    let state = ui.memory_mut(|mem| mem.data.get_temp::<(Pos2, Pos2)>(line_id));
    let state = if let Some(mut p) = state {
        if let Some(pos) = ui.ctx().pointer_latest_pos() {
            p.1 = pos;
        }
        Some(p)
    } else {
        ui.ctx().pointer_latest_pos().map(|x| (x, x))
    };
    if let Some(p) = state {
        ui.painter().line_segment(p.into(), (2.0, col));
        ui.memory_mut(|mem| mem.data.insert_temp::<(Pos2, Pos2)>(line_id, p));
    }
}

fn finish_link_drag(ui: &mut egui::Ui, line_id: egui::Id, col: egui::Color32) {
    let Some(mut p) = ui.memory_mut(|mem| mem.data.get_temp::<(Pos2, Pos2)>(line_id)) else {
        panic!()
    };
    if let Some(pos) = ui.ctx().pointer_latest_pos() {
        p.1 = pos;
    }
    ui.painter().line_segment(p.into(), (2.0, col));
    ui.memory_mut(|mem| {
        mem.data.remove::<(Pos2, Pos2)>(line_id);
        mem.data.remove::<egui::Id>(line_id)
    });
}

#[derive(Default)]
struct DetachedElementResp<R = egui::Response, T = R> {
    element: T,
    past: Option<R>,
    future: Option<R>,
}

fn show_detached_element<R>(
    ui: &mut egui::Ui,
    x: &CodeRange,
    id: egui::Id,
    default_pos: (f32, f32),
    show: impl FnOnce(&mut egui::Ui, &CodeRange, egui::Id, &O) -> R,
) -> egui::InnerResponse<R> {
    let p = ui.available_rect_before_wrap().left_bottom();
    let options = ui
        .memory_mut(|mem| mem.data.get_temp::<O>(id))
        .unwrap_or_default();
    let area = egui::Area::new(id)
        .default_pos(default_pos)
        .show(ui.ctx(), |ui| show(ui, x, id, &options));
    if area.response.hovered() {
        options.on_input(ui.ctx(), id);
        egui::Area::new("full".into())
            .default_size(ui.ctx().screen_rect().size() * 0.1)
            .fixed_pos(p)
            .anchor(egui::Align2::LEFT_BOTTOM, (0.0, 0.0))
            .show(ui.ctx(), |ui| {
                let id = &x.file.commit.id.prefix(6);
                let text = if let Some(range) = &x.range {
                    format!("{}/{}:{:?}", id, x.file.file_path, range)
                } else {
                    format!("{}{}", id, x.file.file_path)
                };
                ui.label(egui::RichText::new(text).background_color(egui::Color32::GRAY))
            });
    }
    area
}

fn show_element(
    ui: &mut egui::Ui,
    store: &Arc<FetchedHyperAST>,
    _global_opt: &LinkConfig,
    x: &CodeRange,
    id: egui::Id,
    options: &O,
) -> DetachedElementResp {
    let past = ui.painter().add(egui::Shape::Noop);
    let futur = ui.painter().add(egui::Shape::Noop);
    let mut prepared = egui::Frame::window(&ui.style()).begin(ui);
    let cui = &mut prepared.content_ui;
    cui.disable();
    if options.commit {
        cui.label(x.file.commit.id.prefix(6).to_string());
    }
    if options.file {
        if let Some(range) = &x.range {
            cui.label(format!("{}:{:?}", x.file.file_path, range));
        } else {
            cui.label(x.file.file_path.to_string());
        }
    }
    if options.path {
        cui.label(format!("{:?}", x.path));
    }
    if let Some(id) = x.path_ids.first() {
        show_element_content(store, options, cui, id);
    }
    cui.min_rect();
    let min = cui.min_rect().min;
    let size = cui.min_rect().size();
    let s = 25.0;
    let other = egui::Color32::BLUE;
    let transp = egui::Color32::TRANSPARENT;
    DetachedElementResp {
        past: {
            let id = id.with("past_interact");
            let col = egui::Color32::RED;
            let top = min;
            let mut bot = min;
            bot.y += size.y;
            let rect = egui::Rect::from_min_max(top + (-2.0 * s, 0.0).into(), bot);
            let points = [
                (top.x - s, top.y - s),
                (top.x, top.y),
                (bot.x, bot.y),
                (bot.x - s, bot.y + s),
            ];
            link_side_hghlt(ui, past, col, other, id, rect, |col| {
                quad(col, transp, points)
            })
        },
        future: {
            let id = id.with("fut_interact");
            let col = egui::Color32::GREEN;
            let mut top = min;
            top.x += size.x;
            let mut bot = top;
            bot.y += size.y;
            let rect = egui::Rect::from_min_max(top, bot + (2.0 * s, 0.0).into());
            let points = [
                (top.x, top.y),
                (top.x + s, top.y - s),
                (bot.x + s, bot.y + s),
                (bot.x, bot.y),
            ];
            link_side_hghlt(ui, futur, col, other, id, rect, |col| {
                quad(transp, col, points)
            })
        },
        element: prepared.end(ui),
    }
}

fn quad(col1: egui::Color32, col2: egui::Color32, quad: [(f32, f32); 4]) -> egui::Mesh {
    let mut out = epaint::Mesh::default();
    out.colored_vertex(quad[0].into(), col2);
    out.colored_vertex(quad[1].into(), col1);
    out.colored_vertex(quad[2].into(), col1);
    out.colored_vertex(quad[3].into(), col2);
    out.add_triangle(0, 1, 2);
    out.add_triangle(0, 2, 3);
    out
}

fn link_side_hghlt(
    ui: &mut egui::Ui,
    shape_id: egui::layers::ShapeIdx,
    col: egui::Color32,
    other: egui::Color32,
    id: egui::Id,
    rect: egui::Rect,
    mesh: impl Fn(egui::Color32) -> egui::Mesh,
) -> Option<egui::Response> {
    if (ui.ctx().pointer_hover_pos()).map_or(false, |x| rect.contains(x)) {
        let resp = ui.interact(rect, id, egui::Sense::click());
        let col = if resp.clicked() {
            other //.gamma_multiply(0.5)
        } else {
            col.gamma_multiply(0.5)
        };
        ui.painter().set(shape_id, mesh(col));
        Some(resp)
    } else if ui.memory_mut(|mem| mem.data.get_temp(D_LINE.into()) == Some(id)) {
        ui.painter().set(shape_id, mesh(other));
        None
    } else {
        None
    }
}

fn show_element_content(
    store: &Arc<FetchedHyperAST>,
    options: &O,
    ui: &mut egui::Ui,
    id: &NodeIdentifier,
) {
    use hyperast::types::WithChildren as _;
    use hyperast::types::{AnyType, Labeled as _, WithStats};
    if options.id {
        ui.label(format!("{:?}", id));
    }
    let node_store = store.node_store.read().unwrap();
    let Some(r) = node_store.try_resolve::<AnyType>(*id) else {
        store.demand_node(*id);
        return;
    };
    if options.kind {
        let kind = store.resolve_type(id);
        ui.label(format!("{}", kind));
    }
    if options.label {
        if let Some(l) = r.try_get_label().copied() {
            if let Some(l) = store.label_store.read().unwrap().try_resolve(&l) {
                ui.label(format!("{:?}", l));
            }
        }
    }
    if options.size {
        let size = r.size();
        ui.label(format!("size: {}", size));
    }

    if !options.extra {
        return;
    }
    let mut q = VecDeque::<NodeIdentifier>::default();
    if let Some(cs) = r.children() {
        cs.0.iter().for_each(|x| q.push_back(*x));
    }
    let mut value = None;
    let mut name = None;
    while let Some(r_id) = q.pop_front() {
        retrieve_extra(store, &mut q, &mut value, &mut name, r_id)
    }
    if let Some(l) = name {
        ui.label(format!("name: {}", l));
    }
    if let Some(l) = value {
        ui.label(format!("value: {}", l));
    }
}

fn retrieve_extra(
    store: &Arc<FetchedHyperAST>,
    q: &mut VecDeque<NodeIdentifier>,
    value: &mut Option<String>,
    name: &mut Option<String>,
    r_id: NodeIdentifier,
) {
    use hyperast::types::AnyType;
    use hyperast::types::Labeled as _;
    use hyperast::types::{HyperType as _, WithChildren as _};
    if value.is_some() && name.is_some() {
        return;
    }
    let node_store = store.node_store.read().unwrap();
    let Some(r) = node_store.try_resolve::<AnyType>(r_id) else {
        store.demand_node(r_id);
        return;
    };
    use hyperast::types::Shared;
    let t = store.resolve_type(&r_id);
    if t.as_shared() == Shared::Literal && name.is_none() {
        let Some(l) = r.try_get_label() else {
            return;
        };
        if let Some(l) = store.label_store.read().unwrap().try_resolve(l) {
            *value = Some(l.to_owned());
        } else {
            store.demand_label(*l);
        }
    } else if t.as_shared() == Shared::Identifier && name.is_none() {
        let Some(l) = r.try_get_label() else {
            return;
        };
        if let Some(l) = store.label_store.read().unwrap().try_resolve(l) {
            *name = Some(l.to_owned());
        } else {
            store.demand_label(*l);
        }
    } else if let Some(cs) = r.children() {
        cs.0.iter().for_each(|x| q.push_back(*x));
    }
}

#[derive(Clone)]
struct O {
    commit: bool,
    file: bool,
    path: bool,
    id: bool,
    kind: bool,
    label: bool,
    size: bool,
    /// search number literal
    extra: bool,
}
impl O {
    fn on_input(self, ctx: &egui::Context, id: egui::Id) {
        const NONE: egui::Modifiers = egui::Modifiers::NONE;
        macro_rules! keys { ($($id:ident: $key:ident,)*) => {{
            let o = ctx.input_mut(|inp|{ O {$(
                $id: self.$id ^ inp.consume_key(NONE, egui::Key::$key)
            ),*}});
            ctx.memory_mut(|mem| mem.data.insert_temp::<O>(id, o));
        }}}
        keys!(
            id: I,
            commit: C,
            file: F,
            path: P,
            kind: K,
            label: L,
            size: S,
            extra: Y,
        )
    }
}
impl Default for O {
    fn default() -> Self {
        Self {
            commit: true,
            file: true,
            path: true,
            id: true,
            kind: true,
            label: true,
            size: true,
            extra: false,
        }
    }
}

fn all(
    result: &DetachedElementResp<CodeRange, HashMap<CodeRange, egui::Rect>>,
    x: &CodeRange,
) -> String {
    (result.element.iter())
            .map(|y| format!(
                "                           {} {} {} {} {} {:?} {:?}\n                           {:?} {:?}\n",
                x.file.commit == y.0.file.commit,
                x.file == y.0.file,
                x.path == y.0.path,
                x.range == y.0.range,
                x.path_ids == y.0.path_ids,
                y.0.path,
                y.0.path_ids,
                y.0.file.commit.id,
                y.0.file.file_path,
            ))
            .collect::<String>()
}
