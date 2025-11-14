use egui::Pos2;
use std::collections::{HashMap, VecDeque};
use std::sync::Arc;

use egui_addon::meta_edge::meta_egde;

use hyperast::store::nodes::fetched::NodeIdentifier;

use crate::app::utils_egui::MyUiExt;

use super::code_tracking::TrackingResult;
use super::tree_view::store::FetchedHyperAST;
use super::types::CodeRange;

const DEBUG: bool = false;

#[derive(serde::Deserialize, serde::Serialize)]
#[serde(default)]
pub(crate) struct DetatchedViewOptions {
    pub(crate) bezier: bool,
    pub(crate) meta: bool,
    pub(crate) three: bool,
    pub(crate) cable: bool,
}

impl DetatchedViewOptions {
    #[rustfmt::skip]
    pub(crate) fn ui(&mut self, ui: &mut egui::Ui) -> egui::Response {
        ui.checkbox(&mut self.bezier, "bezier") |
        ui.checkbox(&mut self.meta, "meta") |
        ui.checkbox(&mut self.three, "three") |
        ui.add_enabled_ui(false, |ui| {
            let r = ui.checkbox(&mut self.cable, "cable");
            ui.wip(Some("cable is disabled"));
            //NOTE the cable variant is not working anymore due to a lack of maintenance of a dependency
            r
        }).inner
    }
}

impl Default for DetatchedViewOptions {
    fn default() -> Self {
        Self {
            bezier: false,
            meta: true,
            three: false,
            cable: false,
        }
    }
}

pub(crate) fn ui_detached<'a>(
    ui: &mut egui::Ui,
    store: Arc<FetchedHyperAST>,
    timeline_window: egui::Rect,
    total_cols: usize,
    detatched_view_options: &DetatchedViewOptions,
    additionnal_links: &mut Vec<[CodeRange; 2]>,
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
    } = ui_detached_nodes(ui, store, detatched_view_options, col_width, it);
    if let (Some(hovered_fut), Some(released_past)) = (hovered_fut, released_past) {
        additionnal_links.push([hovered_fut, released_past]);
    }
    for [m, src] in additionnal_links {
        let m_rect = *rendered.get(m).unwrap();
        let src_rect = *rendered.get(src).unwrap();
        let m_pos = m_rect.center();
        let src_pos = src_rect.center();
        let b_d = m_rect.right() - src_rect.left();
        let mut color = egui::Color32::RED;
        let mut ctrl = (m_pos, src_pos);
        use std::f32::consts::TAU;
        let center_v = m_rect.center() - src_rect.center();
        let angle = ((center_v) * epaint::vec2(0.5, 1.0)).normalized().angle() / TAU + 0.5 - 0.125;
        if (0.03..0.25).contains(&angle) {
        } else if (0.25..0.5).contains(&angle) {
        } else if (0.5..0.71).contains(&angle) {
        } else {
            ctrl.0.x += m_rect.width() / 2.0 - b_d / 2.0 * 1.0;
            ctrl.1.x -= src_rect.width() / 2.0 - b_d / 10.0 * 1.0;
            color = egui::Color32::BLACK;
            if detatched_view_options.meta {
                meta_egde(m_pos, src_pos, m_rect, ctrl, src_rect, color, ui);
            }
            if detatched_view_options.bezier {
                let link = epaint::CubicBezierShape::from_points_stroke(
                    [m_pos, ctrl.0, ctrl.1, src_pos],
                    false,
                    egui::Color32::TRANSPARENT,
                    (5.0, color),
                );
                let tolerance = (m_pos.x - src_pos.x).abs() * 0.01;
                ui.painter().extend(
                    link.to_path_shapes(Some(tolerance), None)
                        .into_iter()
                        .map(|x| epaint::Shape::Path(x)),
                );
            }
            if detatched_view_options.three {
                let points = vec![m_pos, ctrl.0, ctrl.1, src_pos];
                let link = epaint::PathShape::line(points, (1.0, color));
                ui.painter().add(link);
            }
            continue;
        }
        let link = epaint::PathShape::line(vec![m_pos, src_pos], (1.0, color));
        ui.painter().add(link);
    }
}

fn ui_detached_nodes<'a>(
    ui: &mut egui::Ui,
    store: Arc<FetchedHyperAST>,
    options: &DetatchedViewOptions,
    col_width: f32,
    it: impl Iterator<Item = (f32, &'a mut [TrackingResult])>,
) -> DetachedElementResp<CodeRange, HashMap<CodeRange, egui::Rect>> {
    let line_id = egui::Id::new("drag line");
    let mut result = DetachedElementResp::default();
    for (default_x, res) in it {
        for (i, r) in res.iter_mut().enumerate() {
            ui_detached_node(
                ui,
                &store,
                options,
                col_width,
                line_id,
                default_x,
                i,
                &mut result,
                r,
            );
        }
    }
    result
}

fn ui_detached_node(
    ui: &mut egui::Ui,
    store: &Arc<FetchedHyperAST>,
    options: &DetatchedViewOptions,
    col_width: f32,
    line_id: egui::Id,
    default_x: f32,
    i: usize,
    result: &mut DetachedElementResp<CodeRange, HashMap<CodeRange, egui::Rect>>,
    r: &mut TrackingResult,
) {
    use egui::Color32;
    const COL: Color32 = Color32::BLUE;
    let src = &mut r.src;
    let src_id = ui.id().with(&src);
    let src_rect = {
        let default_pos = (default_x + col_width / 2.0, i as f32 * 50.0);
        let resp = show_detached_element_aux(ui, store, options, src, src_id, default_pos);
        if DEBUG {
            ui.painter().debug_rect(
                resp.response.rect.expand(20.0),
                egui::Color32::RED,
                format!("{default_x} {i} {:?}", src.path_ids),
            );
        }
        if let Some(fut) = resp.inner.future {
            result.future = Some(src.clone());
            if fut.double_clicked() {
            } else if fut.is_pointer_button_down_on() {
                let id = src_id;
                ui.memory_mut(|mem| {
                    if let Some(i) = mem.data.get_temp(line_id) {
                        if id.with("fut_interact") != i {
                            panic!();
                        }
                    } else {
                        mem.data.insert_temp(line_id, id.with("fut_interact"));
                    }
                });
                ui.ctx().set_dragged_id(line_id);
            }
        }
        if let Some(past) = resp.inner.past {
            result.past = Some(src.clone());
            if past.double_clicked() {
            } else if past.is_pointer_button_down_on() {
                let id = src_id;
                ui.memory_mut(|mem| {
                    if let Some(i) = mem.data.get_temp(line_id) {
                        if id.with("past_interact") != i {
                            panic!();
                        }
                    } else {
                        mem.data.insert_temp(line_id, id.with("past_interact"));
                    }
                });
                ui.ctx().set_dragged_id(line_id);
            }
        }
        let is_dragged = ui.ctx().is_being_dragged(line_id);
        if is_dragged {
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
                ui.painter().line_segment(p.into(), (2.0, COL));
                ui.memory_mut(|mem| mem.data.insert_temp::<(Pos2, Pos2)>(line_id, p));
            }
        } else if ui
            .memory_mut(|mem| mem.data.get_temp(line_id) == Some(src_id.with("past_interact")))
        {
            let Some(mut p) = ui.memory_mut(|mem| mem.data.get_temp::<(Pos2, Pos2)>(line_id))
            else {
                panic!()
            };
            if let Some(pos) = ui.ctx().pointer_latest_pos() {
                p.1 = pos;
            }
            ui.painter().line_segment(p.into(), (2.0, COL));
            result.past = Some(src.clone());
            ui.memory_mut(|mem| {
                mem.data.remove::<(Pos2, Pos2)>(line_id);
                mem.data.remove::<egui::Id>(line_id)
            });
        } else if ui
            .memory_mut(|mem| mem.data.get_temp(line_id) == Some(src_id.with("fut_interact")))
        {
            let Some(mut p) = ui.memory_mut(|mem| mem.data.get_temp::<(Pos2, Pos2)>(line_id))
            else {
                panic!()
            };
            if let Some(pos) = ui.ctx().pointer_latest_pos() {
                p.1 = pos;
            }
            ui.painter().line_segment(p.into(), (2.0, COL));
            result.future = Some(src.clone());
            ui.memory_mut(|mem| {
                mem.data.remove::<(Pos2, Pos2)>(line_id);
                mem.data.remove::<egui::Id>(line_id)
            });
        }
        result.element.insert(src.clone(), resp.inner.element.rect);
        resp.inner.element.rect
    };
    for m in &mut r.matched {
        let id = ui.id().with(&m);
        let m_rect = if let Some(m_pos) = result.element.get(&m) {
            m_pos.clone()
        } else {
            let default_pos = (default_x, i as f32 * 50.0);
            let resp = show_detached_element_aux(ui, store, options, &m, id, default_pos);

            if DEBUG {
                let all = (result.element.iter())
                    .map(|x| format!(
                        "                           {} {} {} {} {} {:?} {:?}\n                           {:?} {:?}\n",
                        m.file.commit == x.0.file.commit,
                        m.file == x.0.file,
                        m.path == x.0.path,
                        m.range == x.0.range,
                        m.path_ids == x.0.path_ids,
                        x.0.path,
                        x.0.path_ids,
                        x.0.file.commit.id,
                        x.0.file.file_path,
                    ))
                    .collect::<String>();
                ui.painter().debug_rect(
                    resp.response.rect.expand(20.0),
                    egui::Color32::BLUE,
                    format!("{default_x} {i} {:?}\n{:?}\n{}", m.file, m.path_ids, all),
                );
            }
            result.element.insert(m.clone(), resp.inner.element.rect);
            if let Some(_) = resp.inner.future {
                result.future = Some(m.clone());
            }
            if let Some(_) = resp.inner.past {
                result.past = Some(m.clone());
            }
            if let Some(past) = resp.inner.past {
                if past.double_clicked() {
                } else {
                    if past.is_pointer_button_down_on() {
                        ui.memory_mut(|mem| {
                            if let Some(i) = mem.data.get_temp(line_id) {
                                if id.with("past_interact") != i {
                                    panic!();
                                }
                            } else {
                                mem.data.insert_temp(line_id, id.with("past_interact"));
                            }
                        });
                        ui.ctx().set_dragged_id(line_id);
                    }
                }
            }
            if let Some(fut) = resp.inner.future {
                if fut.double_clicked() {
                } else {
                    if fut.is_pointer_button_down_on() {
                        ui.memory_mut(|mem| {
                            if let Some(i) = mem.data.get_temp(line_id) {
                                if id.with("fut_interact") != i {
                                    panic!();
                                }
                            } else {
                                mem.data.insert_temp(line_id, id.with("fut_interact"));
                            }
                        });
                        ui.ctx().set_dragged_id(line_id);
                    }
                }
            }
            let is_dragged = ui.ctx().is_being_dragged(line_id);
            if is_dragged {
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
                    ui.painter().line_segment(p.into(), (2.0, COL));
                    ui.memory_mut(|mem| mem.data.insert_temp::<(Pos2, Pos2)>(line_id, p));
                }
            } else if ui
                .memory_mut(|mem| mem.data.get_temp(line_id) == Some(id.with("past_interact")))
            {
                let Some(mut p) = ui.memory_mut(|mem| mem.data.get_temp::<(Pos2, Pos2)>(line_id))
                else {
                    panic!()
                };
                if let Some(pos) = ui.ctx().pointer_latest_pos() {
                    p.1 = pos;
                }
                ui.painter().line_segment(p.into(), (2.0, COL));
                result.past = Some(m.clone());
                ui.memory_mut(|mem| {
                    mem.data.remove::<(Pos2, Pos2)>(line_id);
                    mem.data.remove::<egui::Id>(line_id)
                });
            } else if ui
                .memory_mut(|mem| mem.data.get_temp(line_id) == Some(id.with("fut_interact")))
            {
                let Some(mut p) = ui.memory_mut(|mem| mem.data.get_temp::<(Pos2, Pos2)>(line_id))
                else {
                    panic!()
                };
                if let Some(pos) = ui.ctx().pointer_latest_pos() {
                    p.1 = pos;
                }
                ui.painter().line_segment(p.into(), (2.0, COL));
                result.future = Some(m.clone());
                ui.memory_mut(|mem| {
                    mem.data.remove::<(Pos2, Pos2)>(line_id);
                    mem.data.remove::<egui::Id>(line_id)
                });
            }
            resp.inner.element.rect
        };
        if options.cable {}
        let m_pos = m_rect.center();
        let src_pos = src_rect.center();
        let b_d = m_rect.right() - src_rect.left();
        let mut color = egui::Color32::RED;
        let mut ctrl = (m_pos, src_pos);
        use std::f32::consts::TAU;
        let center_v = m_rect.center() - src_rect.center();
        let angle = ((center_v) * epaint::vec2(0.5, 1.0)).normalized().angle() / TAU + 0.5 - 0.125;
        if (0.03..0.25).contains(&angle) {
        } else if (0.25..0.5).contains(&angle) {
        } else if (0.5..0.71).contains(&angle) {
        } else {
            ctrl.0.x += m_rect.width() / 2.0 - b_d / 2.0 * 1.0;
            ctrl.1.x -= src_rect.width() / 2.0 - b_d / 10.0 * 1.0;
            color = egui::Color32::BLACK;
            if options.meta {
                meta_egde(m_pos, src_pos, m_rect, ctrl, src_rect, color, ui);
            }
            if options.bezier {
                let link = epaint::CubicBezierShape::from_points_stroke(
                    [m_pos, ctrl.0, ctrl.1, src_pos],
                    false,
                    egui::Color32::TRANSPARENT,
                    (5.0, color),
                );
                let tolerance = (m_pos.x - src_pos.x).abs() * 0.01;
                ui.painter().extend(
                    link.to_path_shapes(Some(tolerance), None)
                        .into_iter()
                        .map(|x| epaint::Shape::Path(x)),
                );
            }
            if options.three {
                let points = vec![m_pos, ctrl.0, ctrl.1, src_pos];
                let link = epaint::PathShape::line(points, (1.0, color));
                ui.painter().add(link);
            }
            continue;
        }
        let link = epaint::PathShape::line(vec![m_pos, src_pos], (1.0, color));
        ui.painter().add(link);
    }
}

#[derive(Default)]
struct DetachedElementResp<R = egui::Response, T = R> {
    element: T,
    past: Option<R>,
    future: Option<R>,
}

fn show_detached_element_aux(
    ui: &mut egui::Ui,
    store: &Arc<FetchedHyperAST>,
    global_opt: &DetatchedViewOptions,
    x: &CodeRange,
    id: egui::Id,
    default_pos: (f32, f32),
) -> egui::InnerResponse<DetachedElementResp> {
    let p = ui.available_rect_before_wrap().left_bottom();
    let options = ui
        .memory_mut(|mem| mem.data.get_temp::<O>(id))
        .unwrap_or_default();
    let area = egui::Area::new(id)
        .default_pos(default_pos)
        .show(ui.ctx(), |ui| {
            show_element(ui, store, global_opt, x, id, &options)
        });
    if area.response.hovered() {
        options.on_input(ui.ctx(), id);
        egui::Area::new("full".into())
            .default_size(ui.ctx().screen_rect().size() * 0.1)
            .fixed_pos(p)
            .anchor(egui::Align2::LEFT_BOTTOM, (0.0, 0.0))
            .show(ui.ctx(), |ui| {
                let id = &x.file.commit.id[..x.file.commit.id.len().min(6)];
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
    _global_opt: &DetatchedViewOptions,
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
        cui.label(format!(
            "{}",
            &x.file.commit.id.chars().take(6).collect::<String>()
        ));
    }
    if options.file {
        if let Some(range) = &x.range {
            cui.label(format!("{}:{:?}", x.file.file_path, range));
        } else {
            cui.label(format!("{}", x.file.file_path));
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
    let past_resp = {
        let mut out = epaint::Mesh::default();
        let top = min;
        let mut bot = min;
        bot.y += size.y;
        let rect = egui::Rect::from_min_max(top + (-2.0 * s, 0.0).into(), bot);
        let right_paint = |col| {
            let transp = egui::Color32::TRANSPARENT;
            out.colored_vertex(epaint::pos2(top.x - s, top.y - s), transp);
            out.colored_vertex(epaint::pos2(top.x, top.y), col);
            out.colored_vertex(epaint::pos2(bot.x, bot.y), col);
            out.colored_vertex(epaint::pos2(bot.x - s, bot.y + s), transp);
            out.add_triangle(0, 1, 2);
            out.add_triangle(0, 2, 3);
            ui.painter().set(past, out);
        };

        if (ui.ctx().pointer_hover_pos()).map_or(false, |x| rect.contains(x)) {
            let resp = ui.interact(rect, id.with("past_interact"), egui::Sense::click());
            let col = if resp.clicked() {
                egui::Color32::BLUE //.gamma_multiply(0.5)
            } else {
                egui::Color32::RED.gamma_multiply(0.5)
            };
            right_paint(col);
            Some(resp)
        } else if ui.memory_mut(|mem| {
            mem.data.get_temp::<egui::Id>(egui::Id::new("drag line"))
                == Some(id.with("past_interact"))
        }) {
            right_paint(egui::Color32::BLUE);
            None
        } else {
            None
        }
    };
    let fut_resp = {
        let mut out = epaint::Mesh::default();
        let mut top = min;
        top.x += size.x;
        let mut bot = top;
        bot.y += size.y;
        let rect = egui::Rect::from_min_max(top, bot + (2.0 * s, 0.0).into());
        let left_paint = |col| {
            let transp = egui::Color32::TRANSPARENT;
            out.colored_vertex(epaint::pos2(top.x, top.y), col);
            out.colored_vertex(epaint::pos2(top.x + s, top.y - s), transp);
            out.colored_vertex(epaint::pos2(bot.x + s, bot.y + s), transp);
            out.colored_vertex(epaint::pos2(bot.x, bot.y), col);
            out.add_triangle(0, 1, 2);
            out.add_triangle(0, 2, 3);
            ui.painter().set(futur, out);
        };
        if ui
            .ctx()
            .pointer_hover_pos()
            .map_or(false, |x| rect.contains(x))
        {
            let resp = ui.interact(rect, id.with("fut_interact"), egui::Sense::click());
            let col = if resp.clicked() {
                egui::Color32::BLUE //.gamma_multiply(0.5)
            } else {
                egui::Color32::GREEN.gamma_multiply(0.5)
            };
            left_paint(col);
            Some(resp)
        } else if ui.memory_mut(|mem| {
            mem.data.get_temp::<egui::Id>(egui::Id::new("drag line"))
                == Some(id.with("fut_interact"))
        }) {
            left_paint(egui::Color32::BLUE);
            None
        } else {
            None
        }
    };
    let response = prepared.end(ui);
    DetachedElementResp {
        element: response,
        past: past_resp,
        future: fut_resp,
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
        let pendings = store.nodes_pending.lock().unwrap();
        if !pendings.iter().any(|x| x.contains(id)) {
            let mut waiting = store.nodes_waiting.lock().unwrap();
            waiting.get_or_insert(Default::default()).insert(*id);
        }
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
    use hyperast::types::{AnyType, Labeled as _};
    use hyperast::types::{HyperType as _, WithChildren as _};
    if value.is_some() && name.is_some() {
        return;
    }
    let node_store = store.node_store.read().unwrap();
    let Some(r) = node_store.try_resolve::<AnyType>(r_id) else {
        let pending = store.nodes_pending.lock().unwrap();
        if !pending.iter().any(|x| x.contains(&r_id)) {
            let mut waiting = store.nodes_waiting.lock().unwrap();
            waiting.get_or_insert(Default::default()).insert(r_id);
        }
        return;
    };
    use hyperast::types::Shared;
    let t = store.resolve_type(&r_id);
    if t.as_shared() == Shared::Literal && name.is_none() {
        let Some(l) = r.try_get_label() else {
            return;
        };
        if let Some(l) = store.label_store.read().unwrap().try_resolve(&l) {
            *value = Some(l.to_owned());
        } else if !(store.labels_pending.lock().unwrap())
            .iter()
            .any(|x| x.contains(l))
        {
            (store.labels_waiting.lock().unwrap())
                .get_or_insert(Default::default())
                .insert(*l);
        }
    } else if t.as_shared() == Shared::Identifier && name.is_none() {
        let Some(l) = r.try_get_label() else {
            return;
        };
        if let Some(l) = store.label_store.read().unwrap().try_resolve(&l) {
            *name = Some(l.to_owned());
        } else if !(store.labels_pending.lock().unwrap())
            .iter()
            .any(|x| x.contains(l))
        {
            (store.labels_waiting.lock().unwrap())
                .get_or_insert(Default::default())
                .insert(*l);
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
        macro_rules! keys { ($($id:ident, $key:ident,)*) => {{
            let o = ctx.input_mut(|inp|{ O {$(
                $id: self.$id ^ inp.consume_key(NONE, egui::Key::$key)
            ),*}});
            ctx.memory_mut(|mem| mem.data.insert_temp::<O>(id, o));
        }}}
        keys!(
            id, I, commit, C, file, F, path, P, kind, K, label, L, size, S, extra, Y,
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
