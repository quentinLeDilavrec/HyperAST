use std::collections::{HashMap, VecDeque};
use std::sync::Arc;

use hyperast::store::nodes::fetched::NodeIdentifier;

use crate::app::querying::DetailedResult;

use super::code_tracking::TrackingResult;
use super::tree_view::store::FetchedHyperAST;
use super::types::CodeRange;
use super::utils_egui::MyUiExt as _;

const DEBUG: bool = false;

pub type LinkConfig = egui_addon::fancy_links::Config;

/// color used to highlight elements getting linked
const SEL_COLOR: egui::Color32 = egui::Color32::BLUE;

pub(crate) fn ui_detached<'a>(
    ui: &mut egui::Ui,
    store: Arc<FetchedHyperAST>,
    timeline_window: egui::Rect,
    total_cols: usize,
    link_config: &LinkConfig,
    manual_links: &mut Vec<[CodeRange; 2]>,
    manual_rm_links: &mut ahash::HashSet<[CodeRange; 2]>,
    it: impl Iterator<Item = (usize, &'a mut [TrackingResult])>,
) {
    let col_width = timeline_window.width() / total_cols as f32;
    let it = it.map(|(col, res)| {
        let default_x = timeline_window.left() + col as f32 * col_width;
        (default_x, res)
    });
    let DetachedElementResp {
        element: rendered,
        past,
        future,
        already_linked,
    } = ui_detached_nodes(
        ui,
        store,
        link_config,
        col_width,
        it,
        manual_rm_links,
        manual_links,
    );
    if let (Some(fut), Some(past)) = (future, past) {
        let value = [fut, past];
        if let Some(index) = manual_links.iter().position(|v| v == &value) {
            log::info!(
                "manual unlink {} -> {}",
                value[0].short_commit_and_path(),
                value[1].short_commit_and_path(),
            );
            manual_links.remove(index);
        } else if let Some(already_linked) = already_linked
            && already_linked == value[1]
        {
            if manual_rm_links.contains(&value) {
                log::info!(
                    "show link {} -> {}",
                    value[0].short_commit_and_path(),
                    value[1].short_commit_and_path(),
                );
                manual_rm_links.remove(&value);
                manual_rm_links.remove(&[value[1].clone(), value[0].clone()]);
            } else {
                log::info!(
                    "hide link {} -> {}",
                    value[0].short_commit_and_path(),
                    value[1].short_commit_and_path(),
                );
                manual_rm_links.insert(value.clone());
                manual_rm_links.insert([value[1].clone(), value[0].clone()]);
            }
        } else {
            log::info!(
                "manual link {} -> {}",
                value[0].short_commit_and_path(),
                value[1].short_commit_and_path(),
            );
            manual_links.push(value);
        }
    }
    for [m, src] in manual_links {
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
    manual_rm_links: &mut ahash::HashSet<[CodeRange; 2]>,
    manual_links: &mut Vec<[CodeRange; 2]>,
) -> DetachedElementResp<CodeRange, HashMap<CodeRange, egui::Rect>> {
    let mut hovered_sink = None;
    let mut result = DetachedElementResp::default();
    let tracking_results = it.flat_map(|(d, x)| x.iter_mut().enumerate().map(move |y| (d, y)));
    for (default_x, (i, r)) in tracking_results {
        let show = |ui: &mut _, x: &_, id, o: &mut _| show_element(ui, &store, options, x, id, o);
        let src = &mut r.src;
        let id = ui.id().with(&src);
        let default_pos = (default_x + col_width / 2.0, i as f32 * 100.0);
        let resp = show_detached_element(ui, src, id, default_pos, show);
        if DEBUG {
            ui.painter().debug_rect(
                resp.response.rect.expand(20.0),
                egui::Color32::RED,
                format!("{default_x} {i} {:?}", src.path_ids),
            );
        }
        interact_detached_element(ui, &mut result, src, id, &resp, None, &mut hovered_sink);
        let resp = resp.inner.element;
        let src_rect = resp.rect;
        for x in &mut r.matched {
            if let Some(m_pos) = result.element.get(&x) {
                if !manual_rm_links.contains(&[src.clone(), x.clone()]) {
                    options.source(src_rect).sink(*m_pos).paint(ui.painter());
                }
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
            interact_detached_element(ui, &mut result, x, id, &resp, Some(&src), &mut hovered_sink);

            let m_rect = resp.inner.element.rect;
            if !manual_rm_links.contains(&[src.clone(), x.clone()]) {
                options.source(src_rect).sink(m_rect).paint(ui.painter());
            }
        }
    }
    if let Some(p) = egui::DragAndDrop::payload::<LinkingPayload>(ui.ctx()) {
        if let Some(pos) = hovered_sink.or(ui.ctx().pointer_latest_pos()) {
            let col = if let Some(_) = hovered_sink {
                let value = if p.right_side {
                    [p.code.clone(), result.past.clone().unwrap()]
                } else {
                    [result.future.clone().unwrap(), p.code.clone()]
                };
                if let Some(_) = manual_links.iter().position(|v| v == &value) {
                    egui::Color32::RED
                } else if let Some(already_linked) = p.already_linked.first()
                    && already_linked == &value[1]
                {
                    if manual_rm_links.contains(&value) {
                        egui::Color32::GREEN
                    } else {
                        egui::Color32::RED
                    }
                } else {
                    egui::Color32::GREEN
                }
            } else {
                SEL_COLOR
            };
            let stroke = (2.0, col);
            ui.painter().line_segment([p.pos, pos], stroke);
        }
    }
    if let Some(_) = ui.ctx().viewport(|wp| wp.interact_widgets.drag_stopped)
        && let Some(pl) = egui::DragAndDrop::take_payload::<LinkingPayload>(ui.ctx())
    {
        wasm_rs_dbg::dbg!(&pl);
        if pl.already_linked.len() > 1 {
            log::warn!("handle multiple already linked");
            // TODO
        }
        if result.already_linked.is_none() {
            result.already_linked = pl.already_linked.get(0).cloned();
        }
        if pl.right_side {
            result.future = Some(pl.code.clone());
        } else {
            result.past = Some(pl.code.clone());
        }
        if DEBUG {
            wasm_rs_dbg::dbg!(pp(&result.future.as_ref()));
            wasm_rs_dbg::dbg!(pp(&result.past.as_ref()));
            wasm_rs_dbg::dbg!(pp(&result.already_linked.as_ref()));
        }
        result
    } else if let Some(_) = ui.ctx().viewport(|wp| wp.interact_widgets.drag_stopped)
        && DEBUG
    {
        wasm_rs_dbg::dbg!(pp(&result.future.as_ref()));
        wasm_rs_dbg::dbg!(pp(&result.past.as_ref()));
        wasm_rs_dbg::dbg!(pp(&result.already_linked.as_ref()));
        DetachedElementResp {
            element: result.element,
            ..Default::default()
        }
    } else {
        DetachedElementResp {
            element: result.element,
            ..Default::default()
        }
    }
}

fn pp(x: &Option<&CodeRange>) -> String {
    x.as_ref()
        .map(|x| format!("{}{:?}", x.file.commit.id.prefix(6), x.path))
        .unwrap_or_default()
}

fn interact_detached_element(
    ui: &mut egui::Ui,
    result: &mut DetachedElementResp<CodeRange, HashMap<CodeRange, egui::Rect>>,
    x: &mut CodeRange,
    id: egui::Id,
    resp: &egui::InnerResponse<DetachedElementResp>,
    src: Option<&CodeRange>,
    hovered_sink: &mut Option<egui::Pos2>,
) {
    result.element.insert(x.clone(), resp.inner.element.rect);
    let fut_interact = id.with("fut_interact");
    let past_interact = id.with("past_interact");
    if let Some(past) = &resp.inner.past
        && past.drag_started()
    {
        past.dnd_set_drag_payload(LinkingPayload {
            id: past_interact,
            pos: past.rect.right_center(),
            code: x.clone(),
            right_side: false,
            already_linked: vec![],
        });
    } else if let Some(fut) = &resp.inner.future
        && fut.drag_started()
    {
        fut.dnd_set_drag_payload(LinkingPayload {
            id: fut_interact,
            pos: fut.rect.left_center(),
            code: x.clone(),
            right_side: true,
            already_linked: src.into_iter().cloned().collect(),
        });
    } else if let Some(_past) = &resp.inner.past
        && let Some(_) = ui.ctx().viewport(|wp|wp.interact_widgets.drag_stopped)// past.drag_stopped()
        && let Some(pl) = egui::DragAndDrop::payload::<LinkingPayload>(ui.ctx())
        && pl.id != past_interact
    {
        result.past = Some(x.clone());
        #[cfg(debug_assertions)]
        wasm_rs_dbg::dbg!(pp(&Some(&*x)));
    } else if let Some(_fut) = &resp.inner.future
        && let Some(_) = ui.ctx().viewport(|wp|wp.interact_widgets.drag_stopped) // fut.drag_stopped()
        && let Some(pl) = egui::DragAndDrop::payload::<LinkingPayload>(ui.ctx())
        && pl.id != fut_interact
    {
        result.already_linked = src.into_iter().next().cloned();
        result.future = Some(x.clone());
        #[cfg(debug_assertions)]
        wasm_rs_dbg::dbg!(pp(&Some(&*x)), pp(&src.into_iter().next()));
    } else if let Some(past) = &resp.inner.past
        && let Some(pl) = egui::DragAndDrop::payload::<LinkingPayload>(ui.ctx())
        && pl.id != past_interact
    {
        result.past = Some(x.clone());
        if pl.right_side {
            *hovered_sink = Some(past.rect.right_center());
        }
    } else if let Some(fut) = &resp.inner.future
        && let Some(pl) = egui::DragAndDrop::payload::<LinkingPayload>(ui.ctx())
        && pl.id != fut_interact
    {
        result.future = Some(x.clone());
        result.already_linked = src.into_iter().next().cloned();
        if !pl.right_side {
            *hovered_sink = Some(fut.rect.left_center());
        }
    }
    if let Some(payload) = egui::DragAndDrop::payload::<LinkingPayload>(ui.ctx())
        && DEBUG
    {
        ui.label(format!("{:?}", payload));
    }
}

#[derive(Default)]
struct DetachedElementResp<R = egui::Response, T = R> {
    element: T,
    past: Option<R>,
    future: Option<R>,
    already_linked: Option<R>,
}

fn show_detached_element<R>(
    ui: &mut egui::Ui,
    x: &CodeRange,
    id: egui::Id,
    default_pos: (f32, f32),
    show: impl FnOnce(&mut egui::Ui, &CodeRange, egui::Id, &mut O) -> R,
) -> egui::InnerResponse<R> {
    let p = ui.available_rect_before_wrap().left_bottom();
    let options = ui
        .memory_mut(|mem| mem.data.get_temp::<O>(id))
        .unwrap_or_default();
    let mut opt = options.clone();
    let area = egui::Area::new(id)
        .default_pos(default_pos)
        .show(ui.ctx(), |ui| show(ui, x, id, &mut opt));
    if area.response.hovered() {
        opt.on_input(ui.ctx(), id);
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
    } else if opt.0 != options.0 {
        ui.memory_mut(|mem| mem.data.insert_temp::<O>(id, opt));
    }
    area
}

struct LinkingPayload {
    id: egui::Id,
    pos: egui::Pos2,
    code: CodeRange,
    right_side: bool,
    /// list of already linked code ranges (those computed by the server)
    already_linked: Vec<CodeRange>,
}

impl std::fmt::Debug for LinkingPayload {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LinkingPayload")
            .field("id", &self.id)
            .field("pos", &self.pos)
            .field(
                "code",
                &format_args!("{}", self.code.short_commit_and_path()),
            )
            .field("right_side", &self.right_side)
            .field(
                "already_linked",
                &format_args!(
                    "[{}]",
                    &self
                        .already_linked
                        .iter()
                        .map(|x| x.short_commit_and_path())
                        .collect::<Vec<_>>()
                        .join(",")
                ),
            )
            .finish()
    }
}

fn show_element(
    ui: &mut egui::Ui,
    store: &Arc<FetchedHyperAST>,
    _global_opt: &LinkConfig,
    x: &CodeRange,
    id: egui::Id,
    options: &mut O,
) -> DetachedElementResp {
    let past = ui.painter().add(egui::Shape::Noop);
    let futur = ui.painter().add(egui::Shape::Noop);
    let mut prepared = egui::Frame::window(&ui.style()).begin(ui);
    let cui = &mut prepared.content_ui;
    if options.0.contains(Opt::Commit) {
        let prefix = x.file.commit.id.prefix(6).to_string();
        cui.show_named_value(None, prefix, Opt::Commit.menu_button(ui.ctx(), false));
    }
    if options.0.contains(Opt::File) {
        let x = if let Some(range) = &x.range {
            format!("{}:{:?}", x.file.file_path, range)
        } else {
            x.file.file_path.to_string()
        };
        cui.show_named_value(None, x, Opt::File.menu_button(ui.ctx(), false));
    }
    if options.0.contains(Opt::Path) {
        cui.show_named_value(
            None,
            format!("{:?}", x.path),
            Opt::Path.menu_button(ui.ctx(), false),
        );
    }
    if let Some(id) = x.path_ids.first() {
        show_element_content(store, options, cui, id);
    }
    let min = cui.min_rect().min;
    let size = cui.min_rect().size();
    let element = prepared.end(ui);
    if !options.0.is_empty() && !element.dragged() {
        let mut tooltip = egui::Tooltip::for_widget(&element);
        tooltip.popup = tooltip.popup.open(
            ui.ctx().viewport(|v| v.interact_widgets.dragged.is_none())
                && egui::Tooltip::should_show_tooltip(&element),
        );
        tooltip.show(|ui| options.show(ui));
    }

    let s = 25.0;
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
            link_side_hghlt(ui, past, col, SEL_COLOR, id, rect, |col| {
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
            link_side_hghlt(ui, futur, col, SEL_COLOR, id, rect, |col| {
                quad(transp, col, points)
            })
        },
        element,
        already_linked: None,
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
        let resp = ui.interact(rect, id, egui::Sense::drag());
        let col = if resp.is_pointer_button_down_on() {
            other
        } else if egui::DragAndDrop::payload::<LinkingPayload>(ui.ctx()).is_some() {
            other
        } else {
            col.gamma_multiply(0.5)
        };
        ui.painter().set(shape_id, mesh(col));
        Some(resp)
    } else if let Some(pl) = egui::DragAndDrop::payload::<LinkingPayload>(ui.ctx())
        && pl.id == id
    {
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
    if options.0.contains(Opt::Id) {
        ui.show_named_value(
            None,
            format!("{:?}", id),
            Opt::Id.menu_button(ui.ctx(), false),
        );
    }
    let node_store = store.node_store.read().unwrap();
    let Some(r) = node_store.try_resolve::<AnyType>(*id) else {
        store.demand_node(*id);
        return;
    };
    if options.0.contains(Opt::Kind) {
        let kind = store.resolve_type(id);
        ui.show_named_value(
            None,
            kind.as_static_str(),
            Opt::Kind.menu_button(ui.ctx(), false),
        );
    }
    if options.0.contains(Opt::Label) {
        if let Some(l) = r.try_get_label().copied() {
            if let Some(l) = store.label_store.read().unwrap().try_resolve(&l) {
                ui.show_named_value(None, l, Opt::Label.menu_button(ui.ctx(), false));
            }
        }
    }
    if options.0.contains(Opt::Size) {
        let size = r.size();
        ui.show_named_value(
            Some("size"),
            format!("{}", size),
            Opt::Size.menu_button(ui.ctx(), false),
        );
    }

    if options.0.contains(Opt::OldExtra) {
        ui.visuals_mut().widgets.noninteractive.bg_stroke =
            egui::Stroke::new(1.0, egui::Color32::BLACK);
        ui.add(egui::Separator::default().spacing(3.0));
        let mut q = VecDeque::<NodeIdentifier>::default();
        if let Some(cs) = r.children() {
            cs.0.iter().for_each(|x| q.push_back(*x));
        }
        let mut value = None;
        let mut name = None;
        while let Some(r_id) = q.pop_front() {
            retrieve_extra(store, &mut q, &mut value, &mut name, r_id)
        }
        let loading = name.is_none() && value.is_none();
        if let Some(l) = name {
            ui.disabled_label(format!("name: {}", l));
        }
        if let Some(l) = value {
            ui.disabled_label(format!("value: {}", l));
        }
        if loading {
            ui.disabled_label("inferring extras...");
        }
    }
    if options.0.contains(Opt::Extra) {
        ui.visuals_mut().widgets.noninteractive.bg_stroke =
            egui::Stroke::new(1.0, egui::Color32::BLACK);
        ui.add(egui::Separator::default().spacing(3.0));
        query_enabled_extras(ui, store, id, options);
    }
}

static DETACHED_NODE_QUERY: &str = "detached_node_query";

pub(crate) fn show_detached_node_extra_config(ui: &mut egui::Ui) -> egui::Response {
    let mut text = ui.data_mut(|d| {
        d.get_persisted_mut_or_default::<String>(DETACHED_NODE_QUERY.into())
            .to_owned()
    });
    ui.label("query for detached nodes extras");
    let resp = ui.text_edit_multiline(&mut text);
    ui.data_mut(|d| d.insert_persisted(DETACHED_NODE_QUERY.into(), text));

    #[allow(static_mut_refs)]
    if let Some(v) = unsafe { STORAGE.get_mut() }
        && resp.lost_focus()
    {
        v.clear();
    }
    resp
}

type ExtraQueryResult = Result<
    crate::utils_poll::Resource<Result<DetailedResult, crate::app::querying::QueryingError>>,
    String,
>;

/// Do something safer
static mut STORAGE: std::sync::OnceLock<
    HashMap<NodeIdentifier, poll_promise::Promise<ExtraQueryResult>>,
> = std::sync::OnceLock::new();

fn query_enabled_extras(
    ui: &mut egui::Ui,
    store: &Arc<FetchedHyperAST>,
    id: &NodeIdentifier,
    options: &O,
) {
    let mut refresh = false;

    #[allow(static_mut_refs)]
    if let Some(v) = unsafe { STORAGE.get_mut() }
        && let Some(prom) = v.get_mut(id)
    {
        match prom.ready_mut() {
            Some(Ok(v)) => match &mut v.content {
                Some(Ok(v)) => {
                    if options.0.contains(Opt::Matches) {
                        ui.label(format!("matches: {:?}", v.counts()));
                    }
                    query_enabled_extras_aux(ui, store, v);
                }
                Some(Err(e)) => {
                    ui.label(format!("error: {:?}", e));
                }
                None => {
                    ui.label("nothing");
                }
            },
            Some(Err(e)) => {
                ui.label(format!("error: {}", e));
            }
            None => {
                ui.label("computing");
            }
        }
    } else if let Some(text) =
        ui.data_mut(|d| d.get_persisted::<String>(DETACHED_NODE_QUERY.into()))
        && !text.trim().is_empty()
    {
        refresh |= true;
    };

    if let Some(text) = ui.data_mut(|d| {
        d.get_persisted::<String>(DETACHED_NODE_QUERY.into())
            .to_owned()
    }) && refresh
    {
        #[allow(static_mut_refs)]
        unsafe {
            STORAGE.get_or_init(|| Default::default())
        };
        use super::querying::remote_compute_query_subtree as search_query;
        let api_addr = "127.0.0.1:8888"; // TODO use the value given in settings
        let script = crate::app::querying::QueryContent {
            language: "Cpp".to_string(),
            query: text.clone(),
            precomp: None, // TODO get this from the server
            commits: 1,
            max_matches: 500, // more is a wast and difficult to interpret anyway
            timeout: 2000,    // 2 seconds seems reasonable in most cases
        };
        let prom = search_query(ui.ctx(), api_addr, id, script);
        #[allow(static_mut_refs)]
        let _ = unsafe { STORAGE.get_mut().unwrap().insert(*id, prom) };
    }
}

fn query_enabled_extras_aux(
    ui: &mut egui::Ui,
    store: &Arc<FetchedHyperAST>,
    v: &mut DetailedResult,
) {
    for (name, _, captures) in v.captures() {
        if captures.len() != 1 {
            let resp = ui.add(
                egui::Label::new(format!("{}: {:?} captures", name, captures.len()))
                    .wrap_mode(egui::TextWrapMode::Extend),
            );
            egui::Popup::from_response(&resp)
                .open(resp.hovered())
                .align(egui::RectAlign::RIGHT_START)
                .show(|ui| {
                    hovered_many_captures(store, captures, ui);
                });
            continue;
        }
        ui.label(format!("{}:", name));
        let nid = captures[0];
        let layout_job = pp_subtree(ui.ctx(), store, nid);
        let col = egui::Color32::RED;
        if layout_job.text.len() < 30 && !layout_job.text.contains('\n') {
            // small piece of code
            let galley = ui.fonts(|f| f.layout_job(layout_job));
            let size = galley.size();
            let min = ui.available_rect_before_wrap().min;
            let (rect, _resp) = ui.allocate_exact_size(size, egui::Sense::hover());
            ui.painter_at(rect.expand(1.0)).galley(min, galley, col);
        } else {
            // need scroll area for large text
            let galley = ui.fonts(|f| f.layout_job(layout_job));
            let size = galley.size();
            egui::ScrollArea::new([size.x > 200.0, size.y > 100.0])
                .id_salt(name)
                .max_width(200.0)
                .max_height(100.0)
                .scroll_bar_visibility(egui::scroll_area::ScrollBarVisibility::AlwaysHidden)
                .show(ui, |ui| {
                    let min = ui.available_rect_before_wrap().min;
                    let (rect, _resp) = ui.allocate_exact_size(size, egui::Sense::hover());
                    ui.painter_at(rect.expand(1.0)).galley(min, galley, col);
                });
        }
    }
}

fn hovered_many_captures(
    store: &Arc<FetchedHyperAST>,
    captures: &[NodeIdentifier],
    ui: &mut egui::Ui,
) {
    #[derive(Copy, Clone, Default, serde::Serialize, serde::Deserialize)]
    struct Cursor(u16);
    let mut cursor = ui.data_mut(|d| *d.get_persisted_mut_or_default::<Cursor>(ui.id()));
    ui.add(
        egui::Label::new(format!("< {}/{} >", cursor.0 + 1, captures.len()))
            .wrap_mode(egui::TextWrapMode::Extend),
    );
    ui.input_mut(|i| {
        if i.consume_key(Default::default(), egui::Key::ArrowRight) {
            if (cursor.0 as usize + 1) < captures.len() {
                cursor.0 += 1;
            }
        } else if i.consume_key(Default::default(), egui::Key::ArrowLeft) {
            cursor.0 = cursor.0.saturating_sub(1);
        }
    });
    let layout_job = pp_subtree(ui.ctx(), store, captures[cursor.0 as usize]);
    let galley = ui.fonts(|f| f.layout_job(layout_job));
    let size = galley.size();
    let min = ui.available_rect_before_wrap().min;
    let (rect, _resp) = ui.allocate_exact_size(size, egui::Sense::hover());
    ui.painter_at(rect.expand(1.0))
        .galley(min, galley, egui::Color32::RED);
    ui.data_mut(|d| d.insert_persisted::<Cursor>(ui.id(), cursor));
}

fn pp_subtree(
    ctx: &egui::Context,
    store: &Arc<FetchedHyperAST>,
    nid: NodeIdentifier,
) -> egui::text::LayoutJob {
    let theme = egui_addon::syntax_highlighting::simple::CodeTheme::from_memory(ctx);
    // TODO fetch entire subtree, line breaks would also be useful
    let adv_theme = super::tree_view::hyperast_layouter::AdvTheme::from(theme);
    let ppbuilder = super::tree_view::pp::PPBuilder::new(store.clone(), nid).theme(adv_theme);
    ppbuilder.compute_incr(ctx)
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

#[derive(enumset::EnumSetType, Debug)]
enum Opt {
    Commit,
    File,
    Path,
    Id,
    Kind,
    Label,
    Size,
    Extra,
    Matches,
    OldExtra,
}

impl Opt {
    fn icon(self) -> Option<&'static re_ui::Icon> {
        None
    }

    fn enable_text(self) -> &'static str {
        match self {
            Opt::Commit => "show commit id",
            Opt::File => "show file and byte range",
            Opt::Path => "show offset path",
            Opt::Id => "show id of subtree in HyperAST",
            Opt::Kind => "show type of syntax node",
            Opt::Label => "show label attached to syntax node",
            Opt::Size => "show size of subtree",
            Opt::Extra => "show extra info captured with the query",
            Opt::Matches => "show number of matches with the query",
            Opt::OldExtra => "show inferred info about the node",
        }
    }

    fn disable_text(self) -> &'static str {
        match self {
            Opt::Commit => "hide commit id",
            Opt::File => "hide file and byte range",
            Opt::Path => "hide offset path",
            Opt::Id => "hide id of subtree in HyperAST",
            Opt::Kind => "hide type of syntax node",
            Opt::Label => "hide label attached to syntax node",
            Opt::Size => "hide size of subtree",
            Opt::Extra => "hide extra info captured with the query",
            Opt::Matches => "hide number of matches with the query",
            Opt::OldExtra => "hide inferred info about the node",
        }
    }

    fn kb_shortcut(self) -> Option<egui::KeyboardShortcut> {
        Some(egui::KeyboardShortcut::new(
            egui::Modifiers::default(),
            match self {
                Opt::Commit => egui::Key::C,
                Opt::File => egui::Key::F,
                Opt::Path => egui::Key::P,
                Opt::Id => egui::Key::I,
                Opt::Kind => egui::Key::K,
                Opt::Label => egui::Key::L,
                Opt::Size => egui::Key::S,
                Opt::Extra => egui::Key::E,
                Opt::Matches => egui::Key::M,
                Opt::OldExtra => egui::Key::Y,
            },
        ))
    }

    pub fn menu_button(self, ctx: &egui::Context, enable: bool) -> egui::Button<'static> {
        let text = if enable {
            self.enable_text()
        } else {
            self.disable_text()
        };
        let mut button = if let Some(icon) = self.icon() {
            egui::Button::image_and_text(
                icon.as_image()
                    .fit_to_exact_size(re_ui::design_tokens_of(egui::Theme::Dark).small_icon_size),
                text,
            )
            .wrap_mode(egui::TextWrapMode::Extend)
        } else {
            egui::Button::new(text).wrap_mode(egui::TextWrapMode::Extend)
        };

        if let Some(shortcut) = self.kb_shortcut() {
            button = button.shortcut_text(ctx.format_shortcut(&shortcut));
        }

        button
    }
}

#[derive(Clone)]
struct O(enumset::EnumSet<Opt>);

impl O {
    fn on_input(mut self, ctx: &egui::Context, id: egui::Id) {
        let all = enumset::EnumSet::<Opt>::default().complement();
        for x in all {
            let Some(shortcut) = x.kb_shortcut() else {
                continue;
            };
            if ctx.input_mut(|inp| inp.consume_shortcut(&shortcut)) {
                self.0 ^= x;
            }
        }
        ctx.memory_mut(|mem| mem.data.insert_temp::<O>(id, self));
    }
}
impl Default for O {
    fn default() -> Self {
        Self(Opt::Commit | Opt::File | Opt::Kind | Opt::Label)
    }
}

impl O {
    fn show(&mut self, ui: &mut egui::Ui) {
        for x in enumset::EnumSet::<Opt>::default().complement() {
            if !self.0.contains(x) {
                if ui.add(x.menu_button(ui.ctx(), true)).clicked() {
                    self.0 ^= x;
                }
            }
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
