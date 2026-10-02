use crate::edition::InteractionResp;
use crate::types::WithDesc;
use crate::utils_results_batched::{ComputeError, ComputeResultsProm, show_short_result};

use super::{EditStatus, EditingContext};

impl<L, S> EditingContext<L, S> {
    pub(crate) fn map<R>(
        &mut self,
        f: impl Fn(&mut L) -> R,
        #[cfg(feature = "collab")] g: impl Fn(&mut std::sync::Arc<std::sync::Mutex<S>>) -> R,
    ) -> R {
        match &mut self.current {
            EditStatus::Local { name: _, content } | EditStatus::Example { i: _, content } => {
                f(content)
            } // ScriptContent::new(content, single),
            #[cfg(feature = "collab")]
            EditStatus::Shared(_, shared) | EditStatus::Sharing(shared) => {
                // let mut code_editors = shared.lock().unwrap();
                g(shared)
                // ScriptContent::new(&mut code_editors, single)
            }
            #[cfg(not(feature = "collab"))]
            EditStatus::Shared(_) => unreachable!(),
        }
    }
    pub(crate) fn when_local<R>(&mut self, mut f: impl FnMut(&mut L) -> R) -> Option<R> {
        match &mut self.current {
            EditStatus::Local { name: _, content } | EditStatus::Example { i: _, content } => {
                Some(f(content))
            }
            _ => None,
        }
    }
}

impl<L: Default, S> Default for EditStatus<L, S> {
    fn default() -> Self {
        Self::Example {
            i: 0,
            content: Default::default(),
        }
    }
}

impl<T> super::Sharing<T> {
    #[cfg(not(feature = "collab"))]
    pub(crate) fn show_interactions<'a, L, S>(
        &mut self,
        ui: &mut egui::Ui,
        context: &'a mut EditingContext<L, S>,
        compute_result: &mut Option<ComputeResultsProm<impl ComputeError + Send + Sync>>,
        examples_names: impl Fn(usize) -> String,
    ) -> InteractionResp<&'a L> {
        show_interactions(ui, context, compute_result, examples_names)
    }
}

pub(crate) fn show_locals<L: Clone, S>(
    ui: &mut egui::Ui,
    context: &mut EditingContext<L, S>,
) -> Option<(egui::Response, L, String)> {
    ui.horizontal_wrapped(|ui| {
        let mut resp = None;
        // let mut n = None;
        for (name, s) in &context.local_scripts {
            let mut text = egui::RichText::new(name);
            if let EditStatus::Local {
                name: n,
                content: _,
            } = &context.current
            {
                if name == n {
                    text = text.strong();
                }
            }
            let button = ui.button(text).interact(egui::Sense::click());
            if button.clicked() || button.secondary_clicked() || button.hovered() {
                assert!(resp.is_none());
                resp = Some((button, s.clone(), name.to_string()))
            }
        }
        resp
    })
    .inner
}

pub(crate) fn show_interactions<'a, L, S>(
    ui: &mut egui::Ui,
    context: &'a mut EditingContext<L, S>,
    #[cfg(feature = "collab")] docs_db: &Option<super::crdt_over_ws::WsDocsDb>,
    compute_result: &mut Option<ComputeResultsProm<impl ComputeError + Send + Sync>>,
    examples_names: impl Fn(usize) -> String,
) -> InteractionResp<&'a L> {
    let mut save_button = None;
    #[cfg(feature = "collab")]
    let mut share_button = None;
    let mut editor: Option<(String, _)> = None;
    ui.horizontal(|ui| match &mut context.current {
        EditStatus::Example { i, content } => {
            save_button = Some(ui.add(egui::Button::new("Save Script")));
            // let name = &EXAMPLES[*i].name;
            let name = examples_names(*i);
            editor = Some((name, &*content));
        }
        EditStatus::Local { name, content } => {
            #[cfg(feature = "collab")]
            if let Some(doc_db) = docs_db {
                if doc_db.is_connected() {
                    share_button = Some(ui.add(egui::Button::new("Share Script")));
                }
            }
            save_button = Some(ui.add(egui::Button::new("Save Script")));
            ui.text_edit_singleline(name);
            editor = Some((name.to_string(), &*content));
        }
        _ => (),
    });
    let compute_button = ui
        .horizontal(|ui| {
            let compute_button = ui.add(egui::Button::new("Compute"));
            show_short_result(&*compute_result, ui, "query_results");
            compute_button
        })
        .inner;

    InteractionResp {
        compute_button,
        editor,
        save_button,
        #[cfg(feature = "collab")]
        share_button,
    }
}

pub(crate) fn show_locals_and_interact<U, L, S>(
    ui: &mut egui::Ui,
    context: &mut EditingContext<L, S>,
) -> Option<(egui::Response, L, String)>
where
    U: AsRef<str>,
    L: Clone + WithDesc<U> + Into<S>,
{
    let (button, content, name) = show_locals(ui, context)?;
    if button.clicked() {
        // res = Some(ex);
        context.current = EditStatus::Local { name, content };
        None
    } else if button.hovered() {
        egui::Tooltip::always_open(
            ui.ctx().clone(),
            ui.layer_id(),
            button.id.with("tooltip"),
            &button,
        )
        .show(|ui| {
            let desc = content.desc().as_ref();
            egui_demo_lib::easy_mark::easy_mark(ui, desc);
        });
        None
    } else {
        Some((button, content, name))
    }
}

#[cfg(not(feature = "collab"))]
pub(crate) fn locals_and_interact_menu<T, L, S>(
    _context: &mut EditingContext<L, S>,
    _docs: &mut super::Sharing<T>,
    (button, _content, _name): (egui::Response, L, String),
) {
    button.context_menu(|ui| {
        if ui.button("close menu").clicked() {
            ui.close()
        }
    });
}

use egui::text::LayoutJob;
use egui_addon::syntax_highlighting::syntect::CodeTheme;

pub(crate) trait MakeHighlights: Copy + std::hash::Hash {
    const COLORS: u8;
    fn highlights(&self, col: u8) -> (egui::Rgba, impl Iterator<Item = (usize, usize)>);
}

impl MakeHighlights for () {
    const COLORS: u8 = 0;
    fn highlights(&self, _col: u8) -> (egui::Rgba, impl Iterator<Item = (usize, usize)>) {
        (egui::Rgba::BLACK, [].iter().copied())
    }
}

/// Helps egui::util::cache::ComputerMut, i.e., avoids computing the hash of the whole file.
/// Otherwise, it can tank fps in non-release builds :/
#[derive(Clone, Copy)]
pub struct FileContainer<'a, T>(pub &'a crate::types::FileIdentifier, pub T);

impl<'a, T> std::hash::Hash for FileContainer<'a, T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

#[derive(Default)]
pub(crate) struct Highlighter0(egui_addon::syntax_highlighting::syntect::Highlighter);
impl
    egui::util::cache::ComputerMut<
        (&CodeTheme, FileContainer<'_, &str>, &str),
        egui::text::LayoutJob,
    > for Highlighter0
{
    fn compute(
        &mut self,
        (theme, code, lang): (&CodeTheme, FileContainer<'_, &str>, &str),
    ) -> LayoutJob {
        self.0.highlight(theme, code.1, lang)
    }
}

/// Wrap a highlighter to also do custom background highlights given a set of ranges with colors (see [`MakeHighlights`]),
/// e.g. visualizing deleted, moved and inserted ranges of code
/// WARN apparently doesn't work well with transparency... the background rectangles are slighlty expanded for looks... so it makes ugly border looking things
#[derive(Default)]
#[allow(unused)] // TODO: move to egui_addon
pub(crate) struct HiHighlighter(egui_addon::syntax_highlighting::syntect::Highlighter);
impl<MH: MakeHighlights>
    egui::util::cache::ComputerMut<
        (&CodeTheme, FileContainer<'_, &str>, &str, MH),
        egui::text::LayoutJob,
    > for HiHighlighter
{
    fn compute(
        &mut self,
        (theme, code, lang, mh): (&CodeTheme, FileContainer<'_, &str>, &str, MH),
    ) -> LayoutJob {
        let mut layout_job = self.0.highlight(theme, code.1, lang); // TODO cache it separatly it takes too much time
        let sections = std::mem::take(&mut layout_job.sections);
        let mut starts = vec![];
        let mut ends = vec![];
        let mut colors = vec![];
        for c in 0..MH::COLORS {
            let x = mh.highlights(c);
            colors.push(x.0);
            for (start, end) in x.1 {
                if end - start > 1 {
                    continue;
                }
                starts.push((start, c));
                ends.push((end, c));
            }
        }
        let colors: Vec<_> = colors.into_iter().map(|c| c.into()).collect();
        starts.sort_by(|a, b| a.0.cmp(&b.0));
        ends.sort_by(|a, b| a.0.cmp(&b.0));

        let mut starts = starts.into_iter().peekable();
        let mut ends = ends.into_iter().peekable();

        let mut current_ratios: Vec<u8> = vec![];
        let mut current_color = egui::Color32::TRANSPARENT;

        let cr_less = |current_ratios: &mut Vec<u8>, less: &u8| {
            let iii = current_ratios.iter().rev().position(|x| x == less).unwrap();
            current_ratios.remove(current_ratios.len() - iii - 1);
        };

        fn acc_col(acc: &mut egui::Color32, curr: egui::Color32) {
            for i in 0..4 {
                acc[i] += curr[i];
            }
        }
        let sections = sections
            .into_iter()
            .flat_map(|mut section| {
                let mut sections = vec![];
                loop {
                    if section.byte_range.len() == 0 {
                        return sections;
                    }
                    let mut f = |pos: usize,
                                 section: &mut egui::text::LayoutSection,
                                 current_ratios: &[u8]| {
                        if section.byte_range.start != pos {
                            let mut prev = section.clone();
                            prev.byte_range.end = pos;
                            acc_col(&mut prev.format.background, current_color);
                            prev.format.line_height = Some(20.0);
                            sections.push(prev);
                        }
                        compute_hi_color(current_ratios, &mut current_color, &colors);
                        section.byte_range.start = pos;
                    };
                    if let (Some((start, more)), Some((end, less))) = (starts.peek(), ends.peek()) {
                        if start == end {
                            if more == less {
                                // special case where color would not have changed
                                starts.next().unwrap();
                                ends.next().unwrap();
                                continue;
                            }
                            if section.byte_range.contains(start) {
                                let pos = *start;
                                cr_less(&mut current_ratios, less);
                                current_ratios.push(*more);
                                f(pos, &mut section, &current_ratios);
                                starts.next().unwrap();
                                ends.next().unwrap();
                                continue;
                            }
                        } else if start < end {
                            if section.byte_range.contains(start) {
                                let pos = *start;
                                current_ratios.push(*more);
                                f(pos, &mut section, &current_ratios);
                                starts.next().unwrap();
                                continue;
                            }
                            assert!(!section.byte_range.contains(end))
                        } else {
                            if section.byte_range.contains(end) {
                                let pos = *end;
                                cr_less(&mut current_ratios, less);
                                f(pos, &mut section, &current_ratios);
                                ends.next().unwrap();
                                continue;
                            }
                            assert!(!section.byte_range.contains(start))
                        }
                    } else if let Some((start, more)) = starts.peek() {
                        if section.byte_range.contains(start) {
                            let pos = *start;
                            current_ratios.push(*more);
                            f(pos, &mut section, &current_ratios);
                            starts.next().unwrap();
                            continue;
                        }
                    } else if let Some((end, less)) = ends.peek() {
                        if section.byte_range.contains(end) {
                            let pos = *end;
                            cr_less(&mut current_ratios, less);
                            f(pos, &mut section, &current_ratios);
                            ends.next().unwrap();
                            continue;
                        }
                    }
                    acc_col(&mut section.format.background, current_color);
                    section.format.line_height = Some(20.0);
                    sections.push(section);
                    break;
                }
                sections
            })
            .collect();

        layout_job.sections = sections;

        layout_job
    }
}

fn compute_hi_color(
    current_ratios: &[u8],
    current_color: &mut egui::Color32,
    colors: &[epaint::Hsva],
) {
    use std::ops::Add;
    use std::ops::Div;
    let mut res = epaint::Hsva::new(0.0, 0.0, 0.0, 0.0);
    for r in 0..current_ratios.len() {
        res.v += colors[current_ratios[r] as usize].v.div(7.0).add(1.0).ln();
        res.a += colors[current_ratios[r] as usize].a.div(7.0).add(1.0).ln();
    }
    if let Some(r) = current_ratios.last() {
        res.h = colors[*r as usize].h;
        res.s = colors[*r as usize].s;
    }
    res.v = res.v.min(0.9);
    res.a = res.a.min(0.9);
    *current_color = res.into();
}

#[derive(Default)]
pub(crate) struct HiHighlighter2;

impl<MH: MakeHighlights, G: AsRef<std::sync::Arc<egui::Galley>>>
    egui::util::cache::ComputerMut<(G, MH), Vec<(egui::Color32, Vec<egui::Rect>)>>
    for HiHighlighter2
{
    fn compute(&mut self, (galley, highlights): (G, MH)) -> Vec<(egui::Color32, Vec<egui::Rect>)> {
        let galley = galley.as_ref();
        let mut colors = vec![];
        let mut results = vec![];
        for c in 0..MH::COLORS {
            let mut shapes = vec![];
            let x = highlights.highlights(c);
            colors.push(x.0);
            let color: egui::Color32 = x.0.into();
            for (start, end) in x.1 {
                let cursor_range = egui_addon::egui_utils::compute_cursor_range(
                    galley,
                    &std::ops::Range { start, end },
                );
                let row_range = cursor_range.map(|c| galley.layout_from_cursor(c).row);
                if row_range[0] + 1 < row_range[1] {
                    let mut mid_rect =
                        egui_addon::egui_utils::compute2_bounding_rect_from_row_range(
                            galley,
                            [row_range[0] + 1, row_range[1] - 1],
                        );
                    mid_rect.max.x += 1.0;
                    mid_rect.min.x -= 1.0;
                    let rect = &galley.rows[row_range[0]];
                    let left = if galley.layout_from_cursor(cursor_range[0]).column == 0 {
                        egui_addon::egui_utils::first_ws_x(rect).unwrap_or(
                            rect.x_offset(galley.layout_from_cursor(cursor_range[0]).column),
                        )
                    } else {
                        rect.x_offset(galley.layout_from_cursor(cursor_range[0]).column) - 5.0
                    };
                    let mut rect = rect.rect();
                    rect.min.x = left; //.min(mid_rect.min.x);
                    // mid_rect.min.x = left.min(mid_rect.min.x);
                    rect.max.x = rect.max.x.max(mid_rect.max.x);
                    shapes.push(rect);
                    let rect = &galley.rows[row_range[1]];
                    let left2 = egui_addon::egui_utils::first_ws_x(rect);
                    let right =
                        rect.x_offset(galley.layout_from_cursor(cursor_range[1]).column) + 2.0;
                    let mut rect = rect.rect();
                    if let Some(left2) = left2 {
                        let x = left2 - 5.0;
                        if mid_rect.min.x < x {
                            rect.min.x = mid_rect.min.x;
                        } else {
                            rect.min.x = x;
                            mid_rect.min.x = ((x + rect.max.x) / 2.0).min(rect.min.x + 5.0);
                        }
                    } else {
                        rect.min.x = rect.min.x.min(mid_rect.left());
                    }
                    rect.max.x = right.min(mid_rect.right());
                    shapes.push(rect);
                    shapes.push(mid_rect);
                } else if row_range[0] + 1 == row_range[1] {
                    let first = &galley.rows[row_range[0]];
                    let second = &galley.rows[row_range[1]];
                    let left = first.x_offset(galley.layout_from_cursor(cursor_range[0]).column);
                    let right = second.x_offset(galley.layout_from_cursor(cursor_range[1]).column);
                    let left2 = second.glyphs.iter().find(|x| !x.chr.is_ascii_whitespace());
                    let mut first = first.rect();
                    let mut second = second.rect();
                    first.min.x = left;
                    second.max.x = right;
                    if let Some(left2) = left2 {
                        second.min.x = left2.pos.x - 5.0;
                    }
                    // if right > left && left2 < first.max.x {
                    // }
                    shapes.push(first);
                    shapes.push(second);
                } else if row_range[0] == row_range[1] {
                    let rect = &galley.rows[row_range[0]];
                    let left = rect.x_offset(galley.layout_from_cursor(cursor_range[0]).column);
                    let right = rect.x_offset(galley.layout_from_cursor(cursor_range[1]).column);
                    let mut rect = rect.rect();
                    rect.min.x = left - 1.0;
                    rect.max.x = right + 1.0;
                    shapes.push(rect);
                } else {
                    // let rect = compute2_bounding_rect(galley, cursor_range);
                    // let rect = rect.translate(aa.inner.text_draw_pos.to_vec2());
                    // shapes.push(egui::Shape::rect_filled(
                    //     rect,
                    //     1.0,
                    //     color.linear_multiply(0.01),
                    // ));
                }
                // // galley.rows[row_range[0]].glyphs[0]
                // let rect = compute2_bounding_rect_from_row_range(galley, row_range);
            }
            results.push((color, shapes))
        }
        results
    }
}
