use std::fmt::Display;

use egui::{Color32, FontFamily, FontId, Pos2, Rect, Shape, Stroke, Vec2, epaint::TextShape};
use egui_graphs::{DisplayNode, DrawContext, NodeProps};
use petgraph::EdgeType;
use petgraph::stable_graph::IndexType;

#[derive(enumset::EnumSetType)]
enum FlexFlags {
    Selected,
    Dragged,
    Hovered,
    RoundedDown,
    Pinned,
}

#[derive(Clone)]
pub struct NodeShapeFlex<N> {
    label: N,
    loc: Pos2,

    zoom: f32,

    size_x: f32,
    size_y: f32,

    flags: enumset::EnumSet<FlexFlags>,
    pub color: Color32,
}
impl<N: Clone> From<NodeProps<N>> for NodeShapeFlex<N> {
    fn from(node_props: NodeProps<N>) -> Self {
        let mut flags = enumset::EnumSet::empty();
        if node_props.selected {
            flags |= FlexFlags::Selected
        }
        if node_props.dragged {
            flags |= FlexFlags::Dragged
        }
        if node_props.hovered {
            flags |= FlexFlags::Hovered
        }
        Self {
            loc: node_props.location(),
            label: node_props.payload.clone(),

            zoom: 1.0,

            size_x: 0.,
            size_y: 0.,
            flags,
            color: node_props.color().unwrap_or_default(),
        }
    }
}

pub trait FlexPayload {
    fn primary(&self) -> impl Display + '_;
    fn secondary(&self) -> impl Display + '_;
}

macro_rules! impl_flex_payload {
    ($($t:ty),+$(,)?) => {$(
        impl FlexPayload for $t {
            fn primary(&self) -> impl Display + '_ { self }
            fn secondary(&self) -> impl Display + '_ { "" }
        }
    )*};
}

impl_flex_payload!(String, u8, u16, u32, u64, usize, f32, f64);

impl<T: Display, U: Display> FlexPayload for (T, U) {
    fn primary(&self) -> impl Display + '_ {
        &self.0
    }
    fn secondary(&self) -> impl Display + '_ {
        &self.1
    }
}
impl FlexPayload for () {
    fn primary(&self) -> impl Display + '_ {
        ""
    }
    fn secondary(&self) -> impl Display + '_ {
        ""
    }
}

impl<N: Clone + FlexPayload, E: Clone, Ty: EdgeType, Ix: IndexType> DisplayNode<N, E, Ty, Ix>
    for NodeShapeFlex<N>
{
    fn is_inside(&self, pos: Pos2) -> bool {
        let size = Vec2::new(self.size_x, self.size_y);
        let rect = Rect::from_center_size(self.loc, size);
        rect.contains(pos)
    }

    fn closest_boundary_point(&self, dir: Vec2) -> Pos2 {
        find_intersection(self.loc, self.size_x * 1.0, self.size_y * 1.0, dir)
    }

    fn shapes(&mut self, ctx: &DrawContext) -> Vec<egui::Shape> {
        // find node center location on the screen coordinates
        let center = ctx.meta.canvas_to_screen_pos(self.loc);

        let color = self.effective_color(ctx);
        let fill = self.effective_fill(ctx);
        let stroke = self.effective_stroke(ctx);

        let font_size = 10.;
        let s = ctx.meta.canvas_to_screen_size(font_size);
        let zoom = s / font_size;
        if ((s - self.zoom) / (s + self.zoom)).abs() > 0.3 {
            self.zoom = s;
        }
        let s = self.zoom;

        // create label
        let (galley, secondary_galley) = ctx.ctx.fonts(|f| {
            let primary = format!("{}", self.label.primary());
            let primary_font = FontId::new(s, FontFamily::Monospace);
            if primary.is_empty() {
                let secondary = format!("{}", self.label.secondary());
                (f.layout_no_wrap(secondary, primary_font, color), None)
            } else {
                let secondary = format!("{}", self.label.secondary());
                let secondary_font = FontId::new(s + 2.0, FontFamily::Monospace);
                let c = Color32::from_rgb(200, 145, 0); // darker ORANGE
                let secondary_color = color.blend(c);
                (
                    f.layout_no_wrap(primary, primary_font, color),
                    Some(f.layout_no_wrap(secondary, secondary_font, secondary_color)),
                )
            }
        });

        // we need to offset label by half its size to place it in the center of the rect
        let offset = Vec2::new(-galley.size().x / 2., -galley.size().y / 2.);

        // create the shape and add it to the layers
        let shape_label = TextShape::new(center + offset, galley, color);

        let rect = shape_label.visual_bounding_rect().scale_from_center(1.1);
        let points = rect_to_points(rect);
        let shape_rect = Shape::convex_polygon(points, fill, stroke);

        // update self size
        self.size_x = rect.size().x / zoom;
        self.size_y = rect.size().y / zoom;

        if let Some(galley) = secondary_galley {
            let o = Vec2::new(-galley.size().x, -galley.size().y);
            let shape_label2 = TextShape::new(center - offset + o, galley, color);
            return vec![shape_rect, shape_label.into(), shape_label2.into()];
        }
        vec![shape_rect, shape_label.into()]
    }

    fn update(&mut self, state: &NodeProps<N>) {
        self.label = state.payload.clone();
        if !self.flags.contains(FlexFlags::Pinned) {
            self.loc = state.location();
        }
        self.flags = self
            .flags
            .difference(FlexFlags::Selected | FlexFlags::Dragged | FlexFlags::Hovered);
        if state.selected {
            self.flags |= FlexFlags::Selected
        }
        if state.dragged {
            self.flags |= FlexFlags::Dragged
        }
        if state.hovered {
            self.flags |= FlexFlags::Hovered
        }
        self.color = state.color().unwrap_or_default();
    }
}

impl<N: Clone + FlexPayload> NodeShapeFlex<N> {
    fn is_interacted(&self) -> bool {
        let interactions = FlexFlags::Selected | FlexFlags::Dragged | FlexFlags::Hovered;
        !self.flags.is_disjoint(interactions)
    }

    fn effective_color(&self, ctx: &DrawContext) -> Color32 {
        if self.color.a() != 0 {
            return self.color;
        }
        let style = if self.is_interacted() {
            ctx.ctx.style().visuals.widgets.active
        } else {
            ctx.ctx.style().visuals.widgets.inactive
        };
        style.fg_stroke.color
    }

    fn effective_fill(&self, ctx: &DrawContext) -> Color32 {
        // let fill = Color32::from_rgba_unmultiplied(40, 40, 40, 180);
        if self.color.a() != 0 {
            return self.color.gamma_multiply(0.8);
        }
        let style = if self.is_interacted() {
            ctx.ctx.style().visuals.widgets.active
        } else {
            ctx.ctx.style().visuals.widgets.inactive
        };
        style.bg_fill.gamma_multiply(0.7)
    }

    fn effective_stroke(&self, ctx: &DrawContext) -> Stroke {
        // let stroke = Stroke::new(ctx.meta.canvas_to_screen_size(0.5), color);
        let base = Stroke::default();
        if let Some(hook) = &ctx.style.node_stroke_hook {
            let style_ref: &egui::Style = &ctx.ctx.style();
            let color = (self.color.a() != 0).then(|| self.color);
            let selected = self.flags.contains(FlexFlags::Selected);
            let dragged = self.flags.contains(FlexFlags::Dragged);
            (hook)(selected, dragged, color, base, style_ref)
        } else {
            base
        }
    }
    pub fn pinned(&self) -> bool {
        self.flags.contains(FlexFlags::Pinned)
    }
    pub fn pin(&mut self) {
        self.flags.insert(FlexFlags::Pinned);
    }
    pub fn unpin(&mut self) {
        self.flags.remove(FlexFlags::Pinned);
    }
}
fn find_intersection(center: Pos2, size_x: f32, size_y: f32, direction: Vec2) -> Pos2 {
    if (direction.x.abs() * size_y) > (direction.y.abs() * size_x) {
        // intersects left or right side
        let x = if direction.x > 0.0 {
            center.x + size_x / 2.0
        } else {
            center.x - size_x / 2.0
        };
        let y = center.y + direction.y / direction.x * (x - center.x);
        Pos2::new(x, y)
    } else {
        // intersects top or bottom side
        let y = if direction.y > 0.0 {
            center.y + size_y / 2.0
        } else {
            center.y - size_y / 2.0
        };
        let x = center.x + direction.x / direction.y * (y - center.y);
        Pos2::new(x, y)
    }
}

fn rect_to_points(rect: Rect) -> Vec<Pos2> {
    let top_left = rect.min;
    let bottom_right = rect.max;
    let top_right = Pos2::new(bottom_right.x, top_left.y);
    let bottom_left = Pos2::new(top_left.x, bottom_right.y);

    vec![top_left, top_right, bottom_right, bottom_left]
}
