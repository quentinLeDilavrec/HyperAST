use egui::layers::ShapeIdx;
use epaint::*;

pub struct MetaEdge {
    pub src_pos: Pos2,
    pub src_rect: Rect,
    pub m_pos: Pos2,
    pub m_rect: Rect,
    pub ctrl: (Pos2, Pos2),
    pub color: Color32,
}

impl MetaEdge {
    pub fn show(&self, ui: &mut egui::Ui) {
        self.paint(ui.painter());
    }
    pub fn paint(&self, painter: &egui::Painter) -> ShapeIdx {
        painter.add(compute_mesh(self))
    }
}

fn compute_mesh(
    MetaEdge {
        m_pos,
        src_pos,
        m_rect,
        ctrl,
        src_rect,
        color,
    }: &MetaEdge,
) -> Mesh {
    let tolerance = (m_pos.x - src_pos.x).abs() * 0.001;
    let offset = Vec2::Y * 8.0;
    let link = CubicBezierShape::from_points_stroke(
        [
            m_rect.right_top() + offset,
            ctrl.0,
            ctrl.1,
            src_rect.left_top() + offset,
        ],
        false,
        Color32::TRANSPARENT,
        (5.0, *color),
    );
    let up = link.flatten(Some(tolerance));
    let link = CubicBezierShape::from_points_stroke(
        [
            m_rect.right_bottom() - offset,
            ctrl.0,
            ctrl.1,
            src_rect.left_bottom() - offset,
        ],
        false,
        Color32::TRANSPARENT,
        (5.0, *color),
    );
    let down = link.flatten(Some(tolerance));
    let l = up.len() + down.len();
    let mut out = Mesh::default();
    let mut up = up.into_iter();
    let mut down = down.into_iter();
    let mut p_up = up.next().unwrap();
    let mut p_down = down.next().unwrap();
    let mut idx = 0;
    let mut color = Color32::GREEN;
    out.colored_vertex(p_down, color);
    let f = |idx| lerp_color_gamma(Color32::GREEN, Color32::RED, idx as f32 / l as f32);
    color = f(idx);
    out.colored_vertex(p_up, color);
    color = f(idx);
    loop {
        if let Some(x) = down.next() {
            p_down = x;
        } else {
            let mut i = idx;
            for x in up.by_ref() {
                out.colored_vertex(x, color);
                color = f(idx);
                i += 1;
                out.add_triangle(idx, i, i + 1);
            }
            break;
        };
        out.colored_vertex(p_down, color);
        color = f(idx);
        out.add_triangle(idx, idx + 1, idx + 2);
        idx += 1;
        if let Some(x) = up.next() {
            p_up = x;
        } else {
            let mut i = idx;
            for x in down.by_ref() {
                out.colored_vertex(x, color);
                color = f(idx);
                i += 1;
                out.add_triangle(idx, i + 1, i);
            }
            break;
        };
        out.colored_vertex(p_up, color);
        color = f(idx);
        out.add_triangle(idx + 1, idx, idx + 2);
        idx += 1;
    }
    out
}

fn lerp_color_gamma(left: Color32, right: Color32, t: f32) -> Color32 {
    use emath::lerp;
    Color32::from_rgba_premultiplied(
        lerp((left[0] as f32)..=(right[0] as f32), t).round() as u8,
        lerp((left[1] as f32)..=(right[1] as f32), t).round() as u8,
        lerp((left[2] as f32)..=(right[2] as f32), t).round() as u8,
        lerp((left[3] as f32)..=(right[3] as f32), t).round() as u8,
    )
}
