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
        painter.add(self.shape())
    }
    pub(crate) fn shape(&self) -> Shape {
        compute_mesh(self).into()
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
    aux(up, down)
}

fn aux(up: Vec<Pos2>, down: Vec<Pos2>) -> Mesh {
    let l = up.len() + down.len();
    let mut up = up.into_iter().peekable();
    let mut down = down.into_iter().peekable();
    let mut p_up = up.next().unwrap();
    let mut p_down = down.next().unwrap();
    let mut out = Mesh::default();
    let mut idx = 0;
    let col1 = Color32::from_rgba_unmultiplied(0, 255, 0, 100);
    let col2 = Color32::from_rgba_unmultiplied(255, 0, 0, 100);
    out.colored_vertex(p_down, col1);
    let f = |idx| lerp_color_gamma(col1, col2, idx as f32 / l as f32);
    let mut color = f(idx);
    out.colored_vertex(p_up, color);
    color = f(idx);
    enum Last {
        Up(u32),
        Down(u32),
    }
    let mut last = Last::Down(idx);
    idx += 1;
    loop {
        if let Some((down_0, up_0)) = down.peek().zip(up.peek()) {
            if down_0.x < up_0.x {
                p_down = down.next().unwrap();
                out.colored_vertex(p_down, color);
                if let Last::Up(_idx) = last {
                    out.add_triangle(_idx, idx, idx + 1);
                } else if let Last::Down(_idx) = last {
                    out.add_triangle(idx, _idx, idx + 1);
                    last = Last::Up(idx);
                }
            } else {
                p_up = up.next().unwrap();
                out.colored_vertex(p_up, color);
                if let Last::Down(_idx) = last {
                    out.add_triangle(_idx, idx, idx + 1);
                } else if let Last::Up(_idx) = last {
                    out.add_triangle(idx, _idx, idx + 1);
                    last = Last::Down(idx);
                }
            }
            color = f(idx);
            idx += 1;
        } else if down.peek().is_none() {
            if let Last::Up(_idx) = last {
                let mut i = idx;
                for x in up.by_ref() {
                    out.colored_vertex(x, color);
                    color = f(i);
                    i += 1;
                }
                (idx as usize + 1..out.vertices.len())
                    .for_each(|i| out.add_triangle(_idx, i as u32 - 1, i as u32));
            } else {
                unimplemented!("should not happen");
            }
            break;
        } else if up.peek().is_none() {
            if let Last::Down(_idx) = last {
                let mut i = idx;
                for x in down {
                    out.colored_vertex(x, color);
                    color = f(i);
                    i += 1;
                }
                (idx as usize + 1..out.vertices.len())
                    .for_each(|i| out.add_triangle(_idx, i as u32 - 1, i as u32));
            } else if let Last::Up(_idx) = last {
                unimplemented!("should not happen");
            }
            break;
        }
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
