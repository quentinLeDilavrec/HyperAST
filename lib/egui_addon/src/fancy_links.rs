use crate::{egui_utils::show_wip, meta_edge::MetaEdge};

#[derive(serde::Deserialize, serde::Serialize, Clone, Copy)]
#[serde(default)]
pub struct Config {
    pub bezier: bool,
    pub meta: bool,
    pub three: bool,
    pub cable: bool,
}

impl Config {
    #[rustfmt::skip]
    pub fn ui(&mut self, ui: &mut egui::Ui) -> egui::Response {
        ui.checkbox(&mut self.bezier, "bezier") |
        ui.checkbox(&mut self.meta, "meta") |
        ui.checkbox(&mut self.three, "three") |
        ui.add_enabled_ui(false, |ui| {
            let r = ui.checkbox(&mut self.cable, "cable");
            show_wip(ui, Some("cable is disabled"));
            //NOTE the cable variant is not working anymore due to a lack of maintenance of a dependency
            r
        }).inner
    }
}

impl Default for Config {
    fn default() -> Self {
        Self {
            bezier: false,
            meta: true,
            three: false,
            cable: false,
        }
    }
}

impl Config {
    pub fn source(&self, source: egui::Rect) -> LinkBuilder<Config, egui::Rect> {
        LinkBuilder(*self, source, ())
    }
}

pub struct LinkBuilder<O, Source = (), Sink = ()>(O, Source, Sink);

impl LinkBuilder<Config, egui::Rect> {
    pub fn sink(self, sink: egui::Rect) -> LinkBuilder<Config, egui::Rect, egui::Rect> {
        LinkBuilder(self.0, self.1, sink)
    }
}

impl LinkBuilder<Config, egui::Rect, egui::Rect> {
    pub fn paint(self, painter: &egui::Painter) {
        let LinkBuilder(options, src_rect, m_rect) = self;
        fancy_link(painter, &options, src_rect, m_rect);
    }
}

fn fancy_link(painter: &egui::Painter, options: &Config, src_rect: egui::Rect, m_rect: egui::Rect) {
    use epaint::*;
    if options.cable {}
    let m_pos = m_rect.center();
    let src_pos = src_rect.center();
    let b_d = m_rect.right() - src_rect.left();
    let mut color = Color32::RED;
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
        color = Color32::BLACK;
        if options.meta {
            MetaEdge {
                m_pos,
                src_pos,
                m_rect,
                ctrl,
                src_rect,
                color,
            }
            .paint(painter);
        }
        if options.bezier {
            let link = CubicBezierShape::from_points_stroke(
                [m_pos, ctrl.0, ctrl.1, src_pos],
                false,
                egui::Color32::TRANSPARENT,
                (5.0, color),
            );
            let tolerance = (m_pos.x - src_pos.x).abs() * 0.01;
            painter.extend(
                link.to_path_shapes(Some(tolerance), None)
                    .into_iter()
                    .map(|x| Shape::Path(x)),
            );
        }
        if options.three {
            let points = vec![m_pos, ctrl.0, ctrl.1, src_pos];
            let link = PathShape::line(points, (1.0, color));
            painter.add(link);
        }
        return;
    }
    let link = PathShape::line(vec![m_pos, src_pos], (1.0, color));
    painter.add(link);
}
