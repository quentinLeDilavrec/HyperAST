use egui::{Painter, Rect};
use enumset::{EnumSet, EnumSetType};
use serde::{Deserialize, Deserializer, Serialize, Serializer};

use crate::egui_utils::show_wip;
use crate::meta_edge::MetaEdge;

#[derive(Clone, Copy)]
pub struct Config(EnumSet<LinkVariant>);

impl Serialize for Config {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_u8(self.0.as_u8())
    }
}
impl<'de> Deserialize<'de> for Config {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        <u8>::deserialize(deserializer)
            .map(|bits| EnumSet::from_u8(bits))
            .map(Config)
    }
}

#[derive(Deserialize, Serialize, EnumSetType)]
enum LinkVariant {
    BEZIER,
    META,
    THREE,
    CABLE,
}
impl AsRef<str> for LinkVariant {
    fn as_ref(&self) -> &str {
        match self {
            BEZIER => "bezier",
            META => "meta",
            THREE => "three",
            CABLE => "cable",
        }
    }
}
use LinkVariant::*;

fn chkbx<T>(ui: &mut egui::Ui, s: &mut EnumSet<T>, v: T) -> egui::Response
where
    T: EnumSetType + AsRef<str>,
{
    let mut b = s.contains(v);
    let resp = ui.checkbox(&mut b, v.as_ref());
    if b {
        s.insert(v);
    } else {
        s.remove(v);
    }
    resp
}

impl Config {
    #[rustfmt::skip]
    pub fn ui(&mut self, ui: &mut egui::Ui) -> egui::Response {
        chkbx(ui,&mut self.0, BEZIER) |
        chkbx(ui,&mut self.0, META  ) |
        chkbx(ui,&mut self.0, THREE ) |
        ui.add_enabled_ui(false, |ui| {
            let r = chkbx(ui,&mut self.0, CABLE);
            show_wip(ui, Some("cable is disabled"));
            //NOTE the cable variant is not working anymore due to a lack of maintenance of a dependency
            r
        }).inner
    }
}
impl From<EnumSet<LinkVariant>> for Config {
    fn from(value: EnumSet<LinkVariant>) -> Self {
        Self(value)
    }
}

impl Default for Config {
    fn default() -> Self {
        EnumSet::from(META).into()
    }
}

impl Config {
    pub fn source(&self, source: Rect) -> LinkBuilder<Config, Rect> {
        LinkBuilder(*self, source, ())
    }
}

pub struct LinkBuilder<O, Source = (), Sink = ()>(O, Source, Sink);

impl LinkBuilder<Config, Rect> {
    pub fn sink(self, sink: Rect) -> LinkBuilder<Config, Rect, Rect> {
        LinkBuilder(self.0, self.1, sink)
    }
}

impl LinkBuilder<Config, Rect, Rect> {
    pub fn paint(self, painter: &Painter) {
        let LinkBuilder(options, src_rect, m_rect) = self;
        fancy_link(painter, &options, src_rect, m_rect);
    }
}

fn fancy_link(painter: &Painter, options: &Config, src_rect: Rect, m_rect: Rect) {
    use epaint::*;
    if options.0.contains(CABLE) {}
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
        if options.0.contains(META) {
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
        if options.0.contains(BEZIER) {
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
        if options.0.contains(THREE) {
            let points = vec![m_pos, ctrl.0, ctrl.1, src_pos];
            let link = PathShape::line(points, (1.0, color));
            painter.add(link);
        }
        return;
    }
    let link = PathShape::line(vec![m_pos, src_pos], (1.0, color));
    painter.add(link);
}
