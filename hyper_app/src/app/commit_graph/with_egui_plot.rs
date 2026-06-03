use egui_plot::*;

pub(crate) fn transform_y(y: i64) -> i64 {
    assert_ne!(y, i64::MIN);
    assert_ne!(y, i64::MAX);
    assert!(y > -1);
    if y == 0 {
        0
    } else {
        -((y as f64).sqrt() as i64) - 1
    }
}

pub fn center(a: [f64; 2], b: [f64; 2]) -> PlotPoint {
    fn apply(a: [f64; 2], b: [f64; 2], f: impl Fn(f64, f64) -> f64) -> [f64; 2] {
        [f(a[0], b[0]), f(a[1], b[1])]
    }
    let position = apply(a, b, |a, b| (a + b) / 2.0).into();
    position
}

/// from egui_plot
/// Fill in all values between [min, max] which are a multiple of `step_size`
fn fill_marks_between(
    step_size: f64,
    (min, max): (f64, f64),
    ori: i64,
) -> impl Iterator<Item = GridMark> {
    debug_assert!(min <= max, "Bad plot bounds: min: {min}, max: {max}");
    let (min, max) = (min - ori as f64, max - ori as f64);
    let first = (min / step_size).ceil() as i64;
    let last = (max / step_size).ceil() as i64;

    (first..last).map(move |i| {
        let value = (i as f64) * step_size + ori as f64;
        GridMark { value, step_size }
    })
}

pub(crate) fn compute_multi_x_marks(i: GridInput, ori: i64) -> Vec<GridMark> {
    // TODO use proper rounded year convention
    let year = 60 * 60 * 24 * 365 + 60 * 60 * 6;
    let years = fill_marks_between(year as f64, i.bounds, ori);
    let month = 60 * 60 * 24 * 30;
    let months = fill_marks_between(month as f64, i.bounds, ori);
    let week = 60 * 60 * 24 * 7;
    let weeks = fill_marks_between(week as f64, i.bounds, ori);
    let day = 60 * 60 * 24;
    let days = fill_marks_between(day as f64, i.bounds, ori);
    years.chain(months).chain(weeks).chain(days).collect()
}

pub struct CommitPoints<'a> {
    pub offsets: Vec<u32>,
    pub points: Points<'a>,
    pub with_data: bool,
}

impl<'a> PlotItem for CommitPoints<'a> {
    // fn shapes(&self, ui: &Ui, transform: &PlotTransform, shapes: &mut Vec<Shape>);
    fn shapes(&self, ui: &egui::Ui, transform: &PlotTransform, shapes: &mut Vec<egui::Shape>) {
        self.points.shapes(ui, transform, shapes)
    }

    fn initialize(&mut self, x_range: std::ops::RangeInclusive<f64>) {
        self.points.initialize(x_range)
    }

    fn name(&self) -> &str {
        PlotItem::name(&self.points)
    }

    fn color(&self) -> egui::Color32 {
        PlotItem::color(&self.points)
    }

    fn highlight(&mut self) {
        PlotItem::highlight(&mut self.points)
    }

    fn highlighted(&self) -> bool {
        self.points.highlighted()
    }

    fn allow_hover(&self) -> bool {
        PlotItem::allow_hover(&self.points)
    }

    fn geometry(&self) -> PlotGeometry<'_> {
        self.points.geometry()
    }

    fn bounds(&self) -> PlotBounds {
        self.points.bounds()
    }

    fn id(&self) -> egui::Id {
        PlotItem::id(&self.points)
    }

    fn on_hover(
        &self,
        _plot_area_response: &egui::Response,
        elem: ClosestElem,
        _shapes: &mut Vec<egui::Shape>,
        _cursors: &mut Vec<Cursor>,
        plot: &PlotConfig<'_>,
        label_formatter: &LabelFormatter<'_>,
    ) {
        let points = match self.geometry() {
            PlotGeometry::Points(points) => points,
            PlotGeometry::None => {
                panic!("If the PlotItem has no geometry, on_hover() must not be called")
            }
            PlotGeometry::Rects => {
                panic!("If the PlotItem is made of rects, it should implement on_hover()")
            }
        };

        // this method is only called, if the value is in the result set of find_closest()
        let value = points[elem.index];
        let pointer = plot.transform.position_from_point(&value);

        let font_id = egui::TextStyle::Body.resolve(plot.ui.style());
        let text = self.label_formatter(elem, label_formatter);
        plot.ui.painter().text(
            pointer + egui::vec2(3.0, -2.0),
            egui::Align2::LEFT_BOTTOM,
            text,
            font_id,
            plot.ui.visuals().text_color(),
        );
        log::debug!("{}", label_formatter.is_some());
    }

    fn base(&self) -> &egui_plot::PlotItemBase {
        self.points.base()
    }

    fn base_mut(&mut self) -> &mut egui_plot::PlotItemBase {
        self.points.base_mut()
    }
}
