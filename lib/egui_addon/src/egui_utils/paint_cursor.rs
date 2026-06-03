use epaint::text::cursor::LayoutCursor;

/// without visualizing newlines
pub(crate) fn paint_cursor_selection2(
    _ui: &mut egui::Ui,
    painter: &egui::Painter,
    pos: egui::Pos2,
    galley: &egui::Galley,
    [min, max]: &[LayoutCursor; 2],
    color: egui::Color32,
) {
    if min == max {
        return;
    }

    // We paint the cursor selection on top of the text, so make it transparent:
    // let [min, max] = cursor_range.sorted_cursors();
    // let min = min.rcursor;
    // let max = max.rcursor;

    for ri in min.row..=max.row {
        let row = &galley.rows[ri];
        let left = if ri == min.row {
            row.x_offset(min.column)
        } else {
            row.rect().left()
        };
        let right = if ri == max.row {
            row.x_offset(max.column)
        } else {
            row.rect().right()
        };
        let rect = egui::Rect::from_min_max(
            pos + egui::vec2(left, row.min_y()),
            pos + egui::vec2(right, row.max_y()),
        );
        painter.rect_filled(rect, 0.0, color);
    }
}
