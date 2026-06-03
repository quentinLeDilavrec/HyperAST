pub struct ValuesConfigButtonsStartReset {
    pub simulation_stopped: bool,
}

pub fn draw_start_reset_buttons(
    ui: &mut egui::Ui,
    mut values: ValuesConfigButtonsStartReset,
) -> (bool, bool) {
    ui.vertical(|ui| {
        ui.label("Stop or start simulation again or reset to default settings.");
        ui.horizontal(|ui| {
            let start_simulation_stopped = values.simulation_stopped;
            let sim_status = match values.simulation_stopped {
                true => "start",
                false => "stop",
            };
            ui.toggle_value(&mut values.simulation_stopped, sim_status);
            let reset_pressed = ui.button("reset").clicked();

            if start_simulation_stopped != values.simulation_stopped || reset_pressed {
                (values.simulation_stopped, reset_pressed)
            } else {
                (false, false)
            }
        })
        .inner
    })
    .inner
}

pub struct ValuesSectionDebug {
    pub zoom: f32,
    pub pan: [f32; 2],
}

pub fn draw_section_debug(ui: &mut egui::Ui, values: ValuesSectionDebug) {
    ui.label(format!("zoom: {:.5}", values.zoom));
    ui.label(format!("pan: [{:.5}, {:.5}]", values.pan[0], values.pan[1]));
}

pub struct ValuesConfigSlidersSimulation {
    pub dt: f32,
    pub cooloff_factor: f32,
    pub scale: f32,
}

pub fn draw_simulation_config_sliders(
    ui: &mut egui::Ui,
    mut values: ValuesConfigSlidersSimulation,
    mut on_change: impl FnMut(f32, f32, f32),
) {
    let start_dt = values.dt;
    let mut delta_dt = 0.;
    ui.horizontal(|ui| {
        let slider = egui::Slider::new(&mut values.dt, 0.00..=0.7).text("dt");
        if ui.add(slider).changed() {
            delta_dt = values.dt - start_dt;
        };
    });

    let start_cooloff_factor = values.cooloff_factor;
    let mut delta_cooloff_factor = 0.;
    ui.horizontal(|ui| {
        let slider =
            egui::Slider::new(&mut values.cooloff_factor, 0.00..=1.).text("cooloff_factor");
        if ui.add(slider).changed() {
            delta_cooloff_factor = values.cooloff_factor - start_cooloff_factor;
        };
    });

    let start_scale = values.scale;
    let mut delta_scale = 0.;
    ui.horizontal(|ui| {
        let slider = egui::Slider::new(&mut values.scale, 1.0..=1000.).text("scale");
        if ui.add(slider).changed() {
            delta_scale = values.scale - start_scale;
        };
    });

    if delta_dt != 0. || delta_cooloff_factor != 0. || delta_scale != 0. {
        on_change(delta_dt, delta_cooloff_factor, delta_scale);
    }
}
