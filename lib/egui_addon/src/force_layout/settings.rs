use super::*;
pub struct SettingsGraph {
    // WIP
    weight_multiplier: f32,
}

impl Default for SettingsGraph {
    fn default() -> Self {
        Self {
            weight_multiplier: 1.0,
        }
    }
}

impl SettingsGraph {
    pub fn show(&mut self, ui: &mut Ui) {
        CollapsingHeader::new("Graph")
            .default_open(false)
            .show(ui, |ui| {
                ui.add(
                    egui::Slider::new(&mut self.weight_multiplier, 0.00..=10.).text("weight mult"),
                );
            });
    }
}

pub struct SettingsInteraction {
    pub dragging_enabled: bool,
    pub node_clicking_enabled: bool,
    pub node_selection_enabled: bool,
    pub node_selection_multi_enabled: bool,
    pub edge_clicking_enabled: bool,
    pub edge_selection_enabled: bool,
    pub edge_selection_multi_enabled: bool,
}

impl Default for SettingsInteraction {
    fn default() -> Self {
        Self {
            dragging_enabled: true,
            node_clicking_enabled: false,
            node_selection_enabled: true,
            node_selection_multi_enabled: false,
            edge_clicking_enabled: false,
            edge_selection_enabled: false,
            edge_selection_multi_enabled: false,
        }
    }
}

impl SettingsInteraction {
    pub fn show(&mut self, ui: &mut Ui) {
        CollapsingHeader::new("Interaction").show(ui, |ui| {
                if ui.checkbox(&mut self.dragging_enabled, "dragging_enabled").clicked() && self.dragging_enabled {
                    self.node_clicking_enabled = true;
                };
                ui.label("To drag use LMB click + drag on a node.");

                ui.add_space(5.);

                ui.add_enabled_ui(!(self.dragging_enabled || self.node_selection_enabled || self.node_selection_multi_enabled), |ui| {
                    ui.vertical(|ui| {
                        ui.checkbox(&mut self.node_clicking_enabled, "node_clicking_enabled");
                        ui.label("Check click events in last events");
                    }).response.on_disabled_hover_text("node click is enabled when any of the interaction is also enabled");
                });

                ui.add_space(5.);

                ui.add_enabled_ui(!self.node_selection_multi_enabled, |ui| {
                    ui.vertical(|ui| {
                        if ui.checkbox(&mut self.node_selection_enabled, "node_selection_enabled").clicked() && self.node_selection_enabled {
                            self.node_clicking_enabled = true;
                        };
                        ui.label("Enable select to select nodes with LMB click. If node is selected clicking on it again will deselect it.");
                    }).response.on_disabled_hover_text("node_selection_multi_enabled enables select");
                });

                if ui.checkbox(&mut self.node_selection_multi_enabled, "node_selection_multi_enabled").changed() && self.node_selection_multi_enabled {
                    self.node_clicking_enabled = true;
                    self.node_selection_enabled = true;
                }
                ui.label("Enable multiselect to select multiple nodes.");

                ui.add_space(5.);

                ui.add_enabled_ui(!(self.edge_selection_enabled || self.edge_selection_multi_enabled), |ui| {
                    ui.vertical(|ui| {
                        ui.checkbox(&mut self.edge_clicking_enabled, "edge_clicking_enabled");
                        ui.label("Check click events in last events");
                    }).response.on_disabled_hover_text("edge click is enabled when any of the interaction is also enabled");
                });

                ui.add_space(5.);

                ui.add_enabled_ui(!self.edge_selection_multi_enabled, |ui| {
                    ui.vertical(|ui| {
                        if ui.checkbox(&mut self.edge_selection_enabled, "edge_selection_enabled").clicked() && self.edge_selection_enabled {
                            self.edge_clicking_enabled = true;
                        };
                        ui.label("Enable select to select edges with LMB click. If edge is selected clicking on it again will deselect it.");
                    }).response.on_disabled_hover_text("edge_selection_multi_enabled enables select");
                });

                if ui.checkbox(&mut self.edge_selection_multi_enabled, "edge_selection_multi_enabled").changed() && self.edge_selection_multi_enabled {
                    self.edge_clicking_enabled = true;
                    self.edge_selection_enabled = true;
                }
                ui.label("Enable multiselect to select multiple edges.");
            });
    }
}

pub struct SettingsNavigation {
    pub fit_to_screen_enabled: bool,
    pub zoom_and_pan_enabled: bool,
    pub zoom_speed: f32,
}

impl Default for SettingsNavigation {
    fn default() -> Self {
        Self {
            zoom_speed: 0.05,
            fit_to_screen_enabled: true,
            zoom_and_pan_enabled: true,
        }
    }
}

impl SettingsNavigation {
    pub fn show(&mut self, ui: &mut Ui) {
        CollapsingHeader::new("Navigation")
            .default_open(true)
            .show(ui, |ui| {
                if ui
                    .checkbox(&mut self.fit_to_screen_enabled, "fit_to_screen")
                    .changed()
                    && self.fit_to_screen_enabled
                {
                    self.zoom_and_pan_enabled = false
                };
                ui.label("Enable fit to screen to fit the graph to the screen on every frame.");

                ui.add_space(5.);

                ui.add_enabled_ui(!self.fit_to_screen_enabled, |ui| {
                    ui.vertical(|ui| {
                        ui.checkbox(&mut self.zoom_and_pan_enabled, "zoom_and_pan");
                        ui.label("Zoom with ctrl + mouse wheel, pan with middle mouse drag.");
                    })
                    .response
                    .on_disabled_hover_text("disable fit_to_screen to enable zoom_and_pan");
                });
                ui.add_space(5.);

                // let zoom_speed = self.zoom_speed;
                // let mut dt =
                // let mut delta_dt = 0.;
                ui.horizontal(|ui| {
                    if ui
                        .add(egui::Slider::new(&mut self.zoom_speed, 0.00..=1.).text("zoom speed"))
                        .changed()
                    {
                        // delta_dt = values.dt - zoom_speed;
                    };
                });
            });
    }
}

#[derive(Default)]
pub struct SettingsStyle {
    pub labels_always: bool,
}

impl SettingsStyle {
    pub fn show(&mut self, ui: &mut Ui) {
        CollapsingHeader::new("Style").show(ui, |ui| {
            ui.checkbox(&mut self.labels_always, "labels_always");
            ui.label("Wheter to show labels always or when interacted only.");
        });
    }
}

pub struct SettingsSimulation {
    pub dt: f32,
    pub cooloff_factor: f32,
    pub scale: f32,
}

impl Default for SettingsSimulation {
    fn default() -> Self {
        Self {
            dt: 0.0001,
            // dt: 0.03,
            cooloff_factor: 0.85,
            scale: 1000.,
        }
    }
}
