use crossbeam::channel::{Receiver, Sender, unbounded};
use drawers::ValuesSectionDebug;
use eframe::{App, CreationContext};
use egui::{CollapsingHeader, Context, ScrollArea, Ui, Vec2};
pub use egui_graphs;
pub use egui_graphs::DisplayNode;
use egui_graphs::Graph;
pub use egui_graphs::GraphView;
pub use egui_graphs::events::Event;
pub use petgraph;
use petgraph::Directed;
pub use petgraph::Incoming;
pub use petgraph::Outgoing;
use petgraph::stable_graph::{DefaultIx, NodeIndex};
use petgraph::visit::IntoNodeReferences;

pub type GVSettings = (
    egui_graphs::SettingsInteraction,
    egui_graphs::SettingsNavigation,
    egui_graphs::SettingsStyle,
);
pub type PrettyGraph<
    N = (),
    E = (),
    Ty = Directed,
    Ix = DefaultIx,
    Nd = node::NodeShapeFlex<N>,
    Ed = egui_graphs::DefaultEdgeShape,
> = egui_graphs::Graph<N, E, Ty, Ix, Nd, Ed>;
pub type PrettyGraphInner<
    N = (),
    E = (),
    Ty = Directed,
    Ix = DefaultIx,
    Nd = node::NodeShapeFlex<N>,
    Ed = egui_graphs::DefaultEdgeShape,
> = StableGraphType<N, E, Ty, Ix, Nd, Ed>;

type StableGraphType<N, E, Ty, Ix, Dn, De> = petgraph::stable_graph::StableGraph<
    egui_graphs::Node<N, E, Ty, Ix, Dn>,
    egui_graphs::Edge<N, E, Ty, Ix, Dn, De>,
    Ty,
    Ix,
>;

pub type AnimatedGraphView<
    'a,
    N = (),
    E = (),
    Ty = Directed,
    Ix = DefaultIx,
    Nd = node::NodeShapeFlex<N>,
    Ed = egui_graphs::DefaultEdgeShape,
> = GraphView<
    'a,
    N,
    E,
    Ty,
    Ix,
    Nd,
    Ed,
    AnimeState,
    egui_graphs::LayoutForceDirected<AnimeLayout>,
    // egui_graphs::LayoutRandom,
    // egui_graphs::LayoutHierarchical,
>;
pub type AnimeLayout = egui_graphs::FruchtermanReingoldWithExtras<AnimeExtra>;

pub type AnimeState = egui_graphs::FruchtermanReingoldWithExtrasState<AnimeExtra>;

pub type AnimeExtra = (CenterExtra, (PinningExtra, ()));
type CenterExtra = egui_graphs::Extra<egui_graphs::CenterGravity, true>;
type PinningExtra = egui_graphs::Extra<extra_forces::Pinning, true>;

pub type HierGraphView<
    'a,
    N = (),
    E = (),
    Ty = Directed,
    Ix = DefaultIx,
    Nd = node::NodeShapeFlex<N>,
    Ed = egui_graphs::DefaultEdgeShape,
> = GraphView<
    'a,
    N,
    E,
    Ty,
    Ix,
    Nd,
    Ed,
    egui_graphs::LayoutStateHierarchical,
    egui_graphs::LayoutHierarchical,
>;

pub fn pin_node(params: &mut extra_forces::PinningParams, node: u32) {
    let mut offset = 0;
    let mut i = 0;
    // find insertion position
    loop {
        if i >= params.pinned.len() {
            // easy case
            params.pinned.push(node - offset);
            return;
        }
        if node == offset {
            // already there
            return;
        }
        if node < offset {
            // found position
            let new = offset - node;
            let ins = params.pinned[i] - new;
            params.pinned[i] = new;
            params.pinned.insert(i, ins);
            return;
        }
        offset += params.pinned[i];
        i += 1;
    }
}

pub fn show_center_gravity_params(
    ui: &mut egui::Ui,
    params: &mut egui_graphs::CenterGravityParams,
) {
    ui.label("c:");
    ui.add(egui::Slider::new(&mut params.c, 0.01..=1.0).clamping(egui::SliderClamping::Never));
}

pub fn show_pinning_params(ui: &mut egui::Ui, params: &mut extra_forces::PinningParams) {
    ui.label("pinning intensity:");
    ui.add(
        egui::Slider::new(&mut params.intensity, 1.0..=1000.0)
            .clamping(egui::SliderClamping::Never),
    );
}

pub fn show_fruchterman_reingold_params(
    ui: &mut egui::Ui,
    base: &mut egui_graphs::FruchtermanReingoldState,
) {
    let clamping = egui::SliderClamping::Never;
    ui.label("c_attract:");
    ui.add(egui::Slider::new(&mut base.c_attract, 0.1..=10.0).clamping(clamping));
    ui.label("c_repulse:");
    ui.add(egui::Slider::new(&mut base.c_repulse, 0.1..=10.0).clamping(clamping));
    ui.label("k_scale:");
    ui.add(egui::Slider::new(&mut base.k_scale, 0.1..=10.0).clamping(clamping));
    ui.label("damping:");
    ui.add(egui::Slider::new(&mut base.damping, 0.1..=10.0).clamping(clamping));
    ui.label("dt:");
    ui.add(egui::Slider::new(&mut base.dt, 0.01..=1.0).clamping(clamping));
    ui.label("epsilon:");
    ui.add(egui::Slider::new(&mut base.epsilon, 0.001..=0.1).clamping(clamping));
}

#[allow(unused)]
// WIP
mod extra_forces {
    use std::u32;

    use egui::{Rect, Vec2};
    use serde::{Deserialize, Serialize};

    use egui_graphs::ExtraForce;
    use egui_graphs::{DisplayEdge, DisplayNode, Graph};
    use petgraph::EdgeType;

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct CollisionParams {
        pub padding: f32,
    }
    impl Default for CollisionParams {
        fn default() -> Self {
            Self { padding: 0.8 }
        }
    }

    #[derive(Debug, Default)]
    pub struct Collision;

    impl ExtraForce for Collision {
        type Params = CollisionParams;

        fn apply<N, E, Ty, Ix, Dn, De>(
            params: &Self::Params,
            g: &Graph<N, E, Ty, Ix, Dn, De>,
            indices: &[petgraph::stable_graph::NodeIndex<Ix>],
            disp: &mut [Vec2],
            area: Rect,
            _k: f32,
        ) where
            N: Clone,
            E: Clone,
            Ty: EdgeType,
            Ix: petgraph::csr::IndexType,
            Dn: DisplayNode<N, E, Ty, Ix>,
            De: DisplayEdge<N, E, Ty, Ix, Dn>,
        {
            // let center = area.center();
            // for (vec_pos, &idx) in indices.iter().enumerate() {
            //     let pos = g.g().node_weight(idx).unwrap().location();
            //     let delta = center - pos;
            //     disp[vec_pos] += delta * params.padding;
            // }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct PinningParams {
        pub intensity: f32,
        pub pinned: Vec<u32>,
    }
    impl Default for PinningParams {
        fn default() -> Self {
            Self {
                intensity: 1024.0,
                pinned: Default::default(),
            }
        }
    }
    impl PinningParams {
        pub fn iter_pinning(&mut self) -> PinningParamsIter<'_> {
            PinningParamsIter {
                pinned: &mut self.pinned,
                offset: 0,
                i: 0,
                node: 0,
            }
        }

        pub fn is_pinned(&mut self, node: u32) -> bool {
            let mut i = 0;
            while i < self.pinned.len() && self.pinned[i] < node {
                i += 1;
            }
            i < self.pinned.len() && self.pinned[i] == node
        }

        pub fn pin_node(&mut self, node: u32) {
            wasm_rs_dbg::dbg!(&self.pinned, node);
            let mut i = 0;
            while i < self.pinned.len() && self.pinned[i] < node {
                i += 1;
            }
            if i < self.pinned.len() {
                if node == self.pinned[i] {
                    self.pinned.remove(i);
                } else {
                    self.pinned.insert(i, node);
                }
            } else {
                self.pinned.push(node);
            }
            // self.pinned.insert(self.pinned.iter().position(|x| node < x).unwrap_or_default(), node);
            // self.pinned.push(node);
            // self.pinned.sort_unstable();
            wasm_rs_dbg::dbg!(&self.pinned, node);
            assert!(self.pinned.is_sorted());
            // if node == u32::MAX {
            //     wasm_rs_dbg::dbg!(&self.pinned);
            //     panic!()
            // }
            // let mut offset = 0;
            // let mut i = 0;
            // // find insertion position
            // loop {
            //     if i >= self.pinned.len() {
            //         // easy case
            //         wasm_rs_dbg::dbg!(offset, i);
            //         assert!(offset < node);
            //         self.pinned.push(node - offset);
            //         return;
            //     }
            //     if node == offset {
            //         // already there
            //         wasm_rs_dbg::dbg!(offset, i);
            //         return;
            //     }
            //     if node < offset {
            //         // found insertion position
            //         wasm_rs_dbg::dbg!(offset, i);
            //         let new = offset - node;
            //         let ins = self.pinned[i] - new;
            //         assert!(ins < new);
            //         self.pinned[i] = ins;
            //         self.pinned.insert(i + 1, new);
            //         return;
            //     }
            //     wasm_rs_dbg::dbg!(offset, i);
            //     offset += self.pinned[i];
            //     i += 1;
        }
    }

    pub struct PinningParamsIter<'a> {
        pinned: &'a mut Vec<u32>,
        offset: u32,
        i: usize,
        node: u32,
    }

    impl PinningParamsIter<'_> {
        pub fn next_node(&mut self) -> Option<()> {
            // // must always be incremented
            // if self.node == u32::MAX {
            //     // its like a wrapping add
            //     self.node = 0;
            // } else {
            //     self.node += 1;
            // }
            if self.i >= self.pinned.len() {
                // easy case
                return None;
            }
            if self.node < self.pinned[self.i] {
                // overstepped interval, so cannot be there
            } else {
                self.i += 1;
            }
            self.node += 1;
            // let is_there = self.node == self.offset;
            // self.offset += self.pinned[self.i];
            // let is_there = self.node == self.pinned[self.i];
            // assert!(self.pinned.is_sorted());
            // is_there
            Some(())
        }
        pub fn is_pinned(&self) -> bool {
            self.i < self.pinned.len() && self.node == self.pinned[self.i]
        }
        pub fn toggle_node(&mut self) {
            if self.i >= self.pinned.len() {
                // easy case
                // self.pinned.push(self.node - self.offset);
                self.pinned.push(self.node);
                assert!(
                    self.pinned.is_sorted(),
                    "{} {:?} {}",
                    self.node,
                    self.pinned,
                    self.i
                );
                return;
            }
            if self.node == self.pinned[self.i] {
                // // already there, so need to remove
                // self.pinned[self.i + 1] != self.pinned[self.i];
                // // NOTE i+1 exists due to previous guard
                self.pinned.remove(self.i);
                assert!(self.pinned.is_sorted());
                return;
            }
            if self.node < self.pinned[self.i] {
                // not there, so must be inserted
                // let new = self.offset - self.node;
                // let ins = self.pinned[self.i] - new;
                // self.pinned[self.i] = new;
                self.pinned.insert(self.i, self.node);
                assert!(self.pinned.is_sorted());
                return;
            }
            // if self.node == self.offset {
            //     // already there, so need to remove
            //     self.pinned[self.i + 1] != self.pinned[self.i];
            //     // NOTE i+1 exists due to previous guard
            //     self.pinned.remove(self.i);
            //     return;
            // }
            // if self.node < self.offset {
            //     // not there, so must be inserted
            //     let new = self.offset - self.node;
            //     let ins = self.pinned[self.i] - new;
            //     self.pinned[self.i] = new;
            //     self.pinned.insert(self.i, ins);
            //     return;
            // }
        }
    }

    #[derive(Debug, Default)]
    pub struct Pinning;

    impl ExtraForce for Pinning {
        type Params = PinningParams;

        fn apply<N, E, Ty, Ix, Dn, De>(
            params: &Self::Params,
            g: &Graph<N, E, Ty, Ix, Dn, De>,
            indices: &[petgraph::stable_graph::NodeIndex<Ix>],
            disp: &mut [Vec2],
            area: Rect,
            _k: f32,
        ) where
            N: Clone,
            E: Clone,
            Ty: EdgeType,
            Ix: petgraph::csr::IndexType,
            Dn: DisplayNode<N, E, Ty, Ix>,
            De: DisplayEdge<N, E, Ty, Ix, Dn>,
        {
            // wasm_rs_dbg::dbg!(indices, &params.pinned);
            // let mut offset = 0;
            let mut i = 0;
            for &o in &params.pinned {
                // offset += o as usize;
                let offset = o as usize;
                while i < indices.len() && indices[i].index() < offset {
                    i += 1;
                }
                if i >= indices.len() {
                    break;
                }
                if indices[i].index() == offset {
                    disp[i] /= params.intensity;
                }
            }
            // if params.pinned.is_empty() {
            //     return;
            // }
            // for (vec_pos, &idx) in indices.iter().enumerate() {
            //     if offset > params.pinned.len() {
            //         break;
            //     }
            //     let pinned = params.pinned[vec_pos / 32] & (1 << (vec_pos % 32)) != 0;
            //     let pin = if pinned { params.intensity } else { 1.0 };
            //     disp[vec_pos] /= pin;
            // }
        }
    }
}
pub fn get_anime_state(ui: &egui::Ui, id: Option<String>) -> AnimeState {
    egui_graphs::get_layout_state(ui, id)
}
pub fn set_layout_state(ui: &mut egui::Ui, state: AnimeState, id: Option<String>) {
    egui_graphs::set_layout_state(ui, state, id);
}
pub fn reset(ui: &mut Ui, id: Option<String>) {
    egui_graphs::reset::<AnimeState>(ui, id);
}

pub type HierarchicalGraphView<
    'a,
    N = (),
    E = (),
    Ty = Directed,
    Ix = DefaultIx,
    Nd = egui_graphs::DefaultNodeShape,
    Ed = egui_graphs::DefaultEdgeShape,
> = GraphView<
    'a,
    N,
    E,
    Ty,
    Ix,
    Nd,
    Ed,
    egui_graphs::LayoutStateHierarchical,
    egui_graphs::LayoutHierarchical,
    // egui_graphs::LayoutRandom,
    // egui_graphs::LayoutHierarchical,
>;

// #[cfg(target_arch = "wasm32")]
// #[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
// struct Instant(std::time::Duration);
// #[cfg(not(target_arch = "wasm32"))]
// #[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
// struct Instant(std::time::Instant);
// impl Instant {
//     fn now() -> Instant {
//         #[cfg(target_arch = "wasm32")]
//         {
//             Instant(std::time::Duration::ZERO)
//         }
//         #[cfg(not(target_arch = "wasm32"))]
//         {
//             Instant(std::time::Instant::now())
//         }
//     }

//     fn duration_since(&self, earlier: Instant) -> std::time::Duration {
//         #[cfg(target_arch = "wasm32")]
//         {
//             // not earlier, its the trick
//             earlier.0 - self.0
//         }

//         #[cfg(not(target_arch = "wasm32"))]
//         {
//             self.0.duration_since(earlier.0)
//         }
//     }

//     #[cfg(target_arch = "wasm32")]
//     fn add(&mut self, dt: f32) {
//         {
//             self.0 += std::time::Duration::from_secs_f32(dt);
//         }
//         #[cfg(not(target_arch = "wasm32"))]
//         {
//             let _ = dt;
//         }
//     }
// }

pub mod drawers;

pub mod settings;

const EVENTS_LIMIT: usize = 100;

type GraphId = String;
pub struct ForceBasedGraphExplorationApp<
    N: Clone = (),
    E: Clone = (),
    Dn: egui_graphs::DisplayNode<N, E, Directed, u32> = egui_graphs::DefaultNodeShape,
> {
    g: Graph<N, E, Directed, DefaultIx, Dn>,

    id: GraphId,
    others: Vec<Option<(Graph<N, E, Directed, DefaultIx, Dn>, GraphId)>>,
    /// the active graph in the `others` vec
    active: usize,

    can_show_graph: bool,
    show_graph: bool,
    show_pattern_list: bool,

    global: Global,
}

struct Global {
    settings_simulation: settings::SettingsSimulation,

    settings_graph: settings::SettingsGraph,
    settings_interaction: settings::SettingsInteraction,
    settings_navigation: settings::SettingsNavigation,
    settings_style: settings::SettingsStyle,

    last_events: Vec<String>,

    simulation_stopped: bool,

    event_publisher: Sender<Event>,
    event_consumer: Receiver<Event>,

    pan: [f32; 2],
    zoom: f32,
}

pub fn graph_pretty<'d, N: 'd + Clone + FlexPayload, E: 'd + Clone>(
    cc: &CreationContext<'_>,
    settings_graph: settings::SettingsGraph,
    settings_simulation: settings::SettingsSimulation,
    g: Graph<N, E, Directed, DefaultIx, node::NodeShapeFlex<N>>,
) -> impl App + 'd {
    ForceBasedGraphExplorationApp::<N, E, node::NodeShapeFlex<N>>::with_graph(
        cc,
        settings_graph,
        settings_simulation,
        g,
    )
}

pub fn multi_graph_pretty<'a, N: 'a + Clone + FlexPayload, E: 'a + Clone>(
    settings_graph: settings::SettingsGraph,
    settings_simulation: settings::SettingsSimulation,
    graph: Vec<Graph<N, E, Directed, DefaultIx, node::NodeShapeFlex<N>>>,
) -> impl App + 'a {
    let mut others: Vec<Option<_>> = graph
        .into_iter()
        .enumerate()
        .map(|(i, g)| {
            let id = format!("Graph {}", i);
            // TODO done by egui_graphs now ?
            // let (force, sim) = force_sim(&settings_simulation, &mut g);
            Some((g, id))
        })
        .collect();
    let (g, id) = others[0].take().unwrap();
    let mut app =
        ForceBasedGraphExplorationApp::<N, E, _>::new(settings_graph, settings_simulation, g, id);
    app.others = others;
    app
}

pub type Simple<N, E> = ForceBasedGraphExplorationApp<N, E, node::NodeShapeFlex<N>>;
pub type EGNode<N = String> = egui_graphs::Node<N, (), Directed, DefaultIx, node::NodeShapeFlex<N>>;
pub use petgraph::stable_graph::StableGraph;
pub struct SimpleExample(pub Graph);
impl SimpleExample {
    pub fn new() -> Self {
        let g = egui_graphs::generate_simple_digraph();
        Self(Graph::from(&g))
    }
    pub fn show(&mut self, ui: &mut egui::Ui) -> egui::Response {
        ui.add(
            &mut egui_graphs::DefaultGraphView::new(&mut self.0)
                .with_id(Some("default_graph".to_string())),
        )
    }
}

pub fn simple_pet_graph<N, E>() -> petgraph::stable_graph::StableGraph<N, E> {
    let g = StableGraph::new();
    // let mut g = StableGraph::new();
    // let n1 = simple_add_node(&mut g, "Coucou".to_string());
    // let n2 = simple_add_node(&mut g, "Bye".to_string());
    // let e1 = g.add_edge(n1, n2, egui_graphs::Edge::new(()));
    // g.edge_weight_mut(e1).unwrap().set_id(e1);
    g
}
pub type PGraph<N, E> = petgraph::Graph<N, E>;

pub use egui_graphs::to_graph;

pub fn simple_add_node<L: FlexPayload + Clone>(
    g: &mut petgraph::stable_graph::StableGraph<L, ()>,
    label: L,
) -> NodeIndex {
    let n1 = label;
    let n1 = g.add_node(n1);
    n1
}
pub fn simple_set_node_label<L: FlexPayload + Clone>(
    g: &mut petgraph::stable_graph::StableGraph<L, ()>,
    node_index: usize,
    label: L,
) {
    let w = g.node_weight_mut(NodeIndex::new(node_index)).unwrap();
    *w = label;
}
pub fn simple_add_edge<L: FlexPayload + Clone>(
    g: &mut petgraph::stable_graph::StableGraph<L, ()>,
    n1: NodeIndex,
    n2: NodeIndex,
) -> petgraph::graph::EdgeIndex {
    let e1 = ();
    let e1 = g.add_edge(n1, n2, e1);
    e1
}
impl<N: Clone, E: Clone, Dn: egui_graphs::DisplayNode<N, E, Directed, DefaultIx>>
    ForceBasedGraphExplorationApp<N, E, Dn>
{
    fn _show_graph(&mut self, ui: &mut Ui) {
        let settings_interaction = &egui_graphs::SettingsInteraction::new()
            .with_node_selection_enabled(self.global.settings_interaction.node_selection_enabled)
            .with_node_selection_multi_enabled(
                self.global
                    .settings_interaction
                    .node_selection_multi_enabled,
            )
            .with_dragging_enabled(self.global.settings_interaction.dragging_enabled)
            .with_node_clicking_enabled(self.global.settings_interaction.node_clicking_enabled)
            .with_edge_clicking_enabled(self.global.settings_interaction.edge_clicking_enabled)
            .with_edge_selection_enabled(self.global.settings_interaction.edge_selection_enabled)
            .with_edge_selection_multi_enabled(
                self.global
                    .settings_interaction
                    .edge_selection_multi_enabled,
            );
        let settings_navigation = &egui_graphs::SettingsNavigation::new()
            .with_zoom_and_pan_enabled(self.global.settings_navigation.zoom_and_pan_enabled)
            .with_fit_to_screen_enabled(self.global.settings_navigation.fit_to_screen_enabled)
            .with_zoom_speed(self.global.settings_navigation.zoom_speed);
        let settings_style = &egui_graphs::SettingsStyle::new()
            .with_labels_always(self.global.settings_style.labels_always);

        let mut md = egui_graphs::MetadataFrame::new(None).load(ui);
        md.reset_bounds();
        ui.add(
            &mut AnimatedGraphView::<N, E, _, _, _, _>::new(&mut self.g)
                .with_interactions(settings_interaction)
                .with_navigations(settings_navigation)
                .with_styles(settings_style)
                .with_event_sink(&self.global.event_publisher),
        );
    }
    pub fn show_graph(&mut self, ui: &mut Ui)
    where
        N: FlexPayload,
    {
        self._show_graph(ui);

        self.handle_events();
        self.sync();
        self.update_simulation();
        // self.global.update_fps(ui.ctx());
    }
}
impl<N: Clone + FlexPayload, E: Clone, Dn: egui_graphs::DisplayNode<N, E, Directed, u32>>
    ForceBasedGraphExplorationApp<N, E, Dn>
{
    pub fn show(&mut self, ctx: &Context) {
        egui::SidePanel::right("right_panel")
            .min_width(250.)
            .show(ctx, |ui| {
                if !self.others.is_empty() {
                    ui.horizontal(|ui| {
                        self.show_graph_selector(ui);

                        ui.add_enabled(
                            self.can_show_graph,
                            egui::Checkbox::new(&mut self.show_graph, "graph"),
                        );
                        ui.checkbox(&mut self.show_pattern_list, "list");
                    });

                    ui.label(format!(
                        "{} connex graph n:{} e:{}",
                        self.others.len(),
                        self.g.node_count(),
                        self.g.edge_count()
                    ));
                }
                ScrollArea::vertical().show(ui, |ui| {
                    CollapsingHeader::new("Simulation")
                        .default_open(true)
                        .show(ui, |ui| self.show_section_simulation(ui));

                    ui.add_space(10.);

                    egui::CollapsingHeader::new("Debug")
                        .default_open(true)
                        .show(ui, |ui| self.show_section_debug(ui));

                    ui.add_space(10.);

                    CollapsingHeader::new("Widget")
                        .default_open(true)
                        .show(ui, |ui| self.show_section_widget(ui));
                });
            });

        egui::CentralPanel::default().show(ctx, |ui| {
            if self.show_pattern_list {
                if !self.can_show_graph {
                    ui.label(
                        egui::RichText::new(format!(
                            "{} nodes and {} edges is too much to display",
                            self.g.node_count(),
                            self.g.edge_count()
                        ))
                        .color(ui.visuals().warn_fg_color),
                    );
                }

                let size = if self.can_show_graph && self.show_graph {
                    ui.available_size() * egui::vec2(1.0, 0.2)
                } else {
                    ui.available_size()
                };
                let (rect, _) = ui.allocate_exact_size(size, egui::Sense::click());
                let ui = &mut ui.new_child(egui::UiBuilder::new().max_rect(rect));
                let total_cols = self.g.node_count();
                let mut rm = None;
                let mut nodes = None;
                crate::hscroll::hscroll_many_columns(ui, 450.0, total_cols, |ui, i| {
                    if nodes.is_none() {
                        nodes = Some(self.g.g().node_references().rev().skip(i));
                    };
                    let nodes = nodes.as_mut().unwrap();
                    let (idx, n) = nodes.next().unwrap();
                    let payload = n.payload();
                    let clicked = ui
                        .horizontal(|ui| {
                            let clicked = ui.button("remove").clicked();
                            let outg = self
                                .g
                                .edges_directed(idx, petgraph::Direction::Outgoing)
                                .count();
                            let inco = self
                                .g
                                .edges_directed(idx, petgraph::Direction::Incoming)
                                .count();
                            ui.label(format!(
                                "#:{} out:{outg:#} in:{inco:#}",
                                payload.secondary()
                            ));
                            clicked
                        })
                        .inner;
                    ui.label(format!("{:#}", payload.primary()));
                    if clicked {
                        rm = Some(idx)
                    }
                });

                if let Some(idx) = rm {
                    log::info!("Removing node {:?}", idx);
                    log::warn!("Removing node is currently disabled");
                    // TODO reenable
                    // self.g.remove_node(idx);
                    // self.sim.remove_node(idx);
                }
            }
            if self.can_show_graph && self.show_graph {
                self._show_graph(ui);
            }
        });

        self.handle_events();
        self.sync();
        self.update_simulation();
        // TODO do it through egui or something else
        // self.global.update_fps(ctx);
    }

    fn show_graph_selector(&mut self, ui: &mut Ui) {
        let left = egui::Button::new("<");
        let left = ui.add_enabled(self.active != 0, left).clicked();
        ui.label(format!("{}", self.active));
        let right = egui::Button::new(">");
        let right = ui
            .add_enabled(self.active != self.others.len() - 1, right)
            .clicked();
        if left {
            self.others.swap(self.active, self.active - 1);
            let new = &mut self.others[self.active];
            self.active -= 1;
            let Some(new) = new else { unreachable!() };
            std::mem::swap(&mut new.0, &mut self.g);
            std::mem::swap(&mut new.1, &mut self.id);
            // self.force = init_force(&self.global.settings_simulation);
        } else if right {
            self.others.swap(self.active, self.active + 1);
            let new = &mut self.others[self.active];
            self.active += 1;
            let Some(new) = new else { unreachable!() };
            std::mem::swap(&mut new.0, &mut self.g);
            std::mem::swap(&mut new.1, &mut self.id);
            if self.g.node_count() < 300 {
                self.can_show_graph = true;
            }
            // self.force = init_force(&self.global.settings_simulation);
        }

        if left || right {
            egui_graphs::MetadataFrame::default().save(ui);
        }
    }
}

impl<N: Clone + FlexPayload, E: Clone, Dn: egui_graphs::DisplayNode<N, E, Directed, DefaultIx>>
    ForceBasedGraphExplorationApp<N, E, Dn>
{
    pub fn with_graph(
        _: &CreationContext<'_>,
        settings_graph: settings::SettingsGraph,
        settings_simulation: settings::SettingsSimulation,
        g: Graph<N, E, Directed, DefaultIx, Dn>,
    ) -> Self {
        // TODO done by egui_graphs ?
        // let (force, sim) = force_sim(&settings_simulation, &mut g);

        Self::new(settings_graph, settings_simulation, g, "solo".into())
    }

    pub fn new(
        settings_graph: settings::SettingsGraph,
        settings_simulation: settings::SettingsSimulation,
        g: Graph<N, E, Directed, u32, Dn>,
        id: GraphId,
    ) -> Self {
        let (event_publisher, event_consumer) = unbounded();
        Self {
            g,
            id,

            others: vec![],
            active: 0,
            can_show_graph: true,
            show_graph: true,
            show_pattern_list: false,

            global: Global {
                event_consumer,
                event_publisher,

                settings_graph,
                settings_simulation,

                settings_interaction: settings::SettingsInteraction::default(),
                settings_navigation: settings::SettingsNavigation::default(),
                settings_style: settings::SettingsStyle::default(),

                last_events: Vec::default(),

                simulation_stopped: false,

                pan: [0., 0.],
                zoom: 10.,
            },
        }
    }

    /// applies forces if simulation is running
    fn update_simulation(&mut self) {
        if self.global.simulation_stopped {
            return;
        }
        if self.can_show_graph {
            // TODO ?
            // self.force.apply(&mut self.sim);
        }
    }

    /// sync locations computed by the simulation with egui_graphs::Graph nodes.
    fn sync(&mut self) {
        // TODO needed now ?
        // self.g.g_mut().node_weights_mut().for_each(|node| {
        //     let sim_computed_point: OPoint<f32, Const<2>> =
        //         self.sim.node_weight(node.id()).unwrap().1;
        //     node.set_location(Pos2::new(
        //         sim_computed_point.coords.x,
        //         sim_computed_point.coords.y,
        //     ));
        // });
    }

    fn handle_events(&mut self) {
        self.global.event_consumer.try_iter().for_each(|e| {
            if self.global.last_events.len() > EVENTS_LIMIT {
                self.global.last_events.remove(0);
            }
            self.global.last_events.push(format!("{e:?}"));
            // .push(serde_json::to_string(&e).unwrap());

            match e {
                Event::NodeDoubleClick(_payload) => {
                    // let node_id = NodeIndex::new(payload.id);
                    // TODO needed now ?
                    // self.g.remove_node(node_id);
                    // self.sim.remove_node(node_id);
                }
                // Event::Pan(payload) => self.global.pan = payload.new_pan,
                // Event::Zoom(payload) => {
                //     if !payload.new_zoom.is_nan() {
                //         self.global.zoom = payload.new_zoom; //.clamp(0.03, 20.0);
                //     }
                // }
                Event::NodeMove(_payload) => {
                    // let node_id = NodeIndex::new(payload.id);

                    // TODO handled by egui_graph now

                    // self.sim.node_weight_mut(node_id).unwrap().1.coords.x = payload.new_pos[0];
                    // self.sim.node_weight_mut(node_id).unwrap().1.coords.y = payload.new_pos[1];
                }
                _ => {}
            }
        });
    }

    pub fn show_section_simulation(&mut self, ui: &mut Ui) {
        ui.horizontal_wrapped(|ui| {
            ui.style_mut().spacing.item_spacing = Vec2::new(0., 0.);
            ui.label("Force-Directed Simulation is done with ");
            ui.hyperlink_to("fdg project", "https://github.com/grantshandy/fdg");
        });

        ui.separator();

        let (simulation_stopped, reset) = drawers::draw_start_reset_buttons(
            ui,
            drawers::ValuesConfigButtonsStartReset {
                simulation_stopped: self.global.simulation_stopped,
            },
        );
        if simulation_stopped || reset {
            self.global.simulation_stopped = simulation_stopped;
        }
        if reset {
            log::warn!("how should we reset now ?")
            //     dbg!();
            //     let mut md = egui_graphs::MetadataFrame::new(None).load(ui);
            //     dbg!(md.graph_bounds());
            //     dbg!(md.zoom);
            //     // md.zoom = 1.5;
            //     md.reset_bounds();
            //     // for n in self.sim.node_weights_mut() {
            //     //     dbg!(&n.1);
            //     //     n.1.x = 1.0;
            //     //     n.1.y = 1.0;
            //     // }
            //     for n in self.g.nodes_iter() {
            //         dbg!(&n.1.location());
            //         md.process_bounds(n.1);
            //     }
            //     for n in self.g.g_mut().node_weights_mut() {
            //         dbg!(&n.location());
            //         n.set_location(Pos2::new(1.0, 1.0));
            //         md.process_bounds(n);
            //     }
            //     dbg!(md.graph_bounds());
            //     md.save(ui);
            //     self.reset();
            //     // egui_graphs::GraphView::<
            //     //     N,
            //     //     E,
            //     //     _,
            //     //     _,
            //     //     Dn,
            //     //     egui_graphs::DefaultEdgeShape,
            //     //     egui_graphs::LayoutStateRandom,
            //     //     egui_graphs::LayoutRandom,
            //     // >::clear_cache(ui);
        }

        ui.add_space(10.);

        drawers::draw_simulation_config_sliders(
            ui,
            drawers::ValuesConfigSlidersSimulation {
                dt: self.global.settings_simulation.dt,
                cooloff_factor: self.global.settings_simulation.cooloff_factor,
                scale: self.global.settings_simulation.scale,
            },
            |delta_dt: f32, delta_cooloff_factor: f32, delta_scale: f32| {
                let s = &mut self.global.settings_simulation;
                s.dt += delta_dt;
                s.cooloff_factor += delta_cooloff_factor;
                s.scale += delta_scale;
                // self.force = init_force(&s); // TODO handled internally ?
            },
        );

        ui.add_space(10.);
    }

    pub fn show_section_widget(&mut self, ui: &mut Ui) {
        self.global.settings_navigation.show(ui);
        self.global.settings_graph.show(ui);
        self.global.settings_style.show(ui);
        self.global.settings_interaction.show(ui);
        self.draw_selected_widget(ui);
        self.global.draw_last_event_widget(ui);
    }

    fn draw_selected_widget(&mut self, ui: &mut Ui) {
        let add_content = |ui: &mut egui::Ui| {
            let rm: Vec<_> = (self.g.selected_nodes().iter())
                .filter_map(|node| {
                    let clicked = ui.button("remove").clicked();
                    ui.label(format!("{node:?}"));
                    let p = &self.g.node(*node).unwrap().payload();
                    ui.label(format!("{:#}", p.secondary()));
                    clicked.then_some(*node)
                })
                .collect();
            for idx in rm {
                log::warn!(
                    "Removing node {:?} should be done through egui_graphs ?",
                    idx
                );
                // self.g.remove_node(idx);
                // self.sim.remove_node(idx);
            }
            self.g.selected_edges().iter().for_each(|edge| {
                ui.label(format!("{edge:?}"));
            });
        };
        CollapsingHeader::new("Selected")
            .default_open(true)
            .show(ui, |ui| {
                ScrollArea::vertical()
                    .auto_shrink([false, true])
                    .max_height(200.)
                    .show(ui, add_content);
            });
    }

    pub fn show_section_debug(&self, ui: &mut Ui) {
        drawers::draw_section_debug(
            ui,
            ValuesSectionDebug {
                zoom: self.global.zoom,
                pan: self.global.pan,
            },
        );
    }
}

impl Global {
    fn draw_last_event_widget(&mut self, ui: &mut Ui) {
        CollapsingHeader::new("Last Events")
            .default_open(true)
            .show(ui, |ui| {
                if ui.button("clear").clicked() {
                    self.last_events.clear();
                }
                ScrollArea::vertical()
                    .auto_shrink([false, true])
                    .show(ui, |ui| {
                        self.last_events.iter().rev().for_each(|event| {
                            ui.label(event);
                        });
                    });
            });
    }
}

impl<N: Clone + FlexPayload, E: Clone, Dn: egui_graphs::DisplayNode<N, E, Directed, u32>> App
    for ForceBasedGraphExplorationApp<N, E, Dn>
{
    fn update(&mut self, ctx: &Context, _: &mut eframe::Frame) {
        self.show(ctx);
    }
}

pub use node::FlexPayload;
pub use node::NodeShapeFlex;
mod node;
