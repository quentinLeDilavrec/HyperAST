//! Smell detection / Code quality issue finder / Code Quality evaluation assistant
//!
//! This HyperAST extension is aimed at assisting developers that want to improve the code quality of one of their project.
//! 1) The developer select/provide examples comming from a patch that improves code quality
//! 2) The developer select a commit in his project.
//! 3) Then a latice of queries is computed corrresponding to the examples:
//!     1) these queries are run on the provided commit
//!     2) the results are used to filter out extreme queries and rank them
//!     3) the developper select the query he prefer
//! 4) the selected query can then be executed on multiple commits or up to their resolution
//! 5) the results are diplayed to the developer and can be explored
//! 6) given feedbacks from the developper alternative queries are suggested
//! 7) go back to 4 by selecting a query
//! 8) generate an issue summarising the possible quality/consitency improvements
//!
//!
//! This tool can also be used by researchers on a more meta level,
//! such as finding how popular/widespread/prevalent/pervasive is a particular code pattern on productivity
//! or evaluating the impact of a particular code pattern on productivity
//!
//! Note: Smell are particularly interesting for their complexiity and context dependant nature,
//! as well as their low frequency (per pattern (not per category)).
//!
//! Note: The temporal analysis (~ searching for the patterns throughout the history of dev)
//! provide confidence and facts about the significance of code quality issues
//!
//!
//! https://github.com/INRIA/spoon/commit/8f967893e5441dbf95b842350234c3185bcaeed7
//! test: Migrate support and testing tests to Junit5
//! @Test(expected = ...)
//!
//! https://github.com/google/gson/commit/99cc4cb11f73a6d672aa6381013d651b7921e00f
//! more specifically:
//! https://github.com/Marcono1234/gson/commit/3d241ca0a6435cbf1fa1cdaed2af8480b99fecde
//! about fixing try catches in tests

use std::hash::Hash;
#[cfg(feature = "force_layout")]
use std::ops::Deref;
use std::ops::{Range, SubAssign};
use wasm_rs_dbg::dbg;

use egui_addon::InteractiveSplitter;
use egui_addon::MultiSplitter;

use super::code_tracking::FetchedFiles;
use super::code_tracking::try_fetch_remote_file;
use super::types;
use super::types::CommitId;
use super::types::{CodeRange, Commit, SelectedConfig};
use super::utils_edition::MakeHighlights;
use crate::utils_poll::{Remote, Resource};

mod config_examples;

#[derive(serde::Deserialize, serde::Serialize, Debug)]
#[serde(default)]
pub(super) struct ComputeConfigQuery {
    commit: Commit,
    /// the query configuring the query generation from examples
    /// eg. `(identifier) @label ["{" ";" "." "try" "(" ")" "}" "catch" "import"] @skip (block ["{" "}"] @show) (block) @imm`
    /// eg. `(identifier) (type_identifier)` same as `(identifier) @label (type_identifier) @label`
    meta_gen: String,
    /// the query configuring the query simplification/generalization
    /// eg. `(predicate (identifier) (#EQ? "EQ") (parameters (string) @label )) @pred`
    meta_simp: String,
    config: types::Config,
    len: usize,
    simple_matching: bool,
    prepro_matching: bool,

    // filterings
    wanted_matches: std::ops::Range<usize>,

    #[serde(skip)]
    // examples
    examples: Vec<config_examples::Example>,

    // just ui stuff, might do better
    advanced_open: bool,
}

impl Default for ComputeConfigQuery {
    fn default() -> Self {
        Self {
            wanted_matches: usize::MAX..usize::MAX,
            advanced_open: false,
            examples: vec![
                config_examples::BASE_TRY_FAIL_CATCH_EX.clone(),
                config_examples::MORE_TRY_FAIL_CATCH_EX.clone(),
            ],
            ..Into::into(&config_examples::MORE_TRY_FAIL_CATCH_EX)
        }
    }
}

pub(crate) fn project_modal_handler(
    data: &mut super::AppData,
    pid: super::ProjectId,
) -> super::ProjectId {
    let projects = &mut data.selected_code_data;
    let commit = data.smells.commits.as_ref().map(|x| &x.commit);
    use crate::app::utils_commit::project_modal_handler;
    let (repo, mut commits) = match project_modal_handler(pid, projects, commit) {
        Ok(value) => value,
        Err(value) => return value,
    };
    data.smells = Default::default();
    data.smells.commits = Some(Default::default());
    data.smells_result = None;
    data.smells_diffs_result = None;
    let commit = &mut data.smells.commits.as_mut().unwrap().commit;
    commit.repo = repo.clone();
    commit.id = *commits.iter_mut().next().unwrap();
    super::ProjectId::INVALID
}

pub(crate) fn commit_modal_handler(data: &mut super::AppData, cid: super::types::CommitId) {
    data.smells.set_commit_id(cid);
    data.smells.diffs = None;
    data.smells.bads = None;
    data.smells_result = None;
    data.smells_diffs_result = None;
}

// pub(crate) type Config = Sharing<ComputeConfigQuery>;
#[derive(serde::Deserialize, serde::Serialize)]
pub(crate) struct Config {
    pub(crate) commits: Option<ComputeConfigQuery>,
    pub(crate) diffs: Option<ExamplesValues>,
    pub(crate) queries: Option<SearchResults>,
    pub(crate) stats: Option<Vec<(types::CodeRange, types::CodeRange)>>,
    pub(crate) bad_matches_bounds: std::ops::RangeInclusive<usize>,
    pub(crate) bads: Option<Vec<usize>>,
    #[cfg(feature = "force_layout")]
    #[serde(skip)]
    graph_view_settings: Vec<GVSetting>,
    #[cfg(feature = "force_layout")]
    #[serde(skip)]
    prepared_graphs: Vec<Option<Box<dyn std::any::Any + Send + Sync>>>,
}
#[cfg(feature = "force_layout")]
struct GVSetting {
    fit_to_screen_enabled: bool,
    view_mode: ViewMode,
    limit: usize,
    pretty_nodes: usize,
    selected_nodes: Vec<u32>,
    pretty_queued: Vec<u32>,
    too_general: Vec<u32>,
}

#[cfg(feature = "force_layout")]
#[derive(PartialEq)]
enum ViewMode {
    /// only show the statistics of the lattice
    StatisticsOnly,
    /// examples as documents and tops as attributes
    ExampleTopsBiGraph,
    /// most expensive to render, can be impractical with many nodes (> 1000s)
    FullGraph,
}

#[cfg(feature = "force_layout")]
impl Default for GVSetting {
    fn default() -> Self {
        Self {
            fit_to_screen_enabled: true,
            view_mode: ViewMode::ExampleTopsBiGraph,
            limit: 3000,
            pretty_nodes: 20,
            selected_nodes: vec![],
            pretty_queued: vec![],
            too_general: vec![],
        }
    }
}

#[cfg(feature = "force_layout")]
struct ExIt<'g> {
    content: &'g G,
    ex_curr: u32,
    ex_offset: usize,
    it: std::ops::Range<usize>,
    i: Option<usize>,
}
#[derive(Clone, Copy)]
struct ExSet(u64);
impl ExSet {
    fn len(&self) -> u32 {
        self.0.count_ones()
    }
    fn iter(&self) -> impl Iterator<Item = u32> {
        (0..64).filter(|&i| self.0 & (1 << i) != 0)
    }
}
impl std::fmt::Display for ExSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0 == 0 {
            Ok(())
        } else {
            write!(f, "{}", self.len())
        }
    }
}
impl std::ops::BitOrAssign for ExSet {
    fn bitor_assign(&mut self, other: Self) {
        self.0 |= other.0;
    }
}
impl<'a> ExIt<'a> {
    fn new(content: &'a G) -> Self {
        Self {
            ex_curr: 0,
            ex_offset: 0,
            it: (0..content.queries.len()).into_iter(),
            content,
            i: None,
        }
    }
    fn next_exset(&mut self) -> Option<ExSet> {
        self.next_agg(|inits, i| inits | (1 << i)).map(ExSet)
    }
    fn next_agg<R: Default>(&mut self, f: impl Fn(R, usize) -> R) -> Option<R> {
        let mut inits = R::default();
        while let Some(ex_offset) = self.next()? {
            // wasm_rs_dbg::dbg!(ex_offset);
            inits = f(inits, ex_offset);
        }
        Some(inits)

        // let i = self.it.next()?;
        // loop {
        //     if self.ex_curr as usize == i {
        //         inits = f(inits, self.ex_offset);
        //         self.ex_offset += 1;
        //         if self.ex_offset >= self.content.ex_query.len() {
        //             break;
        //         }
        //         self.ex_curr += self.content.ex_query[self.ex_offset];
        //     } else if (self.ex_curr as usize) < i {
        //         if self.ex_offset >= self.content.ex_query.len() {
        //             break;
        //         }
        //         self.ex_curr += self.content.ex_query[self.ex_offset];
        //     } else {
        //         break;
        //     }
        // }
        // Some(inits)
    }
}

#[cfg(feature = "force_layout")]
impl Iterator for ExIt<'_> {
    type Item = Option<usize>;
    fn next(&mut self) -> Option<Option<usize>> {
        let i = if let Some(i) = self.i {
            i
        } else {
            self.i = self.it.next();
            self.i?
        };
        if self.ex_curr as usize == i {
            let ex_offset = Some(self.ex_offset);
            self.ex_offset += 1;
            if self.ex_offset >= self.content.ex_query.len() {
                self.i = None;
                return Some(ex_offset);
            }
            self.ex_curr += self.content.ex_query[self.ex_offset];
            Some(ex_offset)
        } else if (self.ex_curr as usize) < i {
            if self.ex_offset >= self.content.ex_query.len() {
                return Some(None);
            }
            self.ex_curr += self.content.ex_query[self.ex_offset];
            self.next()
        } else {
            self.i = None;
            Some(None)
        }
    }
}

#[derive(Clone)]
struct Payload {
    pretty: String,
    inits: ExSet,
}

#[cfg(feature = "force_layout")]
impl egui_addon::force_layout::FlexPayload for Payload {
    fn primary(&self) -> impl std::fmt::Display + '_ {
        &self.pretty
    }
    fn secondary(&self) -> impl std::fmt::Display + '_ {
        self.inits
    }
}

impl Config {
    pub(crate) fn set_commit_id(&mut self, cid: CommitId) {
        self.commits.as_mut().expect("query config").commit.id = cid;
    }

    pub(crate) fn repo(&self) -> Option<&super::Repo> {
        self.commits.as_ref().map(|x| &x.commit.repo)
    }
}
impl Default for Config {
    fn default() -> Self {
        let compute_config_query: ComputeConfigQuery = Default::default();
        Self {
            commits: Some(compute_config_query),
            diffs: None,
            queries: None,
            stats: None,
            bad_matches_bounds: std::ops::RangeInclusive::new(0, 0),
            bads: Default::default(),
            #[cfg(feature = "force_layout")]
            graph_view_settings: Default::default(),
            #[cfg(feature = "force_layout")]
            prepared_graphs: Default::default(),
        }
    }
}

pub(crate) type RemoteResult = Remote<Result<SearchResults, SmellsError>>;
pub(crate) type RemoteResultDiffs = Remote<Result<ExamplesValues, DiffsError>>;

#[derive(serde::Deserialize, serde::Serialize, Debug)]
pub struct SearchResults {
    pub prepare_time: f64,
    pub search_time: f64,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    #[serde(default = "Default::default")]
    pub bad: Vec<SearchResult>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    #[serde(default = "Default::default")]
    pub good: Vec<SearchResult>,

    pub graphs: Vec<G>,
}

#[derive(serde::Deserialize, serde::Serialize, Debug)]
pub struct G {
    /// hold the query ids
    queries: Vec<u32>,
    /// successors (ie. to more general patterns in our case)
    /// multiple opti based on topo sorted lattice:
    /// * sub lists are separated by zero
    /// * each succ is encoded as the difference with prev succ
    ///   * when first the index of the node is the one subtracted from succ
    succ: Vec<u32>,
    /// not necessarily there
    queries_pretty: Vec<String>,
    /// pretty printed examples
    pub ex_pretty: Vec<String>,
    /// query associated to pretty printed examples
    ///
    /// same size as ex_pretty, sorted, and delta encoded similarly to succ,
    /// but here an example has exactly one associated query
    pub ex_query: Vec<u32>,
}
impl G {
    fn pretty_or_id(&self, id: usize) -> String {
        let pretty_payload = self.queries_pretty.get(id);
        pretty_payload.map_or_else(|| self.queries[id].to_string(), |x| x.to_string())
    }

    fn iter_successors<'a>(&'a self) -> impl Iterator<Item = &'a [u32]> {
        self.succ.split(|x| *x == 0)
    }
}

#[derive(serde::Deserialize, serde::Serialize, Debug, Clone)]
pub struct ExamplesValues {
    pub(crate) examples: Vec<ExamplesValue>,
    moves: Vec<(CodeRange, CodeRange)>,
}

#[derive(serde::Deserialize, serde::Serialize, Debug, Clone)]
pub struct ExamplesValue {
    pub(crate) before: CodeRange,
    pub(crate) after: CodeRange,
    pub(crate) inserts: Vec<Range<usize>>,
    pub(crate) deletes: Vec<Range<usize>>,
    pub(crate) moves: Vec<(Range<usize>, Range<usize>)>,
}

#[derive(serde::Deserialize, serde::Serialize, Debug)]
pub struct SearchResult {
    pub query: String,
    // the correspondin
    pub examples: Vec<usize>,
    //stats
    pub matches: usize,
}

// WIP
pub(crate) type Context = ();

#[derive(Debug, serde::Deserialize, serde::Serialize)]
pub enum SmellsError {
    MissingLanguage(String),
    QueryParsing(String),
    MissingExamples(String),
}
#[derive(Debug, serde::Deserialize, serde::Serialize)]
pub enum DiffsError {
    Error(String),
}

pub(crate) const WANTED: SelectedConfig = SelectedConfig::Smells;

pub(crate) fn show_config(
    ui: &mut egui::Ui,
    smells: &mut Config,
) -> (egui::Response, egui::Response) {
    use super::utils_egui::MyUiExt;
    let Some(conf) = &mut smells.commits else {
        return (ui.label(""), ui.label(""));
    };
    ui.label("Source of initial Examples:");
    let (resp_repo, resp_commit) = conf.commit.show_clickable(ui);

    let slider = egui::Slider::new(&mut conf.len, 1..=200)
        .clamping(egui::SliderClamping::Always)
        .integer()
        .logarithmic(true);
    ui.responsive_width_slider(slider, "commits");

    let selected = &mut conf.config;
    selected.show_combo_box(ui, "Repo Config");

    let adv_button = egui::Button::new("🗖 Open Advanced Settings").selected(conf.advanced_open);
    if ui.add(adv_button).clicked() {
        conf.advanced_open ^= true;
    }

    let hint_text = "the query configuring the query generation";
    let meta_gen = egui::TextEdit::multiline(&mut conf.meta_gen).hint_text(hint_text);
    let hint_text = "the query used to direct the simplification of generated queries";
    let meta_simp = egui::TextEdit::multiline(&mut conf.meta_simp).hint_text(hint_text);
    egui::Window::new("Interactive Finder's Advanced Settings")
        .open(&mut conf.advanced_open)
        .show(ui.ctx(), |ui| {
            ui.label("Query Generation:");
            meta_gen.desired_rows(1).interactive(true).show(ui);
            ui.label("Query Simplification:");
            meta_simp.desired_rows(1).interactive(true).show(ui);

            ui.checkbox(&mut conf.simple_matching, "Simple Matching");
            ui.checkbox(&mut conf.prepro_matching, "Incr. Matching");
        });

    ui.checkbox(&mut conf.simple_matching, "Simple Matching");
    ui.checkbox(&mut conf.prepro_matching, "Incr. Matching");

    let bounds_initialized = smells.bad_matches_bounds != (0..=0);
    ui.add_enabled_ui(bounds_initialized, |ui| {
        ui.label("#matches on entire commit:");
        if bounds_initialized && (conf.wanted_matches) == (usize::MAX..usize::MAX) {
            smells.bads = None;
            conf.wanted_matches =
                *smells.bad_matches_bounds.start()..*smells.bad_matches_bounds.end();
        }
        let double_ended_slider = ui.double_ended_slider(
            &mut conf.wanted_matches.start,
            &mut conf.wanted_matches.end,
            smells.bad_matches_bounds.clone(),
        );
        if double_ended_slider.changed() {
            smells.bads = None
        };
    })
    .response
    .on_disabled_hover_text("no patterns to filter");

    // let text = "displays only queries in the given range";
    // double_ended_slider.on_hover_text_at_pointer(text);

    ui.add_space(10.0);
    let mut sel_ex = None;
    ui.label("examples:");
    ui.grouped_wrapped_list(conf.examples.iter().map(Some), |ui, i, e| {
        if e.show(ui).clicked() {
            sel_ex = Some(i);
        }
    });
    if let Some(sel_ex) = sel_ex {
        *conf = ComputeConfigQuery {
            wanted_matches: usize::MAX..usize::MAX,
            advanced_open: conf.advanced_open,
            examples: conf.examples.clone(),
            ..Into::into(&conf.examples[sel_ex])
        };
    }

    #[cfg(feature = "force_layout")]
    ui.label("lattices:");
    #[cfg(feature = "force_layout")]
    ui.grouped_wrapped_list(
        smells.prepared_graphs.iter().map(|x| x.as_ref()),
        |ui, i, pg| {
            let (mode, pg) = pg.downcast_ref::<(ViewMode, GraphTy)>().unwrap();
            ui.label(format!("graph {} n:{}", i, pg.node_count()));
        },
    );

    (resp_repo, resp_commit)
}
#[derive(enumset::EnumSetType)]
pub enum Action {
    OpenGraph,
    Compute,
    Recompute,
    Waiting,
}
pub type Actions = enumset::EnumSet<Action>;

pub fn handle_actions(
    ui: &mut egui::Ui,
    api_addr: &str,
    smells: &mut Config,
    smells_result: &mut Option<RemoteResult>,
    smells_diffs_result: &mut Option<RemoteResultDiffs>,
    fetched_files: &mut FetchedFiles,
    actions: Actions,
) -> Option<u16> {
    if actions.is_empty() {
        return None;
    }
    let mut pg_id = None;
    let center = ui.available_rect_before_wrap().center();
    egui::Window::new("Actions")
        .default_pos(center)
        .pivot(egui::Align2::CENTER_CENTER)
        .show(ui.ctx(), |ui| {
            if actions.contains(Action::Recompute) {
                if ui.button("Recompute Queries").clicked() {
                    let conf = smells.commits.as_mut().unwrap();
                    let examples = smells.diffs.as_mut().unwrap();
                    *smells_result = Some(fetch_results(ui.ctx(), &api_addr, conf, &examples));
                }
            } else if actions.contains(Action::Compute) {
                if ui.button("Compute Queries").clicked() {
                    let conf = smells.commits.as_mut().unwrap();
                    let examples = smells.diffs.as_mut().unwrap();
                    *smells_result = Some(fetch_results(ui.ctx(), &api_addr, conf, &examples));
                }
            }

            if actions.contains(Action::Waiting) {
                ui.spinner();
            }

            handle_open_synth_graph(ui, smells, smells_result, &mut pg_id);
        });
    pg_id
}

fn handle_open_synth_graph(
    ui: &mut egui::Ui,
    smells: &mut Config,
    smells_result: &mut Option<RemoteResult>,
    pg_id: &mut Option<u16>,
) {
    #[cfg(feature = "force_layout")]
    let Some(prom) = smells_result.as_mut() else {
        return;
    };
    let Ok(result) = prep_smells_results(ui, smells, prom) else {
        return;
    };
    let Some(queries) = access_smells_results(ui, result) else {
        return;
    };
    use crate::app::utils_egui::MyUiExt;
    ui.grouped_wrapped_list(queries.graphs.iter().map(Some), |ui, i, x| {
        let b = ui.button(format!(
            "lattice {} q:{} e:{}",
            i,
            x.queries_pretty.len(),
            x.ex_pretty.len()
        ));
        if b.clicked() {
            *pg_id = Some(i as u16);
        }
    });
}
pub(super) fn show_central_panel(
    ui: &mut egui::Ui,
    api_addr: &str,
    smells: &mut Config,
    smells_result: &mut Option<RemoteResult>,
    smells_diffs_result: &mut Option<RemoteResultDiffs>,
    fetched_files: &mut FetchedFiles,
) -> Actions {
    let mut actions = Actions::default();
    if let Some(_x) = &mut smells.stats {
        todo!();
    }
    if let Some(_examples) = &mut smells.queries {
        todo!();
    }
    // let mut show_action_menu = true;
    match (smells_result.as_mut()).map(|prom| prep_smells_results(ui, smells, prom)) {
        Some(Ok(result)) => {
            if let Some(queries) = access_smells_results(ui, result) {
                if let Err(value) =
                    show_smells_result(ui, api_addr, smells, fetched_files, queries, &mut actions)
                {
                    #[cfg(feature = "force_layout")]
                    for gid in 0..queries.graphs.len() {
                        let id = format!("force_graph_patterns{}", gid);
                        egui_addon::force_layout::reset(ui, Some(id))
                    }
                    smells.prepared_graphs.clear();
                    *smells_result = Some(value)
                }
            }
        }
        Some(Err(Some(resp))) => {
            let conf = smells.commits.as_mut().unwrap();
            let examples = smells.diffs.as_mut().unwrap();
            show_examples(ui, api_addr, examples, fetched_files);
            if resp.clicked() {
                *smells_result = Some(fetch_results(ui.ctx(), api_addr, conf, &examples));
            }
        }
        Some(Err(None)) => {
            actions |= Action::Waiting;
        }
        _ => (),
    }
    if let Some(promise) = smells_diffs_result {
        let Some(result) = promise.ready() else {
            ui.spinner();
            return actions;
        };
        match result {
            Ok(resource) => match &resource.content {
                Some(Ok(examples)) => {
                    smells.diffs = Some(examples.clone());
                }
                Some(Err(error)) => {
                    egui::Window::new("QueryError3").show(ui.ctx(), |ui| {
                        ui.label("Error");
                        ui.label(format!("{:?}", error));
                    });
                }
                _ => (),
            },
            Err(error) => {
                let mut retry = false;
                // This should only happen if the fetch API isn't available or something similar.
                egui::Window::new("Diff Error").show(ui.ctx(), |ui| {
                    ui.colored_label(
                        ui.visuals().error_fg_color,
                        if error.is_empty() { "Error" } else { error },
                    );
                    if ui.button("retry").clicked() {
                        retry = true;
                    }
                });
                if ui.button("retry").clicked() || retry {
                    *smells_diffs_result = None;
                }
            }
        };
    }
    if let Some(examples) = &mut smells.diffs {
        let len = examples.examples.len();
        if len == 0 {
            ui.colored_label(ui.visuals().error_fg_color, "No changes found");
            egui::Window::new("Diff Error").show(ui.ctx(), |ui| {
                if ui.button("retry").clicked() {
                    *smells_diffs_result = None;
                }
            });
            if ui.button("retry").clicked() {
                *smells_diffs_result = None;
            }
            return actions;
        }
        // let conf = smells.commits.as_mut().unwrap();
        // let center = ui.available_rect_before_wrap().center();
        show_examples(ui, api_addr, examples, fetched_files);
        // if show_action_menu {
        //     egui::Window::new("Actions")
        //         .default_pos(center)
        //         .pivot(egui::Align2::CENTER_CENTER)
        //         .show(ui.ctx(), |ui| {
        //             if ui.button("Compute Queries").clicked() {
        //                 *smells_result = Some(fetch_results(ui.ctx(), api_addr, conf, &examples));
        //             }
        //         });
        // }
        actions |= Action::Compute;
        return actions;
    }
    if let Some(conf) = &mut smells.commits {
        if smells_diffs_result.is_none() {
            ui.label(format!("{:?}", conf));
            ui.spinner();
            *smells_diffs_result = Some(fetch_examples_at_commits(ui.ctx(), api_addr, conf));
        }
        // TODO display when multiple possible choices
        // egui::Window::new("Actions").show(ui.ctx(), |ui| {
        //     if ui.button("Show diffs").clicked() {
        //         *trigger_compute = true;
        //     }
        // });
        return actions;
    }
    actions
}

#[cfg(not(feature = "force_layout"))]
pub(crate) fn show_smells_graph_config(
    ui: &mut egui::Ui,
    smells: &mut Config,
    result: &Result<Resource<Result<SearchResults, SmellsError>>, String>,
    gid: u16,
) {
    ui.label("enable force_layout feature")
}

#[cfg(feature = "force_layout")]
type GraphTy = egui_addon::force_layout::PrettyGraph<Payload, ()>;

#[cfg(feature = "force_layout")]
pub(crate) fn show_smells_graph_config(
    ui: &mut egui::Ui,
    smells: &mut Config,
    smells_result: Option<&mut RemoteResult>,
    gid: u16,
) {
    use re_ui::UiExt;

    let gid = gid as usize;
    if smells.graph_view_settings.len() <= gid {
        smells
            .graph_view_settings
            .resize_with(gid + 1, || Default::default());
    }
    let graph_view_settings = &mut smells.graph_view_settings[gid];

    let selected = &mut graph_view_settings.fit_to_screen_enabled;
    ui.toggle_value(selected, "fit to screen");
    let view_mode = &mut graph_view_settings.view_mode;
    ui.selectable_toggle(|ui| {
        ui.selectable_value(view_mode, ViewMode::StatisticsOnly, "Stats");
        ui.selectable_value(view_mode, ViewMode::ExampleTopsBiGraph, "Ex-Tops");
        ui.selectable_value(view_mode, ViewMode::FullGraph, "Full");
    });
    ui.label("node limit:");
    if ui
        .add(
            egui::Slider::new(&mut graph_view_settings.limit, 0..=4000)
                .integer()
                .update_while_editing(false)
                .clamping(egui::SliderClamping::Never),
        )
        .changed()
    {
        #[cfg(feature = "force_layout")]
        let id = force_graph_id(gid, &graph_view_settings.view_mode);
        #[cfg(feature = "force_layout")]
        egui_addon::force_layout::reset(ui, Some(id));
        #[cfg(feature = "force_layout")]
        smells.prepared_graphs.clear();
    };
    ui.label("pretty nodes:");
    ui.add(
        egui::Slider::new(&mut graph_view_settings.pretty_nodes, 0..=1000)
            .integer()
            .clamping(egui::SliderClamping::Never),
    );

    let _id = force_graph_id(gid, &graph_view_settings.view_mode);

    let mut s = egui_addon::force_layout::get_anime_state(ui, Some(_id.to_string()));

    egui_addon::force_layout::show_center_gravity_params(ui, &mut s.extras.0.params);
    egui_addon::force_layout::show_fruchterman_reingold_params(ui, &mut s.base);
    egui_addon::force_layout::show_pinning_params(ui, &mut s.extras.1.0.params);

    egui_addon::force_layout::set_layout_state(ui, s, Some(_id.to_string()));

    let g = smells_result
        .and_then(|x| x.ready())
        .and_then(|x| x.as_ref().ok())
        .and_then(|x| x.content.as_ref())
        .and_then(|x| x.as_ref().ok());

    ui.label("too general:");
    ui.group(|ui| {
        let g = g.and_then(|g| g.graphs.get(gid));
        for (_i, id) in graph_view_settings.too_general.iter().enumerate() {
            if ui
                .button(format!("{}", id))
                .on_hover_ui(|ui| {
                    if let Some(g) = g {
                        let pretty = &g.queries_pretty[*id as usize];
                        ui.label(pretty);
                    }
                })
                .clicked()
            {}
            ui.separator();
        }
    });
    ui.label("queued prettifications:");
    ui.horizontal_wrapped(|ui| {
        let g = g.and_then(|g| g.graphs.get(gid));
        let mut to_add = vec![];
        for (i, id) in graph_view_settings.pretty_queued.iter().enumerate() {
            if ui
                .button(format!("{}", id))
                .on_hover_ui(|ui| {
                    if let Some(g) = g {
                        let pretty = &g.queries_pretty[*id as usize];
                        ui.label(pretty);
                    }
                })
                .clicked()
            {
                to_add.push(i);
            }
            ui.separator();
        }
        if let Some(i) = to_add.pop() {
            let x = graph_view_settings.pretty_queued.remove(i);
            let content = g.unwrap();
            if let Some(_pg) = &mut smells.prepared_graphs[gid] {
                let (_mode, pg) = _pg.downcast_mut::<(ViewMode, GraphTy)>().unwrap();
                handle_new_tops(content, pg, vec![x.into()], 4);
            }
        }
    });
    ui.label("selected nodes:");
    ui.group(|ui| {
        // TODO could move expensive computations to selection detection
        let mut to_deselect = vec![];
        let mut s = egui_addon::force_layout::get_anime_state(ui, Some(_id.to_string()));
        let node_pinning = &mut s.extras.1.0.params;
        // let mut node_pinning_iter = s.extras.1.0.params.iter_pinning();
        let mut has_toggled_pin = false;
        // egui_addon::force_layout::pin_node(node_pinning, 0);
        for (i, &n) in graph_view_settings.selected_nodes.iter().enumerate() {
            // let _pinned = node_pinning_iter.is_pinned();
            let _pinned = node_pinning.is_pinned(n);
            let pinned = &mut _pinned.clone();
            show_pattern_details(
                smells.prepared_graphs.get_mut(gid).and_then(|x| x.as_mut()),
                g.and_then(|g| g.graphs.get(gid)),
                ui,
                &mut to_deselect,
                &mut graph_view_settings.pretty_queued,
                &mut graph_view_settings.too_general,
                i,
                n,
                pinned,
            );
            ui.separator();
            if _pinned != *pinned {
                has_toggled_pin = true;
                node_pinning.pin_node(n);
                // node_pinning_iter.toggle_node();
            }
            // node_pinning_iter.next_node();
        }
        if has_toggled_pin {
            egui_addon::force_layout::set_layout_state(ui, s, Some(_id.to_string()));
        }
        if to_deselect.len() > 0 {
            if to_deselect.len() > 1 {
                to_deselect.sort();
                to_deselect.reverse(); // avoids shifting this way
            }
            for i in to_deselect {
                graph_view_settings.selected_nodes.remove(i);
            }
        }
    });
}

#[cfg(feature = "force_layout")]
fn force_graph_id(gid: usize, view_mode: &ViewMode) -> String {
    let view_mode = match view_mode {
        ViewMode::StatisticsOnly => "stats",
        ViewMode::FullGraph => "full",
        ViewMode::ExampleTopsBiGraph => "bi",
    };
    let id = format!("force_graph_patterns_{}_{}", gid, view_mode);
    id
}

#[cfg(feature = "force_layout")]
fn show_pattern_details(
    prepared_graph: Option<&mut Box<dyn std::any::Any + Send + Sync>>,
    raw_graph: Option<&G>,
    ui: &mut egui::Ui,
    to_deselect: &mut Vec<usize>,
    pretty_queued: &mut Vec<u32>,
    too_general: &mut Vec<u32>,
    i: usize,
    n: u32,
    pinned: &mut bool,
) {
    if let Some(_pg) = prepared_graph {
        let (mode, pg) = _pg.downcast_ref::<(ViewMode, GraphTy)>().unwrap();
        let Some(nn) = pg.node(n.into()).map(|x| x.id()) else {
            return;
        };
        ui.label(format!("pattern {n} {}", nn.index()));

        use egui_addon::force_layout::petgraph;
        ui.horizontal_wrapped(|ui| {
            let Some(w) = pg.g().node_weight(n.into()) else {
                return;
            };
            let payload = w.payload();
            ui.label("matches: 42").on_hover_text(
                "number of matches found for this pattern when searching given commit",
            );
            ui.label(format!("inits: {}", payload.inits))
                .on_hover_text("number of initial generated patterns leading to this one");
            ui.label("tops: 42")
                .on_hover_text("number of top generalized patterns reachable from this one");
        });
        let neib_count = |dir| pg.g().neighbors_directed(n.into(), dir).count();
        let specialized = neib_count(petgraph::Outgoing);
        let generalized = neib_count(petgraph::Incoming);
        ui.label(format!("{generalized}-->|pattern {n}|-->{specialized}",))
            .on_hover_text("<number of generalized patterns>-->selected pattern--><number of specialized pattern>");
        // let pinned = pg.node(n.into()).map_or(false, |x| x.display().pinned());
        let pin_label = if *pinned { "unpin" } else { "pin" };
        let [general, ignore, pin] = ui
            .horizontal_wrapped(|ui| ["too general", "ignore", pin_label].map(|txt| ui.button(txt)))
            .inner;
        if general.clicked() {
            let (mode, pg) = _pg.downcast_mut::<(ViewMode, GraphTy)>().unwrap();
            log::info!("pattern {n} deemed too general");
            if let Some(rg) = raw_graph {
                to_deselect.push(i);
                too_general.push(i as u32);
                let content = rg;
                handle_top_too_general(pg, n as usize, |g, ids| {
                    let remaining = handle_new_tops(content, g, ids, 8);
                    pretty_queued.extend(remaining);
                });
            }
        }
        if ignore.clicked() {
            log::info!("ignore pattern {n} and descendants only having it as top");
        }
        if pin.clicked() {
            *pinned = !*pinned;
            // let pg = _pg.downcast_mut::<GraphTy>().unwrap();
            // if let Some(n) = pg.node_mut(n.into()) {
            // if pinned {
            //     n.display_mut().unpin();
            // } else {
            //     n.display_mut().pin();
            // }
            // }
            log::info!("pin/unpin pattern {n}");
        }
    } else {
        ui.label(format!("pattern {n}"));
    }
    if let Some(rg) = raw_graph {
        if let Some(pretty) = &rg.queries_pretty.get(n as usize) {
            ui.label(*pretty);
        }
    }
}

fn handle_new_tops(
    content: &G,
    g: &mut egui_addon::force_layout::PrettyGraph<Payload>,
    mut ids: Vec<egui_addon::force_layout::petgraph::prelude::NodeIndex>,
    max: usize,
) -> Vec<u32> {
    ids.sort_by_key(|id| g.node_mut(*id).unwrap().payload_mut().inits.len());
    let mut _new_tops = ids.into_iter().rev();
    let new_tops: Vec<_> = ((0..max).filter_map(|_| _new_tops.next()))
        .map(|id| {
            let payload = g.node_mut(id).unwrap().payload_mut();
            let pretty = content.pretty_or_id(id.index());
            payload.pretty = pretty;
            id
        })
        .collect();
    use egui_addon::force_layout::petgraph;
    for x in new_tops {
        // now update the display to reflect new payload
        use egui_addon::force_layout::DisplayNode;
        let props = g.node_mut(x).unwrap().props().clone();
        DisplayNode::<_, (), petgraph::Directed, u32>::update(
            g.node_mut(x).unwrap().display_mut(),
            &props,
        );
    }
    _new_tops.map(|x| x.index() as u32).collect()
}

#[cfg(not(feature = "force_layout"))]
pub(crate) fn show_smells_graph(
    ui: &mut egui::Ui,
    api_addr: &str,
    smells: &mut Config,
    queries: &SearchResults,
    gid: u16,
) {
    ui.label("enable force_layout feature")
}

#[cfg(feature = "force_layout")]
pub(crate) fn show_smells_graph(
    ui: &mut egui::Ui,
    _api_addr: &str,
    smells: &mut Config,
    queries: &SearchResults,
    gid: u16,
) {
    let gid = gid as usize;

    if smells.prepared_graphs.len() <= gid {
        smells.prepared_graphs.resize_with(gid + 1, || None);
    }
    if smells.graph_view_settings.len() <= gid {
        smells
            .graph_view_settings
            .resize_with(gid + 1, || Default::default());
    }
    let graph_view_setting = &mut smells.graph_view_settings[gid];
    match graph_view_setting.view_mode {
        ViewMode::StatisticsOnly => {
            let content = &queries.graphs[gid];
            ui.label(format!("Number of patterns: {}", content.queries.len()));
            ui.label(format!("Number of examples: {}", content.ex_pretty.len()));
        }
        ViewMode::ExampleTopsBiGraph => {
            // let _id = format!("force_bigraph_ex_tops{}", gid);
            let _id = force_graph_id(gid, &graph_view_setting.view_mode);
            let prepared_graph = &mut smells.prepared_graphs[gid];
            let content = &queries.graphs[gid];
            show_bigraph(ui, _id, prepared_graph, content, graph_view_setting);
        }
        ViewMode::FullGraph => {
            let _id = force_graph_id(gid, &graph_view_setting.view_mode);
            let prepared_graph = &mut smells.prepared_graphs[gid];
            let content = &queries.graphs[gid];
            show_full_graph(ui, _id, prepared_graph, content, graph_view_setting);
        }
    }
}
#[cfg(feature = "force_layout")]
fn show_bigraph(
    ui: &mut egui::Ui,
    id: String,
    prepared_graph: &mut Option<Box<dyn std::any::Any + Send + Sync>>,
    content: &G,
    graph_view_setting: &mut GVSetting,
) {
    let g: &mut GraphTy = if let Some((ViewMode::ExampleTopsBiGraph, g)) = prepared_graph
        .as_mut()
        .and_then(|g| g.downcast_mut::<(ViewMode, GraphTy)>())
    {
        g
    } else {
        let _g = build_bigraph(
            content,
            graph_view_setting,
            &[],
            &mut egui_addon::force_layout::simple_pet_graph(),
        );
        // use egui_addon::force_layout::*;
        // let g: GraphTy = to_graph(&_g.into());
        let mut g: GraphTy = GraphTy::new(Default::default());
        *g.g_mut() = _g;
        *prepared_graph = Some(Box::new((ViewMode::ExampleTopsBiGraph, g)));
        &mut (prepared_graph.as_mut().unwrap())
            .downcast_mut::<(ViewMode, GraphTy)>()
            .unwrap()
            .1
    };

    let mut _s = egui_addon::force_layout::GVSettings::default();
    _s.1 = _s.1.with_zoom_and_pan_enabled(true);
    _s.1 =
        _s.1.with_fit_to_screen_enabled(graph_view_setting.fit_to_screen_enabled);
    _s.0 = _s.0.with_node_selection_enabled(true);

    let settings_interaction = &_s.0;
    let settings_navigation = &_s.1;
    let settings_style = &_s.2;

    let events = std::rc::Rc::<std::cell::RefCell<Vec<Event>>>::default();
    use egui_addon::force_layout::Event;
    let mut view = egui_addon::force_layout::AnimatedGraphView::<Payload, _, _, _>::new(g)
        .with_id(Some(id.clone()))
        .with_interactions(settings_interaction)
        .with_navigations(settings_navigation)
        .with_styles(settings_style)
        .with_event_sink(&events);
    ui.add(&mut view);
    for event in events.take() {
        match event {
            Event::NodeSelect(e) => {
                let x = e.id as u32;
                wasm_rs_dbg::dbg!(e.id);
                if !graph_view_setting.selected_nodes.contains(&x) {
                    graph_view_setting.selected_nodes.push(x);
                }
                // if let Some(n) = g.node_mut(x.into()) {
                //     n.display_mut().pin();
                // }
                let mut s = egui_addon::force_layout::get_anime_state(ui, Some(id.to_string()));
                s.extras.1.0.params.pin_node(x);
                egui_addon::force_layout::set_layout_state(ui, s, Some(id.to_string()));
            }
            Event::NodeDeselect(e) => {
                wasm_rs_dbg::dbg!(e.id);
                if let Some(i) = graph_view_setting
                    .selected_nodes
                    .iter()
                    .position(|x| *x == e.id as u32)
                {
                    graph_view_setting.selected_nodes.remove(i);
                }
            }
            Event::NodeDoubleClick(e) => {
                graph_view_setting.too_general.push(e.id as u32);
                let pretty_queued = &mut graph_view_setting.pretty_queued;
                handle_top_too_general(g, e.id, |g, ids| {
                    let remaining = handle_new_tops(content, g, ids, usize::MAX);
                    pretty_queued.extend(remaining);
                });
                // use egui_addon::force_layout::*;
                // egui_graphs::Node::new(payload);
                *g.g_mut() = build_bigraph(
                    content,
                    graph_view_setting,
                    graph_view_setting.too_general.as_ref(),
                    g.g_mut(),
                );
                // let g = build_bigraph(content, graph_view_setting, &[e.id as u32]);
                // let g: GraphTy = to_graph(&g.into());
                // *prepared_graph = Some(Box::new(g));
                // return;
            }
            Event::NodeClick(e) => {
                let x = e.id as u32;
                let mut s = egui_addon::force_layout::get_anime_state(ui, Some(id.to_string()));
                s.extras.1.0.params.pin_node(x);
                egui_addon::force_layout::set_layout_state(ui, s, Some(id.to_string()));
                // if let Some(n) = g.node_mut(x.into()) {
                //     if n.display_mut().pinned() {
                //         n.display_mut().unpin();
                //     }
                // }
            }
            _ => {}
        }
    }
}

#[cfg(feature = "force_layout")]
fn build_bigraph(
    content: &G,
    graph_view_settings: &GVSetting,
    too_general_list: &[u32],
    old_g: &mut egui_addon::force_layout::PrettyGraphInner<Payload>,
) -> egui_addon::force_layout::PrettyGraphInner<Payload> {
    use egui_addon::force_layout::*;
    let mut g: PrettyGraphInner<Payload> = simple_pet_graph();
    // let limit = graph_view_settings.limit;
    let limit = usize::MAX;
    let mut ex_it = ExIt::new(content);
    let succs = content.iter_successors().enumerate().take(limit);
    let mut pretty_count = 0;
    // in reverse topo order, go from bots to tops
    wasm_rs_dbg::dbg!(content.queries_pretty.len());
    wasm_rs_dbg::dbg!(content.queries.len());
    let mut waiting_inits = vec![ExSet(0); content.queries.len().min(limit)];
    let mut ex2init = vec![];
    let mut ex2top: Vec<(u32, petgraph::graph::NodeIndex)> = vec![];
    let mut to_remove = vec![];
    let mut new_top_candidates = vec![];

    for (x, succ) in succs {
        let Some(inits) = ex_it.next_exset() else {
            continue;
        };
        let mut i = x as u32;
        let mut all_too_general = true;
        for &succ in succ {
            i += succ;
            if !too_general_list.contains(&i) {
                all_too_general = false;
            }
            if i as usize >= limit {
                break;
            }
            let i = i as usize;
            // let x = (x as u32).into();
            // simple_add_edge(&mut g, i, x);
            waiting_inits[x] |= inits;
            let curr = waiting_inits[x];
            waiting_inits[i as usize] |= curr;
        }
        if x >= limit {
            break;
        }

        if inits.len() > 0 && succ.is_empty() {
            continue;
        }
        // assert!(
        //     !(inits.len() > 0 && succ.is_empty()),
        //     "need to factor {}, {}",
        //     inits.len(),
        //     succ.len()
        // );
        // // NOTE probably only happens in trivial cases that are not very interesting

        if inits.len() > 0 {
            let s = "".to_string();
            let direct_ex = inits;
            let inits = waiting_inits[x];
            let i = g.add_node(egui_graphs::Node::new(Payload { pretty: s, inits }));
            g.node_weight_mut(i).unwrap().set_id(i);
            if let Some(old_n) = old_g.node_weight(i) {
                g.node_weight_mut(i).unwrap().set_location(old_n.location());
            }
            for _ in 0..direct_ex.len() {
                // wasm_rs_dbg::dbg!(i);
                ex2init.push(i);
            }
            continue;
        }

        // if too_general_list.contains(&(x as u32)) {
        //     continue;
        // }

        if succ.is_empty() {
            let s = if pretty_count < graph_view_settings.pretty_nodes {
                pretty_count += 1;
                content.pretty_or_id(x)
            } else {
                "".to_string()
            };
            let inits = waiting_inits[x];
            let is_too_general = too_general_list.contains(&(x as u32));
            let s = if is_too_general {
                format!("too general\n{s}")
            } else {
                s
            };
            let i = g.add_node(egui_graphs::Node::new(Payload { pretty: s, inits }));
            g.node_weight_mut(i).unwrap().set_id(i);
            if let Some(old_n) = old_g.node_weight(i) {
                g.node_weight_mut(i).unwrap().set_location(old_n.location());
            }
            if is_too_general {
                to_remove.push(i);
            }
            ex2top.extend(inits.iter().map(|ex| (ex, i)));
            continue;
        } else if all_too_general {
            wasm_rs_dbg::dbg!(x);

            let is_too_general = too_general_list.contains(&(x as u32));

            let inits = waiting_inits[x];
            let pretty = "".to_string();
            let i = g.add_node(egui_graphs::Node::new(Payload { pretty, inits }));
            g.node_weight_mut(i).unwrap().set_id(i);
            if let Some(old_n) = old_g.node_weight(i) {
                g.node_weight_mut(i).unwrap().set_location(old_n.location());
            }
            if is_too_general {
                to_remove.push(i);
            } else {
                new_top_candidates.push((x, inits));
            }
            continue;
        }
        let i = g.add_node(egui_graphs::Node::new(Payload {
            pretty: content.pretty_or_id(x),
            inits: waiting_inits[x],
        }));
        g.node_weight_mut(i).unwrap().set_id(i);
        if let Some(old_n) = old_g.node_weight(i) {
            g.node_weight_mut(i).unwrap().set_location(old_n.location());
        }
        if !all_too_general {
            to_remove.push(i);
        }
    }
    // choose the new tops that must be displayed
    // stop when all ex are covered
    // first sort the number of covered ex
    new_top_candidates.sort_by(|a, b| a.1.len().cmp(&b.1.len()));
    let mut new_tops = vec![];
    let mut covered = 0u64;
    if let Some(a) = new_top_candidates.pop() {
        covered |= a.1.0;
        new_tops.push(a);
    }
    while covered.count_ones() != 0 && (covered.count_ones() as usize) < content.ex_query.len() {
        // first try to find the largest complement
        let mut cand = None;
        let mut cand_added_cov = 0;
        for i in (0..new_top_candidates.len()).rev() {
            let cov = new_top_candidates[i].1.0;
            let shared = covered & cov;
            let shared = shared.count_ones();
            if shared > 0 {
                continue;
            }
            let added = !covered & cov;
            let added = added.count_ones();
            if added > cand_added_cov {
                cand = Some(i);
                cand_added_cov = added;
            }
        }
        if let Some(i) = cand {
            let cov = new_top_candidates[i].1.0;
            covered |= cov;
            new_tops.push(new_top_candidates.remove(i));
            continue;
        }
        let mut cand_improvement_cov = 0;
        // otherwise try to find the first one increasing coverage
        for i in (0..new_top_candidates.len()).rev() {
            let cov = new_top_candidates[i].1.0;
            let added = !covered & cov;
            let added = added.count_ones();
            let shared = covered & cov;
            let shared = shared.count_ones();
            let improvement = added as i32 - shared as i32;
            if improvement > cand_improvement_cov {
                cand = Some(i);
                cand_improvement_cov = improvement;
            }
        }
        if let Some(i) = cand {
            let cov = new_top_candidates[i].1.0;
            covered |= cov;
            new_tops.push(new_top_candidates.remove(i));
            continue;
        }
        break;
    }
    for (x, inits) in new_tops {
        let i = x as u32;
        let i = i.into();
        let s = if pretty_count < graph_view_settings.pretty_nodes {
            pretty_count += 1;
            content.pretty_or_id(x)
        } else {
            "".to_string()
        };
        g.node_weight_mut(i).unwrap().payload_mut().pretty = s;
        ex2top.extend(inits.iter().map(|ex| (ex, i)));
    }
    let mut ex_it = ExIt::new(content);
    for i in 0..content.queries.len().min(limit) {
        let Some(v) = ex_it.next_agg(|mut inits: Vec<_>, i| {
            inits.push(i);
            inits
        }) else {
            continue;
        };
        for ex in v {
            let s = content.ex_pretty[ex].to_string();
            let j = g.add_node(egui_graphs::Node::new(Payload {
                pretty: s,
                inits: ExSet(0),
            }));
            g.node_weight_mut(j).unwrap().set_id(j);
            if let Some(old_n) = old_g.node_weight(j) {
                g.node_weight_mut(j).unwrap().set_location(old_n.location());
            }
            if ex >= ex2init.len() {
                continue;
            }
            let i = ex2init[ex];
            // let i = i as u32;
            let id = g.add_edge(i.into(), j, egui_graphs::Edge::new(()));
            g.edge_weight_mut(id).unwrap().set_id(id);
        }
    }
    ex2top.sort();
    // let mut prev_i = usize::MAX;
    for (ex, top) in ex2top {
        let init = ex2init[ex as usize];
        // let j = ex2top[_i];
        // if i.index() == prev_i {
        //     continue;
        // }
        // prev_i = i.index();
        let id = g.add_edge(top, init.into(), egui_graphs::Edge::new(()));
        g.edge_weight_mut(id).unwrap().set_id(id);
    }
    let mut to_remove = to_remove.into_iter().peekable();
    g.filter_map_owned(
        |i, w| {
            if to_remove.peek() == Some(&i) {
                to_remove.next();
                None
            } else {
                Some(w)
            }
        },
        |_e, w| Some(w),
    )
}

#[cfg(feature = "force_layout")]
fn show_full_graph(
    ui: &mut egui::Ui,
    id: String,
    prepared_graph: &mut Option<Box<dyn std::any::Any + Send + Sync>>,
    content: &G,
    graph_view_setting: &mut GVSetting,
) {
    let mut _s = egui_addon::force_layout::GVSettings::default();
    _s.1 = _s.1.with_zoom_and_pan_enabled(true);
    _s.1 =
        _s.1.with_fit_to_screen_enabled(graph_view_setting.fit_to_screen_enabled);
    _s.0 = _s.0.with_node_selection_enabled(true);

    let settings_interaction = &_s.0;
    let settings_navigation = &_s.1;
    let settings_style = &_s.2;

    let g: &mut GraphTy = if let Some((ViewMode::FullGraph, g)) = prepared_graph
        .as_mut()
        .and_then(|g| g.downcast_mut::<(ViewMode, GraphTy)>())
    {
        g
    } else {
        let g = build_graph(content, graph_view_setting);
        use egui_addon::force_layout::*;
        let g: GraphTy = to_graph(&g.into());
        *prepared_graph = Some(Box::new((ViewMode::FullGraph, g)));
        &mut (prepared_graph.as_mut().unwrap())
            .downcast_mut::<(ViewMode, GraphTy)>()
            .unwrap()
            .1
    };

    let events = std::rc::Rc::<std::cell::RefCell<Vec<Event>>>::default();
    use egui_addon::force_layout::Event;
    let mut view = egui_addon::force_layout::AnimatedGraphView::<Payload, _, _, _>::new(g)
        .with_id(Some(id.clone()))
        .with_interactions(settings_interaction)
        .with_navigations(settings_navigation)
        .with_styles(settings_style)
        .with_event_sink(&events);
    ui.add(&mut view);
    for event in events.take() {
        match event {
            // Event::Pan(PayloadPan) => {}
            // Event::Zoom(PayloadZoom) => {}
            // Event::NodeMove(PayloadNodeMove) => {}
            // Event::NodeDragStart(PayloadNodeDragStart) => {}
            // Event::NodeDragEnd(PayloadNodeDragEnd) => {}
            Event::NodeSelect(e) => {
                if !graph_view_setting.selected_nodes.contains(&(e.id as u32)) {
                    graph_view_setting.selected_nodes.push(e.id as u32);
                }
            }
            Event::NodeDeselect(e) => {
                if let Some(i) = graph_view_setting
                    .selected_nodes
                    .iter()
                    .position(|x| *x == e.id as u32)
                {
                    graph_view_setting.selected_nodes.remove(i);
                }
            }
            Event::NodeDoubleClick(e) => {
                wasm_rs_dbg::dbg!(e.id);
                graph_view_setting.too_general.push(e.id as u32);
                let pretty_queued = &mut graph_view_setting.pretty_queued;
                handle_top_too_general(g, e.id, |g, ids| {
                    let remaining = handle_new_tops(content, g, ids, 8);
                    pretty_queued.extend(remaining);
                });
            }
            Event::NodeClick(_) => {}
            Event::NodeHoverEnter(_) => {}
            Event::NodeHoverLeave(_) => {}
            // Event::EdgeClick(PayloadEdgeClick) => {}
            // Event::EdgeSelect(PayloadEdgeSelect) => {}
            // Event::EdgeDeselect(PayloadEdgeDeselect) => {}
            _ => {}
        }
    }
}

#[cfg(feature = "force_layout")]
fn build_graph(
    content: &G,
    graph_view_settings: &GVSetting,
) -> egui_addon::force_layout::StableGraph<Payload, ()> {
    use egui_addon::force_layout::*;
    let mut g = simple_pet_graph();

    // let mut r = petgraph::Graph::<IdN, ()>::with_capacity(
    //     g.queries.len(),
    //     g.succ.iter().filter(|x| x.is_zero()).count() + 1,
    // );

    let mut ex_it = ExIt::new(content);
    for _i in 0..content.queries.len().min(graph_view_settings.limit) {
        simple_add_node(
            &mut g,
            Payload {
                pretty: Default::default(),
                inits: ex_it.next_exset().unwrap(),
            },
        );
    }
    let mut pretty_count = 0;
    // in reverse topo order, go from bots to tops
    for (x, succ) in content
        .iter_successors()
        .enumerate()
        .take(graph_view_settings.limit)
    {
        let mut i = x as u32;
        for &succ in succ {
            i += succ;
            if i as usize >= graph_view_settings.limit {
                break;
            }
            let i = i.into();
            let x = (x as u32).into();
            simple_add_edge(&mut g, i, x);
            let curr = g.node_weight(x).unwrap().inits;
            if let Some(inits) = g.node_weight_mut(i) {
                inits.inits |= curr;
            }
        }
        if x >= graph_view_settings.limit {
            break;
        }
        if succ.is_empty() {
            let s = if pretty_count < graph_view_settings.pretty_nodes {
                pretty_count += 1;
                content.pretty_or_id(x)
            } else {
                ".".to_string()
            };
            g.node_weight_mut((x as u32).into())
                .as_mut()
                .unwrap()
                .pretty = s;
        }
    }
    let mut ex_it = ExIt::new(content);
    for i in 0..content.queries.len().min(graph_view_settings.limit) {
        let Some(v) = ex_it.next_agg(|mut inits: Vec<_>, i| {
            inits.push(i);
            inits
        }) else {
            continue;
        };
        for ex in v {
            let s = content.ex_pretty[ex].to_string();
            let j = simple_add_node(
                &mut g,
                Payload {
                    pretty: s,
                    inits: ExSet(1 << ex),
                },
            );
            let i = i as u32;
            simple_add_edge(&mut g, i.into(), j);
        }
    }

    // in reverse topo order, go from tops to bots
    // for (x, succ) in content.succ.split(|x| *x == 0).enumerate().rev() {}
    g
}

/// when a top is what we want but still too general
///
/// first find the new tops and promote them,
/// then remove the one at id (and its edges).
///
/// NOTE could also fix the node in place or hide/dim it and its edges
#[cfg(feature = "force_layout")]
fn handle_top_too_general<N: Clone + egui_addon::force_layout::FlexPayload>(
    g: &mut egui_addon::force_layout::PrettyGraph<N>,
    id: usize,
    mut handle_new_tops: impl FnMut(
        &mut egui_addon::force_layout::PrettyGraph<N>,
        Vec<egui_addon::force_layout::petgraph::graph::NodeIndex>,
    ),
) {
    // if e.id >= g.node_count() {
    //     log::warn!("trying to remove an out of range node");
    //     return;
    // }
    let idx = (id as u32).into();
    use egui_addon::force_layout::petgraph;
    if g.g().neighbors_directed(idx, petgraph::Incoming).count() > 0 {
        log::warn!("{} is not a top", id);
        return;
    }
    let new_tops = g
        .g()
        .neighbors_directed(idx, petgraph::Outgoing)
        .filter(|t| g.g().neighbors_directed(*t, petgraph::Incoming).count() <= 1)
        .collect::<Vec<_>>();

    handle_new_tops(g, new_tops);

    // finally do the removal
    g.remove_node(idx);
}

pub(crate) fn show_smells_result(
    ui: &mut egui::Ui,
    api_addr: &str,
    smells: &mut Config,
    fetched_files: &mut FetchedFiles,
    queries: &SearchResults,
    actions: &mut Actions,
) -> Result<(), RemoteResult> {
    let center = ui.available_rect_before_wrap().center();
    let action_widget = egui::Window::new("Actions")
        .default_pos(center)
        .pivot(egui::Align2::CENTER_CENTER);

    if init_slider(smells, queries) {
        return Ok(());
    };

    let conf = smells.commits.as_mut().unwrap();
    let examples = smells.diffs.as_mut().unwrap();
    let tot_len = queries.bad.len();
    let predicate = |i: &usize| {
        conf.wanted_matches.contains(&queries.bad[*i].matches)
            || conf.wanted_matches.end == queries.bad[*i].matches
    };
    if smells.bads.is_none() {
        smells.bads = Some((0..tot_len).filter(predicate).collect())
        // smells.bads = Some((0..tot_len).collect())
    }
    if !queries.good.is_empty() {
        todo!("handle the queries matching the fixes")
    }

    let bads = smells.bads.as_ref().unwrap();
    let len = bads.len();
    if tot_len == 0 {
        let mut _smells_result = Ok(());
        action_widget.show(ui.ctx(), |ui| {
            ui.colored_label(ui.visuals().error_fg_color, "No queries found");
            ui.label(format!("{}", queries.graphs.len()));
            ui.label(format!(
                "{:?}",
                (queries.graphs.iter())
                    .map(|x| x.queries.len())
                    .collect::<Vec<_>>()
            ));
            if ui.button("Retry Compute Queries").clicked() {
                _smells_result = Err(fetch_results(ui.ctx(), api_addr, conf, &examples));
            }
        });
        return _smells_result;
    }

    if len == 0 {
        ui.colored_label(
            ui.visuals().error_fg_color,
            "No queries selected, change the hyperparameters",
        );
        ui.colored_label(
            ui.visuals().error_fg_color,
            "The hyperparameters can be found in the settings on the left panel",
        );
        return Ok(());
    }

    let scroll = egui::ScrollArea::vertical()
        .scroll_bar_visibility(egui::scroll_area::ScrollBarVisibility::AlwaysVisible)
        // .id_salt(id_salt) // NOTE does not transmit id to children like push_id
        ;
    ui.push_id(
        (
            "query_with_example",
            42,
            (queries.prepare_time * 1000000.0) as u64,
            (queries.search_time * 1000000.0) as u64,
            queries.bad.len(),
            queries.graphs.len(), // TODO precompute a better salt
        ),
        |ui| {
            show_list_of_queries_with_examples(
                ui,
                scroll,
                api_addr,
                fetched_files,
                queries,
                examples,
                bads,
                len,
            )
        },
    );

    let mut _smells_result = Ok(());
    *actions |= Action::Recompute;
    *actions |= Action::OpenGraph;
    // action_widget.show(ui.ctx(), |ui| {
    //     if ui.button("Compute Queries").clicked() {
    //         _smells_result = Err(fetch_results(ui.ctx(), api_addr, conf, &examples));
    //     }
    //     use super::utils_egui::MyUiExt;
    //     #[cfg(feature = "force_layout")]
    //     ui.grouped_wrapped_list(queries.graphs.iter().map(Some), |ui, i, x| {
    //         let b = ui.button(format!(
    //             "lattice {} q:{} e:{}",
    //             i,
    //             x.queries_pretty.len(),
    //             x.ex_pretty.len()
    //         ));
    //         if b.clicked() {
    //             *pg_id = Some(i as u16);
    //         }
    //     });
    // });
    _smells_result
}

fn init_slider(smells: &mut Config, queries: &SearchResults) -> bool {
    if (smells.bad_matches_bounds) == (0..=0) {
        let conf = smells.commits.as_mut().unwrap();
        let matches = queries.bad.iter().map(|x| x.matches);
        let start = matches.min().unwrap_or_default();
        let matches = queries.bad.iter().map(|x| x.matches);
        let end = matches.max().unwrap_or_default();
        if start < end {
            smells.bad_matches_bounds = std::ops::RangeInclusive::new(start, end);
            conf.wanted_matches = start..end; // TODO better open the side panel
            smells.bads = None; // to refresh
            return true;
        }
    }
    false
}

pub(crate) fn access_smells_results<'a>(
    ui: &mut egui::Ui,
    result: &'a Result<Resource<Result<SearchResults, SmellsError>>, String>,
) -> Option<&'a SearchResults> {
    let Ok(resource) = result else {
        return None;
    };
    let Some(content) = &resource.content else {
        return None;
    };
    if let Err(error) = content {
        egui::Window::new("QueryError2").show(ui.ctx(), |ui| {
            ui.label("Error");
            ui.label(format!("{:?}", error));
        });
        return None;
    };
    content.as_ref().ok()
}

pub(crate) fn prep_smells_results<'a>(
    ui: &mut egui::Ui,
    smells: &mut Config,
    smells_result: &'a mut RemoteResult,
) -> Result<&'a Result<Resource<Result<SearchResults, SmellsError>>, String>, Option<egui::Response>>
{
    let Some(result) = smells_result.ready() else {
        // let center = ui.available_rect_before_wrap().center();
        // egui::Window::new("Actions")
        //     .default_pos(center)
        //     .pivot(egui::Align2::CENTER_CENTER)
        //     .show(ui.ctx(), |ui| {
        //         resp = Some(ui.button("Compute Queries"));
        //         ui.spinner();
        //     });
        smells.bad_matches_bounds = 0..=0;
        return Err(None);
    };
    if let Err(error) = result {
        // This should only happen if the fetch API isn't available or something similar.
        let mut resp = None;
        let center = ui.available_rect_before_wrap().center();
        egui::Window::new("Error")
            .default_pos(center)
            .pivot(egui::Align2::CENTER_CENTER)
            .show(ui.ctx(), |ui| {
                ui.colored_label(
                    ui.visuals().error_fg_color,
                    if error.is_empty() { "Error" } else { error },
                );
                resp = Some(ui.button("Retry Compute Queries"));
            });
        return Err(resp);
    };
    Ok(result)
}

fn show_list_of_queries_with_examples(
    ui: &mut egui::Ui,
    scroll: egui::ScrollArea,
    api_addr: &str,
    fetched_files: &mut FetchedFiles,
    queries: &SearchResults,
    examples: &mut ExamplesValues,
    bads: &Vec<usize>,
    len: usize,
) {
    scroll.show_rows(ui, H, len, |ui, rows| {
        let (mut rect, _) = ui.allocate_exact_size(
            egui::Vec2::new(
                ui.available_width(),
                H, // * (rows.end - rows.start) as f32,
            ),
            egui::Sense::hover(),
        );
        let top = rect.top();
        for i in rows.start..rows.end {
            let mut rect = {
                let (t, b) = rect.split_top_bottom_at_y(top + H * (i - rows.start + 1) as f32);
                rect = b;
                t
            };
            rect.bottom_mut().sub_assign(B);

            let line_pos_1 = egui::emath::GuiRounding::round_to_pixels(
                rect.left_bottom(),
                ui.pixels_per_point(),
            );
            let line_pos_2 = egui::emath::GuiRounding::round_to_pixels(
                rect.right_bottom(),
                ui.pixels_per_point(),
            );
            ui.painter()
                .line_segment([line_pos_1, line_pos_2], ui.visuals().window_stroke());
            rect.bottom_mut().sub_assign(B);
            let mut ui = ui.new_child(
                egui::UiBuilder::new()
                    .max_rect(rect)
                    .layout(egui::Layout::top_down(egui::Align::Min)),
            );
            ui.set_clip_rect(rect.intersect(ui.clip_rect()));
            ui.push_id(ui.id().with(bads[i]), |ui| {
                let bad_query = &queries.bad[bads[i]];
                show_query_with_example(ui, api_addr, bad_query, examples, fetched_files);
            });
        }
    });
}

pub(crate) fn show_examples(
    ui: &mut egui::Ui,
    api_addr: &str,
    examples: &mut ExamplesValues,
    fetched_files: &mut FetchedFiles,
) {
    // let id = ui.id();
    let len = examples.examples.len();
    assert!(len > 0);
    let scroll = egui::ScrollArea::vertical()
        // .id_salt()
        .scroll_bar_visibility(egui::scroll_area::ScrollBarVisibility::AlwaysVisible);
    ui.push_id(
        (
            "show_examples",
            examples.moves.len(),
            examples.examples.len(), // TODO precompute a better salt
        ),
        |ui| {
            scroll.show_rows(ui, H, len, |ui, cols| {
                show_examples_aux(ui, api_addr, examples, fetched_files, cols);
            })
        },
    );
}

fn show_examples_aux(
    ui: &mut egui::Ui,
    api_addr: &str,
    examples: &mut ExamplesValues,
    fetched_files: &mut FetchedFiles,
    cols: Range<usize>,
) {
    let (mut rect, _) = ui.allocate_exact_size(
        egui::Vec2::new(ui.available_width(), H * (cols.end - cols.start) as f32),
        egui::Sense::hover(),
    );
    let top = rect.top();
    for i in cols.clone() {
        let mut rect = {
            let (t, b) = rect.split_top_bottom_at_y(top + H * (i - cols.start + 1) as f32);
            rect = b;
            t
        };
        rect.bottom_mut().sub_assign(B);

        let line_pos_1 =
            egui::emath::GuiRounding::round_to_pixels(rect.left_bottom(), ui.pixels_per_point());
        let line_pos_2 =
            egui::emath::GuiRounding::round_to_pixels(rect.right_bottom(), ui.pixels_per_point());
        ui.painter()
            .line_segment([line_pos_1, line_pos_2], ui.visuals().window_stroke());
        rect.bottom_mut().sub_assign(B);
        let mut ui = ui.new_child(
            egui::UiBuilder::new()
                .max_rect(rect)
                .layout(egui::Layout::top_down(egui::Align::Min)),
        );
        ui.set_clip_rect(rect.intersect(ui.clip_rect()));
        ui.push_id(ui.id().with(i), |ui| {
            let example = &examples.examples[i];
            show_diff(ui, api_addr, example, fetched_files);
        });
    }
}

fn show_query_with_example(
    ui: &mut egui::Ui,
    api_addr: &str,
    bad_query: &SearchResult,
    examples: &mut ExamplesValues,
    fetched_files: &mut FetchedFiles,
) {
    InteractiveSplitter::vertical()
        .ratio(0.3)
        .show(ui, |ui1, ui2| {
            ui1.push_id(
                ui1.id().with("query_bad_smell").with(&bad_query.query),
                |ui| {
                    show_query(bad_query, ui);
                },
            );
            let clip_rect = ui2.clip_rect();
            let bad_ex_cont = &bad_query.examples[..bad_query.examples.len().min(12)];
            if bad_ex_cont.is_empty() {
                ui2.label(format!("{}", bad_query.examples.len()));
                return;
            }
            MultiSplitter::horizontal()
                .ratios(if bad_ex_cont.len() <= 8 {
                    vec![1.0 / bad_ex_cont.len() as f32; bad_ex_cont.len() - 1]
                } else {
                    [0.2, 0.2]
                        .into_iter()
                        .chain(
                            (0..bad_ex_cont.len() - 3)
                                .into_iter()
                                .map(|_| 0.6 / (bad_ex_cont.len() - 2) as f32),
                        )
                        .collect()
                })
                .show(ui2, |uis| {
                    for (i, ui) in uis.iter_mut().enumerate() {
                        if !clip_rect.contains_rect(ui.clip_rect()) {
                            continue;
                        }
                        let example = &examples.examples[bad_ex_cont[i]];
                        ui.push_id(ui1.id().with(i), |ui| {
                            show_diff(ui, api_addr, example, fetched_files)
                        });
                        // ui.label(format!(
                        //     "query[{}] prep={:3} search={:3} matches={}",
                        //     i, content.prepare_time, content.search_time, c.matches
                        // ));
                        // ui.text_edit_multiline(&mut c.query.clone());
                    }
                });
        });
}

pub(crate) fn show_query(
    bad_query: &SearchResult,
    ui: &mut egui::Ui,
) -> egui::scroll_area::ScrollAreaOutput<egui_addon::code_editor::generic_text_edit::TextEditOutput>
{
    let mut code: &str = &bad_query.query;
    let language = "clojure";
    // use super::syntax_highlighting::syntax_highlighting_async as syntax_highlighter;
    // let theme = super::syntax_highlighting::syntect::CodeTheme::from_memory(ui.ctx());
    let theme = egui_extras::syntax_highlighting::CodeTheme::from_memory(ui.ctx(), ui.style());

    let mut layouter = |ui: &egui::Ui, code: &dyn egui::TextBuffer, wrap_width: f32| {
        use egui_extras::syntax_highlighting::highlight;
        let mut layout_job = highlight(ui.ctx(), ui.style(), &theme, &code.as_str(), language);
        // syntax_highlighter::highlight(ui.ctx(), &theme, code, language);
        if false {
            layout_job.wrap.max_width = wrap_width;
        }
        ui.fonts(|f| f.layout_job(layout_job))
    };
    let scroll_resp = egui::scroll_area::ScrollArea::both().show(ui, |ui| {
        egui_addon::code_editor::generic_text_edit::TextEdit::multiline(&mut code)
            .layouter(&mut layouter)
            .desired_width(f32::MAX)
            .show(ui)
    });
    let mut font_id = egui::TextStyle::Heading.resolve(ui.style());
    font_id.size *= 3.0;
    ui.painter().text(
        ui.available_rect_before_wrap().right_top(),
        egui::Align2::RIGHT_BOTTOM,
        bad_query.matches,
        font_id,
        matches_color(ui),
    );
    scroll_resp
}

fn matches_color(ui: &egui::Ui) -> egui::Color32 {
    if ui.visuals().dark_mode {
        egui::Color32::YELLOW
    } else {
        egui::Color32::from_rgb(255, 127, 0)
    }
}

const B: f32 = 15.;
const H: f32 = 800.;

pub(crate) fn show_diff(
    ui: &mut egui::Ui,
    api_addr: &str,
    example: &ExamplesValue,
    fetched_files: &mut FetchedFiles,
) {
    let rect = ui.clip_rect();
    let mov_col = move_color(ui);
    InteractiveSplitter::vertical().show(ui, |ui1, ui2| {
        ui1.set_clip_rect(ui1.max_rect().intersect(rect));
        ui2.set_clip_rect(ui2.max_rect().intersect(rect));
        ui2.push_id(ui2.id().with("second"), |ui| {
            let color = insert_color(ui);
            let ma = MH::<false> {
                main: &example.inserts,
                col: color,
                moves: &example.moves,
                mov_col,
                hash: hash(&example.before.file),
            };
            show_either_side(ui, fetched_files, api_addr, &example.after, color, ma);
            ui.separator();
        });
        ui1.push_id(ui1.id().with("first"), |ui| {
            let color = delete_color(ui);
            let ma = MH::<true> {
                main: &example.deletes,
                col: color,
                moves: &example.moves,
                mov_col,
                hash: hash(&example.before.file),
            };
            show_either_side(ui, fetched_files, api_addr, &example.before, color, ma);
            ui.separator();
        });
    });
}

pub fn hash<T: ?Sized + Hash>(x: &T) -> u64 {
    let mut state = std::hash::DefaultHasher::default();
    x.hash(&mut state);
    use std::hash::Hasher;
    state.finish()
}

fn delete_color(ui: &mut egui::Ui) -> egui::Rgba {
    if ui.visuals().dark_mode {
        egui::Color32::from_rgb(255, 50, 50).gamma_multiply(0.1)
    } else {
        egui::Color32::from_rgb(240, 20, 20).gamma_multiply(0.3)
    }
    .into()
}

fn insert_color(ui: &mut egui::Ui) -> egui::Rgba {
    if ui.visuals().dark_mode {
        egui::Color32::from_rgb(40, 235, 40).gamma_multiply(0.1)
    } else {
        egui::Color32::from_rgb(20, 235, 20).gamma_multiply(0.3)
    }
    .into()
}

fn move_color(ui: &mut egui::Ui) -> egui::Rgba {
    if ui.visuals().dark_mode {
        egui::Color32::from_rgb(50, 50, 255).gamma_multiply(0.4)
    } else {
        egui::Color32::BLUE.gamma_multiply(0.1)
    }
    .into()
}

#[derive(Clone, Copy)]
struct MH<'a, const LEFT: bool> {
    main: &'a Vec<Range<usize>>,
    col: egui::Rgba,
    moves: &'a Vec<(Range<usize>, Range<usize>)>,
    mov_col: egui::Rgba,
    hash: u64,
}

impl<'a, const LEFT: bool> std::hash::Hash for MH<'a, LEFT> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        LEFT.hash(state);
        self.hash.hash(state);
    }
}

impl<'a, const LEFT: bool> MakeHighlights for MH<'a, LEFT> {
    const COLORS: u8 = 2;
    fn highlights(&self, col: u8) -> (egui::Rgba, impl Iterator<Item = (usize, usize)>) {
        let main: &[Range<usize>];
        let moves: &[(Range<usize>, Range<usize>)];
        let color;
        if col == 0 {
            main = &self.main;
            moves = &[];
            color = self.col;
        } else if col == 1 {
            moves = &self.moves;
            main = &[];
            color = self.mov_col;
        } else {
            unreachable!()
        };
        let main = main.iter().map(|x| (x.start, x.end));
        let moves = moves
            .iter()
            .map(|x| if LEFT { &x.0 } else { &x.1 })
            .map(|x| (x.start, x.end));
        (color, main.chain(moves))
    }
}

fn show_either_side<MH: MakeHighlights>(
    ui: &mut egui::Ui,
    fetched_files: &mut FetchedFiles,
    api_addr: &str,
    code: &CodeRange,
    color: egui::Rgba,
    highlights: MH,
) {
    let file_result = fetched_files.entry(code.file.clone());
    let id_scroll = ui.id().with("off_scrolled");
    let r = try_fetch_remote_file(&file_result, |file| {
        let mut content: &str = &file.content;
        let language = "java";
        use egui::text::LayoutJob;
        use egui_addon::syntax_highlighting::syntect::CodeTheme;
        let theme = CodeTheme::from_memory(ui.ctx());
        let mut layouter = |ui: &egui::Ui, content: &dyn egui::TextBuffer, _wrap_width: f32| {
            type HighlightCache =
                egui::util::cache::FrameCache<LayoutJob, crate::app::utils_edition::Highlighter0>;
            let layout_job = ui.ctx().memory_mut(|mem| {
                mem.caches.cache::<HighlightCache>().get((
                    &theme,
                    crate::app::utils_edition::FileContainer(&code.file, content.as_str()),
                    language,
                ))
            });

            ui.fonts(|f| {
                let galley = f.layout_job(layout_job);
                galley.into()
            })
        };
        let noop = ui.painter().add(egui::Shape::Noop);
        let scroll = egui::scroll_area::ScrollArea::both().show(ui, |ui| {
            egui_addon::code_editor::generic_text_edit::TextEdit::multiline(&mut content)
                .layouter(&mut layouter)
                .desired_width(f32::MAX)
                .show(ui)
        });
        (scroll, noop)
    });
    if r.is_none() {
        if let std::collections::hash_map::Entry::Vacant(_) = file_result {
            file_result.insert_entry(super::code_tracking::remote_fetch_file(
                ui.ctx(),
                &api_addr,
                &code.file.commit,
                &mut code.file.file_path.clone(),
            ));
        }
    }
    let te = match r {
        Some(Ok(r)) => Some(r),
        None => None,
        Some(Err(error)) => {
            ui.colored_label(
                ui.visuals().error_fg_color,
                if error.is_empty() { "Error" } else { &error },
            );
            None
        }
    };

    if let Some((mut aa, noop)) = te {
        if false {
            // NOTE too slow, need to hop on the galley generation or cache the rectangles
            // for (color, start, end) in highlights {
            //     let ui = &mut ui.child_ui(aa.inner_rect, *ui.layout());
            //     {
            //         let rect = aa.inner.galley.rows[0].rect;
            //         let rect = rect.translate(aa.inner.text_draw_pos.to_vec2());
            //         let stroke = egui::Stroke::new(2., egui::Color32::KHAKI);
            //         ui.painter()
            //             .rect(rect, 1., egui::Color32::KHAKI.linear_multiply(0.1), stroke);
            //     }
            //     ui.set_clip_rect(aa.inner_rect);
            //     egui_addon::egui_utils::highlight_byte_range_aux(
            //         ui,
            //         &aa.inner.galley,
            //         aa.inner.text_draw_pos,
            //         &Range { start, end },
            //         color,
            //     );
            // }
        } else {
            let galley = &aa.inner.galley;
            #[derive(Clone, Copy)]
            struct G<'a>(&'a std::sync::Arc<egui::Galley>);
            impl<'a> std::hash::Hash for G<'a> {
                fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                    self.0.hash(state);
                }
            }
            impl<'a> AsRef<std::sync::Arc<egui::Galley>> for G<'a> {
                fn as_ref(&self) -> &std::sync::Arc<egui::Galley> {
                    self.0
                }
            }

            type Type = egui::util::cache::FrameCache<
                Vec<(egui::Color32, Vec<egui::Rect>)>,
                crate::app::utils_edition::HiHighlighter2,
            >;
            let shape = ui
                .ctx()
                .memory_mut(|mem| mem.caches.cache::<Type>().get((G(galley), highlights)));
            let offset = aa.inner.galley_pos.to_vec2();
            let clip_rect = ui.clip_rect().translate(-offset);
            let shapes = shape
                .into_iter()
                .flat_map(|(color, rects)| {
                    rects.into_iter().filter_map(move |rect| {
                        rect.intersects(clip_rect).then(|| {
                            let mut shape = egui::Shape::rect_filled(rect, 1.0, color);
                            shape.translate(offset);
                            shape
                        })
                    })
                })
                .collect();
            ui.painter().set(noop, egui::Shape::Vec(shapes));
        }
        if let Some(selected_node) = &code.range {
            let ui = &mut ui.new_child(egui::UiBuilder::new().max_rect(aa.inner_rect));
            ui.set_clip_rect(aa.inner_rect.intersect(ui.clip_rect()));
            let mut rect = egui_addon::egui_utils::highlight_byte_range_aux(
                ui,
                &aa.inner.galley,
                aa.inner.galley_pos,
                selected_node,
                color.multiply(0.01).into(),
            );

            let first_there = ui.ctx().data_mut(|d| {
                let r = d.get_temp_mut_or_default::<bool>(id_scroll);
                let tmp = *r;
                *r = true;
                tmp
            });
            if !first_there {
                aa.state.offset.y =
                    rect.top() - (aa.inner_rect.height() - rect.height()).abs() / 2.0;
                aa.state.store(ui.ctx(), aa.id);
            }
            rect = rect.translate(aa.inner.galley_pos.to_vec2());

            let stroke = {
                let mut color = color;
                if ui.visuals().dark_mode {
                    color[0] = color[0] + 0.2;
                    color[1] = color[1] + 0.2;
                    color[2] = color[2] + 0.2;
                    color[3] = color[3] * 2.0;
                } else {
                    color = (egui::Rgba::from(color) * 10.0).into();
                };
                egui::Stroke::new(3., color)
            };
            ui.painter().rect(
                rect,
                1.,
                color.multiply(0.1),
                stroke,
                egui::StrokeKind::Inside,
            );
        }
    };
}

pub(super) fn fetch_results(
    ctx: &egui::Context,
    api_addr: &str,
    smells: &mut ComputeConfigQuery,
    examples: &ExamplesValues,
) -> RemoteResult {
    let ctx = ctx.clone();
    let (sender, promise) = poll_promise::Promise::new();
    let url = format!(
        "http://{}/smells/github/{}/{}/{}/{}",
        api_addr,
        &smells.commit.repo.user,
        &smells.commit.repo.name,
        &smells.commit.id,
        &smells.len,
    );

    #[derive(serde::Deserialize, serde::Serialize, Debug, Clone)]
    pub struct ExamplesValues<S, T, U> {
        meta_gen: S,
        meta_simp: S,
        examples: T,
        moves: U,
        simple_matching: bool,
        prepro_matching: bool,
    }
    let examples = ExamplesValues {
        meta_gen: &smells.meta_gen,
        meta_simp: &smells.meta_simp,
        examples: examples
            .examples
            .iter()
            .map(|x| ExamplesValue {
                before: x.before.clone(),
                after: x.after.clone(),
                inserts: vec![],
                deletes: vec![],
                moves: vec![],
            })
            .collect::<Vec<_>>(),
        moves: (),
        simple_matching: smells.simple_matching,
        prepro_matching: smells.prepro_matching,
    };
    let body = serde_json::to_vec(&examples).unwrap();
    let mut request = ehttp::Request::post(&url, body);
    request.headers.insert(
        "Content-Type".to_string(),
        "application/json; charset=utf-8".to_string(),
    );

    ehttp::fetch(request, move |response| {
        ctx.request_repaint(); // will wake up UI thread
        let resource = response.and_then(|response| {
            Resource::<Result<SearchResults, SmellsError>>::from_response(&ctx, response)
        });
        sender.send(resource);
    });
    promise
}

impl Resource<Result<SearchResults, SmellsError>> {
    pub(super) fn from_response(
        _ctx: &egui::Context,
        response: ehttp::Response,
    ) -> Result<Self, String> {
        let content_type = response.content_type().unwrap_or_default();

        if response.status == 404 {
            let Some(text) = response.text() else {
                dbg!();
                return Err("".to_string());
            };
            return Err(text.to_string());
        }
        if !content_type.starts_with("application/json") {
            let Some(text) = response.text() else {
                return Err(format!("Wrong content type: {}", content_type));
            };
            return Err(format!(
                "Wrong content type: {}\n{}",
                content_type,
                &text[..100.min(text.len())]
            ));
        }
        if response.status != 200 {
            let Some(text) = response.text() else {
                dbg!();
                return Err("".to_string());
            };
            let Ok(json) = serde_json::from_str::<SmellsError>(text) else {
                dbg!();
                return Err("".to_string());
            };
            return Ok(Self {
                response,
                content: Some(Err(json)),
            });
        }

        let text = response.text();
        let text = text.and_then(|text| {
            serde_json::from_str(text)
                .inspect_err(|err| {
                    dbg!(&err);
                })
                .ok()
        });

        Ok(Self {
            response,
            content: text.map(|x| Ok(x)),
        })
    }
}

pub(super) fn fetch_examples_at_commits(
    ctx: &egui::Context,
    api_addr: &str,
    smells: &mut ComputeConfigQuery,
) -> RemoteResultDiffs {
    let ctx = ctx.clone();
    let (sender, promise) = poll_promise::Promise::new();
    let url = format!(
        "http://{}/smells_ex_from_diffs/github/{}/{}/{}/{}",
        api_addr,
        &smells.commit.repo.user,
        &smells.commit.repo.name,
        &smells.commit.id,
        &smells.len,
    );

    let mut request = ehttp::Request::post(&url, serde_json::to_vec(&[""]).unwrap());
    request.headers.insert(
        "Content-Type".to_string(),
        "application/json; charset=utf-8".to_string(),
    );

    ehttp::fetch(request, move |response| {
        ctx.request_repaint(); // will wake up UI thread
        let resource = response.and_then(|response| {
            Resource::<Result<ExamplesValues, DiffsError>>::from_response(&ctx, response)
        });
        sender.send(resource);
    });
    promise
}

impl Resource<Result<ExamplesValues, DiffsError>> {
    pub(super) fn from_response(
        _ctx: &egui::Context,
        response: ehttp::Response,
    ) -> Result<Self, String> {
        let content_type = response.content_type().unwrap_or_default();

        if response.status == 404 {
            let Some(text) = response.text() else {
                dbg!();
                return Err("".to_string());
            };
            return Err(text.to_string());
        }
        if !content_type.starts_with("application/json") {
            return Err(format!("Wrong content type: {}", content_type));
        }
        if response.status != 200 {
            let Some(text) = response.text() else {
                dbg!();
                return Err("".to_string());
            };
            let Ok(json) = serde_json::from_str::<DiffsError>(text) else {
                dbg!();
                return Err("".to_string());
            };
            return Ok(Self {
                response,
                content: Some(Err(json)),
            });
        }

        let text = response.text();
        let text = text.and_then(|text| {
            serde_json::from_str(text)
                .inspect_err(|err| {
                    dbg!(&err);
                })
                .ok()
        });

        Ok(Self {
            response,
            content: text.map(|x| Ok(x)),
        })
    }
}
