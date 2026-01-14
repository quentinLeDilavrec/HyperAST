use egui_addon::{InteractiveSplitter, code_editor::EditorInfo};
use poll_promise::Promise;
use std::sync::{Arc, Mutex};

use crate::utils_poll::Resource;

use super::Sharing;
use super::code_editor_automerge::CodeEditor;
use super::types::{Commit, Config, SelectedConfig, TsgEditor};
use super::types::{EditorHolder, WithDesc};
use super::utils_edition::show_shared_code_edition;
use super::utils_edition::{EditStatus, EditingContext};
use super::utils_edition::{show_available_remote_docs, show_locals_and_interact};
use super::utils_edition::{show_interactions, update_shared_editors};
use super::utils_results_batched::show_long_result;
use super::utils_results_batched::{ComputeError, ComputeResults, ComputeResultsProm};

mod example_queries;

use example_queries::EXAMPLES;

const INFO_QUERY: EditorInfo<&'static str> = EditorInfo {
    title: "Graph Extractor",
    short: "the extractor",
    long: concat!("follows the tree-sitter-graph DSL"),
};

const INFO_DESCRIPTION: EditorInfo<&'static str> = EditorInfo {
    title: "Desc",
    short: "describes what this query should match",
    long: concat!(
        "TODO syntax is similar to markdown.\n",
        "WIP rendering the markdown, there is already an egui helper for that."
    ),
};

pub(crate) const WANTED: SelectedConfig = SelectedConfig::Tsg;

pub(crate) fn show_config(
    ui: &mut egui::Ui,
    single: &mut Sharing<ComputeConfigQuery>,
) -> (egui::Response, egui::Response) {
    let (resp_repo, resp_commit) = single.content.commit.show_clickable(ui);

    ui.add(
        egui::Slider::new(&mut single.content.len, 1..=1)
            .text("commits")
            .clamping(egui::SliderClamping::Never)
            .integer()
            .logarithmic(true),
    );

    let selected = &mut single.content.config;
    selected.show_combo_box(ui, "Repo Config");

    ui.label("path:");
    egui::TextEdit::singleline(&mut single.content.path)
        .clip_text(true)
        .id_salt("path")
        // .desired_width(150.0)
        .desired_rows(1)
        .hint_text("path")
        .interactive(true)
        .show(ui);

    ui.add_space(4.0);
    ui.label("selected attribute:");
    ui.add_enabled(
        cfg!(feature = "force_layout"),
        egui::TextEdit::singleline(&mut single.content.selected_attr)
            .clip_text(true)
            .id_salt("selected attribute")
            // .desired_width(150.0)
            .desired_rows(1)
            .hint_text("select attribute")
            .interactive(true),
    )
    .on_disabled_hover_text("enable force_layout feature");

    #[cfg(feature = "force_layout")]
    {
        let _id = "TSG force_graph";
        let mut s = egui_addon::force_layout::get_anime_state(ui, Some(_id.to_string()));

        egui_addon::force_layout::show_center_gravity_params(ui, &mut s.extras.0.params);
        egui_addon::force_layout::show_fruchterman_reingold_params(ui, &mut s.base);

        egui_addon::force_layout::set_layout_state(ui, s, Some(_id.to_string()));
    }

    (resp_repo, resp_commit)
}

impl<C> From<&example_queries::Query> for TsgEditor<C>
where
    C: From<(EditorInfo<String>, String)> + egui_addon::code_editor::CodeHolder,
{
    fn from(value: &example_queries::Query) -> Self {
        let mut description: C = (INFO_DESCRIPTION.into(), value.description.into()).into();
        description.set_lang("md");
        Self {
            description, // TODO config with markdown, not js
            query: (INFO_QUERY.into(), value.query.into()).into(),
        }
    }
}

impl<C> Default for TsgEditor<C>
where
    C: From<(EditorInfo<String>, String)> + egui_addon::code_editor::CodeHolder,
{
    fn default() -> Self {
        (&example_queries::EXAMPLES[0].query).into()
    }
}

impl<T> WithDesc<T> for TsgEditor<T> {
    fn desc(&self) -> &T {
        &self.description
    }
}

impl<T> EditorHolder for TsgEditor<T> {
    type Item = T;

    fn iter_editors_mut(&mut self) -> impl Iterator<Item = &mut Self::Item> {
        [&mut self.description, &mut self.query].into_iter()
    }
}

impl<T> TsgEditor<T> {
    pub(crate) fn to_shared<U>(self) -> TsgEditor<U>
    where
        T: Into<U>,
    {
        TsgEditor {
            description: self.description.into(),
            query: self.query.into(),
        }
    }
}

impl Into<TsgEditor<CodeEditor>> for TsgEditor {
    fn into(self) -> TsgEditor<CodeEditor> {
        self.to_shared()
    }
}

pub(super) type TsgContext = EditingContext<TsgEditor, TsgEditor<CodeEditor>>;

#[derive(serde::Deserialize, serde::Serialize)]
#[serde(default)]
pub(super) struct ComputeConfigQuery {
    pub(crate) commit: Commit,
    config: Config,
    len: usize,
    path: String,
    pub selected_attr: String,
}

impl Default for ComputeConfigQuery {
    fn default() -> Self {
        Self {
            commit: From::from(&example_queries::EXAMPLES[0].commit),
            config: example_queries::EXAMPLES[0].config,
            // commit: "4acedc53a13a727be3640fe234f7e261d2609d58".into(),
            len: example_queries::EXAMPLES[0].commits,
            path: example_queries::EXAMPLES[0].path.to_string(),
            selected_attr: Default::default(),
        }
    }
}

pub(crate) fn project_modal_handler(
    data: &mut super::AppData,
    pid: super::ProjectId,
) -> super::ProjectId {
    let projects = &mut data.selected_code_data;
    let commit = Some(&data.tsg.content.commit);
    use crate::app::utils_commit::project_modal_handler;
    let (repo, mut commits) = match project_modal_handler(pid, projects, commit) {
        Ok(value) => value,
        Err(value) => return value,
    };
    let commit = &mut data.tsg.content.commit;
    commit.repo = repo.clone();
    commit.id = *commits.iter_mut().next().unwrap();
    super::ProjectId::INVALID
}

pub(crate) fn commit_modal_handler(data: &mut super::AppData, cid: super::types::CommitId) {
    let commit = &mut data.tsg.content.commit;
    commit.id = cid;
}

type QueryingContext = EditingContext<TsgEditor, TsgEditor<CodeEditor>>;

pub(super) fn remote_compute_query(
    ctx: &egui::Context,
    api_addr: &str,
    single: &mut Sharing<ComputeConfigQuery>,
    query_editors: &mut QueryingContext,
) -> ComputeResultsProm<QueryingError> {
    // TODO multi requests from client
    // if single.len > 1 {
    //     let parents = fetch_commit_parents(&ctx, &single.commit, single.len);
    // }
    let ctx = ctx.clone();
    let (sender, promise) = Promise::new();
    let url = format!(
        "http://{}/tsg/github/{}/{}/{}",
        api_addr,
        &single.content.commit.repo.user,
        &single.content.commit.repo.name,
        &single.content.commit.id,
    );
    #[derive(serde::Serialize)]
    struct QueryContent {
        language: String,
        query: String,
        commits: usize,
        path: String,
    }
    let language = match single.content.config {
        Config::Any => "",
        Config::MavenJava => "Java",
        Config::MakeCpp => "Cpp",
    }
    .to_string();
    let script = match &mut query_editors.current {
        EditStatus::Shared(_, shared_script) | EditStatus::Sharing(shared_script) => {
            let code_editors = shared_script.lock().unwrap();
            QueryContent {
                language,
                query: code_editors.query.code().to_string(),
                commits: single.content.len,
                path: single.content.path.clone(),
            }
        }
        EditStatus::Local { name: _, content } | EditStatus::Example { i: _, content } => {
            QueryContent {
                language,
                query: content.query.code().to_string(),
                commits: single.content.len,
                path: single.content.path.clone(),
            }
        }
    };

    let mut request = ehttp::Request::post(&url, serde_json::to_vec(&script).unwrap());
    request.headers.insert(
        "Content-Type".to_string(),
        "application/json; charset=utf-8".to_string(),
    );

    ehttp::fetch(request, move |response| {
        ctx.request_repaint(); // wake up UI thread
        let resource = response.and_then(|response| {
            Resource::<Result<ComputeResults, QueryingError>>::from_response(&ctx, response)
        });
        sender.send(resource);
    });
    promise
}

#[derive(Debug, serde::Deserialize, serde::Serialize)]
pub enum QueryingError {
    MissingLanguage(String),
    TsgParsing(String),
}

pub(super) fn show_querying(
    ui: &mut egui::Ui,
    api_addr: &str,
    query: &mut Sharing<ComputeConfigQuery>,
    query_editors: &mut QueryingContext,
    trigger_compute: &mut bool,
    querying_result: &mut Option<ComputeResultsProm<QueryingError>>,
) {
    let api_endpoint = &format!("{}/sharing-tsg", api_addr);
    update_shared_editors(ui, query, api_endpoint, query_editors);
    let is_portrait = ui.available_rect_before_wrap().aspect_ratio() < 1.0;
    if is_portrait {
        egui::ScrollArea::vertical().show(ui, |ui| {
            show_scripts_edition(ui, api_endpoint, query_editors, query);
            handle_interactions(ui, query_editors, querying_result, query, trigger_compute);
            show_long_result(&*querying_result, ui);

            let selected_attr = &query.content.selected_attr;
            show_result_graph(querying_result, ui, selected_attr);
        });
    } else {
        InteractiveSplitter::vertical()
            .ratio(0.5)
            .show(ui, |ui1, ui2| {
                ui1.push_id(ui1.id().with("input"), |ui| {
                    show_scripts_edition(ui, api_endpoint, query_editors, query);
                });
                let ui = ui2;
                handle_interactions(ui, query_editors, querying_result, query, trigger_compute);
                show_long_result(&*querying_result, ui);

                let selected_attr = &query.content.selected_attr;
                show_result_graph(querying_result, ui, selected_attr);
            });
    }
}

#[cfg(not(feature = "force_layout"))]
pub(crate) fn show_result_graph(
    querying_result: &mut Option<ComputeResultsProm<QueryingError>>,
    ui: &mut egui::Ui,
    selected_attr: &str,
) {
    ui.add_enabled_ui(false, |ui| {
        egui::CollapsingHeader::new("Results (Graph)")
            .default_open(false)
            .show(ui, |ui| {
                // TODO add a screenshot
            })
    })
    .on_disabled_hover_text("enable force_layout feature")
}

#[cfg(feature = "force_layout")]
pub(crate) fn show_result_graph(
    querying_result: &mut Option<ComputeResultsProm<QueryingError>>,
    ui: &mut egui::Ui,
    selected_attr: &str,
) {
    pub(super) type GraphTy = egui_addon::force_layout::PrettyGraph<String, ()>;

    use crate::app::utils_results_batched::prep_compute_res_prom_mut;
    let Some(content) = prep_compute_res_prom_mut(querying_result, ui) else {
        return;
    };
    type Ty = GraphTy;

    let _id = "TSG force_graph";

    let Some(content) = content.as_mut().ok() else {
        ui.label("no data to show yet");
        ui.label("42");
        return;
    };

    let g: &mut Ty = if let Some(g) = content.graph.as_mut() {
        g.downcast_mut::<Ty>().unwrap()
    } else {
        use egui_addon::force_layout::*;
        let mut g = simple_pet_graph();
        if let Some(content) = content.results.first().and_then(|x| x.as_ref().ok()) {
            let content = &content.inner.result;

            let content = content.as_array().unwrap();

            let mut node_map = hyperast::compat::HashMap::new();
            let mut e_vec = vec![];
            for v in content.iter().take(500) {
                let v = v.as_object().unwrap();
                let id = v.get("id").unwrap().as_number().unwrap();
                let attrs = v.get("attrs").unwrap().as_object().unwrap();
                let n1 = if let Some(attr) = attrs.get(selected_attr) {
                    let attr = attr.as_object().unwrap();
                    if attr.get("type").and_then(|x| x.as_str()) == Some("string") {
                        let attr = attr.get("string").unwrap();
                        simple_add_node(&mut g, attr.as_str().unwrap().to_string())
                    } else {
                        let s = attr.get("type").unwrap();
                        let s = s.as_str().unwrap();
                        simple_add_node(&mut g, s.to_string())
                    }
                } else {
                    simple_add_node(&mut g, id.to_string())
                };
                let id = id.as_u64().unwrap();
                node_map.insert(id, n1);

                let edges = v.get("edges").unwrap().as_array().unwrap();
                for e in edges {
                    let _attrs = e.get("attrs").unwrap().as_object().unwrap();
                    let sink = e.get("sink").unwrap().as_number().unwrap();
                    e_vec.push((id, sink.as_u64().unwrap()));
                }
            }

            for (source, sink) in &e_vec {
                let Some(n1) = node_map.get(source) else {
                    continue;
                };
                let Some(n2) = node_map.get(sink) else {
                    continue;
                };
                simple_add_edge(&mut g, *n1, *n2);
            }
        }
        let graph: Ty = to_graph(&g.into());
        content.graph = Some(Box::new(graph));
        (content.graph.as_mut().unwrap())
            .downcast_mut::<Ty>()
            .unwrap()
    };
    egui::CollapsingHeader::new("Results (Graph)")
        .default_open(true)
        .show(ui, |ui| {
            let mut view = egui_addon::force_layout::AnimatedGraphView::<String, _, _, _>::new(g)
                .with_id(Some(_id.to_string()))
                // .with_interactions(settings_interaction)
                // .with_navigations(settings_navigation)
                // .with_styles(settings_style)
            ;

            ui.add(&mut view);
        });
}

fn handle_interactions(
    ui: &mut egui::Ui,
    code_editors: &mut QueryingContext,
    querying_result: &mut Option<ComputeResultsProm<QueryingError>>,
    single: &mut Sharing<ComputeConfigQuery>,
    trigger_compute: &mut bool,
) {
    let interaction = show_interactions(ui, code_editors, &single.doc_db, querying_result, |i| {
        EXAMPLES[i].name.to_string()
    });
    if interaction.share_button.map_or(false, |x| x.clicked()) {
        let (name, content) = interaction.editor.unwrap();
        let content = content.clone().to_shared();
        let content = Arc::new(Mutex::new(content));
        let name = name.to_string();
        code_editors.current = EditStatus::Sharing(content.clone());
        let mut content = content.lock().unwrap();
        let db = &mut single.doc_db.as_mut().unwrap();
        db.create_doc_attempt(&single.rt, name, &mut *content);
    } else if interaction.save_button.map_or(false, |x| x.clicked()) {
        let (name, content) = interaction.editor.unwrap();
        log::warn!("saving query: {:#?}", content.clone());
        let name = name.to_string();
        let content = content.clone();
        code_editors
            .local_scripts
            .insert(name.to_string(), content.clone());
        code_editors.current = EditStatus::Local { name, content };
    } else if interaction.compute_button.clicked() {
        *trigger_compute |= true;
    }
}

fn show_scripts_edition(
    ui: &mut egui::Ui,
    api_endpoint: &str,
    querying_context: &mut QueryingContext,
    single: &mut Sharing<ComputeConfigQuery>,
) {
    egui::warn_if_debug_build(ui);
    egui::CollapsingHeader::new("Examples")
        .default_open(true)
        .show(ui, |ui| {
            show_examples(ui, &mut single.content, querying_context)
        });
    if !querying_context.local_scripts.is_empty() {
        egui::CollapsingHeader::new("Local Queries")
            .default_open(true)
            .show(ui, |ui| {
                show_locals_and_interact(ui, querying_context, single);
            });
    }
    show_available_remote_docs(ui, api_endpoint, single, querying_context);
    let local = querying_context
        .when_local(|code_editors| code_editors.iter_editors_mut().for_each(|c| c.ui(ui)));
    let shared = querying_context
        .when_shared(|query_editors| show_shared_code_edition(ui, query_editors, single));
    assert!(local.or(shared).is_some());
}

fn show_examples(
    ui: &mut egui::Ui,
    single: &mut ComputeConfigQuery,
    querying_context: &mut QueryingContext,
) {
    ui.horizontal_wrapped(|ui| {
        let mut j = 0;
        for ex in EXAMPLES {
            let mut text = egui::RichText::new(ex.name);
            if let EditStatus::Example { i, .. } = &querying_context.current {
                if &j == i {
                    text = text.strong();
                }
            }
            let button = &ui.button(text);
            if button.clicked() {
                single.commit = (&ex.commit).into();
                single.config = ex.config;
                single.len = ex.commits;
                single.path = ex.path.to_string();
                querying_context.current = EditStatus::Example {
                    i: j,
                    content: (&ex.query).into(),
                };
            }
            if button.hovered() {
                egui::Tooltip::always_open(
                    ui.ctx().clone(),
                    ui.layer_id(),
                    button.id.with("tooltip"),
                    button,
                )
                .show(|ui| {
                    let desc = ex.query.description;
                    egui_demo_lib::easy_mark::easy_mark(ui, desc);
                });
            }
            j += 1;
        }
    });
}

impl Resource<Result<ComputeResults, QueryingError>> {
    pub(super) fn from_response(
        _ctx: &egui::Context,
        response: ehttp::Response,
    ) -> Result<Self, String> {
        let content_type = response.content_type().unwrap_or_default();
        if !content_type.starts_with("application/json") {
            return Err(format!("Wrong content type: {}", content_type));
        }
        // let image = if content_type.starts_with("image/") {
        //     RetainedImage::from_image_bytes(&response.url, &response.bytes).ok()
        // } else {
        //     None
        // };
        if response.status != 200 {
            let Some(text) = response.text() else {
                wasm_rs_dbg::dbg!();
                return Err("".to_string());
            };
            let Ok(json) = serde_json::from_str::<QueryingError>(text) else {
                wasm_rs_dbg::dbg!();
                return Err("".to_string());
            };
            return Ok(Self {
                response,
                content: Some(Err(json)),
            });
        }

        let text = response.text();
        // let colored_text = text.and_then(|text| syntax_highlighting(ctx, &response, text));
        let text = text.and_then(|text| {
            serde_json::from_str(text)
                .inspect_err(|err| {
                    wasm_rs_dbg::dbg!(&err);
                })
                .ok()
        });

        Ok(Self {
            response,
            content: text.map(|x| Ok(x)),
        })
    }
}

impl ComputeError for QueryingError {
    fn head(&self) -> &str {
        match self {
            QueryingError::MissingLanguage(_) => "Missing Language:",
            QueryingError::TsgParsing(_) => "Error Parsing Query:",
        }
    }

    fn content(&self) -> &str {
        match self {
            QueryingError::MissingLanguage(err) => err,
            QueryingError::TsgParsing(err) => err,
        }
    }
}
