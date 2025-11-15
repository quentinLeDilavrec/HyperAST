use poll_promise::Promise;
use std::collections::HashSet;
use std::fmt::Debug;
use std::sync::Arc;

use super::tree_view::FetchedViewImpl;
use super::tree_view::store::FetchedHyperAST;
use super::tree_view::{Action, LabelIdentifier, NodeIdentifier, PrefillCache, SimplePacked};
use super::types::{Commit, ComputeConfigAspectViews, Repo, SelectedConfig};
use super::utils_egui::MyUiExt as _;
use crate::utils_poll::Resource;

pub(crate) const WANTED: SelectedConfig = SelectedConfig::Aspects;

pub(crate) fn show_config(
    ui: &mut egui::Ui,
    aspects: &mut ComputeConfigAspectViews,
) -> (egui::Response, egui::Response, egui::Response) {
    let (resp_repo, resp_commit) = aspects.commit.show_clickable(ui);

    let resp_path = egui::TextEdit::singleline(&mut aspects.path)
        .id(ui.id().with("path"))
        .clip_text(true)
        .desired_width(150.0)
        .desired_rows(1)
        .hint_text("path")
        .interactive(true)
        .show(ui)
        .response;

    egui::TextEdit::singleline(&mut aspects.hightlight)
        .id(ui.id().with("hightlight"))
        .clip_text(true)
        .desired_width(150.0)
        .desired_rows(1)
        .hint_text("hightlight")
        .interactive(true)
        .show(ui);

    ui.checkbox(&mut aspects.spacing, "Spacing");
    ui.checkbox(&mut aspects.syntax, "Syntax");
    ui.checkbox(&mut aspects.cst, "CST");
    ui.add_enabled_ui(false, |ui| {
        ui.checkbox(&mut aspects.ast, "AST");
        ui.wip(Some(" soon available"));
        ui.checkbox(&mut aspects.type_decls, "Type Decls");
        ui.wip(Some(" soon available"));
        ui.checkbox(&mut aspects.licence, "Licence");
        ui.wip(Some(" soon available"));
        ui.checkbox(&mut aspects.doc, "Doc");
        ui.wip(Some(" soon available"));
    });
    ui.label("serialized Java:");
    let mut rm = None;
    for x in &*aspects.ser_opt_java {
        let button = &ui.button(x.to_str());
        if button.clicked() {
            rm = Some(x.clone());
        }
    }
    if let Some(rm) = rm {
        aspects.ser_opt_java.remove(&rm);
    }
    ui.label("serialized Cpp:");
    let mut rm = None;
    for x in &*aspects.ser_opt_cpp {
        let button = &ui.button(x.to_str());
        if button.clicked() {
            rm = Some(x.clone());
        }
    }
    if let Some(rm) = rm {
        aspects.ser_opt_cpp.remove(&rm);
    }
    ui.label("hidden Java:");
    let mut rm = None;
    for x in &*aspects.hide_opt_java {
        let button = &ui.button(x.to_str());
        if button.clicked() {
            rm = Some(x.clone());
        }
    }
    if let Some(rm) = rm {
        aspects.hide_opt_java.remove(&rm);
    }
    ui.label("hidden Cpp:");
    let mut rm = None;
    for x in &*aspects.hide_opt_cpp {
        let button = &ui.button(x.to_str());
        if button.clicked() {
            rm = Some(x.clone());
        }
    }
    if let Some(rm) = rm {
        aspects.hide_opt_cpp.remove(&rm);
    }
    (resp_repo, resp_commit, resp_path)
}

#[derive(serde::Deserialize, serde::Serialize)]
pub struct FetchedView {
    #[serde(skip)]
    store: Arc<FetchedHyperAST>,
    #[serde(serialize_with = "ser_node_id", deserialize_with = "de_node_id")]
    pub(crate) root: NodeIdentifier,
    #[serde(skip)]
    /// WARN reset it on changes of state that can affect layout
    prefill_cache: Option<PrefillCache>,
}

impl FetchedView {
    fn new(store: Arc<FetchedHyperAST>, root: NodeIdentifier) -> Self {
        Self {
            store,
            root,
            prefill_cache: Default::default(),
        }
    }
}

type FetchedViewProm = Promise<Result<Resource<FetchedView>, String>>;

fn ser_node_id<S>(id: &NodeIdentifier, s: S) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    s.serialize_u32(id.to_u32())
}

fn de_node_id<'de, D>(d: D) -> Result<NodeIdentifier, D::Error>
where
    D: serde::Deserializer<'de>,
{
    use serde::de;
    use std::fmt;
    struct Visitor;
    impl<'de> de::Visitor<'de> for Visitor {
        type Value = NodeIdentifier;

        fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
            formatter.write_str("an integer between -2^31 and 2^31")
        }

        fn visit_u64<E>(self, v: u64) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            Ok(NodeIdentifier::from_u32((v as u32).try_into().unwrap()))
        }
        fn visit_i64<E>(self, v: i64) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            Ok(NodeIdentifier::from_u32((v as u32).try_into().unwrap()))
        }
    }
    d.deserialize_u64(Visitor)
}

pub(crate) fn show(
    aspects_result: &mut FetchedViewProm,
    ui: &mut egui::Ui,
    api_addr: &str,
    aspects: &mut ComputeConfigAspectViews,
) {
    let Some(aspects_result) = aspects_result.ready_mut() else {
        return;
    };
    if let Err(err) = aspects_result {
        wasm_rs_dbg::dbg!(err);
    }
    let Ok(aspects_result) = aspects_result else {
        return;
    };
    let ui = &mut ui.new_child(egui::UiBuilder::new().max_rect(ui.available_rect_before_wrap()));
    let add_content = |ui: &mut egui::Ui, _viewport| {
        ui.set_height(3_000.0);
        // ui.set_clip_rect(ui.ctx().screen_rect());
        let Some(fetched_view) = &mut aspects_result.content else {
            return;
        };
        let _hightlight = (aspects.hightlight.split("/"))
            .filter_map(|x| x.parse().ok())
            .collect::<Vec<usize>>();
        let action = fetched_view.show(
            ui,
            api_addr,
            aspects,
            None,
            vec![], //(&hightlight, &egui::Color32::RED, &mut None)
            None,
            None,
            &aspects.path,
        );
        aspects.on_action(action);
    };
    let _scroll = egui::ScrollArea::both()
        .auto_shrink([false, false])
        .show_viewport(ui, add_content);
}

pub(crate) fn project_modal_handler(
    data: &mut super::AppData,
    pid: super::ProjectId,
) -> super::ProjectId {
    let projects = &mut data.selected_code_data;
    let commit = Some(&data.aspects.commit);
    use super::utils_commit::project_modal_handler;
    let (repo, mut commits) = match project_modal_handler(pid, projects, commit) {
        Ok(value) => value,
        Err(value) => return value,
    };
    data.aspects.commit.repo = repo.clone();
    data.aspects.commit.id = commits.iter_mut().next().copied().unwrap();
    data.aspects_result = None;
    super::ProjectId::INVALID
}

impl ComputeConfigAspectViews {
    pub(crate) fn on_action(&mut self, action: Action) {
        match action {
            Action::SerializeKind(k) => {
                self.ser_opt_cpp.toggle(&*k);
                self.ser_opt_java.toggle(&*k);
            }
            Action::HideKind(k) => {
                self.hide_opt_cpp.toggle(&*k);
                self.hide_opt_java.toggle(&*k);
            }
            _ => (),
        }
    }
}

pub(crate) struct HighLightHandle<'a> {
    pub path: &'a [usize],
    /// primary key
    pub color: &'a egui::Color32,
    /// secondary key
    pub id: usize,
    /// return value by reference
    pub screen_pos: &'a mut Option<egui::Rect>,
}

pub(crate) struct Focus<'a> {
    pub offsets: &'a [usize],
    pub ids: &'a [NodeIdentifier],
}

impl Debug for Focus<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Focus {{ {:?} {:?} }}", self.offsets, self.ids)
    }
}

impl<'a> Focus<'a> {
    pub fn new(offsets: &'a [usize], ids: &'a [NodeIdentifier]) -> Self {
        if offsets.len() < ids.len() {
            panic!("offsets.len() < ids.len(): {} {}", offsets.len(), ids.len());
        }
        Focus { offsets, ids }
    }
}

// impl<'a> From<(&'a [usize], &'a [NodeIdentifier])> for Focus<'a> {
//     fn from((offsets, ids): (&'a [usize], &'a [NodeIdentifier])) -> Self {
//         assert!(offsets.is_empty() > ids.is_empty());
//         Focus { offsets, ids }
//     }
// }

impl Focus<'_> {
    pub fn is_empty(&self) -> bool {
        if self.offsets.is_empty() {
            assert!(self.ids.is_empty());
        }
        self.offsets.is_empty()
    }
}

impl FetchedView {
    pub(crate) fn show(
        &mut self,
        ui: &mut egui::Ui,
        api_addr: &str,
        aspects: &ComputeConfigAspectViews,
        focus: Option<Focus<'_>>,
        hightlights: Vec<HighLightHandle<'_>>,
        additions: Option<&[u32]>,
        deletions: Option<&[u32]>,
        path: &str,
    ) -> Action {
        let take = self.prefill_cache.take();
        // ui.allocate_space((h, ui.available_size().x).into());
        let path = path.split("/").filter_map(|x| x.parse().ok()).collect();
        let mut imp = FetchedViewImpl::new(
            self.store.clone(),
            aspects,
            take,
            hightlights,
            focus,
            path,
            ui.id(),
            additions,
            deletions,
        );
        let r = imp.show(ui, api_addr, &self.root);
        self.prefill_cache = imp.prefill_cache;
        r
    }
}

pub(super) type RemoteView = crate::utils_poll::Remote<FetchedView>;

#[allow(unused)]
pub(super) fn remote_fetch_tree(
    ctx: &egui::Context,
    api_addr: &str,
    commit: &Commit,
    path: &str,
) -> FetchedViewProm {
    let url = format!(
        "http://{}/view/github/{}/{}/{}/{}",
        api_addr, &commit.repo.user, &commit.repo.name, &commit.id, &path,
    );
    let request = ehttp::Request::get(&url);
    fetch(ctx.clone(), request, Resource::<FetchedView>::from_resp)
}

pub(super) fn remote_fetch_node_old(
    ctx: &egui::Context,
    api_addr: &str,
    store: Arc<FetchedHyperAST>,
    commit: &Commit,
    path: &str,
) -> FetchedViewProm {
    let url = format!(
        "http://{}/fetch/github/{}/{}/{}/{}",
        api_addr, &commit.repo.user, &commit.repo.name, &commit.id, &path,
    );
    let store = store.clone();
    let request = ehttp::Request::get(&url);
    fetch(ctx.clone(), request, move |response| {
        let res = Resource::<FetchedNode>::from_resp(response);
        res.map(move |fetched_node: FetchedNode| {
            let root = single_fetched_node(&store, fetched_node);
            FetchedView::new(store, root)
        })
    })
}

pub(super) fn remote_fetch_node(
    ctx: &egui::Context,
    api_addr: &str,
    store: Arc<FetchedHyperAST>,
    commit: &Commit,
    path: &str,
) -> Promise<Result<NodeIdentifier, String>> {
    let url = format!(
        "http://{}/fetch/github/{}/{}/{}/{}",
        api_addr, &commit.repo.user, &commit.repo.name, &commit.id, &path,
    );

    let request = ehttp::Request::get(&url);
    fetch(ctx.clone(), request, move |response| {
        let res = Resource::<FetchedNode>::from_resp(response);
        let fetched_node = res.content.unwrap();
        single_fetched_node(&store, fetched_node)
    })
}

fn single_fetched_node(store: &Arc<FetchedHyperAST>, fetched_node: FetchedNode) -> NodeIdentifier {
    let mut node_store = store.node_store.write().unwrap();
    node_store.extend(fetched_node.node_store);
    fetched_node.root[0]
}

#[allow(unused)]
pub(super) fn remote_fetch_nodes_by_ids(
    ctx: &egui::Context,
    api_addr: &str,
    store: Arc<FetchedHyperAST>,
    repo: &Repo,
    ids: HashSet<NodeIdentifier>,
) -> Promise<Result<Resource<()>, String>> {
    let mut url = format!("http://{}/fetch-ids", api_addr,);
    // TODO group ids by arch
    for id in ids {
        url.push('/');
        let id = id.to_u32();
        url += &id.to_string();
    }
    let request = ehttp::Request::get(&url);
    let store = store.clone();
    fetch(ctx.clone(), request, move |response| {
        store.nodes_pending.lock().unwrap().pop_front();
        let res = Resource::<FetchedNodes>::from_resp(response);
        let mut node_store = store.node_store.write().unwrap();
        let mut raw = res.content.unwrap().node_store;
        // Hack
        for x in &mut raw.storages_variants {
            x.remove_if(|id| node_store.contains(*id));
        }
        node_store.extend(raw);
        Resource {
            response: res.response,
            content: Some(()),
        }
    })
}

#[allow(unused)]
pub(super) fn remote_fetch_labels(
    ctx: &egui::Context,
    api_addr: &str,
    store: Arc<FetchedHyperAST>,
    repo: &Repo,
    ids: HashSet<LabelIdentifier>,
) -> Promise<Result<Resource<()>, String>> {
    let mut url = format!("http://{}/fetch-labels", api_addr,);
    for id in ids {
        url.push('/');
        let id: u32 = id.into();
        url += &id.to_string();
    }
    let request = ehttp::Request::get(&url);
    let store = store.clone();
    fetch(ctx.clone(), request, move |response| {
        // TODO look at the behavior of this pop
        store.labels_pending.lock().unwrap().pop_front();
        let res = Resource::<FetchedLabels>::from_resp(response);
        res.map(|fetched_labels| {
            let mut hash_map = store.label_store.write().unwrap();
            let label_ids = fetched_labels.label_ids.into_iter();
            for (k, v) in label_ids.zip(fetched_labels.labels) {
                hash_map.insert(k, v);
            }
        })
    })
}

pub fn fetch<T: 'static + Send>(
    ctx: egui::Context,
    request: ehttp::Request,
    on_ok: impl 'static + Send + FnOnce(ehttp::Response) -> T,
) -> Promise<Result<T, String>> {
    let (sender, promise) = Promise::new();
    ehttp::fetch(request, move |response| {
        ctx.request_repaint(); // wake up UI thread
        let resource = response.map(on_ok);
        sender.send(resource);
    });
    promise
}

#[derive(serde::Deserialize)]
pub struct FetchedNodes {
    node_store: SimplePacked<String>,
}
#[derive(serde::Deserialize)]
pub struct FetchedNode {
    root: Vec<NodeIdentifier>,
    node_store: SimplePacked<String>,
}

#[derive(serde::Deserialize, Clone, Debug)]
pub struct FetchedLabels {
    label_ids: Vec<LabelIdentifier>,
    labels: Vec<String>,
}
