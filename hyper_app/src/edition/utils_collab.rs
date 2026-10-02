use automerge::sync::{Message, SyncDoc};
use futures_util::SinkExt;
use std::ops::DerefMut;
use std::sync::{Arc, Mutex};

type SharedCodeEditors<T> = std::sync::Arc<std::sync::Mutex<T>>;

use crate::edition::utils_edition::show_interactions;
use crate::utils_results_batched::{ComputeError, ComputeResultsProm};

use super::EditStatus;
use super::EditingContext;
use super::InteractionResp;
use super::Sharing;
use super::code_editor_automerge;
use super::crdt_over_ws;

// TODO allow to change user name and generate a random default
#[cfg(target_arch = "wasm32")]
const USER: &str = "web";
#[cfg(not(target_arch = "wasm32"))]
const USER: &str = "native";

pub(crate) fn show_available_remote_docs<T, L, S: std::default::Default>(
    ui: &mut egui::Ui,
    api_endpoint: &str,
    single: &mut Sharing<T>,
    context: &mut EditingContext<L, S>,
) {
    if let Some(doc_db) = &single.doc_db {
        let names: Vec<_> = doc_db
            .data
            .read()
            .unwrap()
            .1
            .iter()
            .filter_map(|d| d.as_ref())
            .map(|x| (format!("{}/{}", x.owner, x.name), x.id))
            .collect();
        if !names.is_empty() {
            egui::CollapsingHeader::new("Shared Scripts")
                .default_open(true)
                .show(ui, |ui| {
                    show_shared(ui, api_endpoint, single, context, names)
                });
        }
    }
}

pub(crate) fn show_shared<T, L, S: std::default::Default>(
    ui: &mut egui::Ui,
    api_endpoint: &str,
    single: &mut Sharing<T>,
    context: &mut EditingContext<L, S>,
    names: Vec<(String, usize)>,
) {
    let mut r = None;
    ui.horizontal_wrapped(|ui| {
        for (name, i) in names.iter() {
            let mut text = egui::RichText::new(name);
            if let EditStatus::Shared(j, _) = &context.current {
                if j == i {
                    text = text.strong();
                }
            }
            if ui.button(text).clicked() {
                r = Some(i);
            }
        }
    });
    if let Some(i) = r {
        context.current = EditStatus::Shared(*i, Default::default());
        let doc_db = single.doc_db.as_ref().unwrap();
        let doc_views = doc_db.data.write().unwrap();
        let id = doc_views.1.get(*i).unwrap().as_ref().unwrap().id;
        if let Some(ws) = &mut single.ws {
            let ctx = ui.ctx().clone();
            let rt = single.rt.clone();
            let url = format!("ws://{}/shared/{}", api_endpoint, id);
            *ws = crdt_over_ws::WsDoc::new(&rt, USER.to_string(), ctx, url)
        }
    }
}

impl<L, S> EditStatus<L, S> {
    fn content_mut(&mut self) -> Option<&mut L> {
        match self {
            EditStatus::Local { content, .. } => Some(content),
            EditStatus::Example { content, .. } => Some(content),
            _ => None,
        }
    }
}

pub(crate) fn locals_and_interact_menu<T, L, S, U>(
    context: &mut EditingContext<L, S>,
    docs: &mut Sharing<T>,
    (button, content, name): (egui::Response, L, String),
) where
    U: AsRef<str>,
    L: Clone + crate::types::WithDesc<U> + Into<S>,
    S: autosurgeon::Reconcile,
{
    button.context_menu(|ui| {
        if ui.button("share").clicked() {
            let content = content.into();
            let content = Arc::new(Mutex::new(content));
            context.current = EditStatus::Shared(usize::MAX, content.clone());
            let mut content = content.lock().unwrap();
            docs.doc_db
                .as_mut()
                .unwrap()
                .create_doc_attempt(&docs.rt, name, content.deref_mut());
        }
        if ui.button("close menu").clicked() {
            ui.close()
        }
    });
}

impl<L, S> EditingContext<L, S> {
    pub(crate) fn when_shared<R>(
        &mut self,
        mut g: impl FnMut(&mut std::sync::Arc<std::sync::Mutex<S>>) -> R,
    ) -> Option<R> {
        match &mut self.current {
            EditStatus::Shared(_, shared) | EditStatus::Sharing(shared) => Some(g(shared)),
            _ => None,
        }
    }
}

pub(crate) fn show_shared_code_edition<T, U>(
    ui: &mut egui::Ui,
    query_editors: &mut SharedCodeEditors<T>,
    single: &mut Sharing<U>,
) where
    T: autosurgeon::Reconcile,
    T: crate::types::EditorHolder<Item = code_editor_automerge::CodeEditor>,
    // for<'a> &'a mut T: IntoIterator<Item = &'a mut code_editor_automerge::CodeEditor>,
{
    let resps: Vec<Option<egui::Response>> = {
        let mut ce = query_editors.lock().unwrap();
        // ce.into_iter().map(|c| c.ui(ui)).collect()
        ce.iter_editors_mut().map(|c| c.ui(ui)).collect()
    };

    let Some(ws) = &mut single.ws else {
        return;
    };
    if resps.iter().filter_map(|x| x.as_ref()).any(|x| x.changed()) {
        let timer = if ws.timer != 0.0 {
            let dt = ui.input(|mem| mem.unstable_dt);
            ws.timer + dt
        } else {
            0.01
        };
        let rt = &single.rt;
        timed_updater(ui, timer, ws, query_editors, rt);
    } else if ws.timer != 0.0 {
        let dt = ui.input(|mem| mem.unstable_dt);
        let timer = ws.timer + dt;
        let rt = &single.rt;
        timed_updater(ui, timer, ws, query_editors, rt);
    }
}

fn timed_updater<T: autosurgeon::Reconcile>(
    ui: &mut egui::Ui,
    timer: f32,
    ws: &mut crdt_over_ws::WsDoc,
    code_editors: &mut SharedCodeEditors<T>, // QueryEditor<code_editor_automerge::CodeEditor>
    rt: &crdt_over_ws::Rt,
) {
    const TIMER: u64 = 1;
    if timer < std::time::Duration::from_secs(TIMER).as_secs_f32() {
        ws.timer = timer;
        ui.ctx()
            .request_repaint_after(std::time::Duration::from_secs_f32(TIMER as f32));
    } else {
        ws.timer = 0.0;
        let quote: &mut T = &mut code_editors.lock().unwrap();
        ws.changed(rt, quote);
    }
}

pub(crate) async fn update_handler<T: autosurgeon::Hydrate>(
    mut receiver: futures_util::stream::SplitStream<tokio_tungstenite_wasm::WebSocketStream>,
    mut sender: futures::channel::mpsc::Sender<tokio_tungstenite_wasm::Message>,
    doc: std::sync::Arc<std::sync::RwLock<crdt_over_ws::DocSharingState>>,
    ctx: egui::Context,
    rt: crdt_over_ws::Rt,
    code_editors: SharedCodeEditors<T>,
) {
    use futures_util::StreamExt;

    #[derive(serde::Deserialize, serde::Serialize, Debug, Clone)]
    enum DbMsgToServer {
        Create { name: String },
        User { name: String },
    }
    let owner = USER.to_string();
    sender
        .send(tokio_tungstenite_wasm::Message::Text(
            serde_json::to_string(&DbMsgToServer::User { name: owner }).unwrap(),
        ))
        .await
        .unwrap();
    match receiver.next().await {
        Some(Ok(tokio_tungstenite_wasm::Message::Binary(bin))) => {
            let (doc, sync_state): &mut (_, _) = &mut doc.write().unwrap();
            let message = Message::decode(&bin).unwrap();
            doc.sync()
                .receive_sync_message(sync_state, message)
                .unwrap();
            wasm_rs_dbg::dbg!(&doc);
            if let Ok(t) = autosurgeon::hydrate(&*doc) {
                let mut text = code_editors.lock().unwrap();
                *text = t;
            }
            ctx.request_repaint();
        }
        _ => (),
    }
    while let Some(Ok(msg)) = receiver.next().await {
        use tokio_tungstenite_wasm::Message as Msg;
        match msg {
            Msg::Text(msg) => {
                wasm_rs_dbg::dbg!(&msg);
            }
            Msg::Binary(bin) => {
                wasm_rs_dbg::dbg!();
                let (doc, sync_state): &mut (_, _) = &mut doc.write().unwrap();
                let message = Message::decode(&bin).unwrap();
                // doc.merge(other)
                match doc.sync().receive_sync_message(sync_state, message) {
                    Ok(_) => (),
                    Err(e) => {
                        wasm_rs_dbg::dbg!(e);
                    }
                }
                match autosurgeon::hydrate(doc) {
                    Ok(t) => {
                        let mut text = code_editors.lock().unwrap();
                        *text = t;
                    }
                    Err(e) => {
                        wasm_rs_dbg::dbg!(e);
                    }
                }
                ctx.request_repaint();

                wasm_rs_dbg::dbg!();
                let mut sender = sender.clone();
                if let Some(message) = doc.sync().generate_sync_message(sync_state) {
                    wasm_rs_dbg::dbg!();
                    let message = Msg::Binary(message.encode().to_vec());
                    rt.spawn(async move {
                        sender.send(message).await.unwrap();
                    });
                } else {
                    wasm_rs_dbg::dbg!();
                    let message = Msg::Binary(vec![]);
                    rt.spawn(async move {
                        sender.send(message).await.unwrap();
                    });
                };
            }
            Msg::Close(_) => {
                wasm_rs_dbg::dbg!();
                break;
            }
        }
    }
}

type SparseVecSharedDoc = Vec<Option<crdt_over_ws::SharedDocView>>;
pub(crate) async fn db_update_handler(
    mut sender: futures::channel::mpsc::Sender<tokio_tungstenite_wasm::Message>,
    mut receiver: futures_util::stream::SplitStream<tokio_tungstenite_wasm::WebSocketStream>,
    owner: String,
    ctx: egui::Context,
    data: Arc<std::sync::RwLock<(Option<usize>, SparseVecSharedDoc)>>,
) {
    use futures_util::StreamExt;
    type User = String;

    #[derive(serde::Deserialize, serde::Serialize, Debug, Clone)]
    enum DbMsgToServer {
        Create { name: String },
        User { name: String },
    }
    {
        wasm_rs_dbg::dbg!();
        let name = owner.clone();
        let msg = DbMsgToServer::User { name };
        let msg = serde_json::to_string(&msg).unwrap();
        let msg = tokio_tungstenite_wasm::Message::Text(msg);
        sender.send(msg).await.unwrap();
        wasm_rs_dbg::dbg!();
    }
    while let Some(Ok(msg)) = receiver.next().await {
        wasm_rs_dbg::dbg!();
        match msg {
            tokio_tungstenite_wasm::Message::Text(msg) => {
                wasm_rs_dbg::dbg!(&msg);

                #[derive(serde::Deserialize, serde::Serialize, Debug, Clone)]
                enum DbMsgFromServer {
                    Add(crdt_over_ws::SharedDocView),
                    AddWriter(usize, User),
                    RmWriter(usize, User),
                    // Rename(usize, String),
                    Reset {
                        all: Vec<crdt_over_ws::SharedDocView>,
                    },
                }
                let msg = serde_json::from_str(&msg).unwrap();

                match msg {
                    DbMsgFromServer::Add(x) => {
                        let b = x.owner == owner;
                        let guard = &mut data.write().unwrap();
                        let (waiting, vec) = guard.deref_mut();
                        let id = x.id;
                        vec.resize(id + 1, None);
                        vec[id] = Some(x);
                        if b {
                            *waiting = Some(id);
                        }
                        ctx.request_repaint();
                    }
                    DbMsgFromServer::AddWriter(_, _) => todo!(),
                    DbMsgFromServer::RmWriter(_, _) => todo!(),
                    DbMsgFromServer::Reset { all } => {
                        let guard = &mut data.write().unwrap();
                        let (_, vec) = guard.deref_mut();
                        *vec = vec![];
                        for x in all {
                            let id = x.id;
                            vec.resize(id + 1, None);
                            vec[id] = Some(x);
                        }
                        ctx.request_repaint();
                    }
                }
            }
            tokio_tungstenite_wasm::Message::Binary(_bin) => {
                wasm_rs_dbg::dbg!();
            }
            tokio_tungstenite_wasm::Message::Close(_) => {
                wasm_rs_dbg::dbg!();
                break;
            }
        }
    }
}

pub(crate) fn update_shared_editors<T, L, S: 'static + autosurgeon::Hydrate + std::marker::Send>(
    ui: &mut egui::Ui,
    single: &mut Sharing<T>,
    api_endpoint: &str,
    code_editors: &mut EditingContext<L, S>,
) {
    if let Some(doc_db) = &mut single.doc_db {
        let ctx = ui.ctx().clone();
        let rt = single.rt.clone();
        let owner = USER.to_string();
        let data = doc_db.data.clone();
        if let Err(err) = doc_db.setup_atempt(
            move |sender, receiver| rt.spawn(db_update_handler(sender, receiver, owner, ctx, data)),
            &single.rt,
        ) {
            log::warn!("{}", err);
            if ui.button("try restarting sharing connection").clicked() {
                let url = format!("ws://{}/shared-db", api_endpoint);
                *doc_db = crdt_over_ws::WsDocsDb::new(
                    &single.rt,
                    USER.to_string(),
                    ui.ctx().clone(),
                    url,
                );
            }
        }
        match &mut code_editors.current {
            EditStatus::Sharing(shared_script) => {
                let ctx = ui.ctx().clone();
                let rt = single.rt.clone();
                let db = doc_db;
                let guard = &mut db.data.write().unwrap();
                let (waiting, vec) = guard.deref_mut();
                if let Some(i) = waiting {
                    wasm_rs_dbg::dbg!();
                    let i = *i;
                    if let Some(Some(view)) = vec.get(i) {
                        assert_eq!(view.id, i);
                        let url = format!("ws://{}/shared/{}", api_endpoint, i);
                        single.ws = Some(crdt_over_ws::WsDoc::new(&rt, USER.to_string(), ctx, url));
                    }
                    code_editors.current = EditStatus::Shared(i, shared_script.clone());
                }
            }
            EditStatus::Shared(_, shared_script) => {
                if let Some(ws) = &mut single.ws {
                    wasm_rs_dbg::dbg!();
                    let ctx = ui.ctx().clone();
                    let doc = ws.data.clone();
                    let rt = single.rt.clone();
                    let code_editors = shared_script.clone();
                    if let Err(e) = ws.setup_atempt(
                        |sender, receiver| {
                            single.rt.spawn(update_handler(
                                receiver,
                                sender,
                                doc,
                                ctx,
                                rt,
                                code_editors,
                            ))
                        },
                        &single.rt,
                    ) {
                        log::error!("{}", e);
                    }
                }
            }
            _ => (),
        }
    } else {
        let url = format!("ws://{}/shared-db", api_endpoint);
        single.doc_db = Some(crdt_over_ws::WsDocsDb::new(
            &single.rt,
            USER.to_string(),
            ui.ctx().clone(),
            url,
        ));
    }
}

impl<T> super::Sharing<T> {
    pub(crate) fn show_interactions<
        'a,
        L: super::ToShared<U = S> + Clone,
        S: autosurgeon::Reconcile,
    >(
        &mut self,
        ui: &mut egui::Ui,
        context: &'a mut EditingContext<L, S>,
        compute_result: &mut Option<ComputeResultsProm<impl ComputeError + Send + Sync>>,
        examples_names: impl Fn(usize) -> String,
    ) -> InteractionResp<&'a L> {
        let mut interaction =
            show_interactions(ui, context, &self.doc_db, compute_result, examples_names);

        if interaction
            .share_button
            .as_ref()
            .map_or(false, |x| x.clicked())
        {
            let (name, content) = interaction.editor.take().unwrap();
            let mut interaction = InteractionResp {
                editor: None,
                ..interaction
            };
            let cnt = content.clone();
            let cnt = std::sync::Mutex::new(cnt.to_shared());
            let cnt = std::sync::Arc::new(cnt);
            context.current = EditStatus::Sharing(cnt.clone());
            let mut cnt = cnt.lock().unwrap();
            let db = &mut self.doc_db.as_mut().unwrap();
            db.create_doc_attempt(&self.rt, name.to_string(), &mut *cnt);

            let content = context.current.content_mut().unwrap();
            interaction.editor = Some((name, &*content));
            interaction
        } else {
            let edit = interaction.editor.map(|(n, _)| n);
            let mut interaction = InteractionResp {
                editor: None,
                ..interaction
            };
            let editor = if let Some(n) = edit
                && let Some(c) = context.current.content_mut()
            {
                Some((n, &*c))
            } else {
                None
            };
            interaction.editor = editor;
            interaction
        }
    }
}
