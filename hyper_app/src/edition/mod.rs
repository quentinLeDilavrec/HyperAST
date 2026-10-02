#[cfg(feature = "collab")]
pub(crate) mod code_editor_automerge;
#[cfg(feature = "collab")]
pub(crate) mod crdt_over_ws;
#[cfg(feature = "collab")]
pub(crate) mod utils_collab;

pub(crate) mod utils_edition;

pub(crate) use utils_edition::FileContainer;
pub(crate) use utils_edition::HiHighlighter2;
pub(crate) use utils_edition::Highlighter0;
pub(crate) use utils_edition::MakeHighlights;
pub(crate) use utils_edition::show_locals_and_interact;

#[cfg(not(feature = "collab"))]
pub(crate) use utils_edition::locals_and_interact_menu;

#[cfg(feature = "collab")]
pub(crate) use utils_collab::locals_and_interact_menu;

#[cfg(feature = "collab")]
#[derive(Default, serde::Deserialize, serde::Serialize)]
#[serde(default)]
pub(crate) struct Sharing<T> {
    pub(crate) content: T,
    #[serde(skip)]
    rt: crdt_over_ws::Rt, // TODO do not init
    #[serde(skip)]
    ws: Option<crdt_over_ws::WsDoc>,
    #[serde(skip)]
    doc_db: Option<crdt_over_ws::WsDocsDb>,
}

#[cfg(not(feature = "collab"))]
#[derive(serde::Deserialize, serde::Serialize, Default)]
#[serde(default)]
pub(crate) struct Sharing<T> {
    pub(crate) content: T,
    #[serde(skip)]
    pub(crate) doc_db: Option<std::convert::Infallible>,
}

#[derive(Default, serde::Serialize, serde::Deserialize)]
pub(crate) struct EditingContext<L, S> {
    pub(crate) current: EditStatus<L, S>,
    pub(crate) local_scripts: std::collections::HashMap<String, L>,
    // shared_script: Option<Arc<std::sync::Mutex<S>>>,
    // shared_script: Arc<std::sync::RwLock<Vec<Option<Arc<std::sync::Mutex<S>>>>>>,
    // shared_scripts: DashMap<String, Arc<std::sync::Mutex<S>>>,
}

#[derive(serde::Serialize, serde::Deserialize)]
pub(crate) enum EditStatus<L, S = ()> {
    #[cfg(feature = "collab")]
    Sharing(std::sync::Arc<std::sync::Mutex<S>>), //(Id)
    #[cfg(feature = "collab")]
    Shared(usize, std::sync::Arc<std::sync::Mutex<S>>), //(Id)
    #[cfg(not(feature = "collab"))]
    #[serde(skip)]
    #[allow(unused)]
    Shared(std::marker::PhantomData<S>),
    Local {
        name: String,
        content: L,
    },
    Example {
        i: usize,
        content: L,
    },
}

pub(crate) struct InteractionResp<E> {
    pub(crate) compute_button: egui::Response,
    pub(crate) save_button: Option<egui::Response>,
    #[cfg(feature = "collab")]
    share_button: Option<egui::Response>,
    pub(crate) editor: Option<(String, E)>,
}

#[cfg(feature = "collab")]
pub(crate) trait ToShared {
    type U;
    fn to_shared(self) -> Self::U;
}

#[cfg(feature = "collab")]
impl ToShared for egui_addon::code_editor::CodeEditor<super::Languages> {
    type U = code_editor_automerge::CodeEditor;

    fn to_shared(self) -> Self::U {
        self.into()
    }
}
