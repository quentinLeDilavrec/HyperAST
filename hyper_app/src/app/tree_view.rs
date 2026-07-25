use std::fmt::Debug;
use std::num::NonZeroU32;
use std::sync::Arc;

use super::code_aspects::HighLightHandle;
use super::code_aspects::{remote_fetch_labels, remote_fetch_nodes_by_ids};
use super::store;
use super::store::{LabelIdentifier, NodeIdentifier};

pub use hyperast::store::nodes::fetched::SimplePacked;
use hyperast::types::{AnyType, Labeled, WithChildren, WithStats};

mod cache;

pub(crate) mod hyperast_layouter;

pub(crate) mod pp;
pub(crate) use pp::PPBuilder;

mod ui_impl;

const DEBUG_LAYOUT: bool = false;
/// increase to debug and see culling in action
const CLIP_LEN: f32 = 0.0; //250.0;

#[derive(Debug, Default)]
pub struct PrefillCache {
    head: f32,
    children: Vec<f32>,
    children_sizes: Vec<Option<NonZeroU32>>,
    next: Option<Box<PrefillCache>>,
}

impl PrefillCache {
    fn height(&self) -> f32 {
        self.head
            + self.children.iter().sum::<f32>()
            + self.next.as_ref().map_or(0.0, |x| x.height())
    }
}

#[derive(Clone, Debug, Default)]
pub(crate) enum Action {
    #[default]
    Keep,
    SerializeKind(AnyType),
    HideKind(AnyType),
    PartialFocused(f32),
    Focused(f32),
    Clicked(Offsets),
}

pub(crate) type Offsets = Vec<usize>;

pub(crate) struct FetchedViewImpl<'a> {
    store: Arc<store::FetchedHyperAST>,
    aspects: &'a super::types::ComputeConfigAspectViews,
    pub(super) prefill_cache: Option<PrefillCache>,
    min_before_count: usize,
    draw_count: usize,
    highlights: Vec<HighLightHandle<'a>>,
    focus: Option<super::code_aspects::Focus<'a>>,
    path: Offsets,
    root_ui_id: egui::Id,
    additions: Option<&'a [u32]>,
    deletions: Option<&'a [u32]>,
    global_pos: Option<u32>,
    open_changed: bool,
}

impl<'a> Debug for FetchedViewImpl<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FetchedViewImpl")
            .field("prefill_cache", &self.prefill_cache)
            .field("min_before_count", &self.min_before_count)
            .field("draw_count", &self.draw_count)
            .finish()
    }
}

impl<'a> FetchedViewImpl<'a> {
    pub(crate) fn new(
        store: Arc<store::FetchedHyperAST>,
        aspects: &'a super::types::ComputeConfigAspectViews,
        take: Option<PrefillCache>,
        highlights: Vec<HighLightHandle<'a>>,
        focus: Option<super::code_aspects::Focus<'a>>,
        path: Offsets,
        root_ui_id: egui::Id,
        additions: Option<&'a [u32]>,
        deletions: Option<&'a [u32]>,
    ) -> Self {
        Self {
            store,
            aspects,
            prefill_cache: take,
            draw_count: 0,
            min_before_count: 0,
            highlights,
            focus,
            path,
            root_ui_id,
            additions,
            deletions,
            global_pos: None,
            open_changed: true,
        }
    }

    pub fn show(&mut self, ui: &mut egui::Ui, api_addr: &str, root: &NodeIdentifier) -> Action {
        ui.style_mut().spacing.button_padding.y = 0.0;
        ui.style_mut().spacing.item_spacing.y = 0.0;

        let node_store = self.store.node_store.read().unwrap();
        let action = if let Some(r) = node_store.try_resolve::<NodeIdentifier>(*root) {
            let kind = self.store.resolve_type(&root);
            let l = r.try_get_label().copied();
            let cs = r.children();
            let size = r.size();
            self.global_pos = Some(size as u32);
            if let (Some(cs), Some(label)) = (&cs, l) {
                let cs = cs.0.to_vec();
                // NOTE: Why would it be an issue ?
                // if let Some(label) = self.store.label_store.read().unwrap().try_resolve(&label) {
                //     assert_eq!("", label, "{:?} {:?} {:?}", root, cs.len(), node_store);
                // }
                drop(node_store);
                self.ui_both_impl(ui, kind, size as u32, *root, label, cs.as_ref())
            } else if let Some(cs) = cs {
                let cs = cs.0.to_vec();
                drop(node_store);
                self.ui_children_impl2(ui, kind, size as u32, *root, cs.as_ref())
            } else if let Some(label) = l {
                drop(node_store);
                self.ui_labeled_impl2(ui, kind, size as u32, *root, label)
            } else {
                drop(node_store);
                self.ui_typed_impl2(ui, kind, size as u32)
            }
        } else {
            drop(node_store);
            self.store.demand_node(*root);
            Action::Keep
        };

        if self.store.update_timer(ui) {
            return action;
        }

        let repo = &self.aspects.commit.repo;
        if let Some(waiting) = self.store.prepare_fetching_nodes() {
            let store = self.store.clone();
            remote_fetch_nodes_by_ids(ui.ctx(), api_addr, store, repo, waiting).ready();
        };
        if let Some(waiting) = self.store.prepare_fetching_labels() {
            let store = self.store.clone();
            remote_fetch_labels(ui.ctx(), api_addr, store, repo, waiting).ready();
        };
        action
    }
}
