use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::HashSet;
use std::collections::VecDeque;
use std::sync::{Mutex, MutexGuard, RwLock, RwLockReadGuard};

use hyperast::types::AnyType;

use hyperast::store::nodes::fetched;

use fetched::FetchedLabels;
use fetched::{HashedNodeRef, NodeStore};
pub use fetched::{LabelIdentifier, NodeIdentifier};

#[derive(Default)]
pub struct FetchedHyperAST {
    // the label store
    pub(crate) label_store: RwLock<FetchedLabels>,
    /// the node store
    pub(crate) node_store: RwLock<NodeStore>,
    // /// each set is fetched sequentially, non blocking
    // /// pushed ids are tested against all pending sets because they might not have entered the store
    // /// new set every 100 elements, due to id serialized size in url
    // /// TODO split by arch
    // /// TODO maybe use a crossbeam queue while putting a dummy value in nodestore or use dashmap
    // nodes_waiting: std::sync::Mutex<VecDeque<HashSet<NodeIdentifier>>>,
    // /// each set is fetched sequentially, non blocking
    // /// pushed ids are tested against all pending sets because they might not have entered the store
    // /// new set every 200 elements, due to id serialized size in url
    // labels_waiting: std::sync::Mutex<VecDeque<HashSet<LabelIdentifier>>>,
    /// pending ie. nodes in flight
    nodes_pending: Mutex<VecDeque<HashSet<NodeIdentifier>>>,
    nodes_waiting: Mutex<Option<HashSet<NodeIdentifier>>>,
    labels_pending: Mutex<VecDeque<HashSet<LabelIdentifier>>>,
    labels_waiting: Mutex<Option<HashSet<LabelIdentifier>>>,
    /// timer to avoid flooding server with requests
    timer: Mutex<Option<f32>>,
}

impl FetchedHyperAST {
    pub(crate) fn read(&self) -> LockedFetchedHyperAST<'_> {
        LockedFetchedHyperAST {
            label_store: self.label_store.read().unwrap(),
            node_store: self.node_store.read().unwrap(),
            nodes_pending: self.nodes_pending.lock().unwrap(),
            nodes_waiting: RefCell::new(self.nodes_waiting.lock().unwrap()),
            labels_pending: self.labels_pending.lock().unwrap(),
            labels_waiting: RefCell::new(self.labels_waiting.lock().unwrap()),
        }
    }

    pub fn resolve_type(&self, n: &NodeIdentifier) -> AnyType {
        let ns = self.node_store.read().unwrap();
        let n: HashedNodeRef<'_, NodeIdentifier> = ns.try_resolve(*n).unwrap();
        let lang = n.get_lang();
        resolve_type(n, lang)
    }

    /// Demand fetching of nodes from the server.
    pub(crate) fn demand_nodes(&self, ids: impl Iterator<Item = NodeIdentifier>) {
        let node_store = self.node_store.read().unwrap();
        let pending = self.nodes_pending.lock().unwrap();
        let mut waiting = self.nodes_waiting.lock().unwrap();
        let waiting = waiting.get_or_insert_default();
        for x in ids {
            if pending.iter().any(|y| y.contains(&x)) || node_store.contains(x) {
                continue;
            }
            wasm_rs_dbg::dbg!(x);
            waiting.insert(x);
        }
    }

    /// Demand fetching of a single node from the server.
    pub(crate) fn demand_node(&self, id: NodeIdentifier) {
        let pendings = self.nodes_pending.lock().unwrap();
        if !pendings.iter().any(|x| x.contains(&id)) {
            let mut waiting = self.nodes_waiting.lock().unwrap();
            waiting.get_or_insert_default().insert(id);
        }
    }

    /// take and mark waiting nodes as pending to fetch them from the server.
    #[must_use]
    pub(crate) fn prepare_fetching_nodes(&self) -> Option<HashSet<NodeIdentifier>> {
        let waiting = self.nodes_waiting.lock().unwrap().take()?;
        let mut pendings = self.nodes_pending.lock().unwrap();
        pendings.push_back(waiting.clone());
        Some(waiting)
    }

    pub fn extend_nodes(
        &self,
        ids: HashSet<NodeIdentifier>,
        mut simple_packed: fetched::SimplePacked<String>,
    ) {
        // TODO look at the behavior of this pop
        let p = self.nodes_pending.lock().unwrap().pop_front().unwrap();
        if ids != p {
            log::warn!("different set of nodes was fetched than expected");
        }
        let mut node_store = self.node_store.write().unwrap();
        // Hack to avoid duplicates
        for x in &mut simple_packed.storages_variants {
            x.remove_if(|id| node_store.contains(*id));
        }
        node_store.extend(simple_packed);
    }

    /// Demand fetching a label from the server.
    pub(crate) fn demand_label(&self, id: LabelIdentifier) {
        let pendings = self.labels_pending.lock().unwrap();
        if !pendings.iter().any(|x| x.contains(&id)) {
            let mut waiting = self.labels_waiting.lock().unwrap();
            waiting.get_or_insert_default().insert(id);
        }
    }

    /// take and mark waiting labels as pending to fetch them from the server.
    #[must_use]
    pub(crate) fn prepare_fetching_labels(&self) -> Option<HashSet<LabelIdentifier>> {
        let waiting = self.labels_waiting.lock().unwrap().take()?;
        let mut pendings = self.labels_pending.lock().unwrap();
        pendings.push_back(waiting.clone());
        Some(waiting)
    }

    pub fn extend_labels(&self, fetched_labels: crate::app::code_aspects::FetchedLabels) {
        // TODO look at the behavior of this pop
        self.labels_pending.lock().unwrap().pop_front();
        let mut hash_map = self.label_store.write().unwrap();
        let label_ids = fetched_labels.label_ids.into_iter();
        for (k, v) in label_ids.zip(fetched_labels.labels) {
            hash_map.insert(k, v);
        }
    }

    /// check timer to avoid flooding server with requests
    pub(crate) fn update_timer(&self, ui: &mut egui::Ui) -> bool {
        let mut lock = self.timer.lock().unwrap();
        let Some(mut timer) = lock.take() else {
            *lock = Some(0.0);
            return true;
        };
        let dt = ui.input(|mem| mem.unstable_dt);
        timer += dt;
        if timer < std::time::Duration::from_secs(1).as_secs_f32() {
            *lock = Some(timer);
            true
        } else {
            *lock = Some(0.0);
            false
        }
    }
}

pub(crate) struct LockedFetchedHyperAST<'a> {
    pub(crate) label_store: RwLockReadGuard<'a, FetchedLabels>,
    pub(crate) node_store: RwLockReadGuard<'a, NodeStore>,
    pub(crate) nodes_pending: MutexGuard<'a, VecDeque<HashSet<NodeIdentifier>>>,
    pub(crate) nodes_waiting: RefCell<MutexGuard<'a, Option<HashSet<NodeIdentifier>>>>,
    pub(crate) labels_pending: MutexGuard<'a, VecDeque<HashSet<LabelIdentifier>>>,
    pub(crate) labels_waiting: RefCell<MutexGuard<'a, Option<HashSet<LabelIdentifier>>>>,
}

impl<'a, 'b> hyperast::types::NLending<'a, NodeIdentifier> for LockedFetchedHyperAST<'b> {
    type N = HashedNodeRef<'a, NodeIdentifier>;
}

impl<'b> hyperast::types::NodeStore<NodeIdentifier> for LockedFetchedHyperAST<'b> {
    fn resolve(
        &self,
        id: &NodeIdentifier,
    ) -> <Self as hyperast::types::NLending<'_, NodeIdentifier>>::N {
        if let Some(r) = self.node_store.try_resolve(*id) {
            r
        } else {
            // TODO use a recursive fetch
            // TODO need an additional queue for such recursive fetch
            // TODO use additional nodes that are not fetched but where fetched to avoid transfering more than necessary
            if !self.nodes_pending.iter().any(|x| x.contains(id)) {
                self.nodes_waiting
                    .borrow_mut()
                    .get_or_insert_default()
                    .insert(*id);
            }
            // unimplemented!()
            self.node_store.unavailable_node()
        }
    }
}

impl<'b> hyperast::types::LabelStore<str> for LockedFetchedHyperAST<'b> {
    type I = LabelIdentifier;

    fn get_or_insert<U: Borrow<str>>(&mut self, _node: U) -> Self::I {
        todo!(
            "TODO remove this method from trait as it cannot be implemented on immutable/append_only label stores"
        )
    }

    fn get<U: Borrow<str>>(&self, _node: U) -> Option<Self::I> {
        todo!(
            "TODO remove this method from trait as it cannot be implemented efficiently for all stores"
        )
    }

    fn resolve(&self, id: &Self::I) -> &str {
        if let Some(get) = self.label_store.try_resolve(id) {
            get
        } else {
            if !self.labels_pending.iter().any(|x| x.contains(id)) {
                self.labels_waiting
                    .borrow_mut()
                    .get_or_insert_default()
                    .insert(*id);
            }
            "."
        }
    }
}

impl<'b> hyperast::types::TypeStore for LockedFetchedHyperAST<'b> {
    type Ty = AnyType;
}

impl<'a> hyperast::types::HyperASTShared for LockedFetchedHyperAST<'a> {
    type IdN = NodeIdentifier;
    type Idx = u16;
    type Label = LabelIdentifier;
}

impl<'a, 'b> hyperast::types::AstLending<'a> for LockedFetchedHyperAST<'b> {
    type RT = HashedNodeRef<'a, NodeIdentifier>;
}

impl<'a> hyperast::types::HyperAST for LockedFetchedHyperAST<'a> {
    type NS = Self;

    fn node_store(&self) -> &Self::NS {
        self
    }

    type LS = Self;

    fn label_store(&self) -> &Self::LS {
        self
    }

    type TS = Self;

    fn resolve_type(&self, id: &Self::IdN) -> <Self::TS as hyperast::types::TypeStore>::Ty {
        let ns = &self.node_store;
        let Some(n) = ns.try_resolve::<NodeIdentifier>(*id) else {
            use hyperast::types::HyperType;
            let t = hyperast_gen_ts_java::Type::Dot.as_static();
            return unsafe { AnyType::make(t) };
        };
        let lang = n.get_lang();
        resolve_type(n, lang)
    }
}

impl std::hash::Hash for FetchedHyperAST {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.label_store.read().unwrap().len().hash(state);
        self.node_store.read().unwrap().len().hash(state);
    }
}

fn resolve_type(n: HashedNodeRef<'_, NodeIdentifier>, lang: &str) -> AnyType {
    macro_rules! aux {
        ($g:ident) => {
            if lang == $g::Lang::NAME {
                return AnyType::from_fetched::<$g::Type, $g::Lang>(&n);
            }
        };
    }
    aux!(hyperast_gen_ts_java);
    aux!(hyperast_gen_ts_cpp);
    aux!(hyperast_gen_ts_xml);
    unreachable!("{}", lang)
}
