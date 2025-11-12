use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::HashSet;
use std::collections::VecDeque;
use std::sync::{Mutex, MutexGuard, RwLock, RwLockReadGuard};

use hyperast::types::AnyType;
use hyperast::types::Lang;

use hyperast::store::nodes::fetched;

use fetched::FetchedLabels;
use fetched::{HashedNodeRef, NodeStore};
pub use fetched::{LabelIdentifier, NodeIdentifier};

#[derive(Default)]
pub(crate) struct TStore;

impl<'a> hyperast::types::TypeStore for TStore {
    type Ty = AnyType;
}

#[derive(Default)]
pub struct FetchedHyperAST {
    pub(crate) label_store: RwLock<FetchedLabels>,
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
    pub(crate) nodes_pending: Mutex<VecDeque<HashSet<NodeIdentifier>>>,
    pub(crate) nodes_waiting: Mutex<Option<HashSet<NodeIdentifier>>>,
    pub(crate) labels_pending: Mutex<VecDeque<HashSet<LabelIdentifier>>>,
    pub(crate) labels_waiting: Mutex<Option<HashSet<LabelIdentifier>>>,
    /// timer to avoid flooding
    pub(crate) timer: Mutex<Option<f32>>,
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
        let raw = n.get_raw_type();
        match lang {
            "hyperast_gen_ts_java::types::Lang" => {
                let t: &'static dyn hyperast::types::HyperType =
                    hyperast_gen_ts_java::types::Lang::make(raw);
                t.into()
            }
            "hyperast_gen_ts_cpp::types_alt::Lang" | "hyperast_gen_ts_cpp::types::Lang" => {
                let t: &'static dyn hyperast::types::HyperType =
                    hyperast_gen_ts_cpp::types::Lang::make(raw);
                t.into()
            }
            "hyperast_gen_ts_xml::types::Lang" => {
                let t: &'static dyn hyperast::types::HyperType =
                    hyperast_gen_ts_xml::types::Lang::make(raw);
                t.into()
            }
            l => unreachable!("{}", l),
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
                    .get_or_insert(Default::default())
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
                    .get_or_insert(Default::default())
                    .insert(*id);
            }
            "."
        }
    }
}

impl<'a, 'b> hyperast::types::TypeStore for LockedFetchedHyperAST<'b> {
    type Ty = AnyType;
}

impl<'a, 'b: 'a> hyperast::types::HyperASTShared for LockedFetchedHyperAST<'a>
where
    Self: 'b,
{
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
            return hyperast_gen_ts_java::types::Type::Dot.as_static().into();
        };
        let lang = n.get_lang();
        let raw = n.get_raw_type();
        match lang {
            "hyperast_gen_ts_java::types::Lang" => {
                let t: &'static dyn hyperast::types::HyperType =
                    hyperast_gen_ts_java::types::Lang::make(raw);
                t.into()
            }
            "hyperast_gen_ts_cpp::types_alt::Lang" | "hyperast_gen_ts_cpp::types::Lang" => {
                let t: &'static dyn hyperast::types::HyperType =
                    hyperast_gen_ts_cpp::types::Lang::make(raw);
                t.into()
            }
            "hyperast_gen_ts_xml::types::Lang" => {
                let t: &'static dyn hyperast::types::HyperType =
                    hyperast_gen_ts_xml::types::Lang::make(raw);
                t.into()
            }
            l => unreachable!("{}", l),
        }
    }
}

impl std::hash::Hash for FetchedHyperAST {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.label_store.read().unwrap().len().hash(state);
        self.node_store.read().unwrap().len().hash(state);
    }
}
