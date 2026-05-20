use std::fmt::Debug;
use std::hash::Hash;

use legion::EntityStore;
use legion::storage::IntoComponentSource;

use super::DedupMap;
use crate::compat::hash_map::{HashMap, RawEntryMut, RawVacantEntryMut};
use crate::types::{Typed, TypedNodeId, TypedNodeStore};
use crate::utils::make_hash;

use super::PendingInsert;
use super::TypedNode;
use super::dyn_builder;
use super::elem::{EntryRef, HashedNodeRef, NodeIdentifier};
use super::{NodeStore, NodeStoreInner};

impl NodeStoreInner {
    pub fn with_dedup<'a, 'b>(
        &'a mut self,
        dedup: &'b mut DedupMap,
    ) -> NodeStore<&'a mut NodeStoreInner, &'b mut DedupMap> {
        NodeStore { dedup, inner: self }
    }
}

impl Default for NodeStore {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> PendingInsert<'a> {
    pub fn occupied_id(&self) -> Option<NodeIdentifier> {
        match &self.0 {
            RawEntryMut::Occupied(occupied) => Some(*occupied.key()),
            _ => None,
        }
    }
    pub fn resolve<T>(&self, id: NodeIdentifier) -> HashedNodeRef<'_, T> {
        (self.1.1.internal.entry_ref(id))
            .map(|x| HashedNodeRef::new(x))
            .unwrap()
    }
    pub fn occupied(&'a self) -> Option<(NodeIdentifier, (u64, &'a NodeStoreInner))> {
        match &self.0 {
            RawEntryMut::Occupied(occupied) => Some((*occupied.key(), (self.1.0, self.1.1))),
            _ => None,
        }
    }

    pub fn vacant(
        self,
    ) -> (
        RawVacantEntryMut<'a, legion::Entity, (), ()>,
        (u64, &'a mut NodeStoreInner),
    ) {
        match self.0 {
            RawEntryMut::Vacant(occupied) => (occupied, self.1),
            _ => panic!(),
        }
    }
}

impl NodeStoreInner {
    #[inline]
    pub fn prepare_insertion<'a, Eq: Fn(EntryRef) -> bool, V: Hash>(
        &'a mut self,
        dedup: &'a mut HashMap<NodeIdentifier, (), ()>,
        hashable: &V,
        eq: Eq,
    ) -> PendingInsert<'a> {
        let Self {
            internal: backend,
            hasher,
            ..
        } = self;
        let hash = make_hash(hasher, hashable);
        let entry = dedup.raw_entry_mut().from_hash(hash, |symbol| {
            let r = eq(backend.entry_ref(*symbol).unwrap());
            r
        });
        PendingInsert(entry, (hash, self))
    }
}

impl NodeStore {
    pub fn get<Eq: Fn(EntryRef) -> bool, V: Hash>(
        &self,
        hashable: &V,
        eq: Eq,
    ) -> Option<legion::Entity> {
        let Self {
            dedup,
            inner: NodeStoreInner {
                internal: backend, ..
            },
        } = self;
        let hash = make_hash(&self.inner.hasher, hashable);
        let entry = dedup.raw_entry().from_hash(hash, |symbol| {
            let r = eq(backend.entry_ref(*symbol).unwrap());
            r
        });
        entry.map(|x| *x.0)
    }
    #[inline]
    pub fn prepare_insertion<'a, Eq: Fn(EntryRef) -> bool, V: Hash>(
        &'a mut self,
        hashable: &V,
        eq: Eq,
    ) -> PendingInsert<'a> {
        self.inner.prepare_insertion(&mut self.dedup, hashable, eq)
    }

    #[inline]
    pub fn insert_after_prepare<T>(
        (vacant, (hash, inner)): (
            RawVacantEntryMut<legion::Entity, (), ()>,
            (u64, &mut NodeStoreInner),
        ),
        components: T,
    ) -> legion::Entity
    where
        Option<T>: IntoComponentSource,
    {
        let (&mut symbol, _) = {
            let symbol = inner.internal.push(components);
            vacant.insert_with_hasher(hash, symbol, (), |id| {
                let node: HashedNodeRef<'_, NodeIdentifier> = inner
                    .internal
                    .entry_ref(*id)
                    .map(HashedNodeRef::new)
                    .unwrap();

                make_hash(&inner.hasher, &node)
            })
        };
        symbol
    }

    /// uses the dyn builder see dyn_builder::EntityBuilder
    #[inline]
    pub fn insert_built_after_prepare(
        (vacant, (hash, inner)): (
            RawVacantEntryMut<legion::Entity, (), ()>,
            (u64, &mut NodeStoreInner),
        ),
        components: dyn_builder::BuiltEntity,
    ) -> legion::Entity {
        let (&mut symbol, _) = {
            let symbol = inner.internal.extend(components)[0];
            vacant.insert_with_hasher(hash, symbol, (), |id| {
                let node: HashedNodeRef<'_, NodeIdentifier> = inner
                    .internal
                    .entry_ref(*id)
                    .map(HashedNodeRef::new)
                    .unwrap();

                make_hash(&inner.hasher, &node)
            })
        };
        symbol
    }

    pub fn resolve(&self, id: NodeIdentifier) -> HashedNodeRef<'_, NodeIdentifier> {
        self.inner
            .internal
            .entry_ref(id)
            .map(HashedNodeRef::new)
            .unwrap()
    }

    pub unsafe fn _resolve<T>(&self, id: &NodeIdentifier) -> HashedNodeRef<'_, T> {
        self.inner
            .internal
            .entry_ref(*id)
            .map(|x| HashedNodeRef::new(x))
            .unwrap()
    }

    pub fn resolve_with_type<T: 'static + TypedNodeId<IdN = NodeIdentifier>>(
        &self,
        id: &T::IdN,
    ) -> (T::Ty, HashedNodeRef<'_, T>) {
        let n = self
            .inner
            .internal
            .entry_ref(*id)
            .map(HashedNodeRef::new)
            .unwrap();
        (n.get_type(), n)
    }

    pub fn try_resolve(&self, id: NodeIdentifier) -> Option<HashedNodeRef<'_, NodeIdentifier>> {
        self.inner
            .internal
            .entry_ref(id)
            .map(HashedNodeRef::new)
            .ok()
    }

    pub fn resolve_typed<TIdN: 'static + TypedNodeId<IdN = NodeIdentifier>>(
        &self,
        id: &TIdN,
    ) -> HashedNodeRef<'_, TIdN> {
        let x = self.inner.internal.entry_ref(*id.as_id()).unwrap();
        HashedNodeRef::new(x)
    }

    pub fn try_resolve_typed<TIdN: 'static + TypedNodeId<IdN = NodeIdentifier>>(
        &self,
        id: &TIdN::IdN,
    ) -> Option<(HashedNodeRef<'_, TIdN>, TIdN)> {
        let x = self.inner.internal.entry_ref(*id).unwrap();
        x.get_component::<TIdN::Ty>().ok()?;
        Some((HashedNodeRef::new(x), unsafe { TIdN::from_id(*id) }))
    }

    pub fn try_resolve_typed2<
        L: crate::types::LLang<crate::types::TypeU16<L>, I = u16> + 'static,
    >(
        &self,
        id: &NodeIdentifier,
    ) -> Option<(HashedNodeRef<'_, NodeIdentifier>, L::E)> {
        let x = self.inner.internal.entry_ref(*id).unwrap();
        let ty = x.get_component::<crate::types::TypeU16<L>>().ok()?;
        let ty = ty.e();
        Some((HashedNodeRef::new(x), ty))
    }

    pub fn try_resolve_typed3<
        L: crate::types::LLang<crate::types::TypeU16<L>, I = u16> + 'static,
    >(
        &self,
        id: &NodeIdentifier,
    ) -> Option<TypedNode<HashedNodeRef<'_, NodeIdentifier>, L::E>> {
        let x = self.inner.internal.entry_ref(*id).unwrap();
        let ty = x.get_component::<crate::types::TypeU16<L>>().ok()?;
        let ty = ty.e();
        Some(TypedNode(HashedNodeRef::new(x), ty))
    }
}

impl Debug for NodeStore {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut r = f.debug_struct("NodeStore");
        r.field("count", &self.inner.count)
            .field("errors", &self.inner.errors)
            .field("internal_len", &self.inner.internal.len());
        // .field("internal", &self.internal)

        #[cfg(feature = "subtree-stats")]
        r.field("stats", &self.inner.stats);

        r.finish()
    }
}

impl NodeStoreInner {
    pub fn try_resolve_typed<TIdN: 'static + TypedNodeId<IdN = NodeIdentifier>>(
        &self,
        id: &TIdN::IdN,
    ) -> Option<(HashedNodeRef<'_, TIdN>, TIdN)> {
        let x = self.internal.entry_ref(*id).unwrap();
        x.get_component::<TIdN::Ty>().ok()?;
        Some((HashedNodeRef::new(x), unsafe { TIdN::from_id(*id) }))
    }
}

impl crate::types::lending::NodeStore<NodeIdentifier> for NodeStore {
    fn resolve(
        &self,
        id: &NodeIdentifier,
    ) -> <Self as crate::types::lending::NLending<'_, NodeIdentifier>>::N {
        crate::types::lending::NodeStore::resolve(&self.inner, id)
    }

    fn try_resolve(
        &self,
        id: &NodeIdentifier,
    ) -> Option<crate::types::LendN<'_, Self, NodeIdentifier>> {
        (self.inner.internal)
            .entry_ref(*id)
            .map(HashedNodeRef::new)
            .ok()
    }
}

impl crate::types::lending::NodeStore<NodeIdentifier> for NodeStoreInner {
    fn resolve(
        &self,
        id: &NodeIdentifier,
    ) -> <Self as crate::types::lending::NLending<'_, NodeIdentifier>>::N {
        self.internal
            .entry_ref(*id)
            .map(HashedNodeRef::new)
            .unwrap()
    }
    fn try_resolve(
        &self,
        id: &NodeIdentifier,
    ) -> Option<crate::types::LendN<'_, Self, NodeIdentifier>> {
        self.internal.entry_ref(*id).map(HashedNodeRef::new).ok()
    }
}

impl crate::types::lending::NodeStore<NodeIdentifier> for &NodeStoreInner {
    fn resolve(
        &self,
        id: &NodeIdentifier,
    ) -> <Self as crate::types::lending::NLending<'_, NodeIdentifier>>::N {
        self.internal
            .entry_ref(*id)
            .map(HashedNodeRef::new)
            .unwrap()
    }
    fn try_resolve(
        &self,
        id: &NodeIdentifier,
    ) -> Option<crate::types::LendN<'_, Self, NodeIdentifier>> {
        self.internal.entry_ref(*id).map(HashedNodeRef::new).ok()
    }
}

impl<'a> crate::types::NodeStoreLean<NodeIdentifier> for &'a NodeStoreInner {
    type R = HashedNodeRef<'a, NodeIdentifier>;
    fn resolve(&self, id: &NodeIdentifier) -> Self::R {
        self.internal
            .entry_ref(*id)
            .map(HashedNodeRef::new)
            .unwrap()
    }
}

impl<'a> crate::types::NodeStoreLean<NodeIdentifier> for &'a NodeStore {
    type R = HashedNodeRef<'a, NodeIdentifier>;
    fn resolve(&self, id: &NodeIdentifier) -> Self::R {
        self.inner.resolve(id)
    }
}

impl<TIdN: 'static + TypedNodeId<IdN = NodeIdentifier>> crate::types::TyNodeStore<TIdN>
    for NodeStoreInner
{
    type R<'a> = HashedNodeRef<'a, TIdN>;
}

impl<TIdN: 'static + TypedNodeId<IdN = NodeIdentifier>> TypedNodeStore<TIdN> for NodeStoreInner {
    fn resolve(&self, id: &TIdN) -> Self::R<'_> {
        let x = self.internal.entry_ref(*id.as_id()).unwrap();
        HashedNodeRef::new(x)
    }

    fn try_typed(&self, id: &<TIdN as crate::types::NodeId>::IdN) -> Option<TIdN> {
        let x = self.internal.entry_ref(*id).unwrap();
        x.get_component::<TIdN::Ty>()
            .is_ok()
            .then(|| unsafe { TIdN::from_id(*id) })
    }
}

impl<TIdN: 'static + TypedNodeId<IdN = NodeIdentifier>> crate::types::TyNodeStore<TIdN>
    for NodeStore
{
    type R<'a> = HashedNodeRef<'a, TIdN>;
}

impl<TIdN: 'static + TypedNodeId<IdN = NodeIdentifier>> TypedNodeStore<TIdN> for NodeStore {
    fn resolve(&self, id: &TIdN) -> Self::R<'_> {
        self.inner.resolve(id)
    }

    fn try_typed(&self, id: &<TIdN as crate::types::NodeId>::IdN) -> Option<TIdN> {
        self.inner.try_typed(id)
    }
}

impl NodeStoreInner {
    pub fn len(&self) -> usize {
        self.internal.len()
    }
    #[cfg(feature = "subtree-stats")]
    pub fn stats(&mut self) -> &mut super::NodeStoreStats {
        &mut self.stats
    }
}

impl NodeStore {
    pub fn len(&self) -> usize {
        self.inner.len()
    }
}
