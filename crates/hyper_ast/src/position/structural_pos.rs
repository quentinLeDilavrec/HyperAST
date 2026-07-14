use std::fmt::Debug;

use crate::store::defaults::NodeIdentifier;
use crate::types::TypedNodeId;

use super::{Position, tags};

pub use super::offsets_and_nodes::StructuralPosition;

mod path_store;

mod scouting;
pub use scouting::*;

mod typed_scouting;
pub use typed_scouting::*;

mod path_store2;

mod impls;

#[derive(Clone)]
pub struct ExploreStructuralPositions<'a, IdN, Idx = usize, Config = tags::BottomUpFull> {
    sps: &'a StructuralPositionStore<IdN, Idx>,
    i: usize,
    _phantom: std::marker::PhantomData<Config>,
}

mod esp_impl;

#[derive(Clone, Copy, Debug)]
pub struct SpHandle(pub(super) usize);

pub struct StructuralPositionStore<IdN = NodeIdentifier, Idx = u16> {
    pub nodes: Vec<IdN>,
    parents: Vec<usize>,
    offsets: Vec<Idx>,
}

impl<IdN, Idx> Debug for StructuralPositionStore<IdN, Idx> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("StructuralPositionStore")
            .field("nodes", &self.nodes.len())
            .field("parents", &self.parents.len())
            .field("offsets", &self.offsets.len())
            .finish()
    }
}

impl<TIdN: TypedNodeId, Idx> From<TypedScout<TIdN, Idx>> for Scout<TIdN::IdN, Idx> {
    fn from(value: TypedScout<TIdN, Idx>) -> Self {
        Self {
            ancestors: value.ancestors,
            path: value.path,
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Handle(usize);

/// same as `StructuralPositionStore` but positions can be persisted while still sharing the same store
///
/// its construction assumes a pre-order traversal
///
/// Only persisted positions and positions to persisted positions are stored.
/// Consequently, after traversing a subtree if not position was persisted,
/// then the store should not have changed.
///
/// Positions are laid in preorder, `persisted` grows monotonously.
///
/// Everything before `persisted` becomes immutable
struct StructuralPositionStore2<IdN = NodeIdentifier, Idx = u16> {
    /// handle to last persisted position
    persisted: Handle,
    nodes: Vec<IdN>,
    parents: Vec<Handle>,
    offsets: Vec<Idx>,
}

pub trait CursorHead<IdN, Idx> {
    fn node(&self) -> IdN;
    fn offset(&self) -> Idx;
    fn parent(&self) -> Option<IdN>;
    fn up(&mut self) -> bool;
}

pub trait CursorHeadMove<IdN, Idx>: CursorHead<IdN, Idx> {
    fn inc(&mut self, node: IdN);
    fn down(&mut self, node: IdN, offset: Idx);
}

type SharedPStore<IdN, Idx> = std::rc::Rc<std::cell::RefCell<StructuralPositionStore2<IdN, Idx>>>;

/// Cursor backed by a store, thus allowing to efficiently yield nodes, while sharing the shared sub path between all nodes.
/// As long as a node is not persisted, this cursor reuses and mutate to update itself.
// only tags::BottomUpFull is possible for efficiency
pub struct CursorWithPersistence<IdN, Idx = u16> {
    s: SharedPStore<IdN, Idx>,
    h: Handle,
}

/// great way to deduplicate positions e.g. node matches
pub struct CursorWithPersistenceOrderedSet<IdN, Idx = u16> {
    s: SharedPStore<IdN, Idx>,
    handles: Vec<usize>,
}

/// Node that was persisted i.e. mutating the cursor guarantee that this node observable values won't change.
#[derive(Clone)]
pub struct PersistedNode<IdN, Idx = u16> {
    s: SharedPStore<IdN, Idx>,
    h: Handle,
}

/// Node that is possibly not persisted i.e. cannot safely mutate the cursor at the same time.
/// If you need to read a node and modify the cursor at the same time, make a [`PersistedNode`].
pub struct RefNode<'a, IdN, Idx = u16> {
    s: std::cell::Ref<'a, StructuralPositionStore2<IdN, Idx>>,
    h: Handle,
}

pub struct ExtRefNode<'a, IdN, Idx = u16> {
    s: std::cell::Ref<'a, StructuralPositionStore2<IdN, Idx>>,
    h: Handle,
    ext_nodes: Vec<IdN>,
    ext_offsets: Vec<Idx>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple2() {
        let mut c = CursorWithPersistence::default();
        c.down(0u32, 0u32);
        assert_eq!(c.node(), 0);
        assert_eq!(c.offset(), 0);
        c.inc(1u32);
        assert_eq!(c.node(), 1);
        assert_eq!(c.offset(), 1);
        let n = c.persist();
        assert_eq!(n.node(), 1);
        assert_eq!(n.offset(), 1);
        c.down(2u32, 0u32);
        assert_eq!(c.node(), 2);
        assert_eq!(c.offset(), 0);
        assert!(c.up());
        assert_eq!(c.node(), 1);
        assert_eq!(c.offset(), 1);
        c.down(2u32, 0u32);
        assert_eq!(c.node(), 2);
        assert_eq!(c.offset(), 0);
        assert!(c.up());
        assert_eq!(c.node(), 1);
        assert_eq!(c.offset(), 1);
    }
}
