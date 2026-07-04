use num::one;
use std::fmt::Debug;

use crate::PrimInt;
use crate::store::defaults::NodeIdentifier;
use crate::types::{NodeId, TypedNodeId};

use super::{Position, tags};

pub use super::offsets_and_nodes::StructuralPosition;

mod path_store;

mod scouting;
pub use scouting::*;

mod typed_scouting;
pub use typed_scouting::*;

#[derive(Clone)]
pub struct ExploreStructuralPositions<'a, IdN, Idx = usize, Config = tags::BottomUpFull> {
    sps: &'a StructuralPositionStore<IdN, Idx>,
    i: usize,
    _phantom: std::marker::PhantomData<Config>,
}
impl<IdN, Idx> super::node_filter_traits::Full for ExploreStructuralPositions<'_, IdN, Idx> {}
impl<IdN, Idx> super::node_filter_traits::NoSpace
    for ExploreStructuralPositions<'_, IdN, Idx, tags::BottomUpNoSpace>
{
}

mod esp_impl {
    use super::super::position_accessors::*;
    use super::*;
    impl<IdN: NodeId + Eq + Copy, Idx: PrimInt> SolvedPosition<IdN>
        for ExploreStructuralPositions<'_, IdN, Idx>
    {
        fn node(&self) -> IdN {
            self.sps.nodes[self.i]
        }
    }
    impl<IdN: NodeId + Eq + Copy, Idx: PrimInt> RootedPosition<IdN>
        for ExploreStructuralPositions<'_, IdN, Idx>
    {
        fn root(&self) -> IdN {
            todo!("value must be computed")
        }
    }
    impl<IdN: NodeId + Eq + Copy, Idx: PrimInt> WithOffsets
        for ExploreStructuralPositions<'_, IdN, Idx>
    {
        type Idx = Idx;
    }
    impl<IdN: NodeId + Eq + Copy, Idx: PrimInt> WithPostOrderOffsets
        for ExploreStructuralPositions<'_, IdN, Idx>
    {
        fn iter(&self) -> impl Iterator<Item = Self::Idx> {
            IterOffsets(self.clone())
        }
    }

    pub struct IterOffsets<'a, IdN, Idx = usize>(ExploreStructuralPositions<'a, IdN, Idx>);

    impl<IdN, Idx: PrimInt> Iterator for IterOffsets<'_, IdN, Idx> {
        type Item = Idx;

        fn next(&mut self) -> Option<Self::Item> {
            let o = self.0.sps.offsets[self.0.i];
            self.0.try_go_up().map(|_| o - one())
        }
    }

    impl<IdN: NodeId + Eq + Copy, Idx: PrimInt> WithPath<IdN>
        for ExploreStructuralPositions<'_, IdN, Idx>
    {
    }
    impl<IdN: NodeId + Eq + Copy, Idx: PrimInt> WithPostOrderPath<IdN>
        for ExploreStructuralPositions<'_, IdN, Idx>
    {
        fn iter_offsets_and_parents(&self) -> impl Iterator<Item = (Self::Idx, IdN)> {
            IterOffsetsNodes(self.clone())
        }
    }
    impl<IdN: NodeId + Eq + Copy, Idx: PrimInt> WithFullPostOrderPath<IdN>
        for ExploreStructuralPositions<'_, IdN, Idx>
    {
        fn iter_with_nodes(&self) -> (IdN, impl Iterator<Item = (Self::Idx, IdN)>) {
            (self.node(), IterOffsetsNodes(self.clone()))
        }
    }

    pub struct IterOffsetsNodes<'a, IdN, Idx = usize>(ExploreStructuralPositions<'a, IdN, Idx>);

    impl<IdN: Copy, Idx: PrimInt> Iterator for IterOffsetsNodes<'_, IdN, Idx> {
        type Item = (Idx, IdN);

        fn next(&mut self) -> Option<Self::Item> {
            let o = self.0.sps.offsets[self.0.i];
            self.0
                .try_go_up()
                .map(|h| (o - one(), self.0.sps.nodes[h.0]))
        }
    }
}

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

impl<IdN: NodeId + Eq + Copy, Idx: PrimInt> ExploreStructuralPositions<'_, IdN, Idx> {
    pub(super) fn peek_parent_node(&self) -> Option<IdN> {
        if self.i == 0 {
            return None;
        }
        let i = self.i - 1;
        let r = self.sps.nodes[self.sps.parents[i]];
        Some(r)
    }
    pub(super) fn peek_offset(&self) -> Option<Idx> {
        if self.i == 0 {
            return None;
        }
        let i = self.i - 1;
        let r = self.sps.offsets[i] - one();
        Some(r)
    }
    pub(super) fn peek_node(&self) -> Option<IdN> {
        if self.i == 0 {
            return None;
        }
        let i = self.i - 1;
        let r = self.sps.nodes[i];
        Some(r)
    }
}

impl<IdN: Copy, Idx> Iterator for ExploreStructuralPositions<'_, IdN, Idx> {
    type Item = IdN;

    fn next(&mut self) -> Option<Self::Item> {
        self.try_go_up().map(|i| self.sps.nodes[i.0])
        // if self.i == 0 {
        //     return None;
        // } //println!("next: {} {}", self.i, self.sps.parents[self.i - 1]);
        // let i = self.i - 1;
        // let r = self.sps.nodes[i];
        // if i > 0 {
        //     self.i = self.sps.parents[i] + 1;
        // } else {
        //     self.i = i;
        // }
        // Some(r)
    }
}
impl<IdN, Idx> ExploreStructuralPositions<'_, IdN, Idx> {
    /// return previous index
    #[inline]
    fn try_go_up(&mut self) -> Option<SpHandle> {
        if self.i == 0 {
            return None;
        } //println!("next: {} {}", self.i, self.sps.parents[self.i - 1]);
        let i = self.i - 1;
        let r = i;
        if i > 0 {
            self.i = self.sps.parents[i] + 1;
        } else {
            self.i = i;
        }
        Some(SpHandle(r))
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

/// same as `StructuralPositionStore` but relative to a
struct StructuralPositionStore2<IdN = NodeIdentifier, Idx = u16> {
    persisted: Handle,
    nodes: Vec<IdN>,
    parents: Vec<Handle>,
    offsets: Vec<Idx>,
}

impl<IdN, Idx> Debug for StructuralPositionStore2<IdN, Idx> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("StructuralPositionStore")
            .field("persisted", &self.persisted)
            .field("nodes", &self.nodes.len())
            .field("parents", &self.parents.len())
            .field("offsets", &self.offsets.len())
            .finish()
    }
}

impl<IdN, Idx> StructuralPositionStore2<IdN, Idx> {
    fn persist(&mut self, h: Handle) {
        if self.persisted.0 < h.0 {
            self.persisted.0 = h.0;
        }
    }
    fn parent(&self, h: Handle) -> Option<Handle> {
        if h.0 == 0 {
            panic!();
        }
        if h.0 == 1 {
            return None;
        }
        assert!(self.parents[h.0 - 1].0 < h.0);
        Some(self.parents[h.0 - 1])
    }
    fn node(&self, h: Handle) -> IdN
    where
        IdN: Copy,
    {
        if h.0 == 0 {
            panic!();
        }
        self.nodes[h.0 - 1]
    }
    fn offset(&self, h: Handle) -> Idx
    where
        Idx: Copy,
    {
        if h.0 == 0 {
            panic!();
        }
        self.offsets[h.0 - 1]
    }

    fn inc(&mut self, h: Handle, node: IdN) -> Handle
    where
        Idx: PrimInt,
    {
        if h.0 == 0 {
            panic!();
        }
        if self.persisted.0 < h.0 {
            self.nodes[h.0 - 1] = node;
            self.offsets[h.0 - 1] += num::one();
            return h;
        }
        let Some(p) = self.parent(h) else {
            unreachable!()
        };
        if self.persisted.0 == self.nodes.len() {
            self.nodes.push(node);
            self.offsets.push(self.offsets[h.0 - 1] + num::one());
            self.parents.push(p);
            let mut h = self.persisted;
            h.0 += 1;
            h
        } else {
            assert!(self.nodes.len() > self.persisted.0);
            self.nodes[self.persisted.0] = node;
            self.offsets[self.persisted.0] = self.offsets[h.0 - 1] + num::one();
            self.parents[self.persisted.0] = p;
            let mut h = self.persisted;
            h.0 += 1;
            h
        }
    }

    fn down(&mut self, h: Handle, node: IdN, offset: Idx) -> Handle {
        if self.persisted.0 > h.0 {
            let mut h = h;
            h.0 -= 1;
            self.nodes[self.persisted.0] = node;
            self.offsets[self.persisted.0] = offset;
            self.parents[self.persisted.0] = h;
            let mut r = self.persisted;
            r.0 += 1;
            return r;
        }
        let mut c = h;
        c.0 += 1;
        if self.nodes.len() == c.0 - 1 {
            assert_eq!(self.offsets.len(), c.0 - 1);
            self.nodes.push(node);
            self.offsets.push(offset);
            self.parents.push(h);
        } else if self.nodes.len() < c.0 - 1 {
            dbg!(self.nodes.len());
            dbg!(self.offsets.len());
            dbg!(self.persisted.0);
            dbg!(c.0);
            panic!()
        } else {
            self.nodes[c.0 - 1] = node;
            self.offsets[c.0 - 1] = offset;
            self.parents[c.0 - 1] = h;
        }
        c
    }
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

use std::cell::RefCell;
use std::rc::Rc;

/// Cursor backed by a store, thus allowing to efficiently yield nodes, while sharing the shared sub path between all nodes.
/// As long as a node is not persisted, this cursor reuses and mutate to update itself.
// only tags::BottomUpFull is possible for efficiency
pub struct CursorWithPersistence<IdN, Idx = u16> {
    s: Rc<RefCell<StructuralPositionStore2<IdN, Idx>>>,
    h: Handle,
}

impl<IdN, Idx> PartialEq for CursorWithPersistence<IdN, Idx> {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.s, &other.s) && self.h.0 == other.h.0
    }
}

/// great way to deduplicate positions e.g. node matches
pub struct CursorWithPersistenceOrderedSet<IdN, Idx = u16> {
    s: Rc<RefCell<StructuralPositionStore2<IdN, Idx>>>,
    handles: Vec<usize>,
}

impl<IdN, Idx> CursorWithPersistenceOrderedSet<IdN, Idx> {
    pub fn register(&mut self, p: &PersistedNode<IdN, Idx>) {
        assert!(Rc::ptr_eq(&self.s, &p.s));
        if !self.handles.contains(&p.h.0) {
            self.handles.push(p.h.0);
        }
    }
    pub fn register_ref(&mut self, p: RefNode<'_, IdN, Idx>) {
        use std::ops::Deref;
        let x = self.s.deref().borrow();
        let x = x.deref();
        let y = p.s.deref();
        assert!(std::ptr::addr_eq(x, y));
        if !self.handles.contains(&p.h.0) {
            self.handles.push(p.h.0);
        }
    }
    pub fn remove(&mut self, p: &PersistedNode<IdN, Idx>) -> bool {
        assert!(Rc::ptr_eq(&self.s, &p.s));
        if let Some(pos) = self.handles.iter().position(|&h| h == p.h.0) {
            self.handles.swap_remove(pos);
            true
        } else {
            false
        }
    }
    pub fn collect_vec<R>(self, f: impl Fn(RefNode<IdN, Idx>) -> R) -> Vec<R> {
        self.handles
            .into_iter()
            .map(move |h| {
                let s = self.s.borrow();
                f(RefNode { s, h: Handle(h) })
            })
            .collect()
    }
    pub fn into_iter<R>(self, f: impl Fn(RefNode<IdN, Idx>) -> R) -> impl Iterator<Item = R> {
        self.handles.into_iter().map(move |h| {
            let s = self.s.borrow();
            f(RefNode { s, h: Handle(h) })
        })
    }

    pub fn is_empty(&self) -> bool {
        self.handles.is_empty()
    }
}

impl<'a, IdN: 'a, Idx: 'a> CursorWithPersistenceOrderedSet<IdN, Idx> {
    pub fn iter(&'a self) -> impl Iterator<Item = RefNode<'a, IdN, Idx>> {
        self.handles.iter().map(move |h| {
            let s = self.s.borrow();
            RefNode { s, h: Handle(*h) }
        })
    }
}

impl<IdN, Idx> Eq for CursorWithPersistence<IdN, Idx> {}

impl<IdN, Idx> CursorWithPersistence<IdN, Idx> {
    pub fn new(node: IdN) -> Self
    where
        Idx: PrimInt,
    {
        let mut n = Self::default();
        n.h = n.s.borrow_mut().down(n.h, node, num::zero());
        n
    }
    pub fn build_empty_set(&mut self) -> CursorWithPersistenceOrderedSet<IdN, Idx>
    where
        Idx: PrimInt,
    {
        CursorWithPersistenceOrderedSet {
            s: self.s.clone(),
            handles: vec![],
        }
    }
    pub fn default() -> Self {
        let s = StructuralPositionStore2 {
            persisted: Handle(0),
            nodes: vec![],
            parents: vec![],
            offsets: vec![],
        };
        let s = Rc::new(RefCell::new(s));
        Self { s, h: Handle(0) }
    }
    pub fn persist(&self) -> PersistedNode<IdN, Idx> {
        self.s.borrow_mut().persist(self.h);
        PersistedNode {
            s: self.s.clone(),
            h: self.h,
        }
    }
    pub fn persist_parent(&self) -> Option<PersistedNode<IdN, Idx>> {
        let p = self.s.borrow().parent(self.h)?;
        self.s.borrow_mut().persist(p);
        Some(PersistedNode {
            s: self.s.clone(),
            h: p,
        })
    }
    pub fn ref_node(&self) -> RefNode<'_, IdN, Idx> {
        let s = self.s.borrow();
        RefNode { s, h: self.h }
    }
    pub fn ref_parent(&self) -> Option<RefNode<'_, IdN, Idx>> {
        let p = self.s.borrow().parent(self.h)?;
        let s = self.s.borrow();
        Some(RefNode { s, h: p })
    }
    pub fn ext(&self) -> ExtRefNode<'_, IdN, Idx> {
        let s = self.s.borrow();
        ExtRefNode::new(s, self.h)
    }
}

impl<IdN, Idx> CursorHeadMove<IdN, Idx> for CursorWithPersistence<IdN, Idx>
where
    IdN: Copy,
    Idx: PrimInt,
{
    fn inc(&mut self, node: IdN) {
        self.h = self.s.borrow_mut().inc(self.h, node);
    }

    fn down(&mut self, node: IdN, offset: Idx) {
        self.h = self.s.borrow_mut().down(self.h, node, offset);
    }
}

impl<IdN, Idx> CursorHead<IdN, Idx> for CursorWithPersistence<IdN, Idx>
where
    IdN: Copy,
    Idx: Copy,
{
    fn node(&self) -> IdN {
        self.s.borrow().node(self.h)
    }
    fn offset(&self) -> Idx {
        self.s.borrow().offset(self.h)
    }
    fn up(&mut self) -> bool {
        if let Some(p) = self.s.borrow().parent(self.h) {
            self.h = p;
            return true;
        }
        false
    }
    fn parent(&self) -> Option<IdN> {
        let p = self.s.borrow().parent(self.h)?;
        Some(self.s.borrow().node(p))
    }
}

/// Node that was persisted i.e. mutating the cursor guarantee that this node observable values won't change.
#[derive(Clone)]
pub struct PersistedNode<IdN, Idx = u16> {
    s: Rc<RefCell<StructuralPositionStore2<IdN, Idx>>>,
    h: Handle,
}

impl<IdN, Idx> PartialEq for PersistedNode<IdN, Idx> {
    fn eq(&self, other: &Self) -> bool {
        // TODO check
        Rc::ptr_eq(&self.s, &other.s) && self.h.0 == other.h.0
    }
}

impl<IdN, Idx> Eq for PersistedNode<IdN, Idx> {}

impl<IdN, Idx> PartialOrd for PersistedNode<IdN, Idx> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<IdN, Idx> Ord for PersistedNode<IdN, Idx> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        if !Rc::ptr_eq(&self.s, &other.s) {
            panic!()
        }
        self.h.0.cmp(&other.h.0)
    }
}

impl<IdN, Idx> PersistedNode<IdN, Idx>
where
    IdN: Copy,
    Idx: Copy,
{
    pub fn build(&self) -> ExtRefNode<'_, IdN, Idx> {
        let s = self.s.borrow();
        ExtRefNode::new(s, self.h)
    }

    pub fn ext(&self) -> ExtRefNode<'_, IdN, Idx> {
        let s = self.s.borrow();
        ExtRefNode::new(s, self.h)
    }

    pub fn offsets(mut self) -> Vec<Idx> {
        let mut r = vec![];
        loop {
            r.push(self.offset());
            if !self.up() {
                break;
            }
        }
        r
    }

    pub fn ref_node(&self) -> RefNode<'_, IdN, Idx> {
        let s = self.s.borrow();
        RefNode { s, h: self.h }
    }
}

impl<IdN, Idx> CursorHead<IdN, Idx> for PersistedNode<IdN, Idx>
where
    IdN: Copy,
    Idx: Copy,
{
    fn node(&self) -> IdN {
        self.s.borrow().node(self.h)
    }
    fn offset(&self) -> Idx {
        self.s.borrow().offset(self.h)
    }
    fn up(&mut self) -> bool {
        if let Some(p) = self.s.borrow().parent(self.h) {
            self.h = p;
            return true;
        }
        false
    }
    fn parent(&self) -> Option<IdN> {
        let p = self.s.borrow().parent(self.h)?;
        Some(self.s.borrow().node(p))
    }
}
/// Node that is possibly not persisted i.e. cannot safely mutate the cursor at the same time.
/// If you need to read a node and modify the cursor at the same time, make a [`PersistedNode`].
pub struct RefNode<'a, IdN, Idx = u16> {
    s: std::cell::Ref<'a, StructuralPositionStore2<IdN, Idx>>,
    h: Handle,
}

impl<IdN, Idx> Clone for RefNode<'_, IdN, Idx> {
    fn clone(&self) -> Self {
        Self {
            s: std::cell::Ref::clone(&self.s),
            h: self.h,
        }
    }
}

impl<IdN, Idx> PartialOrd for RefNode<'_, IdN, Idx> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<IdN, Idx> Ord for RefNode<'_, IdN, Idx> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        if !std::ptr::eq(&self.s, &other.s) {
            panic!()
        }
        self.h.0.cmp(&other.h.0)
    }
}

impl<'a, IdN, Idx> RefNode<'a, IdN, Idx>
where
    IdN: Copy,
    Idx: Copy,
{
    pub fn ext(&self) -> ExtRefNode<'a, IdN, Idx> {
        let s = std::cell::Ref::clone(&self.s);
        ExtRefNode::new(s, self.h)
    }

    pub fn offsets(mut self) -> Vec<Idx> {
        let mut r = vec![];
        loop {
            r.push(self.offset());
            if !self.up() {
                break;
            }
        }
        r
    }
}

impl<IdN, Idx> CursorHead<IdN, Idx> for RefNode<'_, IdN, Idx>
where
    IdN: Copy,
    Idx: Copy,
{
    fn node(&self) -> IdN {
        self.s.node(self.h)
    }
    fn offset(&self) -> Idx {
        self.s.offset(self.h)
    }
    fn up(&mut self) -> bool {
        if let Some(p) = self.s.parent(self.h) {
            self.h = p;
            return true;
        }
        false
    }
    fn parent(&self) -> Option<IdN> {
        let p = self.s.parent(self.h)?;
        Some(self.s.node(p))
    }
}

impl<IdN, Idx> PartialEq for RefNode<'_, IdN, Idx> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(&*self.s, &*other.s) && self.h.0 == other.h.0
    }
}

impl<IdN, Idx> Eq for RefNode<'_, IdN, Idx> {}

impl<IdN: std::hash::Hash, Idx: std::hash::Hash> std::hash::Hash for RefNode<'_, IdN, Idx>
where
    IdN: Copy,
    Idx: Copy,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        if cfg!(debug_assertions) {
            todo!("make tests to assert it gives the same results as in offsets_and_nodes")
        }
        // TODO make tests to assert it gives the same results as in offsets_and_nodes
        let mut s = self.clone();
        // self.parents.first().hash(state);
        s.node().hash(state);
        loop {
            // self.offsets.hash(state);
            s.offset().hash(state);
            if !s.up() {
                // self.parents.last().hash(state);
                s.node().hash(state);
                break;
            }
        }
    }
}

pub struct ExtRefNode<'a, IdN, Idx = u16> {
    s: std::cell::Ref<'a, StructuralPositionStore2<IdN, Idx>>,
    h: Handle,
    ext_nodes: Vec<IdN>,
    ext_offsets: Vec<Idx>,
}
impl<IdN: Clone, Idx: Clone> Clone for ExtRefNode<'_, IdN, Idx> {
    fn clone(&self) -> Self {
        Self {
            s: std::cell::Ref::clone(&self.s),
            h: self.h,
            ext_nodes: self.ext_nodes.clone(),
            ext_offsets: self.ext_offsets.clone(),
        }
    }
}
impl<'a, IdN, Idx> ExtRefNode<'a, IdN, Idx> {
    fn new(s: std::cell::Ref<'a, StructuralPositionStore2<IdN, Idx>>, h: Handle) -> Self {
        ExtRefNode {
            s,
            h,
            ext_nodes: vec![],
            ext_offsets: vec![],
        }
    }
}

impl<IdN, Idx> CursorHeadMove<IdN, Idx> for ExtRefNode<'_, IdN, Idx>
where
    IdN: Copy,
    Idx: PrimInt,
{
    fn inc(&mut self, node: IdN) {
        if self.ext_nodes.is_empty() {
            let o = self.s.offset(self.h);
            if let Some(p) = self.s.parent(self.h) {
                self.h = p;
            } else {
                todo!()
            }
            self.ext_nodes.push(node);
            self.ext_offsets.push(o + num::one());
        } else {
            *self.ext_nodes.last_mut().unwrap() = node;
            *self.ext_offsets.last_mut().unwrap() += num::one();
        }
    }

    fn down(&mut self, node: IdN, offset: Idx) {
        self.ext_nodes.push(node);
        self.ext_offsets.push(offset);
    }
}

impl<IdN, Idx> CursorHead<IdN, Idx> for ExtRefNode<'_, IdN, Idx>
where
    IdN: Copy,
    Idx: Copy,
{
    fn node(&self) -> IdN {
        if let Some(n) = self.ext_nodes.last() {
            *n
        } else {
            self.s.node(self.h)
        }
    }
    fn offset(&self) -> Idx {
        if let Some(o) = self.ext_offsets.last() {
            *o
        } else {
            self.s.offset(self.h)
        }
    }
    fn up(&mut self) -> bool {
        if self.ext_nodes.pop().is_some() {
            assert!(self.ext_offsets.pop().is_some());
            return true;
        } else if let Some(p) = self.s.parent(self.h) {
            self.h = p;
            return true;
        }
        false
    }
    fn parent(&self) -> Option<IdN> {
        if self.ext_nodes.len() > 1 {
            self.ext_nodes.get(self.ext_nodes.len() - 2).copied()
        } else if self.ext_nodes.len() == 1 {
            Some(self.s.node(self.h))
        } else {
            let p = self.s.parent(self.h)?;
            Some(self.s.node(p))
        }
    }
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
