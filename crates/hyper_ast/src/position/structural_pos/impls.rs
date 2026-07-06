use std::cell::RefCell;
use std::rc::Rc;

use crate::PrimInt;

use super::CursorWithPersistence;
use super::CursorWithPersistenceOrderedSet;
use super::StructuralPositionStore2;
use super::{CursorHead, CursorHeadMove};
use super::{ExtRefNode, Handle, PersistedNode, RefNode};

impl<IdN, Idx> PartialEq for CursorWithPersistence<IdN, Idx> {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.s, &other.s) && self.h.0 == other.h.0
    }
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
