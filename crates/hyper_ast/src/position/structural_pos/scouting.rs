use num::traits::NumAssign;
use num::{one, zero};
use std::fmt::Debug;

use super::{Position, StructuralPosition, StructuralPositionStore};
use crate::PrimInt;
use crate::position::{TreePath, TreePathMut};
use crate::types::WithSerialization;
use crate::types::{AnyType, Typed};
use crate::types::{Children, LendT};
use crate::types::{HyperAST, HyperType};
use crate::types::{LabelStore as _, WithChildren as _};
use crate::types::{Labeled, NodeId};

#[derive(Clone, Debug)]
pub struct Scout<IdN, Idx> {
    pub(super) path: StructuralPosition<IdN, Idx>,
    pub(super) ancestors: usize,
}

impl<IdN: Eq + Copy, Idx: PrimInt + NumAssign> TreePathMut<IdN, Idx> for Scout<IdN, Idx> {
    fn pop(&mut self) -> Option<(IdN, Idx)> {
        self.path.pop()
    }

    fn goto(&mut self, node: IdN, i: Idx) {
        self.path.goto(node, i)
    }

    fn inc(&mut self, node: IdN) {
        self.path.inc(node)
    }

    fn dec(&mut self, node: IdN) {
        self.path.dec(node)
    }
}

impl<IdN: Eq + Copy, Idx: PrimInt> TreePath<IdN, Idx> for Scout<IdN, Idx> {
    fn node(&self) -> Option<&IdN> {
        self.path.node()
    }

    fn offset(&self) -> Option<&Idx> {
        self.path.offset()
    }

    fn check<HAST>(&self, stores: &HAST) -> Result<(), ()>
    where
        HAST: HyperAST<IdN = IdN::IdN>,
        HAST::IdN: Eq,
        IdN: NodeId,
        IdN::IdN: NodeId<IdN = IdN::IdN>,
    {
        self.path.check(stores)
    }
}

impl<IdN: Eq + Copy, Idx: PrimInt> Scout<IdN, Idx> {
    pub fn node_always(&self, x: &StructuralPositionStore<IdN, Idx>) -> IdN {
        if let Some(y) = self.path.node() {
            *y
        } else {
            x.nodes[self.ancestors]
        }
    }
    pub fn offset_always(&self, x: &StructuralPositionStore<IdN, Idx>) -> Idx {
        if let Some(y) = self.path.offset() {
            *y
        } else {
            x.offsets[self.ancestors]
        }
    }
    pub fn has_parents(&self) -> bool {
        if self.path.parents.is_empty() {
            self.ancestors != zero()
        } else {
            true
        }
    }
}

impl<IdN: Eq + Copy, Idx: PrimInt> Scout<IdN, Idx> {
    pub fn _up(&mut self) {
        self.path.pop();
        assert_eq!(self.path.parents.len(), self.path.offsets.len());
    }

    pub fn make_child(&self, node: IdN, i: Idx) -> Self {
        let mut s = self.clone();
        s.path.goto(node, i);
        s
    }

    pub fn up(&mut self, x: &StructuralPositionStore<IdN, Idx>) -> Option<IdN> {
        if self.path.parents.is_empty() {
            self.path = StructuralPosition::empty();
            assert_eq!(self.path.parents.len(), self.path.offsets.len());
            if self.ancestors == 0 {
                None
            } else {
                self.ancestors = x.parents[self.ancestors];
                Some(self.node_always(x))
            }
        } else {
            self._up();
            Some(self.node_always(x))
        }
    }
}

impl<IdN: Eq + Copy, Idx: PrimInt> Scout<IdN, Idx> {
    pub fn make_position<'store, HAST>(
        &self,
        sp: &StructuralPositionStore<HAST::IdN, Idx>,
        stores: &'store HAST,
    ) -> Position
    where
        HAST: HyperAST<IdN = IdN, Idx = Idx>,
        for<'t> LendT<'t, HAST>: Typed<Type = AnyType> + WithSerialization,
        HAST::Idx: Debug,
        IdN: Copy + Debug + NodeId<IdN = IdN>,
    {
        self.check(stores).unwrap();
        let mut from_file = false;
        let x = self.node_always(sp);
        let b = stores.resolve(&x);
        let t = stores.resolve_type(&x);
        let len = if let Some(y) = b.try_bytes_len() {
            if !(t.is_file() || t.is_directory()) {
                from_file = true;
            }
            y
        } else {
            0
        };
        let mut offset = 0;
        let mut path = vec![];
        if self.path.parents.is_empty() {
            return sp
                .get(super::SpHandle(self.ancestors + 1))
                .make_position_aux(stores, from_file, len, offset, path);
        }
        let mut i = self.path.parents.len() - 1;
        if from_file {
            while i > 0 {
                let p = self.path.parents[i - 1];
                let b = stores.resolve(&p);
                let t = stores.resolve_type(&p);
                let o = self.path.offsets[i];
                let o: HAST::Idx = num::cast(o).unwrap();
                let c: usize = {
                    let v: Vec<_> = b.children().unwrap().before(o - one()).collect();
                    v.iter()
                        .map(|x| {
                            let b = stores.resolve(x);
                            b.try_bytes_len().unwrap()
                        })
                        .sum()
                };
                offset += c;
                if t.is_file() {
                    from_file = false;
                    i -= 1;
                    break;
                } else {
                    i -= 1;
                }
            }
        }
        if self.path.parents.is_empty() {
        } else if !from_file {
            loop {
                from_file = false;
                let n = self.path.parents[i];
                let b = stores.resolve(&n);
                let l = stores.label_store().resolve(b.get_label_unchecked());
                path.push(l);
                if i == 0 {
                    break;
                } else {
                    i -= 1;
                }
            }
        } else {
            let p = if i == 0 {
                sp.nodes[self.ancestors]
            } else {
                self.path.parents[i - 1]
            };
            let b = stores.resolve(&p);
            let t = stores.resolve_type(&p);
            let o = self.path.offsets[i];
            let o: HAST::Idx = num::cast(o).unwrap();
            let c: usize = b
                .children()
                .unwrap()
                .before(o - one())
                .map(|x| stores.resolve(&x).try_bytes_len().unwrap())
                .sum();
            offset += c;
            if t.is_file() {
                from_file = false;
            }
        }
        sp.get(super::SpHandle(self.ancestors + 1))
            .make_position_aux(stores, from_file, len, offset, path)
    }
}

impl<IdN: Clone, Idx: PrimInt> From<(StructuralPosition<IdN, Idx>, usize)> for Scout<IdN, Idx> {
    fn from((path, ancestors): (StructuralPosition<IdN, Idx>, usize)) -> Self {
        let path = if !path.offsets.is_empty() && path.offsets[0].is_zero() {
            assert_eq!(ancestors, 0);
            (path.parents[1..].to_owned(), path.offsets[1..].to_owned()).into()
        } else {
            path
        };
        Self { path, ancestors }
    }
}

// TODO separate concerns
// TODO make_position should be a From<ExploreStructuralPositions> for FileAndOffsetPositionT and moved to relevant place
// TODO here the remaining logic should be about giving an iterator through the structural position
impl<'a, IdN: NodeId + Eq + Copy, Idx: PrimInt> super::ExploreStructuralPositions<'a, IdN, Idx> {
    pub fn make_position<'store, HAST>(self, stores: &'store HAST) -> super::Position
    where
        'a: 'store,
        HAST: HyperAST<IdN = IdN::IdN>,
        for<'t> LendT<'t, HAST>: Typed<Type = AnyType> + WithSerialization,
        HAST::Idx: Debug,
        IdN: Debug + NodeId,
        IdN::IdN: NodeId<IdN = IdN::IdN> + Eq + Debug,
    {
        use crate::types::lending::NodeStore;
        self.sps.check(stores).unwrap();
        let mut from_file = false;
        let len = if let Some(x) = self.peek_node() {
            let b = stores.node_store().resolve(x.as_id());
            let t = stores.resolve_type(x.as_id());
            if let Some(y) = b.try_bytes_len() {
                if t.is_file() {
                    from_file = true;
                }
                y
            } else {
                0
            }
        } else {
            0
        };
        let offset = 0;
        let path = vec![];
        self.make_position_aux(stores, from_file, len, offset, path)
    }

    fn make_position_aux<'store: 'a, HAST>(
        mut self,
        stores: &'store HAST,
        from_file: bool,
        len: usize,
        mut offset: usize,
        mut path: Vec<&'a str>,
    ) -> super::Position
    where
        HAST: HyperAST<IdN = IdN::IdN>,
        for<'t> LendT<'t, HAST>: Typed<Type = AnyType> + WithSerialization,
        IdN: Copy + Debug + NodeId,
        IdN::IdN: NodeId<IdN = IdN::IdN> + Eq + Debug,
    {
        use crate::types::lending::NodeStore;
        if from_file {
            while let Some(p) = self.peek_parent_node() {
                assert_ne!(p, self.peek_node().unwrap());
                assert_eq!(p, self.sps.nodes[self.sps.parents[self.i - 1]]);
                assert_eq!(self.peek_node().unwrap(), self.sps.nodes[self.i - 1]);
                let b = stores.node_store().resolve(p.as_id());
                let t = stores.resolve_type(p.as_id());
                let o = self.peek_offset().unwrap();
                let o: HAST::Idx =
                    num::cast(o).expect("failed to cast, cannot put value of Idx in ChildIdx");
                if self.peek_node().unwrap().as_id() != &b.children().unwrap()[o] {
                    if self.peek_node().unwrap().as_id() != &b.children().unwrap()[o] {
                        log::error!("backtrace: {}", std::backtrace::Backtrace::force_capture());
                    }
                    assert_eq!(
                        self.peek_node().unwrap().as_id(),
                        &b.children().unwrap()[o],
                        "p:{:?} b.cs:{:?} o:{:?} o p:{:?} i p:{}",
                        p,
                        b.children().unwrap().collect::<Vec<_>>(),
                        self.peek_offset().unwrap(),
                        self.sps.offsets[self.sps.parents[self.i - 1]] - one(),
                        self.sps.parents[self.i - 1],
                    );
                }
                let c: usize = {
                    let v: Vec<_> = b.children().unwrap().before(o).collect();
                    v.iter()
                        .map(|x| {
                            let b = stores.node_store().resolve(x);
                            b.try_bytes_len().unwrap()
                        })
                        .sum()
                };
                offset += c;
                if t.is_file() {
                    self.next();
                    break;
                } else {
                    self.next();
                }
            }
        }
        for p in self {
            let b = stores.node_store().resolve(p.as_id());
            if let Some(l) = b.try_get_label() {
                let l = stores.label_store().resolve(l);
                path.push(l)
            }
        }
        let file = std::path::PathBuf::from_iter(path.iter().rev());
        super::Position::new(file, offset, len)
    }
}
