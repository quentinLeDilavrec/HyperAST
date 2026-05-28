use std::fmt::Debug;

use num_traits::{cast, one, zero};

use hyperast::PrimInt;
use hyperast::types::UniformNodeId;
use hyperast::types::WithStats;
use hyperast::types::{Children as _, Childrn as _, WithChildren as _};
use hyperast::types::{HyperAST, LendT};

use super::basic_post_order::{BasicPOSlice, BasicPostOrder};
use super::complete_post_order::CompletePOSlice;
use super::simple_post_order::{SimplePOSlice, SimplePostOrder};
use super::{ContiguousDescendants, DecendantsLending, Decompressed, DeepDecompressedTreeStore};
use super::{DecompressedParentsLending, DecompressedWithParent, DecompressedWithSiblings};
use super::{LazyDecompressedTreeStore, LazyPOBorrowSlice, LazyPOSliceLending};
use super::{PostOrder, PostOrderIterable};
use super::{Shallow, ShallowDecompressedTreeStore};
use crate::matchers::Decompressible;

pub struct LazyPostOrder<IdN, IdD> {
    pub id_compressed: Box<[IdN]>,
    pub id_parent: Box<[IdD]>,
    /// leftmost leaf descendant of nodes
    pub(crate) llds: Box<[IdD]>,
    _phantom: std::marker::PhantomData<IdN>,
}

impl<IdN, IdD: PrimInt> LazyPostOrder<IdN, IdD> {
    pub fn iter(&self) -> impl Iterator<Item = &IdN> {
        self.id_compressed.iter()
    }
}

impl<IdN: Debug, IdD: PrimInt + Debug> Debug for LazyPostOrder<IdN, IdD> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SimplePostOrder")
            .field("id_compressed", &self.id_compressed.len())
            .field("id_parent", &self.id_parent.len())
            .field("llds", &self.llds.len())
            // .field("id_compressed", &self.id_compressed)
            // .field("id_parent", &self.id_parent)
            // .field("llds", &self.llds)
            .finish()
    }
}

impl<IdN: Clone, IdD: Clone> Clone for LazyPostOrder<IdN, IdD> {
    fn clone(&self) -> Self {
        Self {
            id_compressed: self.id_compressed.clone(),
            id_parent: self.id_parent.clone(),
            llds: self.llds.clone(),
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<HAST: HyperAST + Copy, IdD: PrimInt + Shallow<IdD>>
    Decompressible<HAST, &mut LazyPostOrder<HAST::IdN, IdD>>
{
    pub(super) fn position_in_parent<Idx: PrimInt>(&self, c: &IdD) -> Option<Idx> {
        let p = self.parent(c)?;
        let mut r = 0;
        let mut c = *c;
        let min = self.first_descendant(&p);
        loop {
            let lld = self.first_descendant(&c);
            if lld == min {
                break;
            }
            c = lld - one();
            r += 1;
        }
        Some(cast(r).unwrap())
    }
}

mod impl_ref {
    use super::super::Iter;

    use super::*;

    impl<HAST: HyperAST + Copy, IdD: PrimInt + Shallow<IdD>> Decompressed<IdD>
        for Decompressible<HAST, &LazyPostOrder<HAST::IdN, IdD>>
    {
        type IdD = IdD;
    }

    impl<HAST: HyperAST + Copy, IdD: PrimInt + Shallow<IdD>> ShallowDecompressedTreeStore<HAST, IdD>
        for Decompressible<HAST, &LazyPostOrder<HAST::IdN, IdD>>
    {
        fn len(&self) -> usize {
            self._len()
        }

        fn root(&self) -> IdD {
            cast(self._root()).unwrap()
        }

        fn original(&self, id: &IdD) -> HAST::IdN {
            self.id_compressed[id.index()].clone()
        }

        fn child(&self, x: &IdD, p: &[impl PrimInt]) -> IdD {
            let mut r = *x;
            for d in p {
                let a = self.original(&r);
                let node = self.hyperast.resolve(&a);
                let cs = node.children().filter(|x| !x.is_empty());
                let Some(cs) = cs else {
                    panic!("no children in this tree")
                };
                let mut z = 0;
                let cs = cs.before(cast(*d + one()).unwrap());
                let cs: Vec<_> = cs.iter_children().collect();
                for x in cs {
                    z += self.size2(self.hyperast, &x);
                }
                r = self._first_descendant(&r) + cast(z).unwrap() - one();
            }
            r
        }

        fn children(&self, x: &IdD) -> Vec<IdD> {
            debug_assert!(
                self.id_parent.is_empty() || self.id_parent[x.index()] != zero(),
                "x has not been initialized"
            );
            let a = self.original(x);
            let node = self.hyperast.resolve(&a);
            let cs_len = node.child_count().index();
            if cs_len == 0 {
                return vec![];
            }
            let mut r = vec![zero(); cs_len];
            let mut c = *x - one();
            let mut i = cs_len - 1;
            r[i] = c;
            while i > 0 {
                i -= 1;
                let s = self._size(&c);
                c -= s;
                r[i] = c;
            }
            assert_eq!(self._lld(x.index()).index(), self._lld(c.index()).index());
            r
        }
    }

    impl<'a, HAST: HyperAST + Copy, IdD: PrimInt> DecompressedParentsLending<'a, IdD>
        for Decompressible<HAST, &LazyPostOrder<HAST::IdN, IdD>>
    {
        type PIt = IterParents<'a, IdD>;
    }

    impl<HAST: HyperAST + Copy, IdD: PrimInt + Shallow<IdD>> DecompressedWithParent<HAST, IdD>
        for Decompressible<HAST, &LazyPostOrder<HAST::IdN, IdD>>
    {
        fn parent(&self, id: &IdD) -> Option<IdD> {
            if id == &ShallowDecompressedTreeStore::root(self) {
                None
            } else {
                Some(self.id_parent[id.index()])
            }
        }

        fn has_parent(&self, id: &IdD) -> bool {
            self.parent(id).is_some()
        }

        fn position_in_parent<Idx: PrimInt>(&self, c: &IdD) -> Option<Idx> {
            let i = self._position_in_parent(c, &self.parent(c)?);
            Some(cast(i).unwrap())
        }

        fn parents(&self, id: IdD) -> <Self as DecompressedParentsLending<'_, IdD>>::PIt {
            IterParents {
                id,
                id_parent: &self.id_parent,
            }
        }
        fn path<Idx: PrimInt>(&self, parent: &IdD, descendant: &IdD) -> Vec<Idx> {
            let this = self;
            let mut idxs: Vec<Idx> = vec![];
            let mut curr = *descendant;
            while &curr != parent {
                let p = this
                    .parent(&curr)
                    .expect("reached root before given parent");
                let idx = this._position_in_parent(&curr, &p);
                idxs.push(cast(idx).unwrap());
                curr = p;
            }
            idxs.reverse();
            idxs
        }

        fn lca(&self, _a: &IdD, _b: &IdD) -> IdD {
            todo!()
        }
    }

    impl<HAST: HyperAST + Copy, IdD: PrimInt + Shallow<IdD>> DeepDecompressedTreeStore<HAST, IdD>
        for Decompressible<HAST, &LazyPostOrder<HAST::IdN, IdD>>
    {
        fn it_descendants(&self, x: &IdD) -> impl Iterator<Item = IdD> {
            self.first_descendant(x).step_until(*x)
        }

        fn first_descendant(&self, i: &IdD) -> IdD {
            self.llds[(*i).index()]
        }

        fn descendants_count(&self, x: &IdD) -> usize {
            (self._size(x)).index() - 1
        }

        fn is_descendant(&self, desc: &IdD, of: &IdD) -> bool {
            desc < of && &self.first_descendant(of) <= desc
        }
    }

    impl<HAST: HyperAST + Copy, IdD: PrimInt + Shallow<IdD>> PostOrder<HAST, IdD>
        for Decompressible<HAST, &LazyPostOrder<HAST::IdN, IdD>>
    {
        fn lld(&self, i: &IdD) -> IdD {
            self.llds[(*i).index()]
        }

        fn tree(&self, id: &IdD) -> HAST::IdN {
            self.id_compressed[id.index()].clone()
        }

        fn has_children(&self, id: &IdD) -> bool {
            self.lld(id) != *id
        }
    }

    impl<HAST: HyperAST + Copy, IdD: PrimInt + Shallow<IdD>> PostOrderIterable<HAST, IdD>
        for Decompressible<HAST, &LazyPostOrder<HAST::IdN, IdD>>
    {
        type It = Iter<IdD>;
        fn iter_df_post<const ROOT: bool>(&self) -> Iter<IdD> {
            let len = if ROOT {
                cast(self.id_compressed.len()).unwrap()
            } else {
                self.root()
            };
            Iter {
                current: zero(),
                len,
            }
        }
    }
}

impl<'a, HAST: HyperAST + Copy, IdD: PrimInt> DecompressedParentsLending<'a, IdD>
    for Decompressible<HAST, &mut LazyPostOrder<HAST::IdN, IdD>>
{
    type PIt = IterParents<'a, IdD>;
}

impl<HAST: HyperAST + Copy, IdD: PrimInt + Shallow<IdD>> DecompressedWithParent<HAST, IdD>
    for Decompressible<HAST, &mut LazyPostOrder<HAST::IdN, IdD>>
{
    fn parent(&self, id: &IdD) -> Option<IdD> {
        if id == &ShallowDecompressedTreeStore::root(self) {
            None
        } else {
            Some(self.id_parent[id.index()])
        }
    }

    fn has_parent(&self, id: &IdD) -> bool {
        self.parent(id).is_some()
    }

    fn position_in_parent<Idx: PrimInt>(&self, c: &IdD) -> Option<Idx> {
        Decompressible::position_in_parent(&(*self), c)
    }

    fn parents(&self, id: IdD) -> <Self as DecompressedParentsLending<'_, IdD>>::PIt {
        IterParents {
            id,
            id_parent: &self.id_parent,
        }
    }
    fn path<Idx: PrimInt>(&self, parent: &IdD, descendant: &IdD) -> Vec<Idx> {
        let this = self;
        let mut idxs: Vec<Idx> = vec![];
        let mut curr = *descendant;
        while &curr != parent {
            let p = this
                .parent(&curr)
                .expect("reached root before given parent");
            let idx = this._position_in_parent(&curr, &p);
            idxs.push(cast(idx).unwrap());
            curr = p;
        }
        idxs.reverse();
        idxs
    }

    fn lca(&self, _a: &IdD, _b: &IdD) -> IdD {
        todo!()
    }
}

impl<IdN, IdD: PrimInt> LazyPostOrder<IdN, IdD> {
    fn _position_in_parent(&self, c: &IdD, p: &IdD) -> usize {
        let mut r = 0;
        let mut c = *c;
        let min = self._first_descendant(p);
        loop {
            let lld = self._first_descendant(&c);
            if lld == min {
                break;
            }
            c = lld - one();
            r += 1;
        }
        cast(r).unwrap()
    }
}

impl<HAST: HyperAST + Copy, IdD: PrimInt + Shallow<IdD>> DecompressedWithSiblings<HAST, IdD>
    for Decompressible<HAST, &mut LazyPostOrder<HAST::IdN, IdD>>
{
    fn lsib(&self, x: &IdD) -> Option<IdD> {
        let p = self.parent(x)?;
        let p_lld = self.first_descendant(&p);
        self.lsib(x, &p_lld)
    }
}

pub struct IterParents<'a, IdD> {
    pub(super) id: IdD,
    pub(super) id_parent: &'a [IdD],
}

impl<IdD: PrimInt> Iterator for IterParents<'_, IdD> {
    type Item = IdD;

    fn next(&mut self) -> Option<Self::Item> {
        if self.id == cast(self.id_parent.len() - 1).unwrap() {
            return None;
        }
        let r = self.id_parent[self.id.index()];
        self.id = r;
        Some(r)
    }
}

impl<IdN, IdD: PrimInt + Shallow<IdS>, IdS> super::RawContiguousDescendants<IdD, IdS>
    for LazyPostOrder<IdN, IdD>
{
    fn range(&self, id: &IdD) -> std::ops::Range<IdS> {
        self.llds[(*id).index()].to_shallow()..id.to_shallow()
    }
}
impl<HAST: HyperAST + Copy, IdD: PrimInt + Shallow<IdS>, IdS>
    super::RawContiguousDescendants<IdD, IdS>
    for Decompressible<HAST, &mut LazyPostOrder<HAST::IdN, IdD>>
{
    fn range(&self, id: &IdD) -> std::ops::Range<IdS> {
        self.decomp.range(id)
    }
}

impl<HAST: HyperAST + Copy, IdD: PrimInt + Shallow<IdD>> PostOrder<HAST, IdD>
    for Decompressible<HAST, &mut LazyPostOrder<HAST::IdN, IdD>>
{
    fn lld(&self, i: &IdD) -> IdD {
        self.llds[(*i).index()]
    }

    fn tree(&self, id: &IdD) -> HAST::IdN {
        self.id_compressed[id.index()].clone()
    }

    fn has_children(&self, id: &IdD) -> bool {
        self.lld(id) != *id
    }
}

impl<IdN, IdD: PrimInt> LazyPostOrder<IdN, IdD> {
    pub(crate) fn _size(&self, i: &IdD) -> IdD {
        debug_assert!(
            i.index() < self.llds.len(),
            "Accessing _size for index {:?} but llds.len() = {}",
            i,
            self.llds.len()
        );
        *i - self.llds[(*i).index()] + one()
    }

    pub(crate) fn _lld(&self, i: usize) -> IdD {
        self.llds[i]
    }
    pub(crate) fn _len(&self) -> usize {
        self.id_parent.len()
    }
    pub(crate) fn _root(&self) -> usize {
        self._len() - 1
    }
}

impl<IdN, IdD: PrimInt> LazyPostOrder<IdN, IdD> {
    pub fn len(&self) -> usize {
        self._len()
    }
    pub fn is_empty(&self) -> bool {
        self._len() == 0
    }

    pub fn root(&self) -> IdD {
        cast(self._len() - 1).unwrap()
    }
}

impl<IdN: Clone, IdD: PrimInt> LazyPostOrder<IdN, IdD> {
    pub fn original(&self, id: &IdD) -> IdN {
        self.id_compressed[id.index()].clone()
    }
}

impl<IdN, IdD: PrimInt + Shallow<IdD> + Debug> LazyPostOrder<IdN, IdD> {
    pub fn complete<HAST: HyperAST<IdN = IdN> + Copy>(
        mut self,
        store: HAST,
    ) -> SimplePostOrder<IdN, IdD>
    where
        HAST::IdN: Debug,
        HAST::IdN: UniformNodeId,
        for<'t> LendT<'t, HAST>: WithStats,
    {
        let mut dec = Decompressible {
            hyperast: store,
            decomp: &mut self,
        };
        let root = dec.root();
        dec.complete_subtree(&root);
        SimplePostOrder {
            basic: BasicPostOrder {
                id_compressed: self.id_compressed,
                llds: self.llds,
            },
            id_parent: self.id_parent,
        }
    }
}

impl<HAST: HyperAST + Copy, IdD: PrimInt + Shallow<IdD> + Debug>
    Decompressible<HAST, &mut LazyPostOrder<HAST::IdN, IdD>>
where
    HAST::IdN: Debug,
    for<'t> LendT<'t, HAST>: WithStats,
{
    fn continuous_aux(&mut self, x: &IdD) -> Option<Vec<HAST::IdN>> {
        let store = self.hyperast;
        let node = store.resolve(&self.original(x));
        self.llds[x.index()] = *x + one() - cast(node.size()).unwrap();
        let cs = node.children()?;
        let cs_len = cs.child_count().index();
        if cs_len == 0 {
            return None;
        }
        let c = *x - one();
        let i = cs_len - 1;
        let c_n = &cs[cast(i).unwrap()];
        let offset = c.index();
        self.id_compressed[offset] = c_n.clone();
        self.id_parent[offset] = *x;
        let r = cs
            .before(cs.child_count() - one())
            .iter_children()
            .collect();
        Some(r)
    }

    fn decompress_descendants_continuous(&mut self, x: &<Self as Decompressed<IdD>>::IdD) {
        // PAVE CESAR oO
        let mut c = *x;
        let mut s: Vec<(IdD, Vec<HAST::IdN>)> = vec![];
        loop {
            // Termination: when s is empty, done by second loop
            loop {
                // Termination: go down the last child, finish when no more remains
                let rem = self.continuous_aux(&c);
                // let ran = self.first_descendant(&c).index()..=c.index();
                let Some(rem) = rem else {
                    if c > zero() {
                        c -= one();
                    }
                    break;
                };
                s.push((c, rem));
                c -= one();
            }
            let mut next = None;
            loop {
                // Termination: either rem diminish, or if rem is empty s diminish
                let Some((p, mut rem)) = s.pop() else {
                    break;
                };
                let Some(z) = rem.pop() else {
                    assert!(c <= self.lld(&p));
                    continue;
                };
                assert!(self.lld(&p) <= c);
                next = Some((p, z));
                s.push((p, rem));
                break;
            }
            let Some((p, z)) = next else {
                assert!(
                    self._size(x) <= one() || self.tree(&(c + one())) != self.tree(x),
                    "{:?} {:?}",
                    self.tree(&(c + one())),
                    self.tree(x)
                );
                assert!(c == self.lld(x) || c + one() == self.lld(x));
                break;
            };
            self.id_parent[c.index()] = p;
            self.id_compressed[c.index()] = z;
        }
    }

    pub fn decompress_descendants(&mut self, x: &IdD) {
        let mut q = vec![*x];
        while let Some(x) = q.pop() {
            assert!(self.id_parent[x.index()] != zero());
            q.extend(self.decompress_children(&x));
        }
    }
    // WARN just used for debugging
    // TODO remove
    pub fn go_through_descendants(&mut self, x: &IdD) {
        let store = self.hyperast;
        let mut q = vec![*x];
        while let Some(x) = q.pop() {
            assert!(self.id_parent[x.index()] != zero());
            assert_eq!(
                self._size(&x).index(),
                store.resolve(&self.original(&x)).size()
            );
            q.extend(self.children(&x));
        }
    }
    pub fn complete_subtree(&mut self, x: &IdD) {
        assert!(
            self.parent(x).is_none_or(|p| p != zero()),
            "x is not initialized"
        );
        let first = self.first_descendant(x);
        let mut i = *x;
        while i > first {
            if self.id_parent[i.index() - 1] != zero() {
                i -= one();
            } else {
                assert!(
                    self.parent(&i).is_none_or(|p| p != zero()),
                    "i is not initialized"
                );

                self.decompress_descendants_continuous(&i);
                i = self.lld(&i);
            }
            if i == first {
                break;
            }
        }
        self.decompress_children(x).len();
    }
}

impl<HAST, IdD> hyperast::types::DecompressedFrom<HAST> for LazyPostOrder<HAST::IdN, IdD>
where
    IdD: PrimInt + Debug,
    HAST: HyperAST + Copy,
    for<'t> LendT<'t, HAST>: WithStats,
{
    type Out = Self;

    fn decompress(hyperast: HAST, root: &HAST::IdN) -> Self {
        let store = hyperast;
        let pred_len = store.resolve(root).size();
        let mut simple = LazyPostOrder {
            id_compressed: init_boxed_slice(root.clone(), pred_len), // TODO micro bench it and maybe use uninit
            id_parent: init_boxed_slice(zero(), pred_len),
            llds: init_boxed_slice(zero(), pred_len),
            _phantom: Default::default(),
        };
        simple.id_compressed[simple._root()] = root.clone();
        simple.id_parent[simple._root()] = cast(simple._root()).unwrap();
        simple.llds[simple._root()] = zero();
        simple
    }
}

impl<HAST: HyperAST + Copy, IdD: PrimInt + Debug> super::DecompressedSubtree<HAST::IdN>
    for Decompressible<HAST, LazyPostOrder<HAST::IdN, IdD>>
where
    for<'t> LendT<'t, HAST>: WithStats,
{
    type Out = Self;
    fn decompress(self, root: &HAST::IdN) -> Self {
        let store = self.hyperast;
        let pred_len = store.resolve(root).size();
        let mut simple = LazyPostOrder {
            id_compressed: init_boxed_slice(root.clone(), pred_len), // TODO micro bench it and maybe use uninit
            id_parent: init_boxed_slice(zero(), pred_len),
            llds: init_boxed_slice(zero(), pred_len),
            _phantom: Default::default(),
        };
        simple.id_compressed[simple._root()] = root.clone();
        simple.id_parent[simple._root()] = cast(simple._root()).unwrap();
        simple.llds[simple._root()] = zero();
        Decompressible {
            hyperast: store,
            decomp: simple,
        }
    }
}

pub(super) fn init_boxed_slice<T: Clone>(value: T, pred_len: usize) -> Box<[T]> {
    let mut v = Vec::with_capacity(pred_len);
    v.resize(pred_len, value);
    v.into_boxed_slice()
}

impl<HAST: HyperAST + Copy, IdD: PrimInt + Shallow<IdD>> Decompressed<IdD>
    for Decompressible<HAST, &mut LazyPostOrder<HAST::IdN, IdD>>
{
    type IdD = IdD;
}

impl<HAST: HyperAST + Copy, IdD: PrimInt + Shallow<IdD>> ShallowDecompressedTreeStore<HAST, IdD>
    for Decompressible<HAST, &mut LazyPostOrder<HAST::IdN, IdD>>
{
    fn len(&self) -> usize {
        self._len()
    }

    fn root(&self) -> IdD {
        cast(self._root()).unwrap()
    }

    fn original(&self, id: &IdD) -> HAST::IdN {
        self.id_compressed[id.index()].clone()
    }

    fn child(&self, x: &IdD, p: &[impl PrimInt]) -> IdD {
        let store = self.hyperast;
        let mut r = *x;
        for d in p {
            let a = self.original(&r);
            let node = store.resolve(&a);
            let cs = node.children().filter(|x| !x.is_empty());
            let Some(cs) = cs else {
                panic!("no children in this tree")
            };
            let mut z = 0;
            let cs = cs.before(cast(*d + one()).unwrap());
            let cs: Vec<_> = cs.iter_children().collect();
            for x in cs {
                z += self.size2(self.hyperast, &x);
            }
            r = self.first_descendant(&r) + cast(z).unwrap() - one();
        }
        r
    }

    fn children(&self, x: &IdD) -> Vec<IdD> {
        debug_assert!(
            self.id_parent.is_empty() || self.id_parent[x.index()] != zero(),
            "x has not been initialized"
        );
        let a = self.original(x);
        let node = self.hyperast.resolve(&a);
        let cs_len = node.child_count().index();
        if cs_len == 0 {
            return vec![];
        }
        let mut r = vec![zero(); cs_len];
        let mut c = *x - one();
        let mut i = cs_len - 1;
        r[i] = c;
        while i > 0 {
            i -= 1;
            let s = self._size(&c);
            c = c
                .checked_sub(&s)
                .expect("should have called decompress_children before");
            r[i] = c;
        }
        assert_eq!(self._lld(x.index()), self._lld(c.index()));
        r
    }
}

impl<IdN, IdD: PrimInt + Shallow<IdD> + Debug> LazyPostOrder<IdN, IdD> {
    pub fn child_decompressed<HAST>(
        mut self,
        store: HAST,
        x: &IdD,
        p: impl Iterator<Item = HAST::Idx>,
    ) -> IdD
    where
        HAST: HyperAST<IdN = IdN> + Copy,
        HAST::IdN: UniformNodeId,
        for<'t> LendT<'t, HAST>: WithStats,
    {
        let mut r = *x;
        for d in p {
            let mut dec = Decompressible {
                decomp: &mut self,
                hyperast: store,
            };
            let cs = dec.decompress_children(&r);
            let mut z: IdD = zero();
            let cs = cs
                .get(..(d + one()).index())
                .expect("no child corresponding to given path");
            for x in cs {
                z += self._size(x); //Self::size2(store, x);
            }
            let dec = Decompressible {
                decomp: &mut self,
                hyperast: store,
            };
            r = dec.first_descendant(&r) + cast(z).unwrap() - one();
        }
        r
    }
}

impl<HAST: HyperAST + Copy, IdD: PrimInt + Shallow<IdD> + Debug>
    Decompressible<HAST, &mut LazyPostOrder<HAST::IdN, IdD>>
where
    HAST::IdN: UniformNodeId,
    for<'t> LendT<'t, HAST>: WithStats,
{
    pub fn child_decompressed(&mut self, x: &IdD, p: impl Iterator<Item = HAST::Idx>) -> IdD {
        let mut r = *x;
        for d in p {
            let cs = self.decompress_children(&r);
            let mut z: IdD = zero();
            let cs = cs
                .get(..(d + one()).index())
                .expect("no child corresponding to given path");
            for x in cs {
                z += self._size(x); //Self::size2(store, x);
            }
            r = self.first_descendant(&r) + cast(z).unwrap() - one();
        }
        r
    }
}

impl<HAST: HyperAST + Copy, IdD: PrimInt + Shallow<IdD> + Debug>
    LazyDecompressedTreeStore<HAST, IdD>
    for Decompressible<HAST, &mut LazyPostOrder<HAST::IdN, IdD>>
where
    HAST::IdN: UniformNodeId,
    for<'t> LendT<'t, HAST>: WithStats,
{
    fn starter(&self) -> Self::IdD {
        ShallowDecompressedTreeStore::root(self)
    }

    fn decompress_children(&mut self, x: &Self::IdD) -> Vec<Self::IdD> {
        let store = self.hyperast;
        debug_assert!(
            self.id_parent.is_empty() || self.id_parent[x.index()] != zero(),
            "x has not been initialized"
        );
        let node = store.resolve(&self.original(x));
        let Some(cs) = node.children() else {
            return vec![];
        };
        let cs_len = cs.child_count().index();
        if cs_len == 0 {
            return vec![];
        }
        let mut r = vec![zero(); cs_len];
        let mut c = *x - one();
        let mut i = cs_len - 1;
        loop {
            let c_n = &cs[cast(i).unwrap()];
            let s = store.resolve(c_n).size();
            let offset = c.index();
            r[i] = c;
            self.id_compressed[offset] = c_n.clone();
            self.id_parent[offset] = *x;
            self.llds[offset] = c + one() - cast(s).unwrap();
            if i == 0 {
                break;
            }
            c -= cast(s).unwrap();
            i -= 1;
        }

        assert_eq!(self._lld(x.index()), self._lld(c.index()));
        r
    }

    fn decompress_to(&mut self, x: &IdD) -> Self::IdD {
        let mut p = *x;
        // TODO do some kind of dichotomy
        loop {
            if self.is_decompressed(&p) {
                while x < &self.lld(&p) {
                    p = self.parent(&p).unwrap();
                }
                break;
            }
            p += one();
        }
        while &p > x {
            debug_assert!(&self.lld(&p) <= x);
            let cs = self.decompress_children(&p);
            let cs = cs.into_iter().rev();
            // TODO do some kind of dichotomy
            for a in cs {
                if &a < x {
                    break;
                }
                p = a;
                if &a == x {
                    break;
                }
            }
        }
        assert_eq!(&p, x);
        *x
    }
}

impl<IdN, IdD: PrimInt + Shallow<IdD> + Debug> LazyPostOrder<IdN, IdD> {
    fn is_decompressed(&self, x: &IdD) -> bool {
        self.id_parent.is_empty() || self.id_parent[x.index()] != zero()
    }
}

impl<IdN, IdD: PrimInt> LazyPostOrder<IdN, IdD> {
    pub(crate) fn _first_descendant(&self, i: &IdD) -> IdD {
        self.llds[(*i).index()]
    }
}

impl<HAST: HyperAST + Copy, IdD: PrimInt + Shallow<IdD>> DeepDecompressedTreeStore<HAST, IdD>
    for Decompressible<HAST, &mut LazyPostOrder<HAST::IdN, IdD>>
{
    fn it_descendants(&self, x: &IdD) -> impl Iterator<Item = IdD> {
        self.first_descendant(x).step_until(*x)
    }

    fn first_descendant(&self, i: &IdD) -> IdD {
        self.llds[(*i).index()]
    }

    fn descendants_count(&self, x: &IdD) -> usize {
        (self._size(x)).index() - 1
    }

    fn is_descendant(&self, desc: &IdD, of: &IdD) -> bool {
        desc < of && &self.first_descendant(of) <= desc
    }
}

impl<'a, HAST: HyperAST + Copy, IdD: PrimInt> DecendantsLending<'a>
    for Decompressible<HAST, &mut LazyPostOrder<HAST::IdN, IdD>>
{
    type Slice = SimplePOSlice<'a, HAST::IdN, IdD>;
}

impl<HAST: HyperAST + Copy, IdD: PrimInt + Shallow<IdD>> ContiguousDescendants<HAST, IdD>
    for Decompressible<HAST, &mut LazyPostOrder<HAST::IdN, IdD>>
{
    fn descendants_range(&self, x: &IdD) -> std::ops::Range<IdD> {
        self.first_descendant(x)..*x
    }

    /// WIP
    fn slice(&self, _x: &IdD) -> <Self as DecendantsLending<'_>>::Slice {
        // Would need to complete the subtree under x, need the node store to do so
        // TODO could also make a lazy slice !!
        todo!()
    }
}

impl<HAST: HyperAST + Copy, IdD: PrimInt + Shallow<IdD>>
    Decompressible<HAST, &mut LazyPostOrder<HAST::IdN, IdD>>
{
    pub(super) fn slice_range(&self, x: &IdD) -> std::ops::RangeInclusive<usize> {
        self.first_descendant(x).index()..=x.index()
    }
}

impl<'a, HAST: HyperAST + Copy, IdD: PrimInt + Shallow<IdD>> LazyPOSliceLending<'a, HAST, IdD, IdD>
    for Decompressible<HAST, &mut LazyPostOrder<HAST::IdN, IdD>>
{
    type SlicePo = Decompressible<HAST, CompletePOSlice<'a, HAST::IdN, IdD, bitvec::boxed::BitBox>>;
}

impl<HAST: HyperAST + Copy, IdD: PrimInt + Shallow<IdD> + Debug> LazyPOBorrowSlice<HAST, IdD>
    for Decompressible<HAST, &mut LazyPostOrder<HAST::IdN, IdD>>
where
    HAST::IdN: UniformNodeId,
    HAST::IdN: Debug,
    for<'t> LendT<'t, HAST>: WithStats,
{
    fn slice_po(&mut self, x: &IdD) -> <Self as LazyPOSliceLending<'_, HAST, IdD, IdD>>::SlicePo {
        self.complete_subtree(x);
        let range = self.slice_range(x);
        let basic = BasicPOSlice {
            id_compressed: &self.id_compressed[range.clone()],
            llds: &self.llds[range.clone()],
        };
        let kr = Decompressible {
            hyperast: self.hyperast,
            decomp: basic,
        }
        .compute_kr_bitset();
        let simple = SimplePOSlice {
            basic,
            id_parent: &self.id_parent[range],
        };
        Decompressible {
            hyperast: self.hyperast,
            decomp: CompletePOSlice { simple, kr },
        }
    }
}

impl<HAST: HyperAST + Copy, IdD: PrimInt + Debug + Shallow<IdD>> PostOrderIterable<HAST, IdD>
    for Decompressible<HAST, &mut LazyPostOrder<HAST::IdN, IdD>>
{
    type It = super::Iter<IdD>;
    fn iter_df_post<const ROOT: bool>(&self) -> super::Iter<IdD> {
        let len = if ROOT {
            cast(self.id_compressed.len()).unwrap()
        } else {
            self.root()
        };
        super::Iter {
            current: zero(),
            len,
        }
    }
}

impl<HAST: HyperAST + Copy, IdD: PrimInt + Eq + Shallow<IdD>>
    Decompressible<HAST, &mut LazyPostOrder<HAST::IdN, IdD>>
{
    pub fn lsib(&self, c: &IdD, p_lld: &IdD) -> Option<IdD> {
        assert!(p_lld <= c, "{:?}<={:?}", p_lld.to_usize(), c.to_usize());
        let lld = self.first_descendant(c);
        assert!(lld <= *c);
        if lld.is_zero() {
            return None;
        }
        let sib = lld - num_traits::one();
        if &sib < p_lld { None } else { Some(sib) }
    }
}

impl<IdN, IdD: PrimInt> LazyPostOrder<IdN, IdD>
where
    IdN: UniformNodeId,
{
    fn size2<HAST: HyperAST<IdN = IdN> + Copy>(&self, store: HAST, x: &IdN) -> usize {
        fn size<HAST: HyperAST + Copy>(store: HAST, x: &HAST::IdN) -> usize
        where
            HAST::IdN: UniformNodeId,
        {
            let tmp = store.resolve(x);
            let Some(cs) = tmp.children() else {
                return 1;
            };

            let mut z = 0;
            for x in cs.iter_children() {
                z += size(store, &x);
            }
            z + 1
        }
        size(store, x)
    }
}

impl<IdN, IdD: PrimInt> LazyPostOrder<IdN, IdD> {
    pub(crate) fn _compute_kr_bitset(&self) -> bitvec::boxed::BitBox {
        let node_count = self.id_compressed.len();
        let mut kr = bitvec::bitbox!(0;node_count);
        let mut visited = bitvec::bitbox!(0; node_count);
        for i in (1..node_count).rev() {
            if !visited[self._lld(i).index()] {
                kr.set(i, true);
                visited.set(self._lld(i).index(), true);
            }
        }
        kr
    }
}
