use std::borrow::Borrow;
use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::marker::PhantomData;
use std::ops::Deref;

use bitvec::slice::BitSlice;
use num_traits::cast;

use hyperast::types::{HyperAST, LendT};
use hyperast::types::{WithSerialization, WithStats};

use super::Decompressed;
use super::DecompressedWithSiblings;
use super::PrimInt;
use super::simple_post_order::{SimplePOSlice, SimplePostOrder};
use super::{ContiguousDescendants, DecendantsLending, DeepDecompressedTreeStore};
use super::{DecompressedParentsLending, DecompressedWithParent};
use super::{FullyDecompressedTreeStore, ShallowDecompressedTreeStore};
use super::{POBorrowSlice, POSliceLending};
use super::{PostOrdKeyRoots, PostOrder, PostOrderIterable, PostOrderKeyRoots};
use crate::matchers::Decompressible;

/// Decompressed tree with a post-order layout
/// provides:
/// - origines (through [`SimplePostOrder`])
/// - llds (through [`SimplePostOrder`])
/// - parents (through [`SimplePostOrder`])
/// - key roots
#[derive(Clone)]
pub struct CompletePostOrder<IdN, IdD> {
    pub(super) simple: SimplePostOrder<IdN, IdD>,
    /// LR_keyroots(T) = {k | there exists no k < k' such that l(k) = l(k’)}.
    pub(super) kr: bitvec::boxed::BitBox,
}

impl<IdN, IdD> Deref for CompletePostOrder<IdN, IdD> {
    type Target = SimplePostOrder<IdN, IdD>;

    fn deref(&self) -> &Self::Target {
        &self.simple
    }
}

impl<HAST: HyperAST + Copy, IdD> CompletePostOrder<HAST, IdD> {
    pub fn as_slice(&self) -> CompletePOSlice<'_, HAST, IdD, &'_ BitSlice> {
        CompletePOSlice {
            simple: self.simple.as_slice(),
            kr: &self.kr,
        }
    }
}

impl<HAST: HyperAST + Copy, IdD> Decompressible<HAST, CompletePostOrder<HAST::IdN, IdD>> {
    fn as_simple(&self) -> Decompressible<HAST, &SimplePostOrder<HAST::IdN, IdD>> {
        let hyperast = self.hyperast;
        let decomp = &self.simple;
        Decompressible { hyperast, decomp }
    }
}

impl<IdN, IdD: PrimInt> CompletePostOrder<IdN, IdD> {
    pub fn iter(&self) -> impl Iterator<Item = &IdN> {
        self.simple.iter()
    }
}

impl<HAST: HyperAST + Copy, IdD: PrimInt>
    From<Decompressible<HAST, SimplePostOrder<HAST::IdN, IdD>>>
    for Decompressible<HAST, CompletePostOrder<HAST::IdN, IdD>>
{
    fn from(simple: Decompressible<HAST, SimplePostOrder<HAST::IdN, IdD>>) -> Self {
        let hyperast = simple.hyperast;
        let kr = Decompressible {
            hyperast,
            decomp: &simple.basic,
        }
        .as_slice()
        .compute_kr_bitset();
        let simple = simple.decomp;
        let decomp = CompletePostOrder { simple, kr };
        Decompressible { hyperast, decomp }
    }
}
impl<HAST: HyperAST + Copy, IdD: PrimInt>
    From<Decompressible<HAST, super::lazy_post_order::LazyPostOrder<HAST::IdN, IdD>>>
    for Decompressible<HAST, CompletePostOrder<HAST::IdN, IdD>>
where
    HAST::IdN: Debug,
    IdD: PrimInt + super::Shallow<IdD> + Debug,
    for<'t> LendT<'t, HAST>: WithStats,
{
    fn from(
        value: Decompressible<HAST, super::lazy_post_order::LazyPostOrder<HAST::IdN, IdD>>,
    ) -> Self {
        let hyperast = value.hyperast;
        value.map(|x| x.complete(hyperast)).into()
    }
}

impl<IdN: Debug, IdD: PrimInt + Debug> Debug for CompletePostOrder<IdN, IdD> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CompletePostOrder")
            .field("simple", &self.simple)
            .field("kr", &self.kr)
            .finish()
    }
}

pub struct DisplayCompletePostOrder<'a, IdD, HAST, D> {
    inner: &'a D,
    stores: HAST,
    _phantom: PhantomData<&'a IdD>,
}

impl<'a, IdD, HAST, D> DisplayCompletePostOrder<'a, IdD, HAST, D> {
    pub fn new(stores: HAST, inner: &'a D) -> Self {
        Self {
            inner,
            stores,
            _phantom: PhantomData,
        }
    }
}

impl<IdD: PrimInt, HAST, D> Display for DisplayCompletePostOrder<'_, IdD, HAST, D>
where
    HAST: HyperAST + Copy,
    for<'t> LendT<'t, HAST>: WithSerialization,
    D: PostOrder<HAST, IdD> + FullyDecompressedTreeStore<HAST, IdD, IdD = IdD>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let m = super::pre_order_wrapper::SimplePreOrderMapper::from(self.inner);
        let m = super::pre_order_wrapper::DisplaySimplePreOrderMapper {
            inner: &m,
            stores: &self.stores,
        };
        Display::fmt(&m, f)
    }
}

impl<IdD: PrimInt, HAST, D> Debug for DisplayCompletePostOrder<'_, IdD, HAST, D>
where
    HAST: HyperAST + Copy,
    for<'t> LendT<'t, HAST>: WithSerialization,
    D: PostOrder<HAST, IdD> + FullyDecompressedTreeStore<HAST, IdD, IdD = IdD>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let m = super::pre_order_wrapper::SimplePreOrderMapper::from(self.inner);
        let mapper = &super::pre_order_wrapper::DisplaySimplePreOrderMapper {
            inner: &m,
            stores: &self.stores,
        };
        Debug::fmt(mapper, f)
    }
}

impl<'a, HAST: HyperAST + Copy, IdD: PrimInt> DecompressedParentsLending<'a, IdD>
    for Decompressible<HAST, CompletePostOrder<HAST::IdN, IdD>>
{
    type PIt =
        <Decompressible<HAST, SimplePostOrder<HAST::IdN, IdD>> as DecompressedParentsLending<
            'a,
            IdD,
        >>::PIt;
}

impl<HAST: HyperAST + Copy, IdD: PrimInt> DecompressedWithParent<HAST, IdD>
    for Decompressible<HAST, CompletePostOrder<HAST::IdN, IdD>>
{
    fn parent(&self, id: &IdD) -> Option<IdD> {
        self.as_simple().parent(id)
    }

    fn has_parent(&self, id: &IdD) -> bool {
        self.as_simple().has_parent(id)
    }

    fn position_in_parent<Idx: hyperast::PrimInt>(&self, c: &IdD) -> Option<Idx> {
        self.as_simple().position_in_parent(c)
    }

    fn parents(&self, id: IdD) -> <Self as DecompressedParentsLending<'_, IdD>>::PIt {
        let id_parent = &self.simple.id_parent;
        super::simple_post_order::IterParents { id, id_parent }
    }

    fn lca(&self, a: &IdD, b: &IdD) -> IdD {
        self.as_simple().lca(a, b)
    }

    fn path<Idx: hyperast::PrimInt>(&self, parent: &IdD, descendant: &IdD) -> Vec<Idx> {
        self.as_simple().path(parent, descendant)
    }
}

impl<HAST: HyperAST + Copy, IdD: PrimInt> DecompressedWithSiblings<HAST, IdD>
    for Decompressible<HAST, CompletePostOrder<HAST::IdN, IdD>>
{
    fn lsib(&self, x: &IdD) -> Option<IdD> {
        DecompressedWithSiblings::lsib(&self.as_simple(), x)
    }
}

pub struct IterParents<'a, IdD> {
    id: IdD,
    id_parent: &'a [IdD],
}

impl<IdD: PrimInt> Iterator for IterParents<'_, IdD> {
    type Item = IdD;

    fn next(&mut self) -> Option<Self::Item> {
        if self.id == cast(self.id_parent.len() - 1).unwrap() {
            return None;
        }
        let r = self.id_parent[self.id.to_usize().unwrap()];
        self.id = r;
        Some(r)
    }
}

impl<HAST: HyperAST + Copy, IdD: PrimInt> PostOrder<HAST, IdD>
    for Decompressible<HAST, CompletePostOrder<HAST::IdN, IdD>>
{
    fn lld(&self, i: &IdD) -> IdD {
        self.as_simple().lld(i)
    }

    fn tree(&self, id: &IdD) -> HAST::IdN {
        self.as_simple().tree(id)
    }

    fn has_children(&self, id: &IdD) -> bool {
        self.lld(id) != *id
    }
}

impl<HAST: HyperAST + Copy, IdD: PrimInt> PostOrderIterable<HAST, IdD>
    for Decompressible<HAST, CompletePostOrder<HAST::IdN, IdD>>
{
    type It = super::Iter<IdD>;
    fn iter_df_post<const ROOT: bool>(&self) -> super::Iter<IdD> {
        self.as_simple().as_basic().iter_df_post::<ROOT>()
    }
}

impl<'a, HAST: HyperAST + Copy, IdD: PrimInt> PostOrdKeyRoots<'a, HAST, IdD>
    for Decompressible<HAST, CompletePostOrder<HAST::IdN, IdD>>
{
    type Iter = super::IterKr<'a, IdD>;
}

impl<HAST: HyperAST + Copy, IdD: PrimInt> PostOrderKeyRoots<HAST, IdD>
    for Decompressible<HAST, CompletePostOrder<HAST::IdN, IdD>>
{
    fn iter_kr(&self) -> <Self as PostOrdKeyRoots<'_, HAST, IdD>>::Iter {
        super::IterKr(self.kr.iter_ones(), PhantomData)
    }
}

impl<HAST, IdD> hyperast::types::DecompressedFrom<HAST> for CompletePostOrder<HAST::IdN, IdD>
where
    IdD: PrimInt + Debug,
    HAST: HyperAST + Copy,
{
    type Out = Self;

    fn decompress(hyperast: HAST, root: &HAST::IdN) -> Self {
        let decomp = SimplePostOrder::decompress(hyperast, root);
        let r: Decompressible<_, _> = Decompressible { hyperast, decomp }.into();
        r.decomp
    }
}

impl<HAST: HyperAST + Copy, IdD: PrimInt> super::DecompressedSubtree<HAST::IdN>
    for Decompressible<HAST, CompletePostOrder<HAST::IdN, IdD>>
{
    type Out = Self;

    fn decompress(self, root: &HAST::IdN) -> Self {
        let hyperast = self.hyperast;
        let decomp = self.decomp.simple.basic;
        let id_parent = self.decomp.simple.id_parent;
        let basic = Decompressible { hyperast, decomp };
        let basic = basic.decompress(root);
        let basic = basic.decomp;
        let decomp = SimplePostOrder { basic, id_parent };
        Decompressible { hyperast, decomp }.into()
    }
}

impl<HAST: HyperAST + Copy, IdD: PrimInt> Decompressed<IdD>
    for Decompressible<HAST, CompletePostOrder<HAST::IdN, IdD>>
{
    type IdD = IdD;
}

impl<HAST: HyperAST + Copy, IdD: PrimInt> ShallowDecompressedTreeStore<HAST, IdD>
    for Decompressible<HAST, CompletePostOrder<HAST::IdN, IdD>>
{
    fn len(&self) -> usize {
        self.as_simple().len()
    }

    fn root(&self) -> IdD {
        cast(self.len() - 1).unwrap()
    }

    fn original(&self, id: &IdD) -> HAST::IdN {
        self.as_simple().original(id)
    }

    fn child(&self, x: &IdD, p: &[impl hyperast::PrimInt]) -> IdD {
        self.as_simple().child(x, p)
    }

    fn children(&self, x: &IdD) -> Vec<IdD> {
        self.as_simple().children(x)
    }
}

impl<HAST: HyperAST + Copy, IdD: PrimInt> DeepDecompressedTreeStore<HAST, IdD>
    for Decompressible<HAST, CompletePostOrder<HAST::IdN, IdD>>
{
    fn it_descendants(&self, x: &IdD) -> impl Iterator<Item = IdD> {
        self.as_simple().as_basic().iter_descendants(x)
    }

    fn first_descendant(&self, i: &IdD) -> IdD {
        self.as_simple().first_descendant(i)
    }

    fn descendants_count(&self, x: &IdD) -> usize {
        self.as_simple().descendants_count(x)
    }

    fn is_descendant(&self, desc: &IdD, of: &IdD) -> bool {
        self.as_simple().is_descendant(desc, of)
    }
}

// impl<HAST: HyperAST + Copy, IdD: PrimInt> FullyDecompressedTreeStore<HAST, IdD>
//     for Decompressible<HAST, CompletePostOrder<HAST::IdN, IdD>>
// {
// }

impl<'a, HAST: HyperAST + Copy, IdD: PrimInt> DecendantsLending<'a>
    for Decompressible<HAST, CompletePostOrder<HAST::IdN, IdD>>
{
    type Slice = CompletePOSlice<'a, HAST::IdN, IdD, &'a BitSlice>;
}

impl<'a, HAST: HyperAST + Copy, IdD: PrimInt> POSliceLending<'a, HAST, IdD, IdD>
    for Decompressible<HAST, CompletePostOrder<HAST::IdN, IdD>>
{
    type SlicePo = Decompressible<HAST, <Self as DecendantsLending<'a>>::Slice>;
}

impl<HAST: HyperAST + Copy, IdD: PrimInt> ContiguousDescendants<HAST, IdD>
    for Decompressible<HAST, CompletePostOrder<HAST::IdN, IdD>>
{
    fn descendants_range(&self, x: &IdD) -> std::ops::Range<IdD> {
        self.first_descendant(x)..*x
    }

    fn slice(&self, x: &IdD) -> <Self as DecendantsLending<'_>>::Slice {
        let range = self.as_simple().as_basic().slice_range(x);
        let hyperast = self.hyperast;
        let decomp = &self.simple;
        let simple = Decompressible { hyperast, decomp };
        CompletePOSlice {
            simple: simple._slice(x),
            kr: &self.kr[range],
        }
    }
}

impl<HAST: HyperAST + Copy, IdD: PrimInt> POBorrowSlice<HAST, IdD>
    for Decompressible<HAST, CompletePostOrder<HAST::IdN, IdD>>
{
    fn slice_po(&self, x: &IdD) -> <Self as POSliceLending<'_, HAST, IdD, IdD>>::SlicePo {
        let hyperast = self.hyperast;
        let decomp = self.slice(x);
        Decompressible { hyperast, decomp }
    }
}

#[allow(unused)]
pub struct RecCachedProcessor<'a, IdN, D, IdD: Hash + Eq, U, F, G> {
    pub(crate) ds: &'a D,
    root: IdN,
    cache: HashMap<IdD, U>,
    with_p: F,
    with_lsib: G,
}

#[allow(unused)]
impl<'a, IdN, D, IdD: PrimInt + Hash + Eq, U, F, G> From<(&'a D, IdN, F, G)>
    for RecCachedProcessor<'a, IdN, D, IdD, U, F, G>
{
    fn from((ds, root, with_p, with_lsib): (&'a D, IdN, F, G)) -> Self {
        Self {
            ds,
            root,
            cache: Default::default(),
            with_p,
            with_lsib,
        }
    }
}

impl<IdN, D, IdD: PrimInt + Hash + Eq, U: Clone + Default, F, G>
    RecCachedProcessor<'_, IdN, D, IdD, U, F, G>
where
    F: Fn(U, IdN) -> U,
    G: Fn(U, IdN) -> U,
{
    pub fn position<S>(&mut self, _store: S, _c: &IdD) -> &U
    where
        D: DeepDecompressedTreeStore<S, IdD> + DecompressedWithSiblings<S, IdD> + PostOrder<S, IdD>,
        S: for<'t> HyperAST<IdN = IdN> + Copy,
        IdN: Clone + Debug,
    {
        todo!()
        // look at previous attempts in the history or find alternative
    }
    pub fn position2(&mut self, _c: &IdD) -> &U {
        todo!()
        // look at previous attempts in the history or find alternative
    }
}

pub struct CompletePOSlice<'a, IdN, IdD, Kr: Borrow<BitSlice>> {
    pub(super) simple: SimplePOSlice<'a, IdN, IdD>,
    pub(super) kr: Kr,
}

impl<'a, IdN, IdD, Kr: Borrow<BitSlice>> Deref for CompletePOSlice<'a, IdN, IdD, Kr> {
    type Target = SimplePOSlice<'a, IdN, IdD>;

    fn deref(&self) -> &Self::Target {
        &self.simple
    }
}

impl<'a, HAST: HyperAST + Copy, IdD, Kr: Borrow<BitSlice>>
    Decompressible<HAST, CompletePOSlice<'a, HAST::IdN, IdD, Kr>>
{
    pub(crate) fn as_simple(&self) -> Decompressible<HAST, SimplePOSlice<'a, HAST::IdN, IdD>> {
        let hyperast = self.hyperast;
        let decomp = self.decomp.simple;
        Decompressible { hyperast, decomp }
    }
}

impl<'a, HAST: HyperAST + Copy, IdD, Kr: Borrow<BitSlice>>
    Decompressible<HAST, CompletePOSlice<'a, HAST::IdN, IdD, Kr>>
{
    pub(crate) fn as_basic(
        &self,
    ) -> Decompressible<HAST, super::basic_post_order::BasicPOSlice<'a, HAST::IdN, IdD>> {
        self.as_simple().as_basic()
    }
}

impl<HAST: HyperAST + Copy, IdD: PrimInt, Kr: Borrow<BitSlice>> Decompressed<IdD>
    for Decompressible<HAST, CompletePOSlice<'_, HAST::IdN, IdD, Kr>>
{
    type IdD = IdD;
}

impl<HAST: HyperAST + Copy, IdD: PrimInt, Kr: Borrow<BitSlice>>
    ShallowDecompressedTreeStore<HAST, IdD>
    for Decompressible<HAST, CompletePOSlice<'_, HAST::IdN, IdD, Kr>>
{
    fn len(&self) -> usize {
        self.as_basic().len()
    }

    fn root(&self) -> IdD {
        cast(self.len() - 1).unwrap()
    }
    fn original(&self, id: &IdD) -> HAST::IdN {
        self.as_basic().original(id)
    }

    fn child(&self, x: &IdD, p: &[impl hyperast::PrimInt]) -> IdD {
        self.as_basic().child(x, p)
    }

    fn children(&self, x: &IdD) -> Vec<IdD> {
        self.as_basic().children(x)
    }
}

impl<HAST: HyperAST + Copy, IdD: PrimInt, Kr: Borrow<BitSlice>> DeepDecompressedTreeStore<HAST, IdD>
    for Decompressible<HAST, CompletePOSlice<'_, HAST::IdN, IdD, Kr>>
{
    fn it_descendants(&self, x: &IdD) -> impl Iterator<Item = IdD> {
        self.as_basic().iter_descendants(x)
    }

    fn first_descendant(&self, i: &IdD) -> IdD {
        self.as_basic().first_descendant(i)
    }

    fn descendants_count(&self, x: &IdD) -> usize {
        self.as_basic().descendants_count(x)
    }

    fn is_descendant(&self, desc: &IdD, of: &IdD) -> bool {
        self.as_basic().is_descendant(desc, of)
    }
}

impl<HAST: HyperAST + Copy, IdD: PrimInt, Kr: Borrow<BitSlice>> PostOrder<HAST, IdD>
    for Decompressible<HAST, CompletePOSlice<'_, HAST::IdN, IdD, Kr>>
{
    fn lld(&self, i: &IdD) -> IdD {
        self.as_basic().lld(i)
    }

    fn tree(&self, id: &IdD) -> HAST::IdN {
        self.as_basic().tree(id)
    }

    fn has_children(&self, id: &IdD) -> bool {
        self.lld(id) != *id
    }
}

impl<'b, HAST: HyperAST + Copy, IdD: PrimInt, Kr: Borrow<BitSlice>> PostOrdKeyRoots<'b, HAST, IdD>
    for Decompressible<HAST, CompletePOSlice<'_, HAST::IdN, IdD, Kr>>
{
    type Iter = super::IterKr<'b, IdD>;
}

impl<HAST: HyperAST + Copy, IdD: PrimInt, Kr: Borrow<BitSlice>> PostOrderKeyRoots<HAST, IdD>
    for Decompressible<HAST, CompletePOSlice<'_, HAST::IdN, IdD, Kr>>
{
    fn iter_kr(&self) -> <Self as PostOrdKeyRoots<'_, HAST, IdD>>::Iter {
        super::IterKr(self.kr.borrow().iter_ones(), PhantomData)
    }
}
