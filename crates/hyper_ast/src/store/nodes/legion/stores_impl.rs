//! ugly impls without logic

// TODO find a way to factorize some bounds and organize impls better

use crate::store::SimpleStores;
use crate::store::defaults::LabelIdentifier;
use crate::types::TypeStore;
use crate::types::TypeTrait;
use crate::types::TypedLending;
use crate::types::TypedNodeId;
use crate::types::UniformNodeId;
use crate::types::{AstLending, StoreLending, StoreRefAssoc};
use crate::types::{HyperAST, HyperASTShared, TypedHyperAST};
use crate::types::{LStore, LabelStore};
use crate::types::{LendN, NLending, NStore, NStoreRefAssoc, NodeStore};
use crate::types::{Tree, Typed};

impl<TS, NS, LS> HyperASTShared for SimpleStores<TS, NS, LS>
where
    NS: NStore,
    NS: NodeStore<<NS as NStore>::IdN>,
    LS: LStore,
    <NS as NStore>::IdN: UniformNodeId,
{
    type IdN = NS::IdN;
    type Idx = NS::Idx;
    type Label = LS::I;
}

impl<'a, TS, NS, LS> NLending<'a, <NS as NStore>::IdN> for SimpleStores<TS, NS, LS>
where
    NS: NStore,
    NS: NodeStore<<NS as NStore>::IdN>,
    LS: LStore,
    <NS as NStore>::IdN: UniformNodeId,
{
    type N = <NS as NLending<'a, <NS as NStore>::IdN>>::N;
}

impl<'a, TS, NS, LS> AstLending<'a> for SimpleStores<TS, NS, LS>
where
    NS: NStore,
    NS: NodeStore<<NS as NStore>::IdN>,
    LS: LStore,
    <NS as NStore>::IdN: UniformNodeId,
    for<'t> LendN<'t, NS, <NS as NStore>::IdN>: Tree<
            Label = <LS as LStore>::I,
            TreeId = <NS as NStore>::IdN,
            ChildIdx = <NS as NStore>::Idx,
        >,
{
    type RT = <NS as NLending<'a, Self::IdN>>::N;
}

impl<TS, NS, LS> HyperASTShared for &SimpleStores<TS, NS, LS>
where
    NS: NStore,
    NS: NodeStore<<NS as NStore>::IdN>,
    LS: LStore,
    <NS as NStore>::IdN: UniformNodeId,
{
    type IdN = NS::IdN;
    type Idx = NS::Idx;
    type Label = LS::I;
}

impl<'a, TS, NS, LS> NLending<'a, <NS as NStore>::IdN> for &SimpleStores<TS, NS, LS>
where
    NS: NStore,
    NS: NodeStore<<NS as NStore>::IdN>,
    LS: LStore,
    <NS as NStore>::IdN: UniformNodeId,
    for<'t> LendN<'t, NS, <NS as NStore>::IdN>: Tree<
            Label = <LS as LStore>::I,
            TreeId = <NS as NStore>::IdN,
            ChildIdx = <NS as NStore>::Idx,
        >,
{
    type N = <NS as NLending<'a, <NS as NStore>::IdN>>::N;
}

impl<'a, TS, NS, LS> AstLending<'a> for &SimpleStores<TS, NS, LS>
where
    NS: NStore,
    NS: NodeStore<<NS as NStore>::IdN>,
    LS: LStore,
    <NS as NStore>::IdN: UniformNodeId,
    for<'t> LendN<'t, NS, <NS as NStore>::IdN>: Tree<
            Label = <LS as LStore>::I,
            TreeId = <NS as NStore>::IdN,
            ChildIdx = <NS as NStore>::Idx,
        >,
{
    type RT = <NS as NLending<'a, <NS as NStore>::IdN>>::N;
}

impl<TS, NS, LS> HyperAST for SimpleStores<TS, NS, LS>
where
    TS: TypeStore,
    NS: NStore,
    <NS as NStore>::IdN: UniformNodeId,
    NS: NodeStore<NS::IdN>,
    LS: LStore,
    LS: LabelStore<str, I = <LS as LStore>::I>,
    for<'t> LendN<'t, NS, <NS as NStore>::IdN>: Tree<
            Label = <LS as LStore>::I,
            TreeId = <NS as NStore>::IdN,
            ChildIdx = <NS as NStore>::Idx,
        >,
{
    type NS = NS;

    fn node_store(&self) -> &Self::NS {
        &self.node_store
    }

    type LS = LS;

    fn label_store(&self) -> &Self::LS {
        &self.label_store
    }

    type TS = TS;
}
impl<'a, Ty, TS, NS, LS> TypedLending<'a, Ty> for SimpleStores<TS, NS, LS>
where
    NS: NStore,
    NS: NodeStore<<NS as NStore>::IdN>,
    LS: LStore,
    <NS as NStore>::IdN: UniformNodeId,
    for<'t> LendN<'t, NS, <NS as NStore>::IdN>: Tree<
            Label = <LS as LStore>::I,
            TreeId = <NS as NStore>::IdN,
            ChildIdx = <NS as NStore>::Idx,
        >,
    Ty: TypeTrait,
{
    type TT = TypedHolder<<Self as AstLending<'a>>::RT, Ty>;
}

pub struct TypedHolder<RT, Ty> {
    pub rt: RT,
    _p: std::marker::PhantomData<Ty>,
}

impl<RT, Ty> std::ops::Deref for TypedHolder<RT, Ty> {
    type Target = RT;
    fn deref(&self) -> &Self::Target {
        &self.rt
    }
}

impl<RT, Ty: TypeTrait> Typed for TypedHolder<RT, Ty> {
    type Type = Ty;
    fn get_type(&self) -> Self::Type {
        todo!()
    }
}

impl<TS, NS, LS> HyperAST for &SimpleStores<TS, NS, LS>
where
    TS: TypeStore,
    NS: NStore,
    <NS as NStore>::IdN: UniformNodeId,
    NS: NodeStore<NS::IdN>,
    LS: LStore,
    LS: LabelStore<str, I = <LS as LStore>::I>,
    for<'t> LendN<'t, NS, <NS as NStore>::IdN>: Tree<
            Label = <LS as LStore>::I,
            TreeId = <NS as NStore>::IdN,
            ChildIdx = <NS as NStore>::Idx,
        >,
{
    type NS = NS;

    fn node_store(&self) -> &Self::NS {
        &self.node_store
    }

    type LS = LS;

    fn label_store(&self) -> &Self::LS {
        &self.label_store
    }

    type TS = TS;
}

impl<TIdN: TypedNodeId<IdN = Self::IdN>, TS, NS, LS> TypedHyperAST<TIdN>
    for SimpleStores<TS, NS, LS>
where
    TIdN::Ty: TypeTrait,
    TS: TypeStore,
    NS: NStore,
    <NS as NStore>::IdN: UniformNodeId,
    NS: NodeStore<NS::IdN>,
    LS: LStore,
    LS: LabelStore<str, I = <LS as LStore>::I>,
    for<'t> LendN<'t, NS, <NS as NStore>::IdN>: Tree<
            Label = <LS as LStore>::I,
            TreeId = <NS as NStore>::IdN,
            ChildIdx = <NS as NStore>::Idx,
        >,
{
    fn try_resolve(
        &self,
        _id: &Self::IdN,
    ) -> Option<(
        <Self as TypedLending<'_, <TIdN as TypedNodeId>::Ty>>::TT,
        TIdN,
    )> {
        todo!()
    }

    fn try_typed(&self, _id: &Self::IdN) -> Option<TIdN> {
        todo!()
    }

    fn resolve_typed(
        &self,
        _id: &TIdN,
    ) -> <Self as TypedLending<'_, <TIdN as TypedNodeId>::Ty>>::TT {
        todo!()
    }
}

impl<'a, TS, NS> StoreLending<'a> for SimpleStores<TS, NS>
where
    TS: Copy + TypeStore,
    NS: NStore,
    NS: NodeStore<NS::IdN>,
    NS::IdN: UniformNodeId,
    for<'t> LendN<'t, NS, NS::IdN>:
        Tree<Label = LabelIdentifier, TreeId = NS::IdN, ChildIdx = NS::Idx>,
    NS: NStoreRefAssoc,
    &'a NS::S: NStore<IdN = NS::IdN, Idx = NS::Idx>,
    &'a NS::S: NodeStore<NS::IdN>,
    for<'t> &'a NS::S: NLending<'t, NS::IdN, N = <NS as NLending<'t, NS::IdN>>::N>,
{
    type S = SimpleStores<TS, &'a NS::S, &'a crate::store::labels::LabelStore>;
}

impl<TS, NS> StoreRefAssoc for SimpleStores<TS, NS>
where
    TS: Copy + TypeStore,
    NS: NStore,
    for<'a> NS: 'a,
    NS: NodeStore<NS::IdN>,
    NS::IdN: UniformNodeId,
    for<'t> LendN<'t, NS, NS::IdN>:
        Tree<Label = LabelIdentifier, TreeId = NS::IdN, ChildIdx = NS::Idx>,
    NS: NStoreRefAssoc,
    for<'a> &'a NS::S: NStore<IdN = NS::IdN, Idx = NS::Idx>,
    for<'a> &'a NS::S: NodeStore<NS::IdN>,
    for<'a, 't> &'a NS::S: NLending<'t, NS::IdN, N = <NS as NLending<'t, NS::IdN>>::N>,
{
    type S<'a> = SimpleStores<TS, &'a NS::S, &'a crate::store::labels::LabelStore>;
}
