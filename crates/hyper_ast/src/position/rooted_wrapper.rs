use crate::position::position_accessors;

/// Wraps a position with a root node, enabling conversion for positions without a root node
#[derive(PartialEq, Eq, Clone)]
pub struct RootedWrapper<IdN, P> {
    root: IdN,
    inner: P,
}

impl<IdN, P> RootedWrapper<IdN, P> {
    pub fn into_inner(self) -> P {
        self.inner
    }
    pub fn inner(&self) -> &P {
        &self.inner
    }
}

impl<IdN: std::fmt::Debug, P: std::fmt::Debug> std::fmt::Debug for RootedWrapper<IdN, P> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("RootedWrapper")
            .field("root", &self.root)
            .field("inner", &self.inner)
            .finish()
    }
}

impl<IdN, P: super::node_filter_traits::Full> super::node_filter_traits::Full
    for RootedWrapper<IdN, P>
{
}

impl<IdN, P: super::node_filter_traits::NoSpace> super::node_filter_traits::NoSpace
    for RootedWrapper<IdN, P>
{
}

impl<IdN: Copy, P> position_accessors::RootedPosition<IdN> for RootedWrapper<IdN, P> {
    fn root(&self) -> IdN {
        self.root
    }
}

impl<IdN, P: position_accessors::WithOffsets> position_accessors::WithOffsets
    for RootedWrapper<IdN, P>
{
    type Idx = P::Idx;
}

impl<IdN, P: position_accessors::WithPreOrderOffsets> position_accessors::WithPreOrderOffsets
    for RootedWrapper<IdN, P>
{
    type It<'b>
        = <P as position_accessors::WithPreOrderOffsets>::It<'b>
    where
        Self: 'b;

    fn iter_offsets(&self) -> Self::It<'_> {
        self.inner.iter_offsets()
    }
}

impl<IdN, P> RootedWrapper<IdN, P> {
    pub fn with_store<'store, HAST>(
        &self,
        stores: &'store HAST,
    ) -> super::WithHyperAstPositionConverter<'store, '_, Self, HAST> {
        super::PositionConverter::new(self).with_stores(stores)
    }

    pub fn map<P2>(self, f: impl FnOnce(P) -> P2) -> RootedWrapper<IdN, P2> {
        RootedWrapper {
            root: self.root,
            inner: f(self.inner),
        }
    }

    pub fn new(root: IdN, inner: P) -> Self {
        RootedWrapper { root, inner }
    }
}

use super::building;
use building::top_down;

impl<IdN: Clone, P: top_down::CreateBuilder<IdN>> top_down::CreateBuilder<IdN>
    for RootedWrapper<IdN, P>
{
    fn create(root: IdN) -> Self {
        Self {
            root: root.clone(),
            inner: P::create(root),
        }
    }
}

impl<IdN, Idx, P: top_down::ReceiveIdx<Idx, O>, O> top_down::ReceiveIdx<Idx, RootedWrapper<IdN, O>>
    for RootedWrapper<IdN, P>
{
    fn push(self, idx: Idx) -> RootedWrapper<IdN, O> {
        self.map(|inner| inner.push(idx))
    }
}

impl<IdN, Idx, P: top_down::ReceiveIdxNoSpace<Idx, O>, O>
    top_down::ReceiveIdxNoSpace<Idx, RootedWrapper<IdN, O>> for RootedWrapper<IdN, P>
{
    fn push(self, idx: Idx) -> RootedWrapper<IdN, O> {
        self.map(|inner| inner.push(idx))
    }
}

impl<IdN, P: building::Transition<O>, O> building::Transition<RootedWrapper<IdN, O>>
    for RootedWrapper<IdN, P>
{
    fn transit(self) -> RootedWrapper<IdN, O> {
        self.map(|inner| inner.transit())
    }
}
impl<IdN, P: building::ReceiveRows<IdO, O>, O, IdO>
    building::ReceiveRows<IdO, RootedWrapper<IdN, O>> for RootedWrapper<IdN, P>
{
    fn push(self, row: IdO) -> RootedWrapper<IdN, O> {
        self.map(|inner| inner.push(row))
    }
}
impl<IdN, P: building::ReceiveColumns<IdO, O>, O, IdO>
    building::ReceiveColumns<IdO, RootedWrapper<IdN, O>> for RootedWrapper<IdN, P>
{
    fn push(self, col: IdO) -> RootedWrapper<IdN, O> {
        self.map(|inner| inner.push(col))
    }
}
impl<IdN, P: top_down::ReceiveDirName<O>, O> top_down::ReceiveDirName<RootedWrapper<IdN, O>>
    for RootedWrapper<IdN, P>
{
    fn push(self, dir_name: &str) -> RootedWrapper<IdN, O> {
        self.map(|inner| inner.push(dir_name))
    }
}
impl<IdN, P: top_down::ReceiveParent<IdN, O>, O> top_down::ReceiveParent<IdN, RootedWrapper<IdN, O>>
    for RootedWrapper<IdN, P>
{
    fn push(self, parent: IdN) -> RootedWrapper<IdN, O> {
        self.map(|inner| inner.push(parent))
    }
}
impl<IdN, P: top_down::ReceiveOffset<IdO, O>, O, IdO>
    top_down::ReceiveOffset<IdO, RootedWrapper<IdN, O>> for RootedWrapper<IdN, P>
{
    fn push(self, bytes: IdO) -> RootedWrapper<IdN, O> {
        self.map(|inner| inner.push(bytes))
    }
}
impl<IdN, P: top_down::SetFileName<O>, O> top_down::SetFileName<RootedWrapper<IdN, O>>
    for RootedWrapper<IdN, P>
{
    fn set_file_name(self, file_name: &str) -> RootedWrapper<IdN, O> {
        self.map(|inner| inner.set_file_name(file_name))
    }
}
impl<IdN, P: top_down::SetNode<IdN, O>, O> top_down::SetNode<IdN, RootedWrapper<IdN, O>>
    for RootedWrapper<IdN, P>
{
    fn set_node(self, node: IdN) -> RootedWrapper<IdN, O> {
        self.map(|inner| inner.set_node(node))
    }
}
impl<IdN, P: building::SetLen<IdO, O>, O, IdO> building::SetLen<IdO, RootedWrapper<IdN, O>>
    for RootedWrapper<IdN, P>
{
    fn set(self, len: IdO) -> RootedWrapper<IdN, O> {
        self.map(|inner| inner.set(len))
    }
}
impl<IdN, P: building::SetLineSpan<T, O>, O, T> building::SetLineSpan<T, RootedWrapper<IdN, O>>
    for RootedWrapper<IdN, P>
{
    fn set(self, lines: T) -> RootedWrapper<IdN, O> {
        self.map(|inner| inner.set(lines))
    }
}
impl<IdN, P: top_down::FileSysReceiver> top_down::FileSysReceiver for RootedWrapper<IdN, P> {
    type InFile<O> = Self;
}
