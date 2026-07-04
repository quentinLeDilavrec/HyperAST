use crate::PrimInt;

use super::{position_accessors, tags};

pub struct Offsets<Idx, Config = tags::TopDownFull> {
    /// offsets to go through a tree from top to bottom
    offsets: Vec<Idx>,
    _phantom: std::marker::PhantomData<Config>,
}

impl<Idx, C> From<Offsets<Idx, C>> for Vec<Idx> {
    fn from(val: Offsets<Idx, C>) -> Self {
        val.offsets
    }
}

impl<Idx, C> Offsets<Idx, C> {
    pub fn from_iterator(it: impl Iterator<Item = Idx>) -> Offsets<Idx, C> {
        Self {
            offsets: it.collect(),
            _phantom: Default::default(),
        }
    }
}

impl<Idx> Offsets<Idx> {
    pub fn with_root<IdN>(self, root: IdN) -> RootedOffsets<IdN, Idx> {
        RootedOffsets {
            root,
            offsets: self.offsets,
        }
    }
}

pub struct OffsetsRef<'a, Idx, Config = tags::TopDownFull> {
    /// offsets to go through a tree from top to bottom
    offsets: &'a [Idx],
    _phantom: std::marker::PhantomData<Config>,
}
impl<'a, Idx> From<&'a [Idx]> for OffsetsRef<'a, Idx> {
    fn from(offsets: &'a [Idx]) -> Self {
        Self {
            offsets,
            _phantom: Default::default(),
        }
    }
}
impl<'a, Idx> OffsetsRef<'a, Idx> {
    pub fn with_root<IdN>(self, root: IdN) -> RootedOffsetsRef<'a, IdN, Idx> {
        RootedOffsetsRef {
            root,
            offsets: self.offsets,
        }
    }
}

pub struct RootedOffsets<IdN, Idx> {
    root: IdN,
    /// offsets to go through a tree from top to bottom
    offsets: Vec<Idx>,
}

impl<IdN, Idx> super::node_filter_traits::Full for RootedOffsets<IdN, Idx> {}

impl<IdN: Copy, Idx> position_accessors::RootedPosition<IdN> for RootedOffsets<IdN, Idx> {
    fn root(&self) -> IdN {
        self.root
    }
}
impl<IdN: Copy, Idx: PrimInt> position_accessors::WithOffsets for RootedOffsets<IdN, Idx> {
    type Idx = Idx;
}

impl<IdN: Copy, Idx: PrimInt> position_accessors::WithPreOrderOffsets for RootedOffsets<IdN, Idx> {
    type It<'b>
        = std::iter::Copied<std::slice::Iter<'b, Idx>>
    where
        Self: 'b,
        Idx: 'b;

    fn iter_offsets(&self) -> Self::It<'_> {
        self.offsets.iter().copied()
    }
}

impl<IdN: Copy, Idx: PrimInt> RootedOffsets<IdN, Idx> {
    pub fn with_store<'store, HAST>(
        &self,
        stores: &'store HAST,
    ) -> super::WithHyperAstPositionConverter<'store, '_, Self, HAST> {
        super::PositionConverter::new(self).with_stores(stores)
    }
}

// TODO try with a slice, i.e. without putting a ref on offsets slice
pub struct RootedOffsetsRef<'a, IdN, Idx> {
    root: IdN,
    /// offsets to go through a tree from top to bottom
    offsets: &'a [Idx],
}

impl<IdN, Idx> super::node_filter_traits::Full for RootedOffsetsRef<'_, IdN, Idx> {}

impl<IdN: Copy, Idx> position_accessors::RootedPosition<IdN> for RootedOffsetsRef<'_, IdN, Idx> {
    fn root(&self) -> IdN {
        self.root
    }
}
impl<IdN: Copy, Idx: PrimInt> position_accessors::WithOffsets for RootedOffsetsRef<'_, IdN, Idx> {
    type Idx = Idx;
}

impl<IdN: Copy, Idx: PrimInt> position_accessors::WithPreOrderOffsets
    for RootedOffsetsRef<'_, IdN, Idx>
{
    type It<'b>
        = std::iter::Copied<std::slice::Iter<'b, Idx>>
    where
        Self: 'b,
        Idx: 'b;

    fn iter_offsets(&self) -> Self::It<'_> {
        self.offsets.iter().copied()
    }
}

impl<'a, IdN: Copy, Idx: PrimInt> RootedOffsetsRef<'a, IdN, Idx> {
    pub fn with_store<'store, HAST>(
        &'a self,
        stores: &'store HAST,
    ) -> super::WithHyperAstPositionConverter<'store, 'a, Self, HAST> {
        super::PositionConverter::new(self).with_stores(stores)
    }
}

mod impl_receivers {
    use super::super::building;
    use super::Offsets;
    use super::tags;
    use crate::PrimInt;
    use building::top_down;

    impl<Idx: PrimInt, C> top_down::CreateBuilder for Offsets<Idx, C> {
        fn create() -> Self {
            Self {
                offsets: vec![],
                _phantom: std::marker::PhantomData,
            }
        }
    }

    impl<Idx: PrimInt> top_down::ReceiveIdx<Idx, Self> for Offsets<Idx, tags::TopDownNoSpace> {
        fn push(mut self, idx: Idx) -> Self {
            self.offsets.push(idx);
            self
        }
    }

    impl<Idx: PrimInt> top_down::ReceiveIdxNoSpace<Idx, Self> for Offsets<Idx> {
        fn push(self, _idx: Idx) -> Self {
            self
        }
    }

    impl<Idx: PrimInt> top_down::ReceiveIdxNoSpace<Idx, Self> for Offsets<Idx, tags::TopDownNoSpace> {
        fn push(mut self, idx: Idx) -> Self {
            self.offsets.push(idx);
            self
        }
    }

    building::default_impl_receivers! {
        impl<Idx, C>
            building::Transition<Self>
            <IdO> building::ReceiveRows<IdO, Self>
            <IdO> building::ReceiveColumns<IdO, Self>
            top_down::ReceiveDirName<Self>
            <IdN> top_down::ReceiveParent<IdN, Self>
            <IdO> top_down::ReceiveOffset<IdO, Self>
            top_down::SetFileName<Self>
            <IdN> top_down::SetNode<IdN, Self>
            <IdO> building::SetLen<IdO, Self>
            <T> building::SetLineSpan<T, Self>
            top_down::FileSysReceiver
        for Offsets<Idx, C>
    }
}
