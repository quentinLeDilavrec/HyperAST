use crate::PrimInt;

use super::{position_accessors, tags};

/// Partial position materialized by offsets over children
///
/// Position conversion: cannot be converted alone, need to be rooted (top down) or fully resolved (bottom up)
#[derive(PartialEq, Eq, Clone)]
pub struct Offsets<Idx, Config = tags::TopDownFull> {
    /// offsets to go through a tree from top to bottom
    offsets: Vec<Idx>,
    _phantom: std::marker::PhantomData<Config>,
}

pub type RootedOffsets<IdN, Idx, Config = tags::TopDownFull> =
    super::rooted_wrapper::RootedWrapper<IdN, Offsets<Idx, Config>>;

impl<Idx: std::fmt::Debug, Config> std::fmt::Debug for Offsets<Idx, Config> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Offsets")
            .field("offsets", &format_args!("{:?}", self.offsets))
            .field("config", &std::any::type_name::<Config>())
            .finish()
    }
}

impl<Idx, C> From<Offsets<Idx, C>> for Vec<Idx> {
    fn from(val: Offsets<Idx, C>) -> Self {
        val.offsets
    }
}

impl<Idx> Offsets<Idx> {
    pub fn from_iterator(it: impl Iterator<Item = Idx>) -> Offsets<Idx> {
        Self {
            offsets: it.collect(),
            _phantom: Default::default(),
        }
    }
}

impl<Idx> Offsets<Idx, tags::TopDownNoSpace> {
    pub fn from_iterator_no_space(
        it: impl Iterator<Item = Idx>,
    ) -> Offsets<Idx, tags::TopDownNoSpace> {
        Self {
            offsets: it.collect(),
            _phantom: Default::default(),
        }
    }
}

impl<Idx> Offsets<Idx, tags::BottomUpFull> {
    pub fn from_iterator_bottom_up(
        it: impl Iterator<Item = Idx>,
    ) -> Offsets<Idx, tags::BottomUpFull> {
        Self {
            offsets: it.collect(),
            _phantom: Default::default(),
        }
    }
}

impl<Idx> Offsets<Idx, tags::BottomUpNoSpace> {
    pub fn from_iterator_bottom_up(
        it: impl Iterator<Item = Idx>,
    ) -> Offsets<Idx, tags::BottomUpNoSpace> {
        Self {
            offsets: it.collect(),
            _phantom: Default::default(),
        }
    }
}

impl<Idx> Offsets<Idx, tags::TopDownFull> {
    pub fn reverse(mut self) -> Offsets<Idx, tags::BottomUpFull> {
        self.offsets.reverse();
        Offsets {
            offsets: self.offsets,
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<Idx> Offsets<Idx, tags::BottomUpFull> {
    pub fn reverse(mut self) -> Offsets<Idx, tags::TopDownFull> {
        self.offsets.reverse();
        Offsets {
            offsets: self.offsets,
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<Idx, Config> Offsets<Idx, Config> {
    pub fn with_root<IdN>(self, root: IdN) -> super::rooted_wrapper::RootedWrapper<IdN, Self> {
        super::rooted_wrapper::RootedWrapper::new(root, self)
    }
}

impl<Idx> super::node_filter_traits::Full for Offsets<Idx, tags::TopDownFull> {}
impl<Idx> super::node_filter_traits::Full for Offsets<Idx, tags::BottomUpFull> {}

impl<Idx: PrimInt, C> position_accessors::WithOffsets for Offsets<Idx, C> {
    type Idx = Idx;
}

impl<Idx: PrimInt> position_accessors::WithPreOrderOffsets for Offsets<Idx, tags::TopDownFull> {
    type It<'b>
        = std::iter::Copied<std::slice::Iter<'b, Idx>>
    where
        Self: 'b,
        Idx: 'b;

    fn iter_offsets(&self) -> Self::It<'_> {
        self.offsets.iter().copied()
    }
}

impl<Idx: PrimInt> position_accessors::WithPreOrderOffsets for Offsets<Idx, tags::TopDownNoSpace> {
    type It<'b>
        = std::iter::Copied<std::slice::Iter<'b, Idx>>
    where
        Self: 'b,
        Idx: 'b;

    fn iter_offsets(&self) -> Self::It<'_> {
        self.offsets.iter().copied()
    }
}

impl<Idx: PrimInt> position_accessors::WithPreOrderOffsets for Offsets<Idx, tags::BottomUpFull> {
    type It<'b>
        = std::iter::Copied<std::iter::Rev<std::slice::Iter<'b, Idx>>>
    where
        Self: 'b,
        Idx: 'b;

    fn iter_offsets(&self) -> Self::It<'_> {
        self.offsets.iter().rev().copied()
    }
}

impl<Idx: PrimInt> position_accessors::WithPreOrderOffsets for Offsets<Idx, tags::BottomUpNoSpace> {
    type It<'b>
        = std::iter::Copied<std::iter::Rev<std::slice::Iter<'b, Idx>>>
    where
        Self: 'b,
        Idx: 'b;

    fn iter_offsets(&self) -> Self::It<'_> {
        self.offsets.iter().rev().copied()
    }
}

mod impl_receivers {
    use super::super::building;
    use super::Offsets;
    use super::tags;
    use crate::PrimInt;
    use building::bottom_up;
    use building::top_down;

    impl<Idx: PrimInt, IdN, C> top_down::CreateBuilder<IdN> for Offsets<Idx, C> {
        fn create(_root: IdN) -> Self {
            Self {
                offsets: vec![],
                _phantom: std::marker::PhantomData,
            }
        }
    }

    impl<Idx: PrimInt> top_down::ReceiveIdx<Idx, Self> for Offsets<Idx, tags::TopDownFull> {
        fn push(mut self, idx: Idx) -> Self {
            self.offsets.push(idx);
            self
        }
    }

    impl<Idx: PrimInt> top_down::ReceiveIdx<Idx, Self> for Offsets<Idx, tags::BottomUpFull> {
        fn push(mut self, idx: Idx) -> Self {
            self.offsets.insert(0, idx); // here to be compatible, we need to prepend
            self
        }
    }

    impl<Idx: PrimInt> bottom_up::ReceiveIdx<Idx, Self> for Offsets<Idx, tags::BottomUpFull> {
        fn push(mut self, idx: Idx) -> Self {
            self.offsets.push(idx);
            self
        }
    }

    impl<Idx: PrimInt> top_down::ReceiveIdx<Idx, Self> for Offsets<Idx, tags::TopDownNoSpace> {
        fn push(mut self, idx: Idx) -> Self {
            self.offsets.push(idx);
            self
        }
    }

    impl<Idx: PrimInt> top_down::ReceiveIdxNoSpace<Idx, Self> for Offsets<Idx, tags::TopDownFull> {
        fn push(self, _idx: Idx) -> Self {
            self
        }
    }

    impl<Idx: PrimInt> top_down::ReceiveIdxNoSpace<Idx, Self> for Offsets<Idx, tags::BottomUpFull> {
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

    impl<Idx: PrimInt> top_down::ReceiveIdxNoSpace<Idx, Self> for Offsets<Idx, tags::BottomUpNoSpace> {
        fn push(mut self, idx: Idx) -> Self {
            self.offsets.insert(0, idx); // here to be compatible, we need to prepend
            self
        }
    }

    building::default_impl_receivers! {
        impl<Idx, C>
            building::Transition<Self>
            <IdO> building::ReceiveRows<IdO, Self>
            <IdO> building::ReceiveColumns<IdO, Self>
            top_down::ReceiveDirName<Self>
            bottom_up::ReceiveDirName<Self>
            <IdN> top_down::ReceiveParent<IdN, Self>
            <IdO> top_down::ReceiveOffset<IdO, Self>
            <IdN> bottom_up::ReceiveNode<IdN, Self>
            <IdN> bottom_up::SetRoot<IdN, Self>
            top_down::SetFileName<Self>
            <IdO> bottom_up::ReceiveOffset<IdO, Self>
            <IdN> top_down::SetNode<IdN, Self>
            <IdO> building::SetLen<IdO, Self>
            <T> building::SetLineSpan<T, Self>
            top_down::FileSysReceiver
        for Offsets<Idx, C>
    }
}
