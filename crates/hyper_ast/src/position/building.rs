//! Declares interfaces for position builders,
//! while offering state machine traits to orchestrate them statically.
//!
//! Converting positions is a frequent operation, so performances is a major concern.
//!
//! The state machines here are a "zero cost" abstraction, as they are completely monomorphized.
//!
//! With all these traits it is also easier to do multiple conversions with a single traversal,
//! with no impact to performances of "mono" convertions.
//!
//! If you want to implement your own position take inspiration from existing ones.
//! It is also possible to compose positions using [`CompoundPositionPreparer`], check at uses for examples.

pub trait Transition<O> {
    fn transit(self) -> O;
}
pub trait SetLen<IdO, O> {
    fn set(self, len: IdO) -> O;
}
pub trait ReceiveRows<T, O> {
    fn push(self, row: T) -> O;
}
pub trait ReceiveColumns<T, O> {
    fn push(self, col: T) -> O;
}
pub trait SetLineSpan<T, O> {
    fn set(self, lines: T) -> O;
}

pub mod top_down {
    use super::*;
    pub trait CreateBuilder<IdN> {
        fn create(root: IdN) -> Self;
    }
    pub trait ReceiveDirName<O> {
        fn push(self, dir_name: &str) -> O;
    }
    pub trait ReceiveIdx<Idx, O> {
        fn push(self, idx: Idx) -> O;
    }
    pub trait ReceiveIdxNoSpace<Idx, O> {
        fn push(self, idx: Idx) -> O;
    }
    pub trait ReceiveOffset<IdO, O> {
        fn push(self, bytes: IdO) -> O;
    }
    pub trait ReceiveParent<IdN, O> {
        fn push(self, parent: IdN) -> O;
    }
    pub trait SetNode<IdN, O> {
        fn set_node(self, node: IdN) -> O;
    }
    pub trait SetFileName<O> {
        fn set_file_name(self, file_name: &str) -> O;
    }

    pub trait ReceiveDir<IdN, Idx, O>:
        Sized
        + ReceiveParent<IdN, Self::SA1>
        + SetNode<IdN, O>
        + SetFileName<Self::SB1<O>>
        + Transition<Self::SB1<O>>
    {
        type SA1: ReceiveIdx<Idx, Self::SA2>;
        type SA2: ReceiveDirName<Self>;
        type SB1<OO>;
    }
    pub trait FileSysReceiver {
        type InFile<O>;
    }
    impl<IdN, Idx, O, T: FileSysReceiver> ReceiveDir<IdN, Idx, O> for T
    where
        T: ReceiveParent<IdN, T>
            + SetNode<IdN, O>
            + ReceiveIdx<Idx, T>
            + ReceiveDirName<T>
            + SetFileName<T::InFile<O>>
            + Transition<T::InFile<O>>,
    {
        type SA1 = T;
        type SA2 = T;
        type SB1<OO> = T::InFile<OO>;
    }

    pub trait ReceiveInFile<IdN, Idx, IdO, O>:
        Sized + ReceiveParent<IdN, Self::S1> + SetLen<IdO, Self::O0>
    {
        type S1: ReceiveIdx<Idx, Self::S2>;
        type S2: ReceiveOffset<IdO, Self::S3>;
        type S3: ReceiveIdxNoSpace<Idx, Self::S4>;
        type S4: ReceiveRows<IdO, Self>;

        type O0: SetLineSpan<IdO, Self::O1>;
        type O1: SetNode<IdN, O>;
    }

    impl<IdN, Idx, IdO, O, T> ReceiveInFile<IdN, Idx, IdO, O> for T
    where
        T: ReceiveParent<IdN, T>
            + SetNode<IdN, O>
            + ReceiveOffset<IdO, T>
            + ReceiveRows<IdO, T>
            // // TODO should not be possible to add rows after having added columns
            // + ReceiveColumns<IdO, T>
            + ReceiveIdx<Idx, T>
            + SetLen<IdO, T>
            + SetLineSpan<IdO, T>
            + ReceiveIdxNoSpace<Idx, T>,
    {
        type S1 = T;
        type S2 = T;
        type S3 = T;
        type S4 = T;

        type O0 = T;
        type O1 = T;
    }

    // Great bu try to fusion with `ReceiveInFile`s
    pub trait ReceiveInFileNoSpace<IdN, Idx, IdO, O>:
        Sized + ReceiveParent<IdN, Self::S1> + SetNode<IdN, Self::O0>
    {
        type S1: ReceiveIdx<Idx, Self::S2>;
        type S2: ReceiveOffset<IdO, Self>;
        type O0: SetLen<IdO, Self::O1>;
        type O1: Transition<O>;
    }
    impl<IdN, Idx, IdO, O, T> ReceiveInFileNoSpace<IdN, Idx, IdO, O> for T
    where
        T: ReceiveParent<IdN, T>
            + SetNode<IdN, T>
            + ReceiveOffset<IdO, T>
            + ReceiveIdx<Idx, T>
            + SetLen<IdO, T>
            + ReceiveIdxNoSpace<Idx, T>,
        T: Transition<O>,
    {
        type S1 = T;
        type S2 = T;

        type O0 = T;
        type O1 = T;
    }
}
pub mod bottom_up {
    use super::*;
    pub trait CreateBuilder {
        fn create() -> Self;
    }
    pub trait ReceiveDirName<O> {
        fn push(self, dir_name: &str) -> O;
    }
    pub trait ReceiveIdx<Idx, O> {
        fn push(self, idx: Idx) -> O;
    }
    pub trait ReceiveIdxNoSpace<Idx, O> {
        fn push(self, idx: Idx) -> O;
    }
    pub trait ReceiveOffset<IdO, O> {
        fn push(self, bytes: IdO) -> O;
    }
    pub trait ReceiveNode<IdN, O> {
        fn push(self, node: IdN) -> O;
    }
    pub trait SetRoot<IdN, O> {
        fn set_root(self, root: IdN) -> O;
    }
    pub trait FileSysReceiver {
        type InFile<O>;
    }

    pub trait ReceiveInFile<IdN, Idx, IdO, O>:
        Sized + SetLen<IdO, Self::SA0> + Transition<Self::SB1<O>>
    {
        type SA0: SetLineSpan<IdO, Self::SA1>;
        type SA1: ReceiveNode<IdN, Self::SA2> + ReceiveDirName<Self::SB1<O>> + SetRoot<IdN, O>;
        type SA2: ReceiveOffset<IdO, Self::SA3>;
        type SA3: ReceiveRows<IdO, Self::SA4>;
        type SA4: ReceiveColumns<IdO, Self::SA5>;
        type SA5: ReceiveIdx<Idx, Self::SA1>;
        type SB1<OO>;
    }
    impl<IdN, Idx, IdO, O, T: FileSysReceiver> ReceiveInFile<IdN, Idx, IdO, O> for T
    where
        T: ReceiveIdx<Idx, T>
            + ReceiveNode<IdN, T>
            + SetRoot<IdN, O>
            + ReceiveOffset<IdO, T>
            + ReceiveColumns<IdO, T>
            + ReceiveRows<IdO, T>
            // TODO should not be possible to add rows after having added columns
            + ReceiveIdx<Idx, T>
            + ReceiveDirName<T::InFile<O>>
            + SetLen<IdO, T>
            + SetLineSpan<IdO, T>,
        T: Transition<T::InFile<O>>,
        T: Transition<O>,
    {
        type SA0 = T;
        type SA1 = T;
        type SA2 = T;
        type SA3 = T;
        type SA4 = T;
        type SA5 = T;
        type SB1<OO> = T::InFile<O>;
    }
    pub trait ReceiveDir<IdN, Idx, O>:
        Sized + ReceiveNode<IdN, Self::S1> + SetRoot<IdN, O>
    {
        type S1: ReceiveIdx<Idx, Self>;
        type S2: ReceiveDirName<Self>;
    }
    impl<IdN, Idx, O, T> ReceiveDir<IdN, Idx, O> for T
    where
        T: ReceiveIdx<Idx, T> + ReceiveNode<IdN, T> + ReceiveDirName<T> + SetRoot<IdN, O>,
    {
        type S1 = T;
        type S2 = T;
    }
}

pub struct CompoundPositionPreparer<A, B>(pub A, pub B);

mod impl_c_p_p_receivers2 {

    use super::CompoundPositionPreparer;
    use super::Transition;
    use super::bottom_up;
    use super::top_down;
    use crate::PrimInt;

    impl<IdN: Copy, A: top_down::CreateBuilder<IdN>, B: top_down::CreateBuilder<IdN>>
        top_down::CreateBuilder<IdN> for CompoundPositionPreparer<A, B>
    {
        fn create(root: IdN) -> Self {
            Self(
                top_down::CreateBuilder::create(root),
                top_down::CreateBuilder::create(root),
            )
        }
    }

    impl<IdN: Copy, A: top_down::ReceiveParent<IdN, A>, B: top_down::ReceiveParent<IdN, B>>
        top_down::ReceiveParent<IdN, Self> for CompoundPositionPreparer<A, B>
    {
        fn push(self, parent: IdN) -> Self {
            Self(self.0.push(parent), self.1.push(parent))
        }
    }

    impl<A: top_down::ReceiveDirName<A>, B: top_down::ReceiveDirName<B>>
        top_down::ReceiveDirName<Self> for CompoundPositionPreparer<A, B>
    {
        fn push(self, dir_name: &str) -> Self {
            Self(self.0.push(dir_name), self.1.push(dir_name))
        }
    }

    impl<A: bottom_up::ReceiveDirName<A>, B: bottom_up::ReceiveDirName<B>>
        bottom_up::ReceiveDirName<Self> for CompoundPositionPreparer<A, B>
    {
        fn push(self, dir_name: &str) -> Self {
            Self(self.0.push(dir_name), self.1.push(dir_name))
        }
    }

    // impl<IdN, Idx: PrimInt, IdO: PrimInt, C> top_down::ReceiveIdx<Idx, Self> for CompoundPositionPreparer<IdN, Idx, IdO, C> {
    //     fn push(mut self, idx: Idx) -> Self {
    //         self.offsets.push(idx);
    //         self
    //     }
    // }

    impl<Idx: PrimInt, A: top_down::ReceiveIdx<Idx, A>, B: top_down::ReceiveIdx<Idx, B>>
        top_down::ReceiveIdx<Idx, Self> for CompoundPositionPreparer<A, B>
    {
        fn push(self, idx: Idx) -> Self {
            Self(self.0.push(idx), self.1.push(idx))
        }
    }

    // impl<IdN, Idx: PrimInt, IdO: PrimInt, C> top_down::ReceiveIdxNoSpace<Idx, Self> for CompoundPositionPreparer<IdN, Idx, IdO, C> {
    //     fn push(self, _idx: Idx) -> Self {
    //         //self.offsets.push(idx);
    //         self
    //     }
    // }

    impl<
        Idx: PrimInt,
        A: top_down::ReceiveIdxNoSpace<Idx, A>,
        B: top_down::ReceiveIdxNoSpace<Idx, B>,
    > top_down::ReceiveIdxNoSpace<Idx, Self> for CompoundPositionPreparer<A, B>
    {
        fn push(self, idx: Idx) -> Self {
            Self(self.0.push(idx), self.1.push(idx))
        }
    }

    impl<A, B> top_down::FileSysReceiver for CompoundPositionPreparer<A, B> {
        type InFile<O> = Self;
    }

    impl<IdO: PrimInt, A: top_down::ReceiveOffset<IdO, A>, B: top_down::ReceiveOffset<IdO, B>>
        top_down::ReceiveOffset<IdO, Self> for CompoundPositionPreparer<A, B>
    {
        fn push(self, bytes: IdO) -> Self {
            Self(self.0.push(bytes), self.1.push(bytes))
        }
    }
    impl<IdO: PrimInt, A: super::SetLen<IdO, A>, B: super::SetLen<IdO, B>> super::SetLen<IdO, Self>
        for CompoundPositionPreparer<A, B>
    {
        fn set(self, len: IdO) -> Self {
            Self(self.0.set(len), self.1.set(len))
        }
    }
    impl<T: PrimInt, A: super::SetLineSpan<T, A>, B: super::SetLineSpan<T, B>>
        super::SetLineSpan<T, Self> for CompoundPositionPreparer<A, B>
    {
        fn set(self, lines: T) -> Self {
            Self(self.0.set(lines), self.1.set(lines))
        }
    }
    impl<IdN: Copy, A: top_down::SetNode<IdN, A2>, B: top_down::SetNode<IdN, B2>, A2, B2>
        top_down::SetNode<IdN, CompoundPositionPreparer<A2, B2>>
        for CompoundPositionPreparer<A, B>
    {
        fn set_node(self, node: IdN) -> CompoundPositionPreparer<A2, B2> {
            CompoundPositionPreparer(self.0.set_node(node), self.1.set_node(node))
        }
    }

    impl<A: top_down::SetFileName<A>, B: top_down::SetFileName<B>> top_down::SetFileName<Self>
        for CompoundPositionPreparer<A, B>
    {
        fn set_file_name(self, file_name: &str) -> Self {
            Self(
                self.0.set_file_name(file_name),
                self.1.set_file_name(file_name),
            )
        }
    }

    impl<T: Copy, A: super::ReceiveRows<T, A>, B: super::ReceiveRows<T, B>>
        super::ReceiveRows<T, Self> for CompoundPositionPreparer<A, B>
    {
        fn push(self, row: T) -> Self {
            Self(self.0.push(row), self.1.push(row))
        }
    }

    impl<T: Copy, A: super::ReceiveColumns<T, A>, B: super::ReceiveColumns<T, B>>
        super::ReceiveColumns<T, Self> for CompoundPositionPreparer<A, B>
    {
        fn push(self, col: T) -> Self {
            Self(self.0.push(col), self.1.push(col))
        }
    }

    impl<A: Transition<A>, B: Transition<B>> Transition<Self> for CompoundPositionPreparer<A, B> {
        fn transit(self) -> Self {
            Self(self.0.transit(), self.1.transit())
        }
    }

    impl<A, B> Into<(A, B)> for CompoundPositionPreparer<A, B> {
        fn into(self) -> (A, B) {
            (self.0, self.1)
        }
    }
}

/// implements default receivers, i.e. just returning self
///
/// Do not hesitate to inline or split the impls if you have issues
macro_rules! default_impl_receivers {
    // almost look like a multi trait impl
    (impl<$($t:ident),*> $($rest:tt)*) => {
        crate::position::building::
        default_impl_receivers!{@rec1 impl<$($t),*> [] $($rest)*}
    };

    // # first part: the basic tt muncher collecting the traits and the struct $b

    // terminal tt muncher
    //   now that traits have been collected and $b is there
    //   actually call the macro which builds the impls
    (@rec1 impl<$($t:ident),*> [$($traits:tt)*] for $b:ty) => {
        crate::position::building::
        default_impl_receivers!{@trt impl<$($t),*> [$($traits)*] for $b}
    };
    // also terminal, but needed for odd numbers of tt due to next rule
    (@rec1 impl<$($t:ident),*> [$($traits:tt)*] $head1:tt for $b:ty) => {
        crate::position::building::
        default_impl_receivers!{@trt impl<$($t),*> [$($traits)* $head1] for $b}
    };
    // intermediate tt muncher (capture 2 tt to avoid hitting decl macro recursion limit)
    (@rec1 impl<$($t:ident),*> [$($traits:tt)*] $head1:tt $head2:tt $($rest:tt)*) => {
        crate::position::building::
        default_impl_receivers!{@rec1 impl<$($t),*> [$($traits)* $head1 $head2] $($rest)*}
    };

    // # second part: the specialized tt muncher creating the impls now that we have $b

    // terminal tt muncher
    (@trt impl<$($t:ident),*> [] for $b:ty) => {};

    (@trt impl<$($t:ident),*> [building::Transition<Self> $($traits:tt)*] for $b:ty) => {
        impl<$($t),*> building::Transition<Self> for $b {
            fn transit(self) -> Self { self }
        }
        crate::position::building::
        default_impl_receivers!{@trt impl<$($t),*> [$($traits)*] for $b}
    };
    (@trt impl<$($t:ident),*> [$(<$idn0:ident>)? bottom_up::ReceiveNode<$idn:ident, Self> $($traits:tt)*] for $b:ty) => {
        impl<$($t),* , $($idn0)?> bottom_up::ReceiveNode<$idn, Self> for $b {
            fn push(self, _node: $idn) -> Self { self }
        }
        crate::position::building::
        default_impl_receivers!{@trt impl<$($t),*> [$($traits)*] for $b}
    };
    (@trt impl<$($t:ident),*> [$(<$idn0:ident>)? bottom_up::SetRoot<$idn:ident, Self> $($traits:tt)*] for $b:ty) => {
        impl<$($t),* , $($idn0)?> bottom_up::SetRoot<$idn, Self> for $b {
            fn set_root(self, _root: $idn) -> Self { self }
        }
        crate::position::building::
        default_impl_receivers!{@trt impl<$($t),*> [$($traits)*] for $b}
    };
    (@trt impl<$($t:ident),*> [$(<$ido0:ident>)? building::ReceiveRows<$ido:ident, Self> $($traits:tt)*] for $b:ty) => {
        impl<$($t),* , $($ido0)?> building::ReceiveRows<$ido, Self> for $b {
            fn push(self, _row: $ido) -> Self { self }
        }
        crate::position::building::
        default_impl_receivers!{@trt impl<$($t),*> [$($traits)*] for $b}
    };
    (@trt impl<$($t:ident),*> [$(<$ido0:ident>)? building::ReceiveColumns<$ido:ident, Self> $($traits:tt)*] for $b:ty) => {
        impl<$($t),* , $($ido0)?> building::ReceiveColumns<$ido, Self> for $b {
            fn push(self, _col: $ido) -> Self { self }
        }
        crate::position::building::
        default_impl_receivers!{@trt impl<$($t),*> [$($traits)*] for $b}
    };
    (@trt impl<$($t:ident),*> [$(<$idn0:ident>)? top_down::ReceiveParent<$idn:ident, Self> $($traits:tt)*] for $b:ty) => {
        impl<$($t),* , $($idn0)?> top_down::ReceiveParent<$idn, Self> for $b {
            fn push(self, _parent: $idn) -> Self { self }
        }
        crate::position::building::
        default_impl_receivers!{@trt impl<$($t),*> [$($traits)*] for $b}
    };
    (@trt impl<$($t:ident),*> [top_down::ReceiveDirName<Self> $($traits:tt)*] for $b:ty) => {
        impl<$($t),*> top_down::ReceiveDirName<Self> for $b {
            fn push(self, _dir_name: &str) -> Self { self }
        }
        crate::position::building::
        default_impl_receivers!{@trt impl<$($t),*> [$($traits)*] for $b}
    };
    (@trt impl<$($t:ident),*> [bottom_up::ReceiveDirName<Self> $($traits:tt)*] for $b:ty) => {
        impl<$($t),*> bottom_up::ReceiveDirName<Self> for $b {
            fn push(self, _dir_name: &str) -> Self { self }
        }
        crate::position::building::
        default_impl_receivers!{@trt impl<$($t),*> [$($traits)*] for $b}
    };
    (@trt impl<$($t:ident),*> [$(<$idx0:ident>)? top_down::ReceiveIdx<$idx:ident, Self> $($traits:tt)*] for $b:ty) => {
        impl<$($t),* , $($idx0)?> top_down::ReceiveIdx<$idx, Self> for $b {
            fn push(self, _idx: $idx) -> Self { self }
        }
        crate::position::building::
        default_impl_receivers!{@trt impl<$($t),*> [$($traits)*] for $b}
    };
    (@trt impl<$($t:ident),*> [$(<$idx0:ident>)? top_down::ReceiveIdxNoSpace<$idx:ident, Self> $($traits:tt)*] for $b:ty) => {
        impl<$($t),* , $($idx0)?> top_down::ReceiveIdxNoSpace<$idx, Self> for $b {
            fn push(self, _idx: $idx) -> Self { self }
        }
        crate::position::building::
        default_impl_receivers!{@trt impl<$($t),*> [$($traits)*] for $b}
    };
    (@trt impl<$($t:ident),*> [$(<$idx0:ident>)? bottom_up::ReceiveIdx<$idx:ident, Self> $($traits:tt)*] for $b:ty) => {
        impl<$($t),* , $($idx0)?> bottom_up::ReceiveIdx<$idx, Self> for $b {
            fn push(self, _idx: $idx) -> Self { self }
        }
        crate::position::building::
        default_impl_receivers!{@trt impl<$($t),*> [$($traits)*] for $b}
    };
    (@trt impl<$($t:ident),*> [$(<$ido0:ident>)? top_down::ReceiveOffset<$ido:ident, Self> $($traits:tt)*] for $b:ty) => {
        impl<$($t),* , $($ido0)?> top_down::ReceiveOffset<$ido, Self> for $b {
            fn push(self, _bytes: $ido) -> Self { self }
        }
        crate::position::building::
        default_impl_receivers!{@trt impl<$($t),*> [$($traits)*] for $b}
    };
    (@trt impl<$($t:ident),*> [$(<$ido0:ident>)? bottom_up::ReceiveOffset<$ido:ident, Self> $($traits:tt)*] for $b:ty) => {
        impl<$($t),* , $($ido0)?> bottom_up::ReceiveOffset<$ido, Self> for $b {
            fn push(self, _offset: $ido) -> Self { self }
        }
        crate::position::building::
        default_impl_receivers!{@trt impl<$($t),*> [$($traits)*] for $b}
    };
    (@trt impl<$($t:ident),*> [top_down::SetFileName<Self> $($traits:tt)*] for $b:ty) => {
        impl<$($t),*> top_down::SetFileName<Self> for $b {
            fn set_file_name(self, _file_name: &str) -> Self { self }
        }
        crate::position::building::
        default_impl_receivers!{@trt impl<$($t),*> [$($traits)*] for $b}
    };
    (@trt impl<$($t:ident),*> [$(<$idn0:ident>)? top_down::SetNode<$idn:ident, Self> $($traits:tt)*] for $b:ty) => {
        impl<$($t),* $(, $idn0)?> top_down::SetNode<$idn, Self> for $b {
            fn set_node(self, _node: $idn) -> Self { self }
        }
        crate::position::building::
        default_impl_receivers!{@trt impl<$($t),*> [$($traits)*] for $b}
    };
    (@trt impl<$($t:ident),*> [$(<$ido0:ident>)? building::SetLen<$ido:ident, Self> $($traits:tt)*] for $b:ty) => {
        impl<$($t),* $(, $ido0)?> building::SetLen<$ido, Self> for $b {
            fn set(self, _len: $ido) -> Self { self }
        }
        crate::position::building::
        default_impl_receivers!{@trt impl<$($t),*> [$($traits)*] for $b}
    };
    (@trt impl<$($t:ident),*> [$(<$t0:ident>)? building::SetLineSpan<$t_:ident, Self> $($traits:tt)*] for $b:ty) => {
        impl<$($t),* $(, $t0)?> building::SetLineSpan<$t_, Self> for $b {
            fn set(self, _lines: $t_) -> Self { self }
        }
        crate::position::building::
        default_impl_receivers!{@trt impl<$($t),*> [$($traits)*] for $b}
    };
    (@trt impl<$($t:ident),*> [top_down::FileSysReceiver $($traits:tt)*] for $b:ty) => {
        impl<$($t),*> top_down::FileSysReceiver for $b {
            type InFile<O> = Self;
        }
        crate::position::building::
        default_impl_receivers!{@trt impl<$($t),*> [$($traits)*] for $b}
    };
    (@trt impl<$($t:ident),*> [bottom_up::FileSysReceiver $($traits:tt)*] for $b:ty) => {
        impl<$($t),*> bottom_up::FileSysReceiver for $b {
            type InFile<O> = Self;
        }
        crate::position::building::
        default_impl_receivers!{@trt impl<$($t),*> [$($traits)*] for $b}
    };
}

pub(crate) use default_impl_receivers;
