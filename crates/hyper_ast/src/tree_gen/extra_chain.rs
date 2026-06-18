//! Defines structures and implementations composing independent extras.
//! You have to implement [`Extra`] on your own composing accumulator if you want interdependence.

use std::fmt::Debug;
use std::ops::AddAssign;

use super::{AccWithExtra, Accumulator};
use super::{Extra, WithByteRange, WithExtra};
use crate::store::nodes::GatherAttrErazed;
use crate::types::StoreRefAssoc;

pub struct ChainedExtra<A, B>(pub A, pub B);

impl<A, B> From<(A, B)> for ChainedExtra<A, B> {
    fn from((a, b): (A, B)) -> Self {
        Self(a, b)
    }
}

#[derive(Default, Clone)]
pub struct DerivedDataPair<A, B>(pub A, pub B);

impl<A: Debug, B: Debug> Debug for DerivedDataPair<A, B> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("DerivedDataPair")
            .field(&self.0)
            .field(&self.1)
            .finish()
    }
}

impl<A: AddAssign, B: AddAssign> AddAssign for DerivedDataPair<A, B> {
    fn add_assign(&mut self, other: Self) {
        self.0 += other.0;
        self.1 += other.1;
    }
}

impl<HAST, Acc, A, B> Extra<HAST, Acc> for ChainedExtra<A, B>
where
    HAST: StoreRefAssoc,
    Acc: Accumulator,
    Acc: WithByteRange,
    //
    A: Extra<HAST, Acc>,
    B: Extra<HAST, Acc>,
    <<A as Extra<HAST, Acc>>::Acc as WithExtra>::Extra: AddAssign + Default,
    <<B as Extra<HAST, Acc>>::Acc as WithExtra>::Extra: AddAssign + Default,
    //
    HAST::IdN: Copy,
{
    type Acc = AccWithExtra<
        Acc,
        DerivedDataPair<
            <<A as Extra<HAST, Acc>>::Acc as WithExtra>::Extra,
            <<B as Extra<HAST, Acc>>::Acc as WithExtra>::Extra,
        >,
    >;

    type Node = <Self::Acc as Accumulator>::Node;

    fn _from_cache(&mut self, id: HAST::IdN) -> Option<<Self::Acc as WithExtra>::Extra> {
        Some(DerivedDataPair(
            self.0._from_cache(id).unwrap_or_default(),
            self.1._from_cache(id).unwrap_or_default(),
        ))
    }

    fn from_cache(
        &mut self,
        id: HAST::IdN,
        or_else: impl FnOnce() -> <Acc as Accumulator>::Node,
    ) -> Self::Node {
        // // use the _from_cache
        // (or_else(), self._from_cache(id).unwrap_or_default()).into()
        // give to the second extra the opportunity to cache `Acc::Node`
        let a = self.0._from_cache(id).unwrap_or_default();
        let (n, b) = self.1.from_cache(id, or_else).into();
        (n, DerivedDataPair(a, b)).into()
    }

    fn extra(
        &mut self,
        stores: <HAST as StoreRefAssoc>::S<'_>,
        dyn_builder: &mut impl GatherAttrErazed,
        acc: Self::Acc,
        label: Option<&str>,
    ) -> Self::Acc {
        let AccWithExtra(acc, DerivedDataPair(a, b)) = acc;
        let acc = (acc, a).into();
        let acc = Extra::<HAST, _>::extra(&mut self.0, stores, dyn_builder, acc, label.clone());
        let (acc, a) = acc.into();
        let acc = (acc, b).into();
        let acc = Extra::<HAST, _>::extra(&mut self.1, stores, dyn_builder, acc, label.clone());
        let (acc, b) = acc.into();
        let acc = AccWithExtra(acc, DerivedDataPair(a, b));
        acc
    }

    fn _to_cache(
        &mut self,
        id: <HAST>::IdN,
        extra: <Self::Acc as WithExtra>::Extra,
    ) -> <Self::Acc as WithExtra>::Extra {
        let DerivedDataPair(a, b) = extra;
        let a = self.0._to_cache(id, a);
        let b = self.1._to_cache(id, b);
        DerivedDataPair(a, b)
    }

    fn to_cache(
        &mut self,
        id: HAST::IdN,
        node: <Acc as Accumulator>::Node,
        acc: Self::Acc,
    ) -> Self::Node {
        let AccWithExtra(acc, DerivedDataPair(a, b)) = acc;
        let a = self.0._to_cache(id, a);
        let acc: B::Acc = (acc, b).into();
        let (n, b) = self.1.to_cache(id, node, acc).into();
        let extra = DerivedDataPair(a, b);
        (n, extra).into()
    }
}
