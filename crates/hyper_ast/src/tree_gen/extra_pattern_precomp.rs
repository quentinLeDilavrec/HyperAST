//! Defines structures and implementations helping with the computing of
//! extra derived data to accelerate pattern matching and searching.

use std::fmt::Debug;
use std::hash::Hash;
use std::marker::PhantomData;

use super::{AccWithExtra, Accumulator};
use super::{Extra, WithByteRange, WithExtra};
use crate::compat::HashMap;
use crate::store::nodes::GatherAttrErazed;
use crate::types::StoreRefAssoc;

/// Extra data for the pattern pre-computation step.
///
/// More should be an instance of `hyperast_tsquery::PreparedQuerying<&Query, TS, Acc>,`
pub struct PatternPrecompExtra<IdN, Acc, More> {
    pub md_cache: HashMap<IdN, PrecompQueries>,
    more: More,
    _phantom: PhantomData<Acc>,
}

impl<IdN, Acc, More> From<More> for PatternPrecompExtra<IdN, Acc, More> {
    fn from(more: More) -> Self {
        Self::with_cache(more, Default::default())
    }
}

impl<IdN, Acc, More> PatternPrecompExtra<IdN, Acc, More> {
    pub fn with_cache(more: More, md_cache: HashMap<IdN, PrecompQueries>) -> Self {
        Self {
            md_cache,
            more,
            _phantom: PhantomData,
        }
    }
}

#[derive(Default, Clone)]
pub struct PrecompQueries(pub u16);

impl Debug for PrecompQueries {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("PrecompQueries")
            .field(&format_args!("0b{:b}", self.0))
            .finish()
    }
}

impl std::ops::AddAssign for PrecompQueries {
    fn add_assign(&mut self, other: Self) {
        self.0 |= other.0;
    }
}

impl PrecompQueries {
    /// no need to store the full bitset, just consider we cannot skip if there is no precomputed value
    pub fn is_full(&self) -> bool {
        self.0 == u16::MAX
    }
    /// make a saturated bitset, which is equivalent to having no precomputed values.
    /// could be used as a niche value.
    pub fn full() -> Self {
        Self(u16::MAX)
    }
}

impl<HAST, Acc, More> Extra<HAST, Acc> for PatternPrecompExtra<HAST::IdN, Acc, More>
where
    HAST: StoreRefAssoc,
    Acc: Accumulator,
    Acc: WithByteRange,
    // caching
    HAST::IdN: Hash,
    // more
    More: super::More<HAST, Acc = Acc>,
{
    type Acc = AccWithExtra<Acc, PrecompQueries>;

    type Node = <Self::Acc as Accumulator>::Node;

    fn _from_cache(&mut self, id: <HAST>::IdN) -> Option<<Self::Acc as WithExtra>::Extra> {
        self.md_cache.get(&id).cloned()
    }

    fn from_cache(
        &mut self,
        id: HAST::IdN,
        or_else: impl FnOnce() -> <Acc as Accumulator>::Node,
    ) -> Self::Node {
        let precomp = self._from_cache(id).unwrap_or_default();
        (or_else(), precomp).into()
    }

    fn extra(
        &mut self,
        stores: <HAST as StoreRefAssoc>::S<'_>,
        dyn_builder: &mut impl GatherAttrErazed,
        mut acc: Self::Acc,
        label: Option<&str>,
    ) -> Self::Acc {
        use super::More;
        acc.extra().0 |=
            More::<HAST>::match_precomp_queries(&self.more, stores, &acc, label.as_deref());
        super::add_md_precomp_queries(dyn_builder, acc.extra().0);
        acc
    }

    fn _to_cache(
        &mut self,
        id: <HAST>::IdN,
        extra: <Self::Acc as WithExtra>::Extra,
    ) -> <Self::Acc as WithExtra>::Extra {
        self.md_cache.insert(id, extra.clone());
        extra
    }

    fn to_cache(
        &mut self,
        id: HAST::IdN,
        node: <Acc as Accumulator>::Node,
        acc: Self::Acc,
    ) -> Self::Node {
        let (_acc, extra) = acc.into();
        let extra = self._to_cache(id, extra);
        (node, extra).into()
    }
}
