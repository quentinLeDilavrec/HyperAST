//! Defines structures and implementations helping with the computing of
//! extra derived data to accelerate pattern matching and searching.

use std::fmt::Debug;
use std::hash::Hash;

use super::zipped_ts_extra::{FNode, Local};
use super::{AccWithExtra, Accumulator};
use super::{Extra, WithByteRange, WithExtra};
use crate::compat::HashMap;
use crate::store::nodes::legion::dyn_builder::EntityBuilder;
use crate::types::StoreRefAssoc;

/// Extra data for the pattern pre-computation step.
///
/// More should be an instance of `hyperast_tsquery::PreparedQuerying<&Query, TS, Acc>,`
pub struct PatternPrecompExtra<IdN, Acc, More> {
    md_cache: HashMap<IdN, PrecompQueries>,
    more: More,
    _phantom: std::marker::PhantomData<Acc>,
}

impl<IdN, Acc, More> From<More> for PatternPrecompExtra<IdN, Acc, More> {
    fn from(value: More) -> Self {
        Self {
            md_cache: HashMap::default(),
            more: value.into(),
            _phantom: std::marker::PhantomData,
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

impl<HAST, Acc, More> Extra<HAST, Acc> for PatternPrecompExtra<HAST::IdN, Acc, More>
where
    HAST: StoreRefAssoc,
    Acc: Accumulator<Node = FNode<Local>>,
    Acc: WithByteRange,
    // caching
    HAST::IdN: Hash,
    // more
    More: super::More<HAST, Acc = Acc>,
{
    type Acc = AccWithExtra<Acc, PrecompQueries>;

    type Node = <Self::Acc as Accumulator>::Node;

    fn from_cache(
        &mut self,
        id: HAST::IdN,
        or_else: impl FnOnce() -> <Acc as Accumulator>::Node,
    ) -> Self::Node {
        let precomp = self.md_cache.get(&id).cloned().unwrap_or_default();
        (or_else(), precomp).into()
    }

    fn extra(
        &mut self,
        stores: <HAST as StoreRefAssoc>::S<'_>,
        dyn_builder: &mut EntityBuilder,
        acc: &mut Self::Acc,
        label: Option<&str>,
    ) {
        use super::More;
        acc.extra().0 |=
            More::<HAST>::match_precomp_queries(&self.more, stores, &acc, label.as_deref());
        super::add_md_precomp_queries(dyn_builder, acc.extra().0);
    }

    fn to_cache(
        &mut self,
        id: HAST::IdN,
        node: <Acc as Accumulator>::Node,
        mut acc: Self::Acc,
    ) -> Self::Node {
        self.md_cache.insert(id, acc.extra().clone());
        (node, acc.extra().clone()).into()
    }
}
