#![allow(unused)]
use num::ToPrimitive;
use std::cmp::Ordering;
use std::collections::{BTreeMap, HashSet};
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};

use hyperast::position;
use hyperast::position::offsets_and_nodes::SolvedStructuralPosition;
use hyperast::position::position_accessors::{SolvedPosition, WithPreOrderOffsets};
use hyperast::store::SimpleStores;
use hyperast::store::defaults::NodeIdentifier;
use hyperast::types;
use hyperast::types::{Children, HashKind, TypedNodeId};
use hyperast::types::{HyperAST, RoleStore, TypeStore};
use hyperast::types::{WithHashs, WithSerialization, WithStats};

use hyperast_tsquery::{Cursor, Node as _};

use crate::auto::tsq_ser_meta::Converter;
use crate::auto::tsq_transform;
use crate::search::try_ts_query;

// use crate::legion as qgen; // includes indentation, such as spaces and new lines
use crate::no_fmt_legion as qgen; // ignores spaces, new lines,...

type QStore = SimpleStores<crate::types::TStore>;

type IdN = NodeIdentifier;
type Idx = u16;
type IdInit = SolvedStructuralPosition<IdN, Idx>;

#[derive(enumset::EnumSetType, Debug)]
pub enum TrMarker {
    Init,
    Uniqs,
    RMs,
    RMall,
    SimpEQ,
    Focus,
}

impl std::fmt::Display for TrMarker {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_static_str())
    }
}

impl TrMarker {
    fn as_static_str(&self) -> &'static str {
        match self {
            TrMarker::Init => "Init",
            TrMarker::Uniqs => "Uniqs",
            TrMarker::RMs => "RMs",
            TrMarker::RMall => "RMall",
            TrMarker::SimpEQ => "SimpEQ",
            TrMarker::Focus => "Focus",
        }
    }
}

pub type TrMarkers = enumset::EnumSet<TrMarker>;

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum TR<E = IdInit, I = IdNQ> {
    // WARN different NodeIdentifier, this one is referring to the provided examples
    Init(E),
    Uniqs(I),
    RMs(I),
    RMall(I),
    SimpEQ(I),
    Focus(I),
}

impl<E: PartialEq, I: PartialEq> PartialOrd for TR<E, I> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (TR::Init(_), TR::Init(_)) => Some(Ordering::Equal),
            (TR::Init(_), _) => Some(Ordering::Less),
            (TR::Uniqs(_), TR::Uniqs(_)) => Some(Ordering::Equal),
            (TR::Uniqs(_), _) => Some(Ordering::Less),
            (TR::RMall(_), TR::RMall(_)) => Some(Ordering::Equal),
            (TR::RMall(_), _) => Some(Ordering::Less),
            (TR::RMs(_), TR::RMs(_)) => Some(Ordering::Equal),
            (TR::RMs(_), _) => Some(Ordering::Less),
            (TR::SimpEQ(_), TR::SimpEQ(_)) => Some(Ordering::Equal),
            (TR::SimpEQ(_), _) => Some(Ordering::Less),
            (TR::Focus(_), TR::Focus(_)) => Some(Ordering::Equal),
            (TR::Focus(_), _) => Some(Ordering::Less),
            // _ => self.eq(other).then(|| Ordering::Equal),
        }
    }
}

impl<E, I> TR<E, I> {
    pub fn each_str(
        &self,
        mut f: impl FnMut(&'static str, &E),
        mut g: impl FnMut(&'static str, &I),
    ) {
        self._each(|m, x| match x {
            Ok(t) => f(m.as_static_str(), t),
            Err(t) => g(m.as_static_str(), t),
        })
    }
    pub fn each(&self, mut f: impl FnMut(TrMarker, &E), mut g: impl FnMut(TrMarker, &I)) {
        self._each(|m, x| match x {
            Ok(t) => f(m, t),
            Err(t) => g(m, t),
        })
    }
    pub fn _each(&self, mut f: impl FnMut(TrMarker, Result<&E, &I>)) {
        match self {
            TR::Init(t) => f(TrMarker::Init, Ok(t)),
            TR::Uniqs(t) => f(TrMarker::Uniqs, Err(t)),
            TR::RMall(t) => f(TrMarker::RMall, Err(t)),
            TR::RMs(t) => f(TrMarker::RMs, Err(t)),
            TR::SimpEQ(t) => f(TrMarker::SimpEQ, Err(t)),
            TR::Focus(t) => f(TrMarker::Focus, Err(t)),
        }
    }

    pub fn as_init(&self) -> Option<&E> {
        match self {
            Self::Init(c) => Some(c),
            _ => None,
        }
    }
    pub fn no_init(&self) -> Option<&I> {
        match self {
            Self::Uniqs(c) => Some(c),
            Self::RMs(c) => Some(c),
            Self::RMall(c) => Some(c),
            Self::SimpEQ(c) => Some(c),
            Self::Focus(c) => Some(c),
            Self::Init(_) => None,
        }
    }
}

pub struct QueryLattice<E> {
    pub query_store: QStore,
    leaf_queries: Vec<IdNQ>,
    pub raw_rels: std::collections::HashMap<IdNQ, Vec<TR<E>>>,
    pub queries: Vec<(IdNQ, Vec<IdQ>)>,
    sort_cache: Vec<u32>,
    pub(crate) root_cap: IdNQ,
    pub(crate) auto_caps: Vec<IdNQ>,
}

/// make the deduplication through raw entries, probably slower, is it marginal ?
pub struct DedupRawEntry<TR>(hashbrown::HashMap<IdNQ, Vec<(IdNQ, TR)>>);

impl<TR> Default for DedupRawEntry<TR> {
    fn default() -> Self {
        Self(Default::default())
    }
}
// type DedupRawEntry = hashbrown::HashMap<IdN, Vec<(IdN, TR)>>;
// TODO directly handle this kind of dedup referentially in the HyperAST
// i.e. putting all the spaces in a wrapper root.
// It requires a special TreeGen that uses the global context, where it accumulates a topologically sorted list of spaces.
impl<TR> Deref for DedupRawEntry<TR> {
    type Target = hashbrown::HashMap<IdNQ, Vec<(IdNQ, TR)>>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl<TR> DerefMut for DedupRawEntry<TR> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

pub trait Ded {
    fn queries(&self) -> Vec<IdNQ>;
    fn sorted_queries<TS: TypeStore>(&self, stores: &SimpleStores<TS>) -> Vec<IdNQ> {
        let mut v = self.queries();
        v.sort_by_cached_key(|x| query_hash(&stores, x));
        v.dedup();
        v
    }
}
impl<TR> Ded for DedupRawEntry<TR> {
    fn queries(&self) -> Vec<IdNQ> {
        self.0.keys().copied().collect()
    }
}

type IdNQ = IdN;

impl QueryLattice<IdN> {
    pub fn with_examples<TS, TIdN>(
        stores: &SimpleStores<TS>,
        from: impl Iterator<Item = IdN>,
        meta_gen: &hyperast_tsquery::Query,
        meta_simp: &hyperast_tsquery::Query,
    ) -> Self
    where
        TS: TypeStore + RoleStore,
        TIdN: TypedNodeId<IdN = IdN>,
        TIdN::Ty: types::TypeTrait,
        TS::IdF: From<u16> + Into<u16>,
    {
        let b = Self::builder::<TS, TIdN, _>(stores, from, meta_gen, meta_simp, &|x| {
            x.local.metrics.hashs.label
        });
        let mut b: Builder<'_, IdN, DedupRawEntry<TR<IdN>>> = b.dedup_leaf_queries(|from| {
            // from.into_iter().fold(DedupSimp::new(), |mut acc, x| {
            //     let (from, (query, label_h)) = x;
            //     let v = &mut acc.entry(label_h).or_default();
            //     let x = (query, TR::Init(from));
            //     if !v.contains(&x) {
            //         v.push(x);
            //         // v.sort_by(cmp_lat_entry(&s.query_store))
            //     }
            //     acc
            // })
            from.into_iter()
                .fold(DedupRawEntry::<TR<IdN>>::default(), |mut acc, x| {
                    let (from, (query, label_h)) = x;
                    let v = acc.raw_entry_mut().from_hash(label_h as u64, |x| true);
                    use hashbrown::hash_map::RawEntryMut::*;
                    let v = match v {
                        Occupied(occ) => occ.into_key_value().1,
                        Vacant(vacant) => {
                            vacant
                                .insert_with_hasher(label_h as u64, query, vec![], |query| {
                                    query_hash(&stores, query)
                                })
                                .1
                        }
                    };
                    let x = (query, TR::Init(from));
                    if !v.contains(&x) {
                        v.push(x);
                        // v.sort_by(cmp_lat_entry(&s.query_store))
                    }
                    acc
                })
        });
        b.rest0();

        let dedup = &mut b.dedup;
        dedup
            .values_mut()
            .for_each(|x| x.sort_by(cmp_lat_entry(&b.lattice.query_store)));
        dbg!(dedup.len());
        for v in dedup.values() {
            b.lattice.add_raw_rels(v);
        }
        for v in b.dedup.values() {
            b.lattice.queries.push(b.extract(v));
        }
        b.build()
    }
}
impl<Init> QueryLattice<Init> {
    pub fn count(&self) -> usize {
        self.queries.len()
    }

    pub fn iter(&self) -> impl Iterator<Item = (IdNQ, &[IdQ])> {
        self.sort_cache.iter().map(|i| {
            let (q, e) = &self.queries[*i as usize];
            (*q, &e[..])
        })
    }

    pub fn iter_pretty(&self) -> impl Iterator<Item = (String, &[IdQ])> {
        self.sort_cache.iter().filter_map(|i| {
            let (q, e) = &self.queries[*i as usize];
            let q = self.pretty(q);
            if q.is_empty() {
                return None;
            }
            Some((q, &e[..]))
        })
    }

    pub fn pretty(&self, q: &IdNQ) -> String {
        let q = qgen::PP::<_, _>::new(&self.query_store, *q)
            // let q = hyperast::nodes::TextSerializer::<_, _>::new(&self.query_store, *q)
            // .to_string()
            // .trim()
            // .lines()
            // .filter(|x| !x.trim().is_empty())
            // .map(|x| x.to_string() + "\n---\n")
            // .collect::<String>()
        ;
        format!("{}", q)
    }

    pub fn extract2(&self, k: IdNQ) -> (IdNQ, Vec<IdQ>) {
        self._extract2(k, &self.raw_rels[&k])
    }
    fn _extract2(&self, k: IdNQ, v: &[TR<Init>]) -> (IdNQ, Vec<IdQ>) {
        fn extract<'a, Init: 'a>(
            map: &'a std::collections::HashMap<IdNQ, Vec<TR<Init>>>,
            curr: IdNQ,
            downs: impl Iterator<Item = &'a TR<Init>>,
            already: &mut HashSet<IdNQ>,
            r: &mut Vec<IdNQ>,
            leafs: &[IdNQ],
        ) {
            for s in downs {
                if let Some(_) = s.as_init() {
                    if !r.contains(&curr) {
                        assert!(leafs.contains(&curr), "{curr:?}");
                        r.push(curr)
                    }
                } else if let Some(v) = s.no_init() {
                    if !already.contains(v) {
                        already.insert(*v);
                        let downs = map.get(v).unwrap().iter();
                        extract(map, *v, downs, already, r, leafs)
                    }
                }
                // match s {
                //     TR::Init(_) if !r.contains(&curr) => {
                //         assert!(leafs.contains(&curr), "{curr:?}");
                //         r.push(curr)
                //     }
                //     TR::RMs(v) | TR::Uniqs(v) | TR::SimpEQ(v) if !already.contains(v) => {
                //         already.insert(*v);
                //         let downs = map.get(v).unwrap().iter();
                //         extract(map, *v, downs, already, r, leafs)
                //     }
                //     _ => (),
                // }
            }
        }
        let mut already = HashSet::default();
        let mut r = vec![];
        extract(
            &self.raw_rels,
            k,
            v.iter(),
            &mut already,
            &mut r,
            &self.leaf_queries,
        );
        let r = r
            .into_iter()
            .map(|x| self.leaf_queries.iter().position(|y| x == *y).unwrap() as u32)
            .collect();
        (k, r)
    }
}

type VecDedup<Init, T> = Vec<(Init, (IdNQ, T))>;

impl<Init: Clone + SolvedPosition<IdN>> QueryLattice<Init> {
    pub fn get_query(&self, index: usize) -> Option<(String, &[IdQ])> {
        self.queries
            .get(self.sort_cache[index] as usize)
            .and_then(|(q, e)| {
                let q = qgen::PP::<_, _>::new(&self.query_store, *q)
                    .to_string()
                    .trim()
                    .lines()
                    .filter(|x| !x.trim().is_empty())
                    .map(|x| x.to_string() + "\n")
                    .collect::<String>();
                if q.is_empty() {
                    None
                } else {
                    Some((q, &e[..]))
                }
            })
    }

    pub fn sort_by_size(&mut self) {
        if self.sort_cache.is_empty() && !self.queries.is_empty() {
            self.sort_cache = (0..self.queries.len())
                .map(|x| x.to_u32().unwrap())
                .collect();
        }
        self.sort_cache.sort_by(|a, b| {
            (self.queries[*a as usize].1.len()).cmp(&self.queries[*b as usize].1.len())
        });
    }

    pub fn builder<'q, TS, TIdN, T>(
        stores: &SimpleStores<TS>,
        from: impl Iterator<Item = Init>,
        meta_gen: &'q hyperast_tsquery::Query,
        meta_simp: &'q hyperast_tsquery::Query,
        f: &impl Fn(qgen::FNode) -> T,
    ) -> Builder<'q, Init, VecDedup<Init, T>>
    where
        TS: TypeStore + RoleStore,
        TIdN: TypedNodeId<IdN = IdN>,
        TIdN::Ty: types::TypeTrait,
        TS::IdF: From<u16> + Into<u16>,
    {
        let mut s = Self::new();
        let mut md_cache = Default::default();
        let dedup = from
            .filter_map(|from| {
                let x = generate_query_aux::<TS, TIdN, _, Init>(
                    &mut s.query_store,
                    &mut md_cache,
                    stores,
                    from.clone(),
                    meta_gen,
                    f,
                )?;
                // TODO generate multiple initial variants, by adding common meta rules
                if !simp_search_need(&s.query_store, x.0, meta_simp) {
                    return None;
                }
                Some((from, x))
            })
            .collect();
        Builder {
            lattice: s,
            dedup,
            meta_simp,
        }
    }

    /// Similar to with_examples,
    /// but processes queries from the biggest to the smallest,
    /// thus we can use a kind of vec of maps and parallelize inserts in the maps.
    #[cfg(feature = "synth_par")]
    pub fn with_examples_by_size<TS, TIdN>(
        stores: &SimpleStores<TS>,
        from: impl Iterator<Item = Init>,
        meta_gen: &hyperast_tsquery::Query,
        meta_simp: &hyperast_tsquery::Query,
    ) -> Self
    where
        TS: TypeStore + RoleStore,
        TIdN: TypedNodeId<IdN = IdN>,
        TIdN::Ty: types::TypeTrait,
        TS::IdF: From<u16> + Into<u16>,
        Init: Sync + Send + Eq,
    {
        let b = Self::builder::<TS, TIdN, _>(stores, from, meta_gen, meta_simp, &|x| {
            // TODO use size ignoring spaces
            (x.local.metrics.size, x.local.metrics.hashs.label)
        });
        let mut b = b.dedup_leaf_queries(|from: Vec<(_, (_, (u32, u32)))>| group_by_size(from));
        b.loop_par();
        b.post();
        b.lattice.sort_by_size();
        b.build()
    }

    /// Similar to with_examples_by_size,
    /// but tries to merge simplifications in parallel through a shared ref to query store
    /// then merges requiring additional subtrees are merged sequentially.
    #[cfg(feature = "synth_par")]
    pub fn with_examples_by_size_try<TS, TIdN>(
        stores: &SimpleStores<TS>,
        from: impl Iterator<Item = Init>,
        meta_gen: &hyperast_tsquery::Query,
        meta_simp: &hyperast_tsquery::Query,
    ) -> Self
    where
        TS: TypeStore + RoleStore,
        TIdN: TypedNodeId<IdN = IdN>,
        TIdN::Ty: types::TypeTrait,
        TS::IdF: From<u16> + Into<u16>,
        Init: Sync + Send + Eq,
    {
        let b = Self::builder::<TS, TIdN, _>(stores, from, meta_gen, meta_simp, &|x| {
            // TODO use size ignoring spaces
            (x.local.metrics.size, x.local.metrics.hashs.label)
        });
        let mut b = b.dedup_leaf_queries(|from: Vec<(_, (_, (u32, u32)))>| group_by_size(from));
        b.loop_par_par();
        b.post();
        b.lattice.sort_by_size();
        b.build()
    }
}

#[derive(Default)]
pub struct DedupBySize(Vec<std::collections::HashMap<u32, Vec<(IdNQ, TR)>>>);

impl Deref for DedupBySize {
    type Target = Vec<std::collections::HashMap<u32, Vec<(IdNQ, TR)>>>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl DerefMut for DedupBySize {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
impl Ded for DedupBySize {
    fn queries(&self) -> Vec<IdNQ> {
        self.0
            .iter()
            .flat_map(|x| x.values().flat_map(|v| v.iter().map(|x| x.0)))
            .collect()
    }
}

pub struct DedupBySize2<TR = self::TR>(Vec<hashbrown::HashMap<IdNQ, Vec<TR>>>);

impl<TR> Default for DedupBySize2<TR> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<TR> Deref for DedupBySize2<TR> {
    type Target = Vec<hashbrown::HashMap<IdNQ, Vec<TR>>>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl<TR> DerefMut for DedupBySize2<TR> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
impl<TR> Ded for DedupBySize2<TR> {
    fn queries(&self) -> Vec<IdNQ> {
        (self.0.iter()).flat_map(|x| x.keys().copied()).collect()
    }
}

#[cfg(feature = "synth_par")]
pub fn group_by_size<Init: Clone + SolvedPosition<IdN> + Eq + Sync + Send>(
    from: VecDedup<Init, (u32, u32)>,
) -> DedupBySize2<TR<Init>> {
    use rayon::iter::IntoParallelIterator as _;
    use rayon::iter::ParallelIterator;

    let r = from
        .into_iter()
        // grouping on size of queries (i.e. number of nodes)
        .fold(Vec::<Vec<(Init, (IdNQ, u32))>>::new(), |mut acc, x| {
            let (fr, (query, (size, label_h))) = x;
            let size = size as usize;
            if size >= acc.len() {
                acc.resize(size + 1, Default::default());
            }
            acc[size].push((fr, (query, label_h)));
            acc
        })
        .into_par_iter()
        .map(|acc| {
            // grouping by id ignoring indentation and spaces (by leveraging crate::no_fmt_legion)
            acc.into_iter().fold(
                hashbrown::HashMap::<IdNQ, Vec<TR<Init>>>::new(),
                |mut acc, x| {
                    let (from, (query, label_h)) = x;
                    let v = &mut acc.entry(query).or_default();
                    // dbg!(query);
                    let x = (query, TR::Init(from));
                    if !v.contains(&x.1) {
                        v.push(x.1.clone());
                    }
                    acc
                },
            )
        })
        .collect::<Vec<_>>();
    DedupBySize2(r)
}

type IdQ = u32;

pub struct Builder<'q, E, D = DedupBySize2<TR<E>>> {
    pub lattice: QueryLattice<E>,
    // the deduplicated patterns
    pub dedup: D,
    pub meta_simp: &'q hyperast_tsquery::Query,
}

impl<'q, E, D> Builder<'q, E, D> {
    pub fn build(self) -> QueryLattice<E> {
        self.lattice
    }
    pub fn dedup_leaf_queries<D2: Ded>(self, f: impl Fn(D) -> D2) -> Builder<'q, E, D2> {
        let mut b = Builder {
            lattice: self.lattice,
            dedup: f(self.dedup),
            meta_simp: self.meta_simp,
        };
        b.lattice.leaf_queries = b.dedup.queries();
        b.lattice
            .leaf_queries
            .sort_by_cached_key(|x| query_hash(&b.lattice.query_store, x));
        b.lattice.leaf_queries.dedup();
        b
    }

    #[cfg(feature = "synth_par")]
    #[doc(hidden)]
    fn by_pattern_metric<M, TR>(
        &self,
        uniques: Vec<(IdNQ, TR)>,
        metric: impl Fn(hyperast::store::nodes::legion::HashedNodeRef<'_>) -> M + Sync,
    ) -> BTreeMap<M, Vec<(IdNQ, TR)>>
    where
        M: Ord,
        TR: Send + Sync,
        M: Send + Sync,
    {
        let query_store = &self.lattice.query_store;
        by_metric(uniques, |x: &IdNQ| metric(query_store.resolve(x)))
    }
}

fn query_hash<TS: TypeStore>(query_store: &SimpleStores<TS>, x: &IdN) -> u64 {
    WithHashs::hash(&query_store.resolve(x), &HashKind::label()) as u64
}

impl Builder<'_, IdN, DedupRawEntry<TR<IdN>>> {
    fn rest0(&mut self) {
        let s = &mut self.lattice;
        let dedup = &mut self.dedup;
        let meta_simp = self.meta_simp;
        let mut active: Vec<IdNQ> = (dedup.keys().copied())
            .filter(|x| {
                let entry = dedup
                    .raw_entry()
                    .from_hash(query_hash(&s.query_store, x), |y| true);
                simp_search_need(&s.query_store, entry.unwrap().1[0].0, meta_simp)
            })
            .collect();

        for _ in 0..4 {
            dbg!(active.len());
            let rms = std::mem::take(&mut active)
                .into_iter()
                .flat_map(|x| {
                    let Some((_, x)) = dedup
                        .raw_entry()
                        .from_hash(query_hash(&s.query_store, &x), |x| true)
                    else {
                        return vec![];
                    };
                    let query = x[0].0;
                    simp_rms(&mut s.query_store, query, meta_simp)
                        .map(|new_q| (new_q, TR::RMs(query)))
                        .collect::<Vec<_>>()
                })
                .collect::<Vec<_>>();
            dbg!(rms.len());
            for x in rms {
                let v = dedup.entry(x.0);
                use hashbrown::hash_map::Entry::*;
                let v = match v {
                    Occupied(occ) => occ.into_mut(),
                    Vacant(vacant) => {
                        active.push(x.0);
                        vacant.insert(vec![])
                    }
                };
                if !v.contains(&x) {
                    v.push(x);
                }
            }
            // TODO add pass to replace some symbols with a wildcard
        }
        dbg!(dedup.0.len());
        let simp_eq = (dedup.0.values())
            .filter_map(|x| {
                let query = x[0].0;
                let new_q =
                    simp_positional_eq(&mut s.query_store, query, meta_simp, &mut s.auto_caps)?;
                Some((new_q, TR::RMs(query)))
            })
            .collect::<Vec<_>>();

        for x in simp_eq {
            let v = dedup.entry(x.0);
            use hashbrown::hash_map::Entry::*;
            let v = match v {
                Occupied(occ) => occ.into_mut(),
                Vacant(vacant) => {
                    active.push(x.0);
                    vacant.insert(vec![])
                }
            };
            if !v.contains(&x) {
                v.push(x);
            }
        }
        dbg!(dedup.0.len());
    }
}

// the parallel implementations
#[cfg(feature = "synth_par")]
impl<Init: Clone + SolvedPosition<IdN> + Sync + Send> Builder<'_, Init, DedupBySize2<TR<Init>>> {
    fn loop_par(&mut self)
    where
        Init: Eq,
    {
        let mut active_size = self.dedup.0.len() - 1;
        let mut active: Vec<_> = self.actives(active_size);

        loop {
            dbg!(active_size);
            dbg!(active.len());
            // TODO add pass to replace some symbols with a wildcard
            let rms = self.removes_par(active_size, &mut active);
            dbg!(rms.len());
            self.dedup_removes_par(active_size, &mut active, rms);
            if self.between(&mut active_size, &mut active) {
                break;
            }
        }
    }

    /// Must use dedup_removes_par on output to properly progress
    #[must_use]
    fn removes_par(&mut self, active_size: usize, active: &mut Vec<IdNQ>) -> Vec<Simplified<Init>> {
        let s = &mut self.lattice;
        let dedup = &mut self.dedup;
        let meta_simp = self.meta_simp;
        let rms = std::mem::take(active).into_iter();
        rms.flat_map(|x| {
            let Some(y) = dedup.0[active_size].get(&x) else {
                return vec![];
            };
            let query = x;
            simp_rms(&mut s.query_store, query, meta_simp)
                .map(|new_q| (new_q, TR::RMs(query)))
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>()
    }

    pub fn dedup_removes_par(
        &mut self,
        active_size: usize,
        active: &mut Vec<IdNQ>,
        rms: Vec<(IdNQ, TR<Init>)>,
    ) where
        Init: Eq,
    {
        let by_size = self.by_pattern_size(rms);
        let act = dedup_patterns_by_metric(by_size, &mut self.dedup);
        active.extend(act);
    }

    pub fn dedup_uniques_par(
        &mut self,
        active_size: usize,
        // size, origin, curr, tr
        uniques: Vec<(IdNQ, TR<Init>)>,
    ) -> Vec<IdNQ>
    where
        Init: Eq,
    {
        let by_size = self.by_pattern_size(uniques);
        dedup_patterns_by_metric(by_size, &mut self.dedup)

        // use rayon::iter::IndexedParallelIterator as _;
        // use rayon::iter::IntoParallelRefMutIterator as _;
        // use rayon::iter::ParallelIterator;

        // // now use a Vec<T> in place of the BTree<usize, T>
        // let by_size = by_size
        //     .into_iter()
        //     .fold(vec![vec![]; active_size], |mut acc, x| {
        //         debug_assert!(x.0 < active_size);
        //         acc[x.0] = x.1;
        //         acc
        //     });
        // // dedup patterns in parallel (by size)
        // let dedup = &mut self.dedup;
        // ParallelIterator::flat_map(
        //     dedup.0[..active_size].par_iter_mut().enumerate(),
        //     |(i, dedup)| {
        //         let mut r = vec![];
        //         for x in &by_size[i] {
        //             let v = dedup.entry(x.0);
        //             use hashbrown::hash_map::Entry;
        //             let v = match v {
        //                 Entry::Occupied(x) => x.into_mut(),
        //                 Entry::Vacant(x) => {
        //                     r.push(*x.key());
        //                     x.insert(vec![])
        //                 }
        //             };
        //             if !v.contains(&x.1) {
        //                 v.push(x.1.clone());
        //             }
        //         }
        //         r
        //     },
        // )
        // .collect()
    }

    #[cfg(feature = "synth_par")]
    pub fn by_pattern_size(
        &self,
        uniques: Vec<(IdNQ, TR<Init>)>,
    ) -> BTreeMap<usize, Vec<(IdNQ, TR<Init>)>>
    where
        Init: Eq + Clone + SolvedPosition<IdN>,
        Init: Send + Sync,
    {
        self.by_pattern_metric(uniques, |q| q.size())
    }

    pub fn dedup_uniques_par2(
        &mut self,
        active_size: usize,
        // size, origin, curr, tr
        uniques: Vec<(IdNQ, TR<Init>)>,
    ) -> Vec<IdNQ>
    where
        Init: Eq,
    {
        let by_metric = self.by_pattern_size(uniques);
        let max_active = by_metric.last_key_value().map_or(active_size, |x| *x.0);
        let len = max_active.max(self.dedup.len());
        self.dedup.resize(len, Default::default());
        dedup_patterns_by_metric(by_metric, &mut self.dedup)
    }

    pub fn post(&mut self)
    where
        Init: Send,
    {
        use rayon::iter::{IntoParallelRefMutIterator, ParallelIterator};
        self.dedup.0.par_iter_mut().for_each(|x| {
            let qstores = &self.lattice.query_store;
            for y in x.values_mut() {}
        });
        for v in self.dedup.iter().flat_map(|x| x.iter()) {
            self.lattice.add_raw_rels2(*v.0, v.1);
        }
        for v in self.dedup.iter().flat_map(|x| x.iter()) {
            let value = self.lattice._extract2(*v.0, v.1);
            self.lattice.queries.push(value);
        }
    }

    pub fn loop_par_par(&mut self)
    where
        Init: Eq,
    {
        let mut active_size = self.dedup.len() - 1;
        let mut active: Vec<_> = self.actives(active_size);
        loop {
            dbg!(active_size);
            dbg!(active.len());
            let (uniqs, already) = self.uniques_par_par(active_size, &mut active);
            dbg!(uniqs.len());
            self.dedup_uniques_par(active_size, uniqs);
            active = already;
            let rms = self.removes_par_par(active_size, &mut active);
            dbg!(rms.len());
            self.dedup_removes_par(active_size, &mut active, rms);

            self.simp_focus(active_size);
            let mut a = self.actives(active_size);
            let v = self.simp_eq(&mut a);
            dbg!(v.0.len());
            let mut tmp_active_size = active_size;
            for (x, tr) in v.0 {
                let size = self.lattice.query_store.resolve(&x).size();
                let dedup = &mut self.dedup[size];
                let v = dedup.entry(x).or_default();
                if !v.contains(&tr) {
                    if size > active_size {
                        tmp_active_size = tmp_active_size.max(size);
                        dbg!((size, active_size));
                    }
                    active.push(x);
                    v.push(tr.clone());
                }
            }

            let rms = self.removes_par_par(tmp_active_size, &mut active);
            dbg!(rms.len());
            self.dedup_removes_par(tmp_active_size, &mut active, rms);

            if self.between(&mut active_size, &mut active) {
                break;
            }
        }
    }

    /// Applies focuses simplifications
    ///
    /// see [`simp_focus`] for more details on focuses
    ///
    /// materialized by @focus in `self.meta_simp`
    fn simp_focus(&mut self, active_size: usize)
    where
        Init: Eq,
    {
        for query in self.actives(active_size) {
            let mut query_store = &mut self.lattice.query_store;
            let tr = TR::Focus(query);
            let v = simp_focus(query_store, query, self.meta_simp);
            for x in v {
                let size = query_store.resolve(&x).size();
                let dedup = &mut self.dedup[size];
                let v = dedup.entry(x).or_default();
                if !v.contains(&tr) {
                    v.push(tr.clone());
                }
            }
        }
    }

    /// Must use dedup_removes_par on output to properly progress
    #[must_use]
    pub fn removes_par_par(
        &mut self,
        _active_size: usize,
        active: &mut Vec<IdNQ>,
    ) -> Vec<Simplified<Init>> {
        use rayon::iter::IntoParallelIterator as _;
        use rayon::iter::ParallelIterator;

        let s = &mut self.lattice;
        let meta_simp = self.meta_simp;
        let rms = std::mem::take(active).into_par_iter();
        let rms = ParallelIterator::flat_map(rms, |x| {
            let query = x;
            let Some(cid) = meta_simp.capture_index_for_name("rm") else {
                return vec![];
            };
            try_simp_rms(&s.query_store, query, meta_simp, cid)
                .map(|x| match x {
                    Ok(new_q) => Ok((new_q, TR::RMs(query))),
                    Err(e) => Err(e),
                })
                .collect::<Vec<_>>()
        });
        let (remains, mut rms): (Vec<SimpRmsRemains>, Vec<(IdN, TR<Init>)>) =
            ParallelIterator::partition_map(rms, |x| x.into());
        log::info!("remains: {}", remains.len());
        log::info!("rms: {}", rms.len());

        let rem_count = remains.len();
        remains.chunks(1000).enumerate().for_each(|(i, x)| {
            let i = i * 1000;
            log::info!("remains removes {i:4}/{rem_count}");
            rms.extend(x.iter().filter_map(|(query, path)| {
                let query = *query;
                let label_h;
                let new_q = {
                    let query = apply_rms_aux(&mut s.query_store, query, path)?;
                    if !simp_search_need(&s.query_store, query, meta_simp) {
                        return None;
                    }
                    label_h = s.query_store.resolve(&query).hash(&HashKind::label());
                    query
                };
                Some((new_q, TR::RMs(query)))
            }))
        });
        rms
    }
    #[must_use]
    pub fn removesall_par_par(
        &mut self,
        active_size: usize,
        active: &mut Vec<IdNQ>,
        cid: hyperast_tsquery::CaptureId,
    ) -> Vec<Simplified<Init>> {
        use rayon::iter::IntoParallelIterator as _;
        use rayon::iter::ParallelIterator;

        let s = &mut self.lattice;
        let meta_simp = self.meta_simp;
        let rms = std::mem::take(active).into_par_iter();
        let rms = ParallelIterator::filter_map(rms, |x| {
            let query = x;
            try_simp_rmalls(&s.query_store, query, meta_simp, cid).map(|x| match x {
                Ok(new_q) => Ok((new_q, TR::RMall(query))),
                Err(e) => Err(e),
            })
        });
        let (remains, mut rms): (Vec<RmAllAlt>, Vec<Simplified<Init>>) =
            ParallelIterator::partition_map(rms, |x| x.into());
        log::info!("remains: {}", remains.len());
        log::info!("rms: {}", rms.len());

        let rem_count = remains.len();
        const CHUNK_SIZE: usize = 1000;
        remains.chunks(CHUNK_SIZE).enumerate().for_each(|(i, x)| {
            let i = i * CHUNK_SIZE;
            log::info!("remains removesall {i:4}/{rem_count}");

            rms.extend(x.iter().filter_map(|(query, curr, paths)| {
                let query = *query;
                let mut curr = *curr;
                // dbg!(&paths);
                for path in paths.iter() {
                    curr = apply_rms_aux2(&mut s.query_store, curr, path)?;
                }
                if !simp_search_need(&s.query_store, curr, meta_simp) {
                    return None;
                }
                let new_q = curr;
                if new_q == query {
                    todo!()
                    // return None;
                }
                assert_ne!(new_q, query);
                Some((new_q, TR::RMall(query)))
            }))
        });
        rms
    }

    #[must_use]
    // remove incomplete predicates and unused predicates from a pattern so they compile
    pub fn repair_par(&mut self, rms: Vec<(IdNQ, TR<Init>)>) -> Vec<Simplified<Init>> {
        use rayon::iter::IntoParallelIterator as _;
        use rayon::iter::ParallelIterator;

        let s = &mut self.lattice;
        let repair_query = r#"(predicate
            (identifier) (#EQ? "eq")
            (parameters
                (capture) @id1
                (capture) @id2
            )
        ) @pred
        (predicate
            (parameters
                (capture) @ref
            )
        )
        (named_node (capture) @decl)
        (anonymous_node (capture) @decl)
        (grouping (capture) @decl)
        (list (capture) @decl)
        "#;
        let repair_query = &hyperast_tsquery::Query::new(&repair_query, crate::language())
            .map_err(|e| format!("error in repair_query: {e}"))
            .unwrap();
        let pred_cid = repair_query.capture_index_for_name("pred").unwrap();
        let id1_cid = repair_query.capture_index_for_name("id1").unwrap();
        let id2_cid = repair_query.capture_index_for_name("id2").unwrap();
        let decl_cid = repair_query.capture_index_for_name("decl").unwrap();
        let ref_cid = repair_query.capture_index_for_name("ref").unwrap();
        let root_cap = s.root_cap;

        let rms = ParallelIterator::map(rms.into_par_iter(), |x| {
            use hyperast::position::structural_pos::CursorHead;
            let mut candidates = vec![];
            let mut decls = HashSet::new();
            let query = x.0;
            let mut pos = hyperast::position::structural_pos::CursorWithPersistence::new(query);
            let mut to_rm = pos.build_empty_set();
            let mut decl_nodes = pos.build_empty_set();
            let mut refs = pos.build_empty_set();
            let cursor = hyperast_tsquery::hyperast_opt::TreeCursor::new(&s.query_store, pos);
            let mut matches = repair_query.matches(cursor);
            loop {
                let Some(m) = matches.next() else {
                    break;
                };
                if let Some(r) = m.nodes_for_capture_index(ref_cid).next() {
                    refs.register(&r.pos);
                } else if let Some(decl) = m.nodes_for_capture_index(decl_cid).next() {
                    decl_nodes.register(&decl.pos);
                    decls.insert(decl.pos.node());
                } else if let Some(pred) = m.nodes_for_capture_index(pred_cid).next() {
                    let id1 = m.nodes_for_capture_index(id1_cid).next().unwrap();
                    let id2 = m.nodes_for_capture_index(id2_cid).next().unwrap();
                    candidates.push((id1.pos.clone(), id2.pos.clone(), pred.pos.clone()));
                }
            }
            if candidates.is_empty() {
                return Ok((query, x.1));
            }

            // (id_missing, id_useless, caps_useless)
            // let invalid_preds =
            candidates
                .into_iter()
                .filter(|x| {
                    if !decls.contains(&x.0.node()) {
                        refs.remove(&x.1);
                        true
                    } else if !decls.contains(&x.1.node()) {
                        refs.remove(&x.0);
                        true
                    } else {
                        false
                    }
                })
                // .map(|x| x.2.offsets())
                // .collect::<Vec<_>>();
                .for_each(|x| to_rm.register(&x.2));

            let refs: HashSet<_> = refs.into_iter(|x| x.node()).chain([root_cap]).collect();

            // let unused_decls =
            decl_nodes
                .iter()
                .filter(|x| !refs.contains(&x.node()))
                .for_each(|x| to_rm.register_ref(x));
            // .map(|x| x.offsets())
            // .collect::<Vec<_>>();

            // removes preds with absent caps and remove unused caps
            // unused_decls.iter().for_each(|x|rm(x));
            // invalid_preds.iter().for_each(|x|rm(x.2));

            // let to_rm = invalid_preds.into_iter().chain(unused_decls.into_iter());
            // let to_rm: Vec<_> = to_rm.into_iter().collect();
            let mut to_rm: Vec<_> = to_rm
                .into_iter(|x| x.offsets())
                // .collect_vec(|x| x.offsets())
                // .into_iter()
                .map(|mut path| {
                    path.pop();
                    path.reverse();
                    path
                })
                .collect();
            to_rm.sort_by(|a, b| b.cmp(a));
            let mut curr = query;
            log::info!("to_rm: {:?}", to_rm);
            for rm in 0..to_rm.len() {
                let mut path = to_rm[rm].clone();
                // path.pop();
                // path.reverse();
                let actions = vec![tsq_transform::Action::Delete { path }];
                match tsq_transform::try_regen_query(&s.query_store, curr, actions.clone()) {
                    Some(query) => curr = query,
                    None => {
                        return Err((
                            query,
                            curr,
                            x.1.clone(),
                            to_rm[rm..].iter().map(|x| x.clone()).collect(),
                        ));
                    }
                }
            }

            Ok((curr, x.1))

            // let mut to_rm: Vec<_> = to_rm
            //     .into_iter()
            //     .map(|path| tsq_transform::Action::Delete { path })
            //     .collect();
            // to_rm.sort_by(|a, b| b.path().cmp(a.path()));
            // match tsq_transform::try_regen_query(&s.query_store, query, to_rm.clone()) {
            //     Some(query) => Some(Ok((query, x.1))),
            //     None => return Some(Err((query, x.1.clone(), to_rm))),
            // }
        });
        type RepairRemains<Init> = (IdNQ, IdNQ, TR<Init>, Vec<PendingRmPath>);
        let (remains, mut rms): (Vec<RepairRemains<Init>>, Vec<Simplified<Init>>) =
            ParallelIterator::partition_map(rms, |x| x.into());
        log::info!("remains: {}", remains.len());
        log::info!("rms: {}", rms.len());

        let rem_count = remains.len();
        const CHUNK_SIZE: usize = 1000;
        rms.extend(remains.into_iter().enumerate().filter_map(
            |(i, (_query, curr, tr, mut paths))| {
                if i % CHUNK_SIZE == 0 {
                    log::info!("remains repairs {i:4}/{rem_count}");
                }

                log::info!("to_rm mut: {:?}", paths);
                // dbg!(&paths);
                // eprintln!(
                //     "{}",
                //     hyperast::nodes::SyntaxSerializer::new(&s.query_store, curr)
                // );
                let mut curr = curr;
                for path in paths {
                    // dbg!(&path);
                    let actions = vec![tsq_transform::Action::Delete { path }];
                    curr = tsq_transform::regen_query(&mut s.query_store, curr, actions)?;
                }
                let new_q = curr;
                Some((new_q, tr))
            },
        ));
        rms
    }

    #[must_use]
    /// normalize captures to avoid duplicates due to what are actually free variable
    fn norm_caps(&mut self, rms: Vec<(IdNQ, TR<Init>)>) -> Vec<Simplified<Init>> {
        use rayon::iter::IntoParallelIterator as _;
        use rayon::iter::ParallelIterator;

        let s = &mut self.lattice;
        let norm_query = r#"(program
            (predicate
                (identifier) (#EQ? "eq")
                (parameters
                    (capture) @id1
                    (capture) @id2
                )
            ) @pred
        )
        (named_node (capture) @decl)
        (anonymous_node (capture) @decl)
        (grouping (capture) @decl)
        (list (capture) @decl)
        "#;
        let norm_query = &hyperast_tsquery::Query::new(&norm_query, crate::language())
            .map_err(|e| format!("error in norm_query: {e}"))
            .unwrap();
        let pred_cid = norm_query.capture_index_for_name("pred").unwrap();
        let id1_cid = norm_query.capture_index_for_name("id1").unwrap();
        let id2_cid = norm_query.capture_index_for_name("id2").unwrap();
        let decl_cid = norm_query.capture_index_for_name("decl").unwrap();
        let root_cap = s.root_cap;

        let rms =
            ParallelIterator::map(rms.into_par_iter(), |x| {
                use hyperast::nodes::TextSerializer;
                use hyperast::position::structural_pos::CursorHead;
                let mut candidates = vec![];
                let query = x.0;
                let mut pos = hyperast::position::structural_pos::CursorWithPersistence::new(query);
                let mut to_rm = pos.build_empty_set();
                let mut decl_nodes = pos.build_empty_set();
                let mut refs = pos.build_empty_set();
                let cursor = hyperast_tsquery::hyperast_opt::TreeCursor::new(&s.query_store, pos);
                let mut matches = norm_query.matches(cursor);
                loop {
                    let Some(m) = matches.next() else {
                        break;
                    };
                    if let Some(decl) = m.nodes_for_capture_index(decl_cid).next() {
                        let cap = TextSerializer::new(&s.query_store, decl.pos.node()).to_string();
                        if cap.matches("^[@]p[0-9]+$").next().is_some() {
                            decl_nodes.register(&decl.pos);
                        }
                    } else if let Some(pred) = m.nodes_for_capture_index(pred_cid).next() {
                        let id1 = m.nodes_for_capture_index(id1_cid).next().unwrap();
                        let id2 = m.nodes_for_capture_index(id2_cid).next().unwrap();
                        candidates.push((id1.pos.clone(), id2.pos.clone(), pred.pos.clone()));
                    }
                }
                if candidates.is_empty() && decl_nodes.is_empty() {
                    return Ok((query, x.1));
                }

                // @p3 @p2 @p0 @p1                // decl_nodes
                // (#eq? @p1 @p2) (#eq? @p0 @p3)  // candidates
                // p3 -> p0 ; p2 -> p1 ; p0 -> p2 ; p1 -> p3

                let mut map: Vec<IdNQ> = vec![];
                let mut repl_decls: Vec<(Vec<u16>, IdNQ)> = vec![];
                for c in decl_nodes.iter() {
                    if let Some(i) = map.iter().position(|p| p.node() == c.node()) {
                        repl_decls.push((c.offsets(), s.auto_caps[i]));
                    } else {
                        let i = map.len();
                        map.push(c.node());
                        repl_decls.push((c.offsets(), s.auto_caps[i]));
                    }
                }
                // @p0 @p1 @p2 @p3
                // p1 == p2 ; p0 == p3
                // p0 <- p3 == p0 -> p2 ; p1 <- p2 == p1 -> p3
                // p0 => p2 ; p1 => p3

                // ( p p) ( p p) ( p p)
                let mut eq_id_rels = candidates
                    .iter()
                    .map(|(id1, id2, _)| {
                        let p1 = map
                            .iter()
                            .position(|x| *x == id1.node())
                            .unwrap_or(usize::MAX - 1); // to preserve order of non auto preds
                        let p2 = map
                            .iter()
                            .position(|x| *x == id1.node())
                            .unwrap_or(usize::MAX);
                        if p1 < p2 { (p1, p2) } else { (p2, p1) }
                    })
                    .collect::<Vec<_>>();
                eq_id_rels.sort();

                let repl_refs = candidates.into_iter().zip(eq_id_rels).flat_map(
                    |((id1, id2, pred), (i1, i2))| {
                        let i1 = s.auto_caps.get(i1).copied();
                        let i2 = s.auto_caps.get(i2).copied();
                        if Some(id1.node()) == i1 && Some(id2.node()) == i2 {
                            return Default::default();
                        }
                        let i1 = i1.unwrap_or(id1.node());
                        let i2 = i2.unwrap_or(id2.node());
                        vec![(id1.offsets(), i1), (id2.offsets(), i2)]
                    },
                );
                let mut repl = repl_decls;
                repl.extend(repl_refs);
                repl.iter_mut().for_each(|(path, _)| {
                    path.pop();
                    path.reverse();
                });
                repl.sort_by(|a, b| b.0.cmp(&a.0));
                let mut curr = query;
                log::info!("repl: {} {:?}", repl.len(), repl);
                for rm in 0..repl.len() {
                    let (path, new) = repl[rm].clone();
                    // path.pop();
                    // path.reverse();
                    let actions = vec![tsq_transform::Action::Replace { path, new }];
                    match tsq_transform::try_regen_query(&s.query_store, curr, actions.clone()) {
                        Some(query) => curr = query,
                        None => {
                            return Err((
                                query,
                                curr,
                                x.1.clone(),
                                repl[rm..].iter().map(|x| x.clone()).collect(),
                            ));
                        }
                    }
                }
                // eprintln!(
                //     "{}",
                //     hyperast::nodes::SyntaxSerializer::new(&s.query_store, curr)
                // );

                Ok((curr, x.1))

                // let mut to_rm: Vec<_> = to_rm
                //     .into_iter()
                //     .map(|path| tsq_transform::Action::Delete { path })
                //     .collect();
                // to_rm.sort_by(|a, b| b.path().cmp(a.path()));
                // match tsq_transform::try_regen_query(&s.query_store, query, to_rm.clone()) {
                //     Some(query) => Some(Ok((query, x.1))),
                //     None => return Some(Err((query, x.1.clone(), to_rm))),
                // }
            });
        type NormRemains<Init> = (IdNQ, IdNQ, TR<Init>, Vec<(PendingRmPath, IdNQ)>);
        let (remains, mut rms): (Vec<NormRemains<Init>>, Vec<Simplified<Init>>) =
            ParallelIterator::partition_map(rms, |x| x.into());
        log::info!("remains: {}", remains.len());
        log::info!("rms: {}", rms.len());

        let rem_count = remains.len();
        const CHUNK_SIZE: usize = 1000;
        rms.extend(remains.into_iter().enumerate().filter_map(
            |(i, (_query, curr, tr, mut paths))| {
                if i % CHUNK_SIZE == 0 {
                    log::info!("remains repairs {i:4}/{rem_count}");
                }

                log::info!("repl mut: {:?}", paths);
                // dbg!(&paths);
                // eprintln!(
                //     "{}",
                //     hyperast::nodes::SyntaxSerializer::new(&s.query_store, curr)
                // );
                let mut curr = curr;
                for (path, new) in paths {
                    // dbg!(&path);
                    let actions = vec![tsq_transform::Action::Replace { path, new }];
                    curr = tsq_transform::regen_query(&mut s.query_store, curr, actions)?;
                }
                let new_q = curr;
                Some((new_q, tr))
            },
        ));
        rms
    }

    /// Must use dedup_uniques_par on output to properly progress
    #[must_use]
    pub fn uniques_par_par(
        &mut self,
        active_size: usize,
        active: &mut Vec<IdNQ>,
    ) -> (Vec<Simplified<Init>>, Vec<IdNQ>) {
        use rayon::iter::IntoParallelIterator as _;
        use rayon::iter::ParallelIterator;

        let s = &mut self.lattice;
        let dedup = &mut self.dedup;
        let meta_simp = self.meta_simp;
        let rms = std::mem::take(active).into_par_iter();
        let rms = ParallelIterator::flat_map(rms, |x| {
            let Some(y) = dedup[active_size].get(&x) else {
                return vec![];
            };
            let query = x;
            use rayon::iter::Either;
            try_simp_uniq(&s.query_store, query, meta_simp)
                .map(|x| match x {
                    ResSimpUniq::Deduplicated(new_q) => Either::Right((new_q, TR::Uniqs(query))),
                    ResSimpUniq::AlreadyUniq(i) => Either::Left(Either::Right(i)),
                    ResSimpUniq::NeedMut(a, b, c) => Either::Left(Either::Left((a, b, c))),
                })
                .collect::<Vec<_>>()
        });

        let (alt, mut rms): (_, Vec<Simplified<Init>>) =
            ParallelIterator::partition_map(rms, |x| x);
        log::info!("uniqs: {}", rms.len());
        let (remains, already): (Vec<RmAllAlt>, Vec<IdNQ>) = alt;
        log::info!("remains: {}", remains.len());
        log::info!("already: {}", already.len());

        // now handling the remaining patterns that require mut access to query_store,
        // ie. they need to create new subtrees.
        // TODO allow parallel reads of query_store at state before creating new mut handle
        // by leveraging append only nature of HyperAST
        rms.extend(remains.into_iter().filter_map(|(query, mut curr, paths)| {
            for path in paths.into_iter() {
                curr = apply_rms_aux(&mut s.query_store, curr, &path)?;
            }
            if !simp_search_need(&s.query_store, curr, meta_simp) {
                return None;
            }
            let new_q = curr;
            let label_h = s.query_store.resolve(&new_q).hash(&HashKind::label());
            if new_q == query {
                todo!()
                // return None;
            }
            assert_ne!(new_q, query);
            Some((new_q, TR::Uniqs(query)))
        }));
        (rms, already)
    }
}

/// dedup using any usize representable metric,
/// `by_metric` is indexed by the metric as its key
/// `dedup` is also indexed by the metric as its offset
#[cfg(feature = "synth_par")]
pub fn dedup_patterns_by_metric<TR: Clone + PartialEq + Send + Sync>(
    by_metric: BTreeMap<usize, Vec<(legion::Entity, TR)>>,
    dedup: &mut Vec<hashbrown::HashMap<IdNQ, Vec<TR>>>,
) -> Vec<legion::Entity> {
    use rayon::iter::IndexedParallelIterator as _;
    use rayon::iter::IntoParallelRefMutIterator as _;
    use rayon::iter::ParallelIterator;
    ParallelIterator::flat_map(dedup[..].par_iter_mut().enumerate(), |(i, dedup)| {
        let mut r = vec![];
        for x in by_metric.get(&i).map_or(&vec![], |x| x) {
            let v = dedup.entry(x.0);
            use hashbrown::hash_map::Entry;
            let v = match v {
                Entry::Occupied(x) => x.into_mut(),
                Entry::Vacant(x) => {
                    r.push(*x.key());
                    x.insert(vec![])
                }
            };
            if !v.contains(&x.1) {
                v.push(x.1.clone());
            }
        }
        r
    })
    .collect()
}

#[cfg(feature = "synth_par")]
/// the rayon parallelization enabling fold-reduce
fn by_metric<TR, M>(
    uniques: Vec<(IdN, TR)>,
    metric: impl Fn(&IdN) -> M + Sync,
) -> BTreeMap<M, Vec<(IdN, TR)>>
where
    M: Ord,
    TR: Send + Sync,
    M: Send + Sync,
{
    use rayon::iter::IntoParallelIterator as _;
    use rayon::iter::ParallelIterator;
    let by_metric = ParallelIterator::fold(
        uniques.into_par_iter(),
        BTreeMap::<M, Vec<_>>::default,
        |mut acc, x: (IdNQ, TR)| {
            let size = metric(&x.0);
            acc.entry(size).or_default().push(x);
            acc
        },
    );
    let by_metric = ParallelIterator::reduce(
        by_metric,
        BTreeMap::<M, Vec<(IdNQ, TR)>>::default,
        |mut acc, b| {
            for (size, v) in b {
                acc.entry(size).or_default().extend(v);
            }
            acc
        },
    );
    by_metric
}
#[cfg(feature = "synth_par")]
pub fn filter_by_key_par<TR: Clone + PartialEq + Send + Sync>(
    by_metric: &mut BTreeMap<usize, Vec<(legion::Entity, TR)>>,
    dedup: &Vec<hashbrown::HashMap<IdNQ, Vec<TR>>>,
) {
    use rayon::iter::{IntoParallelRefMutIterator, ParallelIterator};
    by_metric.par_iter_mut().for_each(|(s, x)| {
        x.extract_if(.., |x| {
            dedup.get(*s).map_or(false, |y| y.contains_key(&x.0))
        })
        .for_each(|_| ());
    });
}
#[cfg(feature = "synth_par")]
impl<Init: Clone + SolvedPosition<IdN> + Send + Sync> Builder<'_, Init> {
    pub fn actives(&mut self, active_size: usize) -> Vec<IdN> {
        use rayon::iter::ParallelIterator;

        self.dedup[active_size]
            .par_keys()
            .copied()
            .filter(|x| {
                // simp_search_need(
                //     &self.lattice.query_store,
                //     self.dedup[active_size].get(&x).unwrap()[0].0,
                //     self.meta_simp,
                // )
                simp_search_need(&self.lattice.query_store, *x, self.meta_simp)
            })
            .collect()
    }
    pub fn between(&mut self, active_size: &mut usize, active: &mut Vec<IdNQ>) -> bool {
        if !active.is_empty() {
            return false;
        }
        loop {
            if *active_size == 0 {
                return true;
            }
            *active_size -= 1;
            if !self.dedup[*active_size].is_empty() {
                break;
            }
        }
        *active = self.actives(*active_size);
        false
    }
}

#[cfg(not(feature = "synth_par"))]
impl<Init: Clone + SolvedPosition<IdN>> Builder<'_, Init> {
    pub fn actives(&mut self, active_size: usize) -> Vec<IdN> {
        self.dedup[active_size]
            .keys()
            .copied()
            .filter(|x| {
                // simp_search_need(
                //     &self.lattice.query_store,
                //     self.dedup[active_size].get(&x).unwrap()[0].0,
                //     self.meta_simp,
                // )
                simp_search_need(&self.lattice.query_store, *x, self.meta_simp)
            })
            .collect()
    }
    pub fn between(&mut self, active_size: &mut usize, active: &mut Vec<IdNQ>) -> bool {
        if !active.is_empty() {
            return false;
        }
        loop {
            if *active_size == 0 {
                return true;
            }
            *active_size -= 1;
            if !self.dedup[*active_size].is_empty() {
                break;
            }
        }
        *active = self.actives(*active_size);
        false
    }
}

impl<Init: Clone + SolvedPosition<IdN>> Builder<'_, Init> {
    pub fn simp_eq(&mut self, active: &mut Vec<IdNQ>) -> (Vec<(IdNQ, TR<Init>)>, Vec<IdNQ>) {
        let s = &mut self.lattice;
        let dedup = &mut self.dedup;
        let meta_simp = self.meta_simp;
        let act = std::mem::take(active).into_iter();
        let mut already = vec![];
        let simp_eq = act
            .flat_map(|x| {
                let query = x;
                let Some(new_q) =
                    simp_positional_eq(&mut s.query_store, query, meta_simp, &mut s.auto_caps)
                else {
                    already.push(query);
                    return vec![];
                };
                assert_ne!(new_q, query);
                vec![(new_q, TR::<Init>::SimpEQ(query))]
            })
            .collect::<Vec<_>>();
        (simp_eq, already)
    }
}

impl<E: Clone> QueryLattice<E> {
    fn add_raw_rels(&mut self, v: &Vec<(IdN, TR<E>)>) {
        for v in v {
            self.raw_rels.entry(v.0).or_default().push(v.1.clone());
        }
    }

    fn add_raw_rels2(&mut self, k: IdN, v: &Vec<TR<E>>) {
        for v in v {
            self.raw_rels.entry(k).or_default().push(v.clone());
        }
    }

    pub fn leaf(&self, id: IdQ) -> IdNQ {
        self.leaf_queries[id as usize]
    }
}

impl<Init> Builder<'_, Init, DedupRawEntry<TR<Init>>> {
    fn extract(&self, v: &[(IdNQ, TR<Init>)]) -> (IdNQ, Vec<IdQ>) {
        fn extract<'a, Init: 'a>(
            map: &'a std::collections::HashMap<IdNQ, Vec<TR<Init>>>,
            curr: IdNQ,
            downs: impl Iterator<Item = &'a TR<Init>>,
            already: &mut HashSet<IdNQ>,
            r: &mut Vec<IdNQ>,
            leafs: &[IdNQ],
        ) {
            for s in downs {
                match s {
                    TR::Init(_) if !r.contains(&curr) => {
                        assert!(leafs.contains(&curr), "{curr:?}");
                        r.push(curr)
                    }
                    TR::RMs(v) | TR::Uniqs(v) | TR::SimpEQ(v) if !already.contains(v) => {
                        already.insert(*v);
                        extract(map, *v, map.get(v).unwrap().iter(), already, r, leafs)
                    }
                    _ => (),
                }
            }
        }
        let mut already = HashSet::default();
        let mut r = vec![];
        extract(
            &self.lattice.raw_rels,
            v[0].0,
            v.iter().map(|x| &x.1),
            &mut already,
            &mut r,
            &self.lattice.leaf_queries,
        );
        let r = r
            .into_iter()
            .map(|x| {
                dbg!(x);
                dbg!(&self.lattice.leaf_queries);
                dbg!(
                    (self.lattice.raw_rels.get(&x).unwrap().iter())
                        .position(|x| matches!(x, TR::Init(_)))
                );
                (self.lattice.leaf_queries.iter())
                    .position(|y| x == *y)
                    .unwrap() as u32
            })
            .collect();
        (v[0].0, r)
    }
}

impl<Init, D> Builder<'_, Init, D> {
    fn extract2(&self, k: IdNQ, v: &[TR<Init>]) -> (IdNQ, Vec<IdQ>) {
        self.lattice._extract2(k, v)
    }
}

fn cmp_lat_entry<TS: TypeStore + RoleStore, T: PartialOrd>(
    stores: &SimpleStores<TS>,
) -> impl Fn(&(IdN, T), &(IdN, T)) -> Ordering {
    |a, b| {
        let tr = a.1.partial_cmp(&b.1);
        if tr != Some(Ordering::Equal) {
            return tr.unwrap();
        }
        let a_l = (stores.node_store().resolve(a.0))
            .try_bytes_len()
            .unwrap_or_default();
        let b_l = (stores.node_store().resolve(b.0))
            .try_bytes_len()
            .unwrap_or_default();

        a_l.cmp(&b_l)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct QueryId(
    // even if I use just a NodeIdentifier, queries are dedup early
    NodeIdentifier,
);

impl<E> QueryLattice<E> {
    pub fn new() -> Self {
        let mut query_store = crate::search::ts_query_store();
        let root_cap = make_cap(&mut query_store, "_root");
        Self {
            query_store,
            leaf_queries: vec![],
            queries: vec![],
            raw_rels: Default::default(),
            sort_cache: Default::default(),
            root_cap,
            auto_caps: vec![],
        }
    }
}

impl<E> Default for QueryLattice<E> {
    fn default() -> Self {
        Self::new()
    }
}

fn generate_query<TS: TypeStore + RoleStore, TIdN: TypedNodeId<IdN = NodeIdentifier>>(
    query_store: &mut QStore,
    // stores: &JStore,
    stores: &SimpleStores<TS>,
    from: NodeIdentifier,
) -> NodeIdentifier
where
    for<'t> TIdN::Ty: From<&'t str>,
    TIdN::Ty: types::TypeTrait,
    TS::IdF: From<u16> + Into<u16>,
{
    struct Conv<Ty>(PhantomData<Ty>);
    impl<Ty> Default for Conv<Ty> {
        fn default() -> Self {
            Self(Default::default())
        }
    }
    impl<Ty: for<'t> From<&'t str>> Converter for Conv<Ty> {
        type Ty = Ty;
        fn conv(s: &str) -> Option<Self::Ty> {
            Some(Ty::from(s))
        }
    }
    use crate::auto::tsq_ser_meta::TreeToQuery;
    let _query: TreeToQuery<_, TIdN, Conv<TIdN::Ty>> =
        TreeToQuery::with_pred(stores, from, "(identifier) (type_identifier)");
    let _query = _query.to_string();
    let (mut query_store, query) = crate::search::ts_query(_query.as_bytes());
    const M0: &str = r#"(predicate (identifier) @op (#eq? @op "eq") (parameters (capture (identifier) @id ) (string) @label ))"#;
    println!();
    println!("\nThe meta query:\n{}", M0);

    let (query_store1, query1) = crate::search::ts_query(M0.as_bytes());
    use hyperast::position::structural_pos::StructuralPosition as Pos;
    let path = Pos::new(query);
    let prepared_matcher =
        crate::search::PreparedMatcher::<crate::types::Type>::new(&query_store1, query1);

    let mut per_label = std::collections::HashMap::<String, Vec<(String, Pos<_, _>)>>::default();

    for e in crate::iter::IterAll::new(&query_store, path, query) {
        let capts = prepared_matcher
            .is_matching_and_capture::<_, crate::types::TIdN<_>>(&query_store, e.node());
        let Some(capts) = capts else { continue };
        dbg!(&capts);
        let l_l = (prepared_matcher.captures.iter())
            .position(|x| &x.name == "label")
            .unwrap();
        let l_i = (prepared_matcher.captures.iter())
            .position(|x| &x.name == "id")
            .unwrap();
        let k = capts.by_capture_id(l_l as u32).unwrap().clone();
        let k = k.try_label(&query_store).unwrap();
        let v = capts.by_capture_id(l_i as u32).unwrap().clone();
        let v = v.try_label(&query_store).unwrap();
        per_label
            .entry(k.to_string())
            .or_insert(vec![])
            .push((v.to_string(), e));
    }
    dbg!(&per_label);
    let query_bis = tsq_transform::regen_query(
        &mut query_store,
        query,
        (per_label.values())
            .filter(|l| l.len() == 2)
            .flatten()
            .map(|x| tsq_transform::Action::Delete {
                path: x.1.iter_offsets().collect(),
            })
            .collect(),
    );
    let query = qgen::PP::<_, _>::new(&query_store, query_bis.unwrap()).to_string();
    let query = format!("{} {}", query, PerLabel(per_label.clone()));
    println!("\nThe generified query:\n{}", query);
    let query = crate::search::ts_query2(&mut query_store, query.as_bytes());
    query
}

// TODO remove LabelH, no_fmt_gen already guaranties identity by ignoring leaves holding formatting information
type LabelH = u32;

/// Applies focuses simplifications
///
/// materialized by @focus in provided `meta_simp`
///
/// takes query and try to find occurrences matched with `meta_simp`,
/// the nodes captured by @focus (i.e., the focused nodes)
/// will be used to produce corresponding top level patterns
///
/// Example:
/// ```scheme
/// (block
///     (try_statement)
/// ) @_root
/// ```
/// becomes
/// ```scheme
/// (try_statement) @_root
/// ```
/// when provided with meta_simp:
/// ```scheme
/// (named_node
///     (identifier) (#EQ? "block") .
///     (named_node
///         (identifier) (#EQ? "try_statement")
///     ) @focus .
///     (capture
///         (identifier) (#EQ? "_root")
///     )
/// )
/// ```
fn simp_focus(
    query_store: &mut QStore,
    query: IdNQ,
    meta_simp: &hyperast_tsquery::Query,
) -> Vec<IdNQ> {
    let focus = if let Some(cid) = meta_simp.capture_index_for_name("focus") {
        use hyperast::position::structural_pos::CursorHead;
        find_matches_aux(query_store, query, meta_simp, cid).collect_vec(|x| x.node())
    } else {
        vec![]
    };

    let root_cap = make_cap(query_store, "_root");
    use hyperast::types::WithChildren;
    focus
        .into_iter()
        .map(|query| {
            use hyperast::types::HyperType;
            let main_query = query_store.node_store.resolve(query);
            assert_eq!(
                query_store.resolve_type(&query).as_static_str(),
                "named_node"
            );
            let mut new_q: Vec<_> = main_query.children().unwrap().collect();
            new_q.push(root_cap);

            let mut md_cache = Default::default();
            use hyperast::types::LabelStore as _;
            let mty_l = query_store.label_store.get_or_insert("");
            let mut query_tree_gen = qgen::TsQueryTreeGen::new(query_store, &mut md_cache);
            use crate::types::Type::NamedNode;
            let new_q = query_tree_gen.build_then_insert(query, NamedNode, None, new_q);
            use crate::types::Type::Program;
            let new_q = query_tree_gen.build_then_insert(query, Program, Some(mty_l), vec![new_q]);
            new_q
        })
        .collect()
}

fn simp_positional_eq(
    query_store: &mut QStore,
    query: IdNQ,
    meta_simp: &hyperast_tsquery::Query,
    auto_caps: &mut Vec<IdNQ>,
) -> Option<IdNQ> {
    // merge positional predicates with identical labels
    let mut per_label = simp_search_positional_preds(query_store, query, meta_simp);
    let query = replace_preds_with_caps(
        query_store,
        query,
        per_label.values_mut().collect(),
        auto_caps,
    )?;
    let preds = format!("(_) {}", PerLabel(per_label));
    let mut md_cache = Default::default();
    let preds = try_ts_query(query_store, &mut md_cache, preds.as_bytes(), |n, t| {
        log::warn!(
            "Error parsing predicates for simp_positional_eq: {}\n{}",
            t.root_node().to_sexp(),
            preds
        );
        panic!();
        Some(n)
    })?;
    let preds = preds.local.compressed_node;
    use hyperast::types::WithChildren;
    // eprintln!(
    //     "{}",
    //     hyperast::nodes::SyntaxSerializer::new(query_store, preds)
    // );

    // dbg!(qgen::PP::<_, _>::new(&*query_store, preds).to_string());
    let main_query = query_store.node_store.resolve(query).child(&0).unwrap();

    let mut new_q = vec![main_query];
    let preds = query_store.node_store.resolve(preds);
    let preds = Children::<Idx, _>::after(&(preds).children().unwrap(), 1);
    new_q.extend(preds);

    use hyperast::types::LabelStore as _;
    let mty_l = query_store.label_store.get_or_insert("");
    let mut query_tree_gen = qgen::TsQueryTreeGen::new(query_store, &mut md_cache);
    use crate::types::Type::Program;
    let new_q = query_tree_gen.build_then_insert(query, Program, Some(mty_l), new_q);

    // eprintln!(
    //     "{}",
    //     hyperast::nodes::SyntaxSerializer::new(query_store, new_q)
    // );

    Some(new_q)
}

/// remove a matched thing from query
fn simp_rms<'a>(
    query_store: &'a mut QStore,
    query: NodeIdentifier,
    meta_simp: &'a hyperast_tsquery::Query,
) -> impl Iterator<Item = NodeIdentifier> + 'a {
    let rms = if let Some(cid) = meta_simp.capture_index_for_name("rm") {
        find_matches(query_store, query, meta_simp, cid)
    } else {
        vec![]
    };
    rms.into_iter().filter_map(move |path| {
        let query = apply_rms_aux(query_store, query, &path)?;
        if !simp_search_need(query_store, query, meta_simp) {
            return None;
        }
        Some(query)
    })
}

type SimpRmsRemoves = IdNQ;
type SimpRmsRemains = (IdNQ, PendingRmPath);
type PendingRmPath = Vec<u16>;
type Simplified<Init> = (IdNQ, TR<Init>);

fn try_simp_rms<'a>(
    query_store: &'a QStore,
    query: IdNQ,
    meta_simp: &'a hyperast_tsquery::Query,
    cid: hyperast_tsquery::CaptureId,
) -> impl Iterator<Item = Result<SimpRmsRemoves, SimpRmsRemains>> + 'a {
    let rms = find_matches(query_store, query, meta_simp, cid);
    rms.into_iter().filter_map(move |path| {
        log::info!("to remove: {:?}", path);
        let Some(query) = try_apply_rms_aux(query_store, query, &path) else {
            return Some(Err((query, path)));
        };
        if !simp_search_need(query_store, query, meta_simp) {
            return None;
        }
        Some(Ok(query))
    })
}

type RmAllAlt = (IdNQ, IdNQ, Vec<PendingRmPath>);

fn try_simp_rmalls<'a>(
    query_store: &'a QStore,
    query: IdNQ,
    meta_simp: &'a hyperast_tsquery::Query,
    cid: hyperast_tsquery::CaptureId,
) -> Option<Result<SimpRmsRemoves, RmAllAlt>> {
    let mut rms = find_matches(query_store, query, meta_simp, cid);
    for rm in &mut rms {
        rm.pop();
        rm.reverse();
    }
    rms.sort();
    rms.reverse();
    let mut curr = query;
    for i in 0..rms.len() {
        let Some(query) = try_apply_rms_aux2(query_store, curr, &rms[i]) else {
            return Some(Err((query, curr, rms[i..].into())));
        };
        if !simp_search_need(query_store, query, meta_simp) {
            return None;
        }
        curr = query;
    }
    if curr == query {
        return None;
    }
    let query = curr;
    Some(Ok(query))
}

fn simp_uniq<'a>(
    query_store: &'a mut QStore,
    query: IdNQ,
    meta_simp: &'a hyperast_tsquery::Query,
) -> impl Iterator<Item = SimpRmsRemoves> + 'a {
    let m = if let Some(cid) = meta_simp.capture_index_for_name("uniq") {
        find_matches(query_store, query, meta_simp, cid)
    } else {
        vec![]
    };
    (0..m.len()).filter_map(move |i| {
        let paths = m
            .iter()
            .enumerate()
            .filter_map(|(j, x)| (i != j).then_some(x))
            .rev();
        let mut curr = query;

        // TODO perf: do all the removes at once
        dbg!(&paths);
        for path in paths {
            curr = apply_rms_aux(query_store, curr, path)?;
        }
        if !simp_search_need(query_store, query, meta_simp) {
            return None;
        }
        Some(query)
    })
}

enum ResSimpUniq {
    Deduplicated(IdNQ),                      // Some Ok
    NeedMut(IdNQ, IdNQ, Vec<PendingRmPath>), // Some Err
    AlreadyUniq(IdNQ),                       // None
}

fn try_simp_uniq<'a>(
    query_store: &'a QStore,
    query: IdNQ,
    meta_simp: &'a hyperast_tsquery::Query,
) -> impl Iterator<
    Item = ResSimpUniq, // Result<(NodeIdentifier, LabelH), (NodeIdentifier, NodeIdentifier, Vec<Vec<u16>>)>,
> + 'a {
    let m = if let Some(cid) = meta_simp.capture_index_for_name("uniq") {
        find_matches(query_store, query, meta_simp, cid)
    } else {
        vec![vec![]]
    };
    let already = if m.len() == 1 { vec![query] } else { vec![] };
    let candidates = if m.len() == 1 { 0..0 } else { 0..m.len() };
    candidates
        .map(move |i| {
            assert_ne!(m.len(), 1);
            let mut curr = query;
            // TODO perf: do all then removes at once
            let mut it = m.iter().enumerate().rev();
            // while let Some((j, path)) = it.next() {
            // if i == j {
            //     continue;
            // }
            // let Some(q) = try_apply_rms_aux(query_store, curr, path) else {
            return ResSimpUniq::NeedMut(
                query,
                curr,
                it
                    // .chain([(j, path)])
                    .filter_map(|(j, x)| (i != j).then_some(x).cloned())
                    .collect(),
            );
            // };
            // curr = q
            // }
            if !simp_search_need(query_store, curr, meta_simp) {
                unimplemented!("don't know what to do there");
            }
            assert_ne!(curr, query);
            ResSimpUniq::Deduplicated(curr)
        })
        .chain(already.into_iter().map(ResSimpUniq::AlreadyUniq))
}

fn generate_query_aux<
    TS: TypeStore + RoleStore,
    TIdN: TypedNodeId<IdN = NodeIdentifier>,
    T,
    Init: SolvedPosition<IdN>,
>(
    query_store: &mut QStore,
    md_cache: &mut qgen::MDCache,
    stores: &SimpleStores<TS>,
    from: Init,
    meta_gen: &hyperast_tsquery::Query,
    f: &impl Fn(qgen::FNode) -> T,
) -> Option<(IdNQ, T)>
where
    TIdN::Ty: types::TypeTrait,
    TS::IdF: From<u16> + Into<u16>,
{
    use crate::auto::tsq_ser_meta2::TreeToQuery;
    let query = TreeToQuery::<_, TIdN>::new(stores, from.node(), meta_gen.clone());
    let query = format!("{} @_root", query);
    try_ts_query(query_store, md_cache, query.as_bytes(), |n, t| {
        log::warn!("Error parsing query: {}", t.root_node().to_sexp());
        None
    })
    .map(|full_node| (full_node.local.compressed_node, f(full_node)))
}

fn simp_search_atleast(
    query_store: &QStore,
    query: IdNQ,
    meta_simp: &hyperast_tsquery::Query,
) -> bool {
    let Some(cid) = meta_simp.capture_index_for_name("atleast") else {
        return true;
    };
    let pos = hyperast::position::structural_pos::CursorWithPersistence::new(query);
    let cursor = hyperast_tsquery::hyperast_opt::TreeCursor::new(query_store, pos);
    let mut matches = meta_simp.matches(cursor);
    // at least one match
    loop {
        let Some(m) = matches.next() else {
            return false;
        };
        if m.nodes_for_capture_index(cid).next().is_some() {
            return true;
        }
    }
}

fn simp_search_need(
    query_store: &QStore,
    query: IdNQ,
    meta_simp: &hyperast_tsquery::Query,
) -> bool {
    let Some(cid) = meta_simp.capture_index_for_name("need") else {
        return true;
    };
    let pos = hyperast::position::structural_pos::CursorWithPersistence::new(query);
    let cursor = hyperast_tsquery::hyperast_opt::TreeCursor::new(query_store, pos);
    let mut matches = meta_simp.matches(cursor);
    let mut bs = bitvec::bitvec!(0;meta_simp.pattern_count());
    meta_simp
        .quants(cid)
        .for_each(|i| bs.set(i.to_usize(), true));
    loop {
        let Some(m) = matches.next() else {
            return bs.count_ones() == 0;
        };
        if m.nodes_for_capture_index(cid).next().is_some() {
            bs.set(m.pattern_index.to_usize(), false);
        }
    }
}

pub fn pred_uniq(query_store: &QStore, query: IdNQ, meta_simp: &hyperast_tsquery::Query) -> bool {
    let cid = meta_simp.capture_index_for_name("uniq");
    let Some(cid) = cid else {
        return true;
    };
    let pos = hyperast::position::structural_pos::CursorWithPersistence::new(query);
    let cursor = hyperast_tsquery::hyperast_opt::TreeCursor::new(query_store, pos);
    let mut matches = meta_simp.matches(cursor);
    // exactly one match, unique
    let mut found = false;
    loop {
        let Some(m) = matches.next() else {
            return found;
        };
        if m.nodes_for_capture_index(cid).next().is_some() {
            if found {
                return false;
            }
            found = true;
        }
    }
}

// fn simp_search_need2(
//     query_store: &QStore,
//     query: IdNQ,
//     meta_simp: &hyperast_tsquery::Query,
// ) -> bool {
//     let need = meta_simp.capture_index_for_name("need");
//     let uniq = meta_simp.capture_index_for_name("uniq");
//     if need.is_none() && uniq.is_none() {
//         return true;
//     };
//     let pos = hyperast::position::structural_pos::CursorWithPersistence::new(query);
//     let cursor = hyperast_tsquery::hyperast_opt::TreeCursor::new(query_store, pos);
//     let mut matches = meta_simp.matches(cursor);
//     // // at least one match
//     // loop {
//     //     let Some(m) = matches.next() else {
//     //         return false;
//     //     };
//     //     if m.nodes_for_capture_index(cid).next().is_some() {
//     //         return true;
//     //     }
//     // }
//     // // exactly one match, unique
//     // let mut found = false;
//     // loop {
//     //     let Some(m) = matches.next() else {
//     //         return found;
//     //     };
//     //     if m.nodes_for_capture_index(cid).next().is_some() {
//     //         if found {
//     //             return false;
//     //         }
//     //         found = true;
//     //     }
//     // }
//     // both
//     let mut found = false;
//     let mut found_need = false;
//     loop {
//         let Some(m) = matches.next() else {
//             return found;
//         };
//         if let Some(_cid) = uniq {}
//         if let Some(cid) = need {
//             let q = meta_simp.quant(m.pattern_index, cid);
//             if matches!(
//                 q,
//                 hyperast_tsquery::CaptureQuantifier::One
//                     | hyperast_tsquery::CaptureQuantifier::OneOrMore
//             ) {}
//         }

//         let mut need = need.iter().flat_map(|cid| m.nodes_for_capture_index(*cid));
//         let mut uniq = uniq.iter().flat_map(|cid| m.nodes_for_capture_index(*cid));
//         loop {
//             if uniq.next().is_some() {
//                 found = true;
//             }
//             if need.next().is_some() {
//                 if found {
//                     panic!("cannot match @uniq and @need in the same pattern")
//                     // TODO think about sem. of this case
//                 }
//                 return true;
//             }
//         }
//         // if m.nodes_for_capture_index(cid).next().is_some() {
//         //     if found {
//         //         return false;
//         //     }
//         //     found = true;
//         // }
//     }
// }

fn simp_search_rm(
    query_store: &QStore,
    query: IdNQ,
    meta_simp: &hyperast_tsquery::Query,
) -> Vec<P> {
    let Some(cid) = meta_simp.capture_index_for_name("rm") else {
        return vec![];
    };
    find_matches(query_store, query, meta_simp, cid)
}

fn simp_search_uniq(
    query_store: &QStore,
    query: IdNQ,
    meta_simp: &hyperast_tsquery::Query,
) -> Vec<P> {
    let Some(cid) = meta_simp.capture_index_for_name("uniq") else {
        return vec![];
    };
    find_matches(query_store, query, meta_simp, cid)
}

pub fn find_matches(
    query_store: &QStore,
    query: IdNQ,
    meta_simp: &hyperast_tsquery::Query,
    cid: hyperast_tsquery::CaptureId,
) -> Vec<P> {
    let set = find_matches_aux(query_store, query, meta_simp, cid);
    set.collect_vec(|p| p.offsets())
}

pub fn find_matches_aux(
    query_store: &QStore,
    query: IdNQ,
    meta_simp: &hyperast_tsquery::Query,
    cid: hyperast_tsquery::CaptureId,
) -> position::structural_pos::CursorWithPersistenceOrderedSet<IdNQ> {
    let mut pos = hyperast::position::structural_pos::CursorWithPersistence::new(query);
    let mut set = pos.build_empty_set();
    let cursor = hyperast_tsquery::hyperast_opt::TreeCursor::new(query_store, pos);
    let mut matches = meta_simp.matches(cursor);
    loop {
        let Some(m) = matches.next() else {
            break;
        };
        log::trace!("found match {}", m.pattern_index.to_usize());
        for p in m.nodes_for_capture_index(cid) {
            log::trace!("found capture for match");
            set.register(&p.pos);
        }
    }
    set
}

fn apply_rms_aux(query_store: &mut QStore, query: IdNQ, path: &PendingRmPath) -> Option<IdNQ> {
    let mut path = path.clone();
    path.pop();
    path.reverse();
    let action = tsq_transform::Action::Delete { path };
    let actions = vec![action];

    tsq_transform::regen_query(query_store, query, actions)
}

fn try_apply_rms_aux(query_store: &QStore, query: IdNQ, path: &PendingRmPath) -> Option<IdNQ> {
    let mut path = path.clone();
    path.pop();
    path.reverse();
    let action = tsq_transform::Action::Delete { path };
    let actions = vec![action];

    tsq_transform::try_regen_query(query_store, query, actions)
}

fn apply_rms_aux2(query_store: &mut QStore, query: IdNQ, path: &PendingRmPath) -> Option<IdNQ> {
    let mut path = path.clone();
    let action = tsq_transform::Action::Delete { path };
    let actions = vec![action];

    tsq_transform::regen_query(query_store, query, actions)
}

fn try_apply_rms_aux2(query_store: &QStore, query: IdNQ, path: &PendingRmPath) -> Option<IdNQ> {
    let mut path = path.clone();
    let action = tsq_transform::Action::Delete { path };
    let actions = vec![action];

    tsq_transform::try_regen_query(query_store, query, actions)
}

pub fn replace_preds_with_caps(
    query_store: &mut QStore,
    query: IdNQ,
    per_label_values: Vec<&mut Vec<(String, PendingRmPath)>>,
    auto_caps: &mut Vec<IdNQ>,
) -> Option<IdNQ> {
    let mut count = 0;
    let mut values: Vec<_> = per_label_values;
    values.sort_by_key(|x| x.iter().map(|x| &x.1).max().unwrap_or(&vec![]).clone());
    let mut actions: Vec<_> = values
        .into_iter()
        .filter(|l| l.len() >= 2)
        .flatten()
        .map(|x| {
            let new = if x.0.is_empty() {
                x.0 = format!("p{}", count);
                let cap = if let Some(cap) = auto_caps.get(count) {
                    *cap
                } else {
                    auto_caps.push(make_cap(query_store, &x.0));
                    *auto_caps.get(count).unwrap()
                };
                count += 1;
                cap
            } else {
                make_cap(query_store, &x.0)
            };
            let mut path = x.1.clone();
            path.pop();
            path.reverse();
            (path, new)
        })
        .collect();
    if actions.is_empty() {
        return None;
    }
    actions.sort_by(|a, b| a.0.cmp(&b.0));
    let actions: Vec<_> = actions
        .into_iter()
        .map(|(path, new)| tsq_transform::Action::Replace { path, new })
        .collect();

    tsq_transform::regen_query(query_store, query, actions)
}

// can be cached for a given query_store, but such caching is better done by caller.
// note most importantly it would not require to hold the store mutably
pub fn make_cap(query_store: &mut QStore, name: &str) -> IdNQ {
    assert!(!name.is_empty());
    let q = format!("_ @{}", name);
    let mut md_cache = Default::default();
    let q = try_ts_query(query_store, &mut md_cache, q.as_bytes(), |n, t| Some(n))
        .unwrap_or_else(|| unreachable!("some is returned on_err"))
        .local
        .compressed_node;
    use hyperast::types::WithChildren;
    let q = query_store.node_store.resolve(q).child(&0).unwrap();
    let q = query_store.node_store.resolve(q).child(&1).unwrap(); // NOTE: no spaces now
    // let q = query_store.node_store.resolve(q).child(&2).unwrap();
    // TODO add debug assertion
    q
}

type P = Vec<u16>;
type Lab = String;
type Cap = String;

fn simp_search_positional_preds(
    query_store: &QStore,
    query: IdNQ,
    meta_simp: &hyperast_tsquery::Query,
) -> std::collections::HashMap<Lab, Vec<(Cap, P)>> {
    let mut per_label = std::collections::HashMap::default();
    let pos = hyperast::position::structural_pos::CursorWithPersistence::new(query);
    let cursor = hyperast_tsquery::hyperast_opt::TreeCursor::new(query_store, pos);
    let mut matches = meta_simp.matches(cursor);
    let Some(cid_p) = meta_simp.capture_index_for_name("pred") else {
        return Default::default();
    };
    let Some(cid_l) = meta_simp.capture_index_for_name("label") else {
        return Default::default();
    };
    // let cid_i = meta_simp.capture_index_for_name("id").unwrap();
    loop {
        let Some(capts) = matches.next() else { break };
        let Some(p) = capts.nodes_for_capture_index(cid_p).next() else {
            continue;
        };
        let k = capts.nodes_for_capture_index(cid_l).next().unwrap();
        let k = k.text(matches.cursor().text_provider());
        // let v = capts.nodes_for_capture_index(cid_i).next().unwrap();
        // let v = v.text(());
        let v = "";
        let p = p.pos.clone().offsets();
        per_label
            .entry(k.to_string())
            .or_insert(vec![])
            .push((v.to_string(), p));
    }
    per_label
}

struct PerLabel<P>(std::collections::HashMap<String, Vec<(String, P)>>);
impl<P: Ord> std::fmt::Display for PerLabel<P> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut values: Vec<_> = self.0.values().collect();
        values.sort_by_key(|x| x.iter().map(|x| &x.1).max().unwrap());
        for x in values {
            if x.len() == 2 {
                assert!(!x[0].0.is_empty());
                assert!(!x[1].0.is_empty());
                writeln!(f, "(#eq? @{} @{})", x[0].0, x[1].0)?;
            } else if x.len() == 1 {
                // noop
            } else {
                for y in &x[1..] {
                    assert!(!x[0].0.is_empty());
                    assert!(!y.0.is_empty());
                    writeln!(f, "(#eq? @{} @{})", x[0].0, y.0)?;
                }
                // todo!("need to do combination")
            }
        }
        Ok(())
    }
}

#[cfg(feature = "synth_par")]
pub fn semi_interactive_poset_build<P>(
    b: &mut Builder<'_, P>,
    meta_simp: &hyperast_tsquery::Query,
    mut timeout: impl FnMut() -> bool,
    size_threshold: impl Fn(usize) -> usize,
) where
    P: Eq + Clone + SolvedPosition<IdN> + Sync + Send,
{
    #[derive(PartialEq, Eq, Debug)]
    enum Phase {
        Uniq,
        Removes,
        RemovesAll,
        SimpEq,
        RemovesAll2,
        Removes2,
    }

    type BySize<T> = Vec<T>;
    type SimpEqResult = IdN;
    type SimpEqSource = IdN;
    type SimpEqedSet = Vec<(SimpEqResult, Vec<SimpEqSource>)>;
    // let mut simp_eq_valid_by_construction: BySize<SimpEqedSet> = vec![];

    let mut too_slow = DedupBySize2::<TR<P>>::default();

    assert!(!b.dedup.is_empty());
    let mut active_size = b.dedup.len() - 1;
    let mut active: Vec<_> = b.actives(active_size);
    let mut phase = Phase::Uniq;
    loop {
        if timeout() {
            break;
        }
        log::info!("dedup len: {}", b.dedup.len());
        log::info!("active_size: {}", active_size);
        log::info!("actives: {}", active.len());
        // TODO find an alternative to phases
        // NOTE simp_eq creates larger pattern
        if phase == Phase::Uniq {
            // do it first to reduce noise and remove as many patts as possible
            let (rms, _already) = b.uniques_par_par(active_size, &mut active);
            // b.dedup_uniques_par(active_size, rms);
            // avoid hanging, by ignoring the patterns not shrinking enough
            let mut by_size = b.by_pattern_size(rms);
            let mut largest_by_size = by_size.split_off(&size_threshold(active_size));
            let act = dedup_patterns_by_metric(by_size, &mut b.dedup);
            active.extend(act);
            log::info!("actives after remove all: {}", active.len());

            filter_by_key_par(&mut largest_by_size, &b.dedup);
            let act = dedup_patterns_by_metric(largest_by_size, &mut too_slow);
            log::info!("too_slow added: {}", act.len());
        } else if phase == Phase::SimpEq {
            // the most tricky simp phase, as it adds a predicate and their captures
            // only do it on patterns under size threshold
            let (simps, not) = b.simp_eq(&mut active);
            let act = {
                b.dedup_uniques_par2(active_size, simps)

                // // avoid hanging, by ignoring the patterns not shrinking enough
                // use hyperast_gen_ts_tsquery::code2query::dedup_patterns_by_metric;
                // let mut by_size = b.by_pattern_size(simps);
                // {
                //     let max_active = by_size.last_key_value().map_or(active_size, |x| *x.0);
                //     let len = max_active.max(b.dedup.len());
                //     b.dedup.resize(len, Default::default());
                // }
                // let shrank_enough =
                //     by_size.split_off(&skink_threshold.min(active_size / skink_factor));
                // let act = dedup_patterns_by_metric(shrank_enough, &mut b.dedup);
                // log::info!("actives after remove all: {}", active.len());

                // hyperast_gen_ts_tsquery::code2query::filter_by_key_par(&mut by_size, &b.dedup);
                // let _act = dedup_patterns_by_metric(by_size, &mut too_slow);
                // log::info!("too_slow added: {}", _act.len());
                // act
            };
            if let Some(cid) = meta_simp.capture_index_for_name("rm.all") {
                active = act; // first rm on the ones that were simp
                let rms = b.removesall_par_par(active_size, &mut active, cid);
                let rms = b.repair_par(rms);
                let rms = b.norm_caps(rms);
                b.dedup_uniques_par2(active_size, rms);
                active = not.clone(); // then the other that were not simp
                let rms = b.removesall_par_par(active_size, &mut active, cid);
                let rms = b.repair_par(rms);
                let rms = b.norm_caps(rms);
                b.dedup_uniques_par2(active_size, rms);
            }
            // TODO link all found simpeq (then rm.all) to previous respective simpeq action,
            // ie. follow one back then re-apply same simpeq action and link to result
            // let call this a reduction and model it as TR::SimpEqReduction(TR)
        } else if phase == Phase::RemovesAll || phase == Phase::RemovesAll2 {
            // do not use naively after simp_eq as it might remove captures
            if let Some(cid) = meta_simp.capture_index_for_name("rm.all.full") {
                let rms = b.removesall_par_par(active_size, &mut active, cid);
                let rms = if phase == Phase::RemovesAll2 {
                    let rms = b.repair_par(rms);
                    let rms = b.norm_caps(rms);
                    rms
                } else {
                    rms
                };
                // TODO add post-removal to make invalid patterns valid again

                // avoid hanging, by ignoring the patterns not shrinking enough
                let mut by_size = b.by_pattern_size(rms);
                let mut largest_by_size = by_size.split_off(&size_threshold(active_size));
                let act = dedup_patterns_by_metric(by_size, &mut b.dedup);
                active.extend(act);
                log::info!("actives after remove all: {}", active.len());

                filter_by_key_par(&mut largest_by_size, &b.dedup);
                let act = dedup_patterns_by_metric(largest_by_size, &mut too_slow);
                log::info!("too_slow added: {}", act.len());
            }
        } else if phase == Phase::Removes || phase == Phase::Removes2 {
            // do not use naively after simp_eq as it might remove captures
            for a in &active {
                log::trace!("try remove: {:?}", b.lattice.pretty(&a));
            }
            let rms = b.removes_par_par(active_size, &mut active);
            for (a, _) in &rms {
                log::info!("to remove: {:?}", b.lattice.pretty(&a));
            }
            let rms = if phase == Phase::Removes2 {
                let rms = b.repair_par(rms);
                let rms = b.norm_caps(rms);
                rms
            } else {
                rms
            };
            for (a, _) in &rms {
                log::info!("repaired: {:?}", b.lattice.pretty(&a));
            }
            // TODO add post-removal to make invalid patterns valid again
            // not that difficult, just need to remove predicate using absent capture
            // and then also remove the unused captures to help with deduplication
            // TODO or put invalid patterns in a separated set
            // detecting invalid is easy, for now I only see capture predicates missing captures
            // then can try to do removals
            // NOTE directly removing the rest is probably faster

            // reduce hanging, by ignoring the patterns not shrinking enough
            let mut by_size = b.by_pattern_size(rms);
            let mut largest_by_size = by_size.split_off(&size_threshold(active_size));
            let act = dedup_patterns_by_metric(by_size, &mut b.dedup);
            active.extend(act);
            log::info!("actives after removes: {}", active.len());

            filter_by_key_par(&mut largest_by_size, &b.dedup);
            let act = dedup_patterns_by_metric(largest_by_size, &mut too_slow);
            log::info!("too_slow added: {}", act.len());
        }
        log::error!("{phase:?} size: {} actives: {}", &active_size, active.len());
        log::warn!("");
        log::info!("");
        if b.between(&mut active_size, &mut active) {
            log::error!("finish phase: {phase:?}");
            if phase == Phase::Uniq {
                // assert!(!b.dedup.is_empty());
                // active_size = b.dedup.len() - 1;
                // active = b.actives(active_size);
                // phase = Phase::RemovesAll;
                phase = Phase::SimpEq;
                // // this phase is complex, so only reasonably sized patterns are considered
                // // tips consider more patterns in remove all
                assert!(!b.dedup.is_empty());
                active_size = b.dedup.len() - 1;
                active = b.actives(active_size);
            } else if phase == Phase::RemovesAll {
                assert!(!b.dedup.is_empty());
                active_size = b.dedup.len() - 1;
                active = b.actives(active_size);
                phase = Phase::Removes;
            } else if phase == Phase::Removes {
                assert!(!b.dedup.is_empty());
                active_size = b.dedup.len() - 1;
                // this phase produce many variants so only reasonably sized patterns are considered
                // tips consider more patterns in remove all
                // active_size = active_size.min(2000);
                phase = Phase::SimpEq;
                // this phase is complex, so only reasonably sized patterns are considered
                // tips consider more patterns in remove all
                active_size = active_size.min(size_threshold(0));
                active = b.actives(active_size);
            } else if phase == Phase::SimpEq {
                phase = Phase::Removes2;
                assert!(!b.dedup.is_empty());
                active_size = b.dedup.len() - 1;
                // active_size = active_size.min(size_threshold(0));
                active = b.actives(active_size);
            } else if phase == Phase::Removes2 {
                phase = Phase::RemovesAll2;
                assert!(!b.dedup.is_empty());
                active_size = b.dedup.len() - 1;
                // active_size = active_size.min(size_threshold(0));
                active = b.actives(active_size);
            } else if phase == Phase::RemovesAll2 {
                break;
            } else {
                unreachable!();
            }
        }
    }
}
