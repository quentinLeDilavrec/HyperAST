use dashmap::SharedValue;

use hyperast_vcs_git::git::Repo;

use hyper_diff::decompressed_tree_store::{Shallow, lazy_post_order};
use hyperast::position::position_accessors::{self};
use hyperast::store::{SimpleStores, defaults::NodeIdentifier};
use hyperast::types::{self, HyperAST, PrimInt, TypeStore, WithStats};
use hyperast_vcs_git::TStore;
use hyperast_vcs_git::git::Oid;
use num::ToPrimitive;

pub(crate) type IdN = NodeIdentifier;
pub(crate) type Idx = u16;
pub(crate) type IdD = u32;

pub type LPO<T> = SharedValue<lazy_post_order::LazyPostOrder<T, IdD>>;

pub type Position = hyperast::position::StructuralPosition<IdN, Idx>;
pub type Arena<'a, 'b, IdN = self::IdN, IdD = self::IdD> = hyper_diff::matchers::Decompressible<
    &'a NoS<'a>,
    &'b mut lazy_post_order::LazyPostOrder<IdN, IdD>,
>;

pub type NoS<'a> = SimpleStores<
    hyperast_vcs_git::TStore,
    hyperast_vcs_git::no_space::NoSpaceNodeStoreWrapper<'a>,
    &'a hyperast::store::labels::LabelStore,
>;

/// CAUTION a cache should be used on a single HyperAST
/// btw a given HyperAST can be used by multiple caches
pub(crate) fn bind_tree_pair<'a>(
    partial_comp_cache: &'a crate::PartialDecompCache,
    src: &IdN,
    dst: &IdN,
) -> PairLock<
    &'a clashmap::RwLock<hashbrown::HashTable<(IdN, lazy_post_order::LazyPostOrder<IdN, u32>)>>,
> {
    let hasher = partial_comp_cache.hasher().clone();
    let hash1 = partial_comp_cache.hash_usize(src);
    let hash2 = partial_comp_cache.hash_usize(dst);
    let index1 = partial_comp_cache.determine_shard(hash1);
    let index2 = partial_comp_cache.determine_shard(hash2);
    let shards = partial_comp_cache.shards();
    let (shard1, shard2) = if index1 == index2 {
        (&shards[index1], None)
    } else if index1 < index2 {
        let (shards1, shards2) = shards.split_at(index2);
        (&shards1[index1], Some(&shards2[0]))
    } else {
        let (shards2, shards1) = shards.split_at(index1);
        (&shards1[0], Some(&shards2[index2]))
    };
    PairLock {
        shard1: std::ops::Deref::deref(shard1),
        shard2: shard2.map(std::ops::Deref::deref),
        src: *src,
        dst: *dst,
        hasher,
    }
}

pub(crate) struct PairLock<T> {
    shard1: T,
    shard2: Option<T>,
    src: IdN,
    dst: IdN,
    hasher: std::hash::RandomState,
}

impl
    PairLock<
        &clashmap::RwLock<hashbrown::HashTable<(IdN, lazy_post_order::LazyPostOrder<IdN, u32>)>>,
    >
{
    pub fn lock(
        &self,
    ) -> PairLock<
        lock_api::RwLockWriteGuard<
            '_,
            clashmap::RawRwLock,
            hashbrown::HashTable<(IdN, lazy_post_order::LazyPostOrder<IdN, u32>)>,
        >,
    > {
        PairLock {
            shard1: self.shard1.write(),
            shard2: self.shard2.as_ref().map(|x| x.write()),
            src: self.src,
            dst: self.dst,
            hasher: self.hasher.clone(),
        }
    }
}

impl
    PairLock<
        lock_api::RwLockWriteGuard<
            '_,
            clashmap::RawRwLock,
            hashbrown::HashTable<(IdN, lazy_post_order::LazyPostOrder<IdN, u32>)>,
        >,
    >
{
    pub fn as_mut<HAST: HyperAST<IdN = IdN> + Copy>(
        &mut self,
        hyperast: HAST,
    ) -> (
        &mut lazy_post_order::LazyPostOrder<IdN, u32>,
        &mut lazy_post_order::LazyPostOrder<IdN, u32>,
    )
    where
        for<'t> <HAST as types::AstLending<'t>>::RT: WithStats,
    {
        use hyperast::types::DecompressedFrom;
        use hyperast::utils::make_hash;
        use lazy_post_order::LazyPostOrder;
        let src = self.src;
        let dst = self.dst;
        let h1 = make_hash(&self.hasher, &src);
        let h2 = make_hash(&self.hasher, &dst);
        let shard1 = &mut self.shard1;
        let shard2 = &mut self.shard2;
        if shard2.is_none() {
            shard1
                .entry(h1, |(k, _)| *k == src, |(k, _)| make_hash(&self.hasher, k))
                .or_insert_with(|| {
                    let _src = LazyPostOrder::<_, u32>::decompress(hyperast, &src);
                    (src, _src)
                });
            shard1
                .entry(h2, |(k, _)| *k == dst, |(k, _)| make_hash(&self.hasher, k))
                .or_insert_with(|| {
                    let _dst = LazyPostOrder::<_, u32>::decompress(hyperast, &dst);
                    (dst, _dst)
                });
            assert_ne!(src, dst);
            let [v1, v2] =
                shard1.get_many_mut(
                    [h1, h2],
                    |i, (k, _)| if i == 0 { *k == src } else { *k == dst },
                );
            (&mut v1.unwrap().1, &mut v2.unwrap().1)
        } else {
            let v1 = shard1
                .entry(h1, |(k, _)| *k == src, |(k, _)| make_hash(&self.hasher, k))
                .or_insert_with(|| {
                    let _src = LazyPostOrder::<_, u32>::decompress(hyperast, &src);
                    (src, _src)
                });
            let v2 = shard2
                .as_mut()
                .unwrap()
                .entry(h2, |(k, _)| *k == dst, |(k, _)| make_hash(&self.hasher, k))
                .or_insert_with(|| {
                    let _dst = LazyPostOrder::<_, u32>::decompress(hyperast, &dst);
                    (dst, _dst)
                });
            (&mut v1.into_mut().1, &mut v2.into_mut().1)
        }
    }
}

/// Ensures the range is preprocessed --doing it if needed-- while avoiding to lock global state
pub(crate) fn handle_pre_processing(
    state: &std::sync::Arc<crate::AppState>,
    repo: &mut hyperast_vcs_git::processing::ConfiguredRepo2,
    before: &str,
    after: &str,
    limit: usize,
) -> Result<Vec<hyperast_vcs_git::git::Oid>, Box<dyn std::error::Error>> {
    let rw = hyperast_vcs_git::git::Builder::new(&repo.repo)?
        .before(before)?
        .after(after)?
        .walk()?
        .take(limit)
        .map(|x| x.unwrap());
    // all_commits_between(&repository.repo, before, after)?;
    // NOTE the read with a fallback on a write ensures that we are not waiting to, in the end, not writing anything
    // TODO later start processing the commit subset and schedule the remaining range for processing
    // NOTE a sceduling approach would be much cleaner than the current lock approach
    Ok(handle_pre_processing_aux(state, repo, rw))
}

pub(crate) fn walk_commits_multi<R: AsRef<str>>(
    repo: &hyperast_vcs_git::processing::ConfiguredRepo2,
    after: impl Iterator<Item = R>,
) -> Result<impl Iterator<Item = hyperast_vcs_git::git::Oid> + '_, Box<dyn std::error::Error>> {
    let mut rw = hyperast_vcs_git::git::Builder::new(&repo.repo)?;
    for after in after {
        rw = rw.after(after.as_ref())?;
    }
    let rw = rw.walk()?.map(|x| x.unwrap());
    Ok(rw)
}

/// Ensures the range is preprocessed --doing it if needed-- while avoiding to lock global state
pub(crate) fn handle_pre_processing_aux(
    state: &std::sync::Arc<crate::AppState>,
    repo: &hyperast_vcs_git::processing::ConfiguredRepo2,
    rw: impl Iterator<Item = hyperast_vcs_git::git::Oid>,
) -> Vec<hyperast_vcs_git::git::Oid> {
    let mut rw = rw.peekable();
    let commits = {
        state
            .repositories
            .read()
            .unwrap()
            .processor
            .ensure_prepro(&mut rw, repo)
    };
    match commits {
        Ok(commits) => commits,
        Err(mut commits) => {
            let repository_processor = &mut state.repositories.write().unwrap().processor;
            commits.extend(repository_processor.pre_pro(&mut rw, repo, usize::MAX));
            commits
        }
    }
}

#[derive(serde::Deserialize, serde::Serialize, Debug, Clone)]
pub struct PieceOfCode<IdN = self::IdN, Idx = usize> {
    pub user: String,
    pub name: String,
    #[serde(deserialize_with = "string_to_oid")]
    #[serde(serialize_with = "oid_to_string")]
    pub commit: Oid,
    #[serde(default = "Vec::new")]
    pub path: Vec<Idx>,
    #[serde(default)]
    pub file: String,
    #[serde(default)]
    pub start: usize,
    #[serde(default)]
    pub end: usize,
    #[serde(bound(serialize = "IdN: Clone + Into<self::IdN>"))]
    #[serde(serialize_with = "custom_ser")]
    #[serde(skip_serializing_if = "Vec::is_empty")]
    #[serde(default = "Vec::new")]
    pub path_ids: Vec<IdN>, // WARN this is not fetched::NodeIdentifier
}

pub(crate) fn oid_to_string<S: serde::Serializer>(
    x: &Oid,
    serializer: S,
) -> Result<S::Ok, S::Error> {
    serializer.collect_str(x)
}

pub(crate) fn string_to_oid<'de, D: serde::Deserializer<'de>>(
    deserializer: D,
) -> Result<Oid, D::Error> {
    let s = <&str as serde::Deserialize>::deserialize(deserializer)?;
    Oid::from_str(s).map_err(|e| serde::de::Error::custom(e))
}

fn custom_ser<IdN: Clone + Into<self::IdN>, S>(
    x: &Vec<IdN>,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    use serde::ser::SerializeSeq;
    let mut seq = serializer.serialize_seq(Some(x.len()))?;
    for element in x {
        let element: self::IdN = element.clone().into();
        let id: u64 = unsafe { std::mem::transmute(element) };
        if id > u32::MAX as u64 {
            log::error!("node ids are too big, it will lead to bugs when used")
        }
        seq.serialize_element(&id)?;
    }
    seq.end()
}

#[derive(Clone, PartialEq, Debug)]
pub struct LocalPieceOfCode<IdN, Idx> {
    pub file: String,
    pub start: usize,
    pub end: usize,
    pub path: Vec<Idx>,
    pub path_ids: Vec<IdN>,
}

impl<'a, S, IdN: Clone, Idx: Clone> From<(&S, &'a LocalPieceOfCode<IdN, Idx>)>
    for LocalPieceOfCode<IdN, Idx>
{
    fn from((_, p): (&S, &'a LocalPieceOfCode<IdN, Idx>)) -> Self {
        p.clone()
    }
}

impl<Idx> LocalPieceOfCode<IdN, Idx> {
    pub(crate) fn from_root_and_offsets<TS: TypeStore>(
        stores: &SimpleStores<TS>,
        root: IdN,
        path: Vec<impl PrimInt>,
    ) -> Self
    where
        Idx: PrimInt,
    {
        use hyperast::position::compute_position_and_nodes;
        let (pos, path_ids) = compute_position_and_nodes(root, &mut path.iter().copied(), stores);
        let offsets = path.into_iter().map(|x| x.cast()).collect();
        Self::from_position(&pos, offsets, path_ids)
    }
}
impl<IdN, Idx> LocalPieceOfCode<IdN, Idx> {
    pub(crate) fn from_position(
        pos: &hyperast::position::Position,
        path: Vec<Idx>,
        path_ids: Vec<IdN>,
    ) -> Self {
        let range = pos.range();
        let file = pos.file();
        Self::from_file_and_range(file, range, path, path_ids)
    }
    pub(crate) fn from_file_and_range(
        file: &std::path::Path,
        range: std::ops::Range<usize>,
        path: Vec<Idx>,
        path_ids: Vec<IdN>,
    ) -> Self {
        let std::ops::Range { start, end } = range;
        let file = file.to_str().unwrap().to_string();
        Self {
            file,
            start,
            end,
            path,
            path_ids,
        }
    }
    pub(crate) fn from_pos<P>(pos: &P) -> Self
    where
        P: position_accessors::WithOffsets<Idx = Idx>
            + position_accessors::WithPreOrderPath<IdN>
            + position_accessors::FileAndOffsetPostionT<IdN, IdO = usize>,
    {
        let mut path = vec![];
        let mut path_ids = vec![];
        for (o, i) in pos.iter_offsets_and_nodes() {
            path.push(o);
            path_ids.push(i);
        }
        Self::from_file_and_range(&pos.file(), pos.start()..pos.end(), path, path_ids)
    }
    pub(crate) fn globalize(self, spec: &Repo, commit: Oid) -> PieceOfCode<IdN, Idx> {
        PieceOfCode {
            user: spec.user().to_string(),
            name: spec.name().to_string(),
            commit: commit,
            path: self.path,
            path_ids: self.path_ids,
            file: self.file,
            start: self.start,
            end: self.end,
        }
    }
    fn map_path<Idx2, F: Fn(Idx) -> Idx2>(self, f: F) -> LocalPieceOfCode<IdN, Idx2> {
        let path = self.path.into_iter().map(f).collect();
        LocalPieceOfCode {
            path,
            path_ids: self.path_ids,
            file: self.file,
            start: self.start,
            end: self.end,
        }
    }
}

pub(crate) fn remap<HAST: HyperAST, NoS, IdD>(
    stores: &HAST,
    x: &impl position_accessors::WithPreOrderOffsets,
    arena: &mut hyper_diff::matchers::Decompressible<
        NoS,
        &mut lazy_post_order::LazyPostOrder<HAST::IdN, IdD>,
    >,
    tr: HAST::IdN,
) -> IdD
where
    for<'t> types::LendT<'t, HAST>: types::WithSerialization,
    IdD: PrimInt + Shallow<IdD>,
    HAST::IdN: types::NodeId<IdN = HAST::IdN>,
    NoS: HyperAST<IdN = HAST::IdN> + Copy,
    for<'t> types::LendT<'t, NoS>: types::WithStats,
{
    let (_, _, path) =
        hyperast::position::compute_position_with_no_spaces(tr, &mut x.iter_offsets(), stores);
    let mut src = arena.root();
    for i in 0..path.len() {
        use hyper_diff::decompressed_tree_store::LazyDecompressedTreeStore;
        let cs = arena.decompress_children(&src);
        let Some(s) = cs.get(path[i].to_usize().unwrap()) else {
            log::debug!("failed to remap at path {:?}.{:?}", &path[..i], &path[i..]);
            return src;
        };
        src = *s;
    }
    src
}
