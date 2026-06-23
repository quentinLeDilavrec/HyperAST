use hyperast::store::defaults::NodeIdentifier;

use crate::processing::caches::NamedMap;
use crate::processing::{ObjectMapper, ObjectName};

type MDCache = hyperast::compat::HashMap<NodeIdentifier, super::PrecompQueries>;

#[derive(Default)]
pub struct CCache {
    pub(crate) md_cache: MDCache,
    pub(crate) dedup: hyperast::store::nodes::legion::DedupMap,
    pub object_map: NamedMap<super::FullNode>,
}

impl ObjectMapper for CCache {
    type K = (git2::Oid, ObjectName);

    type V = super::FullNode;

    fn get(&self, key: &Self::K) -> Option<&Self::V> {
        self.object_map.get(key)
    }

    fn insert(&mut self, key: Self::K, value: Self::V) -> Option<Self::V> {
        self.object_map.insert(key, value)
    }
}

impl crate::processing::CachesHolding for super::CProc {
    type Caches = CCache;
}

impl crate::processing::CacheHolding<CCache> for super::CProc {
    fn get_caches_mut(&mut self) -> &mut CCache {
        &mut self.cache
    }
    fn get_caches(&self) -> &CCache {
        &self.cache
    }
}
