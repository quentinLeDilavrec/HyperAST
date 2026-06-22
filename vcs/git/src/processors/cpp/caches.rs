use crate::processing::{ObjectMapper, ObjectName, caches::NamedMap};

#[derive(Default)]
pub struct CppCache {
    pub(crate) md_cache: hyperast_gen_ts_cpp::legion::MDCache,
    pub(crate) dedup: hyperast::store::nodes::legion::DedupMap,
    pub object_map: NamedMap<super::FullNode>,
}

impl ObjectMapper for CppCache {
    type K = (git2::Oid, ObjectName);

    type V = super::FullNode;

    fn get(&self, key: &Self::K) -> Option<&Self::V> {
        self.object_map.get(key)
    }

    fn insert(&mut self, key: Self::K, value: Self::V) -> Option<Self::V> {
        self.object_map.insert(key, value)
    }
}

impl crate::processing::CachesHolding for super::CppProc {
    type Caches = CppCache;
}

impl crate::processing::CacheHolding<CppCache> for super::CppProc {
    fn get_caches_mut(&mut self) -> &mut CppCache {
        &mut self.cache
    }
    fn get_caches(&self) -> &CppCache {
        &self.cache
    }
}
