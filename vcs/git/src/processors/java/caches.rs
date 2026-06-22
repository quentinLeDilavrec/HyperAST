use hyperast::store::nodes::legion::DedupMap;

use crate::processing::caches::NamedMap;
use crate::processing::{ObjectMapper, ObjectName};

#[derive(Default)]
pub struct JavaCaches {
    pub(crate) md_cache: hyperast_gen_ts_java::legion_with_refs::MDCache,
    /// Passed to subtree builder when deriving different data (assumed to be incompatible).
    pub(crate) dedup: DedupMap,
    pub object_map: NamedMap<super::FullNode>,
}

impl ObjectMapper for JavaCaches {
    type K = (git2::Oid, ObjectName);

    type V = super::FullNode;

    fn get(&self, key: &Self::K) -> Option<&Self::V> {
        self.object_map.get(key)
    }

    fn insert(&mut self, key: Self::K, value: Self::V) -> Option<Self::V> {
        self.object_map.insert(key, value)
    }
}

impl crate::processing::CachesHolding for super::JavaProc {
    type Caches = JavaCaches;
}

impl crate::processing::CacheHolding<JavaCaches> for super::JavaProc {
    fn get_caches_mut(&mut self) -> &mut JavaCaches {
        &mut self.cache
    }
    fn get_caches(&self) -> &JavaCaches {
        &self.cache
    }
}
