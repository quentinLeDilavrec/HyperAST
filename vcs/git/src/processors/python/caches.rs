use hyperast::store::defaults::NodeIdentifier;

use crate::processing::caches::NamedMap;
use crate::processing::{CacheHolding, CachesHolding, ObjectMapper, ObjectName};

use super::PrecompQueries;

type MDCache = hyperast::compat::HashMap<NodeIdentifier, PrecompQueries>;

#[derive(Default)]
pub struct Python {
    pub(crate) md_cache: MDCache,
    /// Passed to subtree builder when deriving different data (assumed to be incompatible).
    pub(crate) dedup: hyperast::store::nodes::legion::DedupMap,
    pub object_map: NamedMap<super::FullNode>,
}

impl ObjectMapper for Python {
    type K = (git2::Oid, ObjectName);

    type V = super::FullNode;

    fn get(&self, key: &Self::K) -> Option<&Self::V> {
        self.object_map.get(key)
    }

    fn insert(&mut self, key: Self::K, value: Self::V) -> Option<Self::V> {
        self.object_map.insert(key, value)
    }
}

impl CachesHolding for super::PythonProc {
    type Caches = Python;
}

impl CacheHolding<Python> for super::PythonProc {
    fn get_caches_mut(&mut self) -> &mut Python {
        &mut self.cache
    }
    fn get_caches(&self) -> &Python {
        &self.cache
    }
}
