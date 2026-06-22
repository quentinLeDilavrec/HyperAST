use crate::processing::{ObjectMapper, ObjectName, caches::NamedMap};

#[derive(Default)]
pub struct Cpp {
    pub(crate) md_cache: hyperast_gen_ts_cpp::legion::MDCache,
    pub(crate) dedup: hyperast::store::nodes::legion::DedupMap,
    pub object_map: NamedMap<super::FullNode>,
}

impl ObjectMapper for Cpp {
    type K = (git2::Oid, ObjectName);

    type V = super::FullNode;

    fn get(&self, key: &Self::K) -> Option<&Self::V> {
        self.object_map.get(key)
    }

    fn insert(&mut self, key: Self::K, value: Self::V) -> Option<Self::V> {
        self.object_map.insert(key, value)
    }
}
