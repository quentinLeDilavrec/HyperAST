use crate::processing::caches::OidMap;
use crate::processing::{CacheHolding, ObjectMapper};

#[derive(Default)]
pub struct FileSys {
    pub object_map: OidMap<super::FullNode>,
}

impl ObjectMapper for FileSys {
    type K = git2::Oid;

    type V = super::FullNode;

    fn get(&self, key: &Self::K) -> Option<&Self::V> {
        self.object_map.get(key)
    }

    fn insert(&mut self, key: Self::K, value: Self::V) -> Option<Self::V> {
        self.object_map.insert(key, value)
    }
}

impl CacheHolding<FileSys> for super::FileSysProc {
    fn get_caches_mut(&mut self) -> &mut FileSys {
        &mut self.cache
    }
    fn get_caches(&self) -> &FileSys {
        &self.cache
    }
}
