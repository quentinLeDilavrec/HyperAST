pub type CCache = super::super::ProcessorCache;

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
