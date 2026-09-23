use crate::processing::{CacheHolding, CachesHolding};

pub type Python = crate::processors::ProcessorCache;

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
