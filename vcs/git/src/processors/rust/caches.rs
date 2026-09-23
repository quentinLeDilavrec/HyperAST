use crate::processing::{CacheHolding, CachesHolding};

pub type Rust = crate::processors::ProcessorCache;

impl CachesHolding for super::RustProc {
    type Caches = Rust;
}

impl CacheHolding<Rust> for super::RustProc {
    fn get_caches_mut(&mut self) -> &mut Rust {
        &mut self.cache
    }
    fn get_caches(&self) -> &Rust {
        &self.cache
    }
}
