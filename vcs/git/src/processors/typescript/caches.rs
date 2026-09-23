use crate::processing::{CacheHolding, CachesHolding};

pub type Typescript = super::super::ProcessorCache;

impl CachesHolding for super::TypescriptProc {
    type Caches = Typescript;
}

impl CacheHolding<Typescript> for super::TypescriptProc {
    fn get_caches_mut(&mut self) -> &mut Typescript {
        &mut self.cache
    }
    fn get_caches(&self) -> &Typescript {
        &self.cache
    }
}
