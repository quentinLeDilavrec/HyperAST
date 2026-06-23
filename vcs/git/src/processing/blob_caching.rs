use git2::{Oid, Repository};
use std::marker::PhantomData;

use super::ParametrizedProcessorHandle;
use super::ProcessorHolder;
use super::erased::CommitProc;
use super::erased::ParametrizedCommitProc2;
use super::erased::ParametrizedProc2 as _;
use super::erased::ProcessorMap;
use super::{CacheHolding, CachesHolding, ObjectMapper};

impl ProcessorMap {
    pub(crate) fn caching_blob_handler<C>(&mut self) -> CachingBlobWrapper2<'_, C> {
        CachingBlobWrapper2 {
            processors: self,
            phantom: PhantomData,
        }
    }
}

pub(crate) struct CachingBlobWrapper2<'cache, C> {
    pub(crate) processors: &'cache mut ProcessorMap,
    pub(crate) phantom: PhantomData<C>,
}

impl<'cache, Sys> CachingBlobWrapper2<'cache, Sys> {
    pub fn handle<T, N, E, F>(
        &mut self,
        oid: Oid,
        repository: &Repository,
        name: &N,
        parameters: ParametrizedProcessorHandle<T>,
        wrapped: F,
    ) -> Result<<Sys::Caches as ObjectMapper>::V, E>
    where
        E: From<std::str::Utf8Error>,
        F: FnOnce(&mut ProcessorMap, &N, &[u8]) -> Result<<Sys::Caches as ObjectMapper>::V, E>,
        //
        for<'t> &'t N: TryInto<&'t str>,
        Sys: CachesHolding,
        Sys::Caches: 'static + ObjectMapper<K = Oid> + Send + Sync + Default,
        <Sys::Caches as ObjectMapper>::V: Clone,
        //
        T: Send + Sync + 'static,
        T: CacheHolding<Sys::Caches>,
    {
        let caches = self.processors.proc_mut::<ProcessorHolder<T>>();
        let caches = caches.with_parameters_mut0(parameters);
        let caches = caches.get_caches_mut();
        use crate::processing::ObjectMapper;
        if let Some(already) = caches.get(&oid) {
            // TODO reinit already computed node for post order
            let full_node = already.clone();
            return Ok(full_node);
        }
        log::info!(target: "blob",
            "blob {:?} {:?}",
            name.try_into().unwrap_or("'non utf8 name'"),
            oid
        );
        let blob = repository.find_blob(oid).unwrap();
        std::str::from_utf8(blob.content())?;
        let text = blob.content();
        let full_node = wrapped(self.processors, &name, text);
        if let Ok(x) = &full_node {
            self.processors
                // .mut_or_default::<Sys::Holder>().get_caches_mut()
                .proc_mut::<ProcessorHolder<T>>()
                .with_parameters_mut0(parameters)
                .get_caches_mut()
                .insert(oid, x.clone());
        }
        full_node
    }

    pub fn handle2<T, N, E, F>(
        &mut self,
        oid: Oid,
        repository: &Repository,
        name: &N,
        parameters: ParametrizedProcessorHandle<T>,
        wrapped: F,
    ) -> Result<<Sys::Caches as ObjectMapper>::V, E>
    where
        T: CommitProc,
        N: Clone,
        E: From<std::str::Utf8Error>,
        F: FnOnce(&mut ProcessorMap, &N, &[u8]) -> Result<<Sys::Caches as ObjectMapper>::V, E>,
        //
        for<'a> &'a N: TryInto<&'a str>,
        Sys: CachesHolding,
        Sys::Caches: 'static + ObjectMapper<K = (Oid, N)> + Send + Sync + Default,
        <Sys::Caches as ObjectMapper>::V: Clone,
        //
        T: Send + Sync + 'static,
        T: CacheHolding<Sys::Caches>,
    {
        let caches = self.processors.commit_proc_mut::<ProcessorHolder<T>>();
        let caches = caches.with_parameters_mut(parameters);
        let caches = caches.get_caches_mut();
        use crate::processing::ObjectMapper;
        if let Some(already) = caches.get(&(oid, name.clone())) {
            // TODO reinit already computed node for post order
            let full_node = already.clone();
            return Ok(full_node);
        }
        log::info!(target: "blob",
            "blob {:?} {:?}",
            name.try_into().unwrap_or("'non utf8 name'"),
            oid
        );
        let blob = repository.find_blob(oid).unwrap();
        if let Err(err) = std::str::from_utf8(blob.content()) {
            log::warn!("non utf8 char in blob content {}", err);
        }

        // TODO make a proper test later when attempting to be more resilient to ill-formating, ...
        // for ref on dubbo because of chinese chars vscode detects GB 2312
        // [2024-08-27T11:22:37Z INFO  hyperast_vcs_git::preprocessed] handle commit: f8cb608c1eb242544be640f3ae994391729b2175
        // [2024-08-27T11:22:37Z INFO  hyperast_vcs_git::java_processor] tree Ok("com")
        // [2024-08-27T11:22:37Z INFO  hyperast_vcs_git::java_processor] tree Ok("alibaba")
        // [2024-08-27T11:22:37Z INFO  hyperast_vcs_git::java_processor] tree Ok("dubbo")
        // [2024-08-27T11:22:37Z INFO  hyperast_vcs_git::java_processor] tree Ok("config")
        // [2024-08-27T11:22:37Z INFO  hyperast_vcs_git::java_processor] tree Ok("utils")
        // [2024-08-27T11:22:37Z INFO  hyperast_vcs_git::preprocessed] blob "ReferenceConfigCacheTest.java" 3452c2ebc75e77ccf23a5c6cd7c1823f77096aa3
        // thread 'tokio-runtime-worker' panicked at cvs/git/src/java_processor.rs:112:26:

        let text = blob.content();
        let full_node = wrapped(self.processors, &name, text);
        if let Ok(x) = &full_node {
            self.processors
                .commit_proc_mut::<ProcessorHolder<T>>()
                .with_parameters_mut(parameters)
                .get_caches_mut()
                .insert((oid, name.clone()), x.clone());
        }
        full_node
    }
}
