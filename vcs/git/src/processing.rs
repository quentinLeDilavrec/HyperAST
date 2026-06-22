//! traits and structures modeling the processing model of this crate

mod blob_caching;
pub(crate) mod caches;
pub mod configurations;
pub mod erased;
pub mod object_mapper;

pub use configurations::RepoConfig;
pub use erased::ProcessorHolder;
pub use object_mapper::{ObjectMapper, ObjectName};

use crate::git::Repo;

/// A git Commit contains a Tree (ie. a directory in a file system) that contain other Trees and end with Blobs (ie. files).
/// It can follow a specific scheme,
/// and is often related to a specific build system or language.
pub mod file_sys;

pub trait ConfiguredRepoTrait {
    fn spec(&self) -> &Repo;
    type Config;
    fn config(&self) -> &Self::Config;
}

pub struct ConfiguredRepoHandle {
    pub spec: Repo,
    pub config: RepoConfig,
}

// NOTE could have impl deref but it is a bad idea (see rust book, related to ownership)
impl ConfiguredRepoTrait for ConfiguredRepoHandle {
    fn spec(&self) -> &Repo {
        &self.spec
    }
    type Config = RepoConfig;
    fn config(&self) -> &Self::Config {
        &self.config
    }
}

impl ConfiguredRepoHandle {
    pub fn fetch(self) -> ConfiguredRepo {
        ConfiguredRepo {
            repo: self.spec.fetch(),
            spec: self.spec,
            config: self.config,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ConfiguredRepoHandle2 {
    pub spec: Repo,
    pub config: erased::ParametrizedCommitProcessorHandle,
}

// NOTE could have impl deref but it is a bad idea (see rust book, related to ownership)
impl ConfiguredRepoTrait for ConfiguredRepoHandle2 {
    fn spec(&self) -> &Repo {
        &self.spec
    }
    type Config = erased::ParametrizedCommitProcessorHandle;

    fn config(&self) -> &Self::Config {
        &self.config
    }
}
impl ConfiguredRepoHandle2 {
    pub fn fetch(self) -> ConfiguredRepo2 {
        ConfiguredRepo2 {
            repo: self.spec.fetch(),
            spec: self.spec,
            config: self.config,
        }
    }
    pub fn nofetch(self) -> ConfiguredRepo2 {
        ConfiguredRepo2 {
            repo: self.spec.nofetch(),
            spec: self.spec,
            config: self.config,
        }
    }
}

pub struct ConfiguredRepo {
    pub spec: Repo,
    pub repo: git2::Repository,
    pub config: RepoConfig,
}

impl ConfiguredRepoTrait for ConfiguredRepo {
    fn spec(&self) -> &Repo {
        &self.spec
    }
    type Config = RepoConfig;
    fn config(&self) -> &Self::Config {
        &self.config
    }
}

pub struct ConfiguredRepo2 {
    pub spec: Repo,
    pub repo: git2::Repository,
    pub config: erased::ParametrizedCommitProcessorHandle,
}

impl ConfiguredRepoTrait for ConfiguredRepo2 {
    fn spec(&self) -> &Repo {
        &self.spec
    }
    type Config = erased::ParametrizedCommitProcessorHandle;
    fn config(&self) -> &Self::Config {
        &self.config
    }
}

/// Handle over a set of configuration parameters for a parametrized commit processor.
#[derive(Clone, Copy, Debug)]
pub struct ConfigParametersHandle(
    /// A unique identifier for the set of configuration parameters.
    pub usize,
);

/// Parametrized handle over a processor T, composing [`ConfigParametersHandle`].
///
/// If you want to store a [`ParametrizedProcessor2Handle`] at runtime, use [`erased::ParametrizedCommitProcessorHandle`] instead.
#[derive(Debug)]
pub struct ParametrizedProcessor2Handle<T>(
    pub ConfigParametersHandle,
    pub(crate) std::marker::PhantomData<T>,
);
use ParametrizedProcessor2Handle as PCP2Handle;

impl<T> PartialEq for PCP2Handle<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0.0 == other.0.0 && self.1 == other.1
    }
}
impl<T> Eq for PCP2Handle<T> {}
impl<T> Clone for PCP2Handle<T> {
    fn clone(&self) -> Self {
        PCP2Handle(self.0, self.1)
    }
}
impl<T> Copy for PCP2Handle<T> {}
impl<T> std::ops::Deref for PCP2Handle<T> {
    type Target = ConfigParametersHandle;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

// TODO make a macro to generate the implementation and the wrapping type
pub trait CachesHolding {
    /// WARN if you use the same cache type in multiple holders it mean that they are effectively shared caches
    /// TIPs use a privately constructed wrapping type to protect against inadvertent sharing
    type Caches;

    // fn mut_or_default(&mut self) -> &mut Self::Caches;
}

pub trait CacheHolding<Caches> {
    fn get_caches_mut(&mut self) -> &mut Caches;
    fn get_caches(&self) -> &Caches;
}

pub trait HoldedCache: Sized {
    type Holder: CacheHolding<Self>;
}

pub trait InFiles {
    fn matches(name: &ObjectName) -> bool;
}

pub trait NamedObject {
    fn name(&self) -> &ObjectName;
}

pub enum ObjectType {
    File,
    Dir,
}

pub trait TypedObject {
    fn r#type(&self) -> ObjectType;
}
pub trait UniqueObject {
    type Id: Clone;
    fn id(&self) -> &Self::Id;
}

mod experiments {

    macro_rules! make_multi {
        ($($wb:tt)*) => {};
    }

    make_multi! {
        Java(Java, ),
        Pom,
        Cpp,
        MakeFile,
        Ts,
        Js,
        ;
        Maven [Java] Xml => crate::maven::Md,
        Make [Cpp] MakeFile => crate::make::Md,
        Npm [Ts, Js] Xml => crate::make::Md,
        None => crate::make::Md,
    }
}
