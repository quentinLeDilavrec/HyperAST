//! traits and structures modeling the processing model of this crate

mod blob_caching;
pub(crate) mod caches;
pub mod configurations;
pub mod erased;
pub mod object_mapper;

pub use configurations::RepoConfig;
pub use object_mapper::{ObjectMapper, ObjectName};

/// A git Commit contains a Tree (ie. a directory in a file system) that contain other Trees and end with Blobs (ie. files).
/// It can follow a specific scheme,
/// and is often related to a specific build system or language.
pub mod file_sys;

use crate::git::Repo;

////////////// repository //////////////

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

////////////// configuration system //////////////

/// Handle over a set of configuration parameters for a parametrized commit processor.
#[derive(Clone, Copy, Debug)]
pub struct ConfigParametersHandle(
    /// A unique identifier for the set of configuration parameters.
    pub usize,
);

/// Parametrized handle over a processor T, composing [`ConfigParametersHandle`].
///
/// If you want to store a [`ParametrizedProcessorHandle`] at runtime, use [`erased::ParametrizedCommitProcessorHandle`] instead.
pub struct ParametrizedProcessorHandle<T>(ConfigParametersHandle, std::marker::PhantomData<T>);

use ParametrizedProcessorHandle as PPHandle;

impl<T> std::fmt::Debug for ParametrizedProcessorHandle<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("PPHandle")
            .field(&self.0)
            .field(&self.1)
            .finish()
    }
}
impl<T> PartialEq for PPHandle<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0.0 == other.0.0 && self.1 == other.1
    }
}
impl<T> Eq for PPHandle<T> {}
impl<T> Clone for PPHandle<T> {
    fn clone(&self) -> Self {
        PPHandle(self.0, self.1)
    }
}
impl<T> Copy for PPHandle<T> {}

/// holds a vector of processors,
/// allowing to register multiple instances of the same processor configured with different parameters
pub struct ProcessorHolder<Proc>(Vec<Proc>);
impl<Proc> Default for ProcessorHolder<Proc> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<Proc> ProcessorHolder<Proc> {
    /// Register a processor `Proc` with the given parameters `T`,
    /// if a processor with the same parameters is already registered, returns the existing handle.
    ///
    /// The returned handle can then be used to retrieve `Proc`.
    pub fn register_param<T: Into<Proc> + PartialEq<Proc>>(&mut self, t: T) -> PPHandle<Proc> {
        let l = self.0.iter().position(|x| t.eq(x)).unwrap_or_else(|| {
            let l = self.0.len();
            self.0.push(t.into());
            l
        });
        PPHandle(ConfigParametersHandle(l), std::marker::PhantomData)
    }
}

impl<Proc: erased::CommitProc + 'static> erased::ParametrizedCommitProc_ for ProcessorHolder<Proc> {
    type Proc = Proc;

    fn _with_parameters_mut(&mut self, parameters: ConfigParametersHandle) -> &mut Self::Proc {
        &mut self.0[parameters.0]
    }

    fn _with_parameters(&self, parameters: ConfigParametersHandle) -> &Self::Proc {
        &self.0[parameters.0]
    }
}

impl<Proc: 'static> erased::ParametrizedProc_ for ProcessorHolder<Proc> {
    type Proc = Proc;

    fn _with_parameters_mut0(&mut self, parameters: ConfigParametersHandle) -> &mut Self::Proc {
        &mut self.0[parameters.0]
    }

    fn _with_parameters0(&self, parameters: ConfigParametersHandle) -> &Self::Proc {
        &self.0[parameters.0]
    }
}

////////////// mostly caching //////////////

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
