use std::any::Any;
use std::marker::PhantomData;
use std::ops::Deref;

use crate::preprocessed::CommitBuilder;

// TODO erase output type from PreparedCommitProc without loosing type information and transparently to implementers
use hyperast::store::defaults::NodeIdentifier as ProcessorOutput;
// same but for erasing the store as it is associated with the output type
use crate::preprocessed::RepositoryProcessor;
// also erase the commit returned by the commit processor
use crate::Commit;

#[derive(Clone)]
#[allow(unused)]
#[allow(deprecated)]
/// A config holding arbitrary parameters for a parametrized commit processor.
pub struct ConfigParameters(std::rc::Rc<dyn std::any::Any>);

pub struct ProcessorHolder<Proc>(Vec<Proc>);
impl<Proc> Default for ProcessorHolder<Proc> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<Proc: CommitProcExt> ProcessorHolder<Proc> {
    pub fn register_param<T: Into<Proc> + PartialEq<Proc>>(&mut self, t: T) -> PCP2Handle<Proc> {
        let l = self.0.iter().position(|x| t.eq(x)).unwrap_or_else(|| {
            let l = self.0.len();
            self.0.push(t.into());
            l
        });
        PCP2Handle(ConfigParametersHandle(l), PhantomData)
    }
}

impl<Proc: CommitProcExt + 'static> ParametrizedCommitProc2 for ProcessorHolder<Proc> {
    type Proc = Proc;

    fn with_parameters_mut(&mut self, parameters: ConfigParametersHandle) -> &mut Self::Proc {
        &mut self.0[parameters.0]
    }

    fn with_parameters(&self, parameters: ConfigParametersHandle) -> &Self::Proc {
        &self.0[parameters.0]
    }
}

/// Handle over a commit processor, resulting from type erasure when registering a commit processor.
#[derive(Clone, Copy, Debug)]
pub struct CommitProcessorHandle(pub(crate) std::any::TypeId);

/// Handle over a set of configuration parameters for a parametrized commit processor.
#[derive(Clone, Copy, Debug)]
pub struct ConfigParametersHandle(
    /// A unique identifier for the set of configuration parameters.
    pub usize,
);

/// Parametrized handle over a processor T, composing [`ConfigParametersHandle`].
///
/// If you want to store a [`ParametrizedProcessor2Handle`] at runtime, use [`ParametrizedCommitProcessorHandle`] instead.
#[derive(Debug)]
pub struct ParametrizedProcessor2Handle<T>(pub ConfigParametersHandle, pub(crate) PhantomData<T>);
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
impl<T: CommitProcExt> Deref for PCP2Handle<T> {
    type Target = ConfigParametersHandle;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: CommitProcExt + 'static> PCP2Handle<T> {
    pub(crate) fn erase(&self) -> PCPHandle {
        let tid = std::any::TypeId::of::<ProcessorHolder<T>>();
        PCPHandle(CommitProcessorHandle(tid), self.0)
    }
}

/// Parametrized handle over a processor like [`ParametrizedProcessor2Handle`], using the erased [`CommitProcessorHandle`].
///
/// If you want to easily refer to the specific commit processor type, use [`ParametrizedProcessor2Handle`] instead.
#[derive(Clone, Copy, Debug)]
pub struct ParametrizedCommitProcessorHandle(pub CommitProcessorHandle, pub ConfigParametersHandle);
use ParametrizedCommitProcessorHandle as PCPHandle;

pub trait CommitProc {
    // TODO remove, just for debugging dynamic dispatch
    fn p(&self) {
        dbg!()
    }
    fn prepare_processing<'repo>(
        &self,
        repository: &'repo git2::Repository,
        commit_builder: CommitBuilder,
        param_handle: PCPHandle,
    ) -> Box<dyn PreparedCommitProc + 'repo>;

    fn commit_count(&self) -> usize;
    fn get_commit(&self, commit_oid: git2::Oid) -> Option<&Commit>;

    // TODO move it to a separate trait
    fn get_precomp_query(&self) -> Option<hyperast_tsquery::ZeroSepArrayStr> {
        None
    }
    fn get_lang_handle(&self, _lang: &str) -> Option<PCPHandle> {
        None
    }
}
pub trait PreparedCommitProc {
    fn process(self: Box<Self>, prepro: &mut RepositoryProcessor) -> ProcessorOutput;
}
pub trait CommitProcExt: CommitProc {
    fn register_param(
        h: &mut ProcessorHolder<Self>,
        t: impl Into<Self> + PartialEq<Self>,
    ) -> PCP2Handle<Self>
    where
        Self: Sized,
    {
        h.register_param(t)
    }
}

impl<T: CommitProc> CommitProcExt for T {}

pub trait ParametrizedCommitProc: std::any::Any {
    fn erased_handle(&self) -> CommitProcessorHandle
    where
        Self: 'static,
    {
        CommitProcessorHandle(std::any::TypeId::of::<Self>())
    }

    fn get_mut(&mut self, parameters: ConfigParametersHandle) -> &mut dyn CommitProc;
    fn get(&self, parameters: ConfigParametersHandle) -> &dyn CommitProc;
}

pub trait ParametrizedCommitProc2: ParametrizedCommitProc {
    type Proc: CommitProcExt;
    fn with_parameters(&self, parameters: ConfigParametersHandle) -> &Self::Proc;
    fn with_parameters_mut(&mut self, parameters: ConfigParametersHandle) -> &mut Self::Proc;
}

impl<T: ParametrizedCommitProc2> ParametrizedCommitProc for T {
    fn get_mut(&mut self, parameters: ConfigParametersHandle) -> &mut dyn CommitProc {
        ParametrizedCommitProc2::with_parameters_mut(self, parameters)
    }

    fn get(&self, parameters: ConfigParametersHandle) -> &dyn CommitProc {
        ParametrizedCommitProc2::with_parameters(self, parameters)
    }
}

pub type ProcessorMap = spreaded::ProcessorMap<Box<dyn spreaded::ErasableProcessor>>;
pub use spreaded::ToErasedProc;

mod spreaded {
    use super::*;

    pub struct ProcessorMap<V>(std::collections::HashMap<std::any::TypeId, V>);
    impl<V> Default for ProcessorMap<V> {
        fn default() -> Self {
            Self(Default::default())
        }
    }
    impl<V> ProcessorMap<V> {
        pub(crate) fn clear(&mut self) {
            self.0.clear()
        }
    }

    unsafe impl<V> Send for ProcessorMap<V> {}
    unsafe impl<V> Sync for ProcessorMap<V> {}
    pub trait ToErasedProc {
        fn to_erasable_processor(self: Box<Self>) -> Box<dyn ErasableProcessor>;
        fn as_mut_any(&mut self) -> &mut dyn Any;
        fn as_any(&self) -> &dyn Any;
    }

    impl<T: ErasableProcessor> ToErasedProc for T {
        fn to_erasable_processor(self: Box<Self>) -> Box<dyn ErasableProcessor> {
            self
        }
        fn as_mut_any(&mut self) -> &mut dyn Any {
            self
        }
        fn as_any(&self) -> &dyn Any {
            self
        }
    }

    /// Trait for dynamic dispatch of processors.
    /// Do not export it in the parent module, keeping it only public internally to this module.
    pub trait ErasableProcessor: Any + ToErasedProc + ParametrizedCommitProc {}
    impl<T> ErasableProcessor for T where T: Any + ParametrizedCommitProc {}

    // NOTE crazy good stuff
    impl ProcessorMap<Box<dyn ErasableProcessor>> {
        pub fn by_id_mut(
            &mut self,
            id: &CommitProcessorHandle,
        ) -> Option<&mut (dyn ErasableProcessor + 'static)> {
            self.0.get_mut(&id.0).map(|x| x.as_mut())
        }
        pub fn by_id(
            &self,
            id: &CommitProcessorHandle,
        ) -> Option<&(dyn ErasableProcessor + 'static)> {
            self.0.get(&id.0).map(|x| x.as_ref())
        }
        pub fn mut_or_default<T: 'static + ToErasedProc + Default + Send + Sync>(
            &mut self,
        ) -> &mut T {
            let r = self
                .0
                .entry(std::any::TypeId::of::<T>())
                .or_insert_with(|| Box::new(T::default()).to_erasable_processor());
            let r = r.as_mut();
            let r = <dyn Any>::downcast_mut(r.as_mut_any());
            r.unwrap()
        }
        pub fn get<T: 'static + ToErasedProc + Default + Send + Sync>(&self) -> Option<&T> {
            let r = self.0.get(&std::any::TypeId::of::<T>())?;
            <dyn Any>::downcast_ref(r.as_any())
        }
        // pub fn mut_or_default_with_param<T: 'static + CommitProcExt>(
        //     &mut self,
        //     handle: Parametrized
        //Processor2Handle<T>,
        // ) -> &mut T {
        //     let r = self
        //         .0
        //         .entry(std::any::TypeId::of::<T>())
        //         .or_insert_with(|| Box::new(T::default()).to_erasable_processor());
        //     let r = r.as_mut();
        //     let r = <dyn Any>::downcast_mut(r.as_mut_any());
        //     r.unwrap()
        // }
    }
    #[allow(unused)]
    #[test]
    fn a() {
        #[derive(Clone, PartialEq, Eq)]
        struct S(u8);
        #[derive(Clone, PartialEq, Eq)]
        struct S0(u8);

        impl Into<P> for S {
            fn into(self) -> P {
                P(S(self.0))
            }
        }

        impl PartialEq<P> for S {
            fn eq(&self, other: &P) -> bool {
                self == &other.0
            }
        }

        struct P(S);
        impl CommitProc for P {
            fn prepare_processing(
                &self,
                repository: &git2::Repository,
                tree_oid: crate::preprocessed::CommitBuilder,
                param_handle: PCPHandle,
            ) -> Box<dyn PreparedCommitProc> {
                unimplemented!()
            }

            fn get_commit(&self, commit_oid: git2::Oid) -> Option<&crate::Commit> {
                unimplemented!()
            }

            fn commit_count(&self) -> usize {
                unimplemented!()
            }
        }

        use super::CommitProcExt;
        let mut h = ProcessorMap::<Box<dyn ErasableProcessor>>::default();
        // The registered parameter is type checked
        let hh = h
            .mut_or_default::<ProcessorHolder<P>>()
            .register_param(S(42))
            .erase();
        // You can easily store hh in any collection.
        // You can easily add a method to CommitProc.
        h.by_id_mut(&hh.0).unwrap().get_mut(hh.1).p();
    }
}
