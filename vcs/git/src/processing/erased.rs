//! type erasure utilities for processors

use std::any::Any;

use super::ConfigParametersHandle;
use super::ParametrizedProcessorHandle as PPHandle;
use super::ProcessorHolder;

//
// processor erasure
//

/// Handle over a processor, resulting from type erasure when registering a  processor.
#[derive(Clone, Copy, Debug)]
pub struct ProcessorHandle(pub(crate) std::any::TypeId);

pub trait Proc {}
impl<T> Proc for T {}

pub trait ParametrizedProc {
    fn erased_handle(&self) -> ProcessorHandle
    where
        Self: 'static,
    {
        ProcessorHandle(std::any::TypeId::of::<Self>())
    }

    fn get_mut0(&mut self, parameters: ConfigParametersHandle) -> &mut dyn Proc;
    fn get0(&self, parameters: ConfigParametersHandle) -> &dyn Proc;
}

pub trait ParametrizedProc2: ParametrizedProc {
    type Proc: Proc;
    fn with_parameters0(&self, parameters: ConfigParametersHandle) -> &Self::Proc;
    fn with_parameters_mut0(&mut self, parameters: ConfigParametersHandle) -> &mut Self::Proc;
}

impl<T: ParametrizedProc2> ParametrizedProc for T {
    fn get_mut0(&mut self, parameters: ConfigParametersHandle) -> &mut dyn Proc {
        ParametrizedProc2::with_parameters_mut0(self, parameters)
    }

    fn get0(&self, parameters: ConfigParametersHandle) -> &dyn Proc {
        ParametrizedProc2::with_parameters0(self, parameters)
    }
}

//
// **commit** processor erasure
//

// TODO erase output type from PreparedCommitProc without loosing type information and transparently to implementers
use hyperast::store::defaults::NodeIdentifier as ProcessorOutput;
// same but for erasing the store as it is associated with the output type
use crate::preprocessed::RepositoryProcessor;
// also erase the commit returned by the commit processor
use crate::Commit;
use crate::preprocessed::CommitBuilder;

/// Handle over a commit processor, resulting from type erasure when registering a commit processor.
#[derive(Clone, Copy, Debug)]
pub struct CommitProcessorHandle(std::any::TypeId);

/// Parametrized handle over a processor like [`ParametrizedProcessorHandle`], using the erased [`CommitProcessorHandle`].
///
/// If you want to easily refer to the specific commit processor type, use [`ParametrizedProcessorHandle`] instead.
#[derive(Clone, Copy, Debug)]
pub struct ParametrizedCommitProcessorHandle(CommitProcessorHandle, pub ConfigParametersHandle);
use ParametrizedCommitProcessorHandle as PCPHandle;

impl<T: CommitProc + 'static> PPHandle<T> {
    pub(crate) fn erase(&self) -> PCPHandle {
        let tid = std::any::TypeId::of::<ProcessorHolder<T>>();
        dbg!(tid);
        PCPHandle(CommitProcessorHandle(tid), self.0)
    }
}

impl Into<CommitProcessorHandle> for ParametrizedCommitProcessorHandle {
    fn into(self) -> CommitProcessorHandle {
        self.0
    }
}

impl<T: 'static> TryFrom<ParametrizedCommitProcessorHandle> for PPHandle<T> {
    type Error = String;

    fn try_from(value: ParametrizedCommitProcessorHandle) -> Result<Self, Self::Error> {
        if std::any::TypeId::of::<ProcessorHolder<T>>() != value.0.0 {
            return Err(format!(
                "this is not a handle to {}",
                std::any::type_name::<T>()
            ));
        }
        Ok(PPHandle(value.1, std::marker::PhantomData))
    }
}

pub trait CommitProc: Proc {
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

/// erased commit processor
pub trait PreparedCommitProc {
    fn process(self: Box<Self>, prepro: &mut RepositoryProcessor) -> ProcessorOutput;
}

pub trait ParametrizedCommitProc {
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
    type Proc: CommitProc;
    fn with_parameters42(&self, parameters: PPHandle<Self::Proc>) -> &Self::Proc {
        self.with_parameters(parameters.0)
    }
    fn with_parameters42_mut(&mut self, parameters: PPHandle<Self::Proc>) -> &mut Self::Proc {
        self.with_parameters_mut(parameters.0)
    }
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

/// stores both commit and non-commit processors
pub type ProcessorMap = spreaded::ProcessorMap<spreaded::ErasedProcessorBunch>;
#[doc(hidden)]
pub type ProcessorMapOnlyNonCommit = spreaded::ProcessorMap<Box<dyn spreaded::ErasableProcessor>>;
#[doc(hidden)]
pub type ProcessorMapOnlyCommit =
    spreaded::ProcessorMap<Box<dyn spreaded::ErasableCommitProcessor>>;
#[doc(hidden)]
pub type ProcessorMap2 = spreaded::ProcessorMap2<
    Box<dyn spreaded::ErasableProcessor>,
    Box<dyn spreaded::ErasableCommitProcessor>,
>;

#[doc(hidden)]
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

    ////// spreaded proc

    pub trait ToErasedProc {
        fn as_mut_any(&mut self) -> &mut dyn Any;
        fn as_any(&self) -> &dyn Any;
        fn to_erasable_processor(self: Box<Self>) -> Box<dyn ErasableProcessor>;
    }

    impl<T: ErasableProcessor> ToErasedProc for T {
        fn as_mut_any(&mut self) -> &mut dyn Any {
            self
        }
        fn as_any(&self) -> &dyn Any {
            self
        }
        fn to_erasable_processor(self: Box<Self>) -> Box<dyn ErasableProcessor> {
            self
        }
    }

    pub trait ErasableProcessor: Any + ToErasedProc + ParametrizedProc + Send + Sync {}
    impl<T> ErasableProcessor for T where T: Any + ParametrizedProc + Send + Sync {}

    // basic collection of erased processors
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

        pub fn proc_mut<T: 'static + ToErasedProc + Default + Send + Sync>(&mut self) -> &mut T {
            let r = self.0.entry(std::any::TypeId::of::<T>());
            let r = r.or_insert_with(|| Box::new(T::default()).to_erasable_processor());
            let r = r.as_mut();
            let r = <dyn Any>::downcast_mut(r.as_mut_any());
            r.unwrap()
        }
        pub fn get<T: 'static + ToErasedProc + Default + Send + Sync>(&self) -> Option<&T> {
            let r = self.0.get(&std::any::TypeId::of::<T>())?;
            <dyn Any>::downcast_ref(r.as_any())
        }
    }

    ////// spreaded commit proc

    pub trait ToErasedCommitProc: ToErasedProc {
        fn to_erasable_commit_processor(self: Box<Self>) -> Box<dyn ErasableCommitProcessor>;
    }

    impl<T: ErasableCommitProcessor> ToErasedCommitProc for T {
        fn to_erasable_commit_processor(self: Box<Self>) -> Box<dyn ErasableCommitProcessor> {
            self
        }
    }

    /// Trait for dynamic dispatch of processors.
    /// Do not export it in the parent module, keeping it only public internally to this module.
    pub trait ErasableCommitProcessor:
        Any + ToErasedCommitProc + ParametrizedCommitProc + ParametrizedProc + Send + Sync
    {
    }
    impl<T> ErasableCommitProcessor for T where
        T: Any + ParametrizedCommitProc + ParametrizedProc + Send + Sync
    {
    }

    // NOTE crazy good stuff
    // collection of erased commit processors
    impl ProcessorMap<Box<dyn ErasableCommitProcessor>> {
        pub fn by_id_mut(
            &mut self,
            id: &CommitProcessorHandle,
        ) -> Option<&mut (dyn ErasableCommitProcessor + 'static)> {
            self.0.get_mut(&id.0).map(|x| x.as_mut())
        }
        pub fn by_id(
            &self,
            id: &CommitProcessorHandle,
        ) -> Option<&(dyn ErasableCommitProcessor + 'static)> {
            self.0.get(&id.0).map(|x| x.as_ref())
        }
        pub fn commit_proc_mut<T: 'static + ToErasedCommitProc + Default + Send + Sync>(
            &mut self,
        ) -> &mut T {
            let r = self.0.entry(std::any::TypeId::of::<T>());
            let r = r.or_insert_with(|| Box::new(T::default()).to_erasable_commit_processor());
            let r = r.as_mut();
            let r = <dyn Any>::downcast_mut(r.as_mut_any());
            r.unwrap()
        }
        pub fn get<T: 'static + ToErasedCommitProc + Default + Send + Sync>(&self) -> Option<&T> {
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

    ////// spreaded both commit and non-commit proc

    pub enum ErasedProcessorBunch {
        NonCommit(Box<dyn ErasableProcessor>),
        Commit(Box<dyn ErasableCommitProcessor>),
    }

    // NOTE even crazier
    // collection of erased both commit and non-commit processors
    impl ProcessorMap<ErasedProcessorBunch> {
        /// give write access to process commits
        pub fn by_id_mut(
            &mut self,
            id: impl Into<CommitProcessorHandle>,
        ) -> Result<&mut (dyn ErasableCommitProcessor + 'static), ProcAccessError> {
            use ErasedProcessorBunch::*;
            match self.0.get_mut(&id.into().0) {
                Some(Commit(p)) => Ok(p.as_mut()),
                Some(_) => Err(ProcAccessError::NotCommitEnabled),
                _ => {
                    log::trace!("not a commit enabled processor");
                    Err(ProcAccessError::NotFound)
                }
            }
        }

        /// give access to already processed commits
        pub fn by_id(
            &self,
            id: impl Into<CommitProcessorHandle>,
        ) -> Result<&(dyn ErasableCommitProcessor + 'static), ProcAccessError> {
            use ErasedProcessorBunch::*;
            match self.0.get(&id.into().0) {
                Some(Commit(p)) => Ok(p.as_ref()),
                Some(_) => Err(ProcAccessError::NotCommitEnabled),
                _ => {
                    log::trace!("not a commit enabled processor");
                    Err(ProcAccessError::NotFound)
                }
            }
        }

        /// write enabled processor T
        ///
        /// call `commit_proc_mut` if you want to let a processor handle commits
        pub fn proc_mut<T: 'static + ToErasedProc + Default + Send + Sync>(&mut self) -> &mut T {
            use ErasedProcessorBunch::*;
            let r = self.0.entry(std::any::TypeId::of::<T>());
            let lse = || Box::new(T::default()).to_erasable_processor();
            let r = r.or_insert_with(|| NonCommit(lse()));
            match r {
                NonCommit(p) => <dyn Any>::downcast_mut(p.as_mut().as_mut_any()),
                Commit(p) => <dyn Any>::downcast_mut(p.as_mut().as_mut_any()),
            }
            .unwrap()
        }

        /// write enabled **commit** processor T
        ///
        /// panic if the processor is not a commit enabled processor
        pub fn commit_proc_mut<T: 'static + ToErasedCommitProc + Default + Send + Sync>(
            &mut self,
        ) -> &mut T {
            use ErasedProcessorBunch::*;
            let r = self.0.entry(std::any::TypeId::of::<T>());
            let lse = || Box::new(T::default()).to_erasable_commit_processor();
            let r = r.or_insert_with(|| Commit(lse()));
            if let NonCommit(_) = r {
                // case where we have to "recycle" the previously allocated processor
                // as it might already contain data that we should reuse
                let d = Box::new(T::default()).to_erasable_commit_processor();
                let NonCommit(p) = std::mem::replace(r, Commit(d)) else {
                    unreachable!()
                };
                let p: Box<T> = Box::<dyn Any>::downcast(p).unwrap();
                let p = p.to_erasable_commit_processor();
                *r = Commit(p);
            }
            if let Commit(p) = r {
                return <dyn Any>::downcast_mut(p.as_mut().as_mut_any()).unwrap();
            }
            unreachable!("not a commit enabled processor")
        }

        /// read-only **commit** processor T
        pub fn get<T: 'static + ToErasedCommitProc + Default + Send + Sync>(&self) -> Option<&T> {
            use ErasedProcessorBunch::*;
            if let Commit(r) = self.0.get(&std::any::TypeId::of::<T>())? {
                return <dyn Any>::downcast_ref(r.as_any());
            };
            log::trace!("not a commit enabled processor");
            None
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum ProcAccessError {
        NotFound,
        NotCommitEnabled,
    }

    /////// attempt at tiered

    pub struct ProcessorMap2<U, V>(
        pub(super) std::collections::HashMap<std::any::TypeId, U>,
        pub(super) std::collections::HashMap<std::any::TypeId, V>,
    );

    impl<U, V> Default for ProcessorMap2<U, V> {
        fn default() -> Self {
            Self(Default::default(), Default::default())
        }
    }

    impl<U, V> ProcessorMap2<U, V> {
        #[allow(unused)]
        pub(crate) fn clear(&mut self) {
            self.0.clear();
            self.1.clear();
        }
    }

    impl ProcessorMap2<Box<dyn ErasableProcessor>, Box<dyn ErasableCommitProcessor>> {
        pub fn by_id_mut(
            &mut self,
            id: &CommitProcessorHandle,
        ) -> Option<&mut (dyn ErasableCommitProcessor + 'static)> {
            self.1.get_mut(&id.0).map(|x| x.as_mut())
        }

        pub fn by_id(
            &self,
            id: &CommitProcessorHandle,
        ) -> Option<&(dyn ErasableCommitProcessor + 'static)> {
            self.1.get(&id.0).map(|x| x.as_ref())
        }

        pub fn mut_or_default0<T: 'static + ToErasedProc + Default + Send + Sync>(
            &mut self,
        ) -> &mut T {
            let r = self.0.entry(std::any::TypeId::of::<T>());
            let r = r.or_insert_with(|| Box::new(T::default()).to_erasable_processor());
            let r = r.as_mut();
            let r = <dyn Any>::downcast_mut(r.as_mut_any());
            r.unwrap()
        }

        pub fn mut_or_default<T: 'static + ToErasedCommitProc + Default + Send + Sync>(
            &mut self,
        ) -> &mut T {
            let r = self.1.entry(std::any::TypeId::of::<T>());
            let r = r.or_insert_with(|| Box::new(T::default()).to_erasable_commit_processor());
            let r = r.as_mut();
            let r = <dyn Any>::downcast_mut(r.as_mut_any());
            r.unwrap()
        }

        pub fn get<T: 'static + ToErasedProc + Default + Send + Sync>(&self) -> Option<&T> {
            let r = self.0.get(&std::any::TypeId::of::<T>())?;
            <dyn Any>::downcast_ref(r.as_any())
        }
    }

    // #[allow(unused)]
    // #[test]
    // fn a() {
    //     #[derive(Clone, PartialEq, Eq)]
    //     struct S(u8);
    //     #[derive(Clone, PartialEq, Eq)]
    //     struct S0(u8);

    //     impl Into<P> for S {
    //         fn into(self) -> P {
    //             P(S(self.0))
    //         }
    //     }

    //     impl PartialEq<P> for S {
    //         fn eq(&self, other: &P) -> bool {
    //             self == &other.0
    //         }
    //     }

    //     struct P(S);
    //     impl CommitProc for P {
    //         fn prepare_processing(
    //             &self,
    //             repository: &git2::Repository,
    //             tree_oid: crate::preprocessed::CommitBuilder,
    //             param_handle: PCPHandle,
    //         ) -> Box<dyn PreparedCommitProc> {
    //             unimplemented!()
    //         }

    //         fn get_commit(&self, commit_oid: git2::Oid) -> Option<&crate::Commit> {
    //             unimplemented!()
    //         }

    //         fn commit_count(&self) -> usize {
    //             0
    //         }
    //     }

    //     use super::CommitProcExt;
    //     let mut h = ProcessorMap::<Box<dyn ErasableCommitProcessor>>::default();
    //     // The registered parameter is type checked
    //     let hh = h
    //         .mut_or_default::<ProcessorHolder<P>>()
    //         .register_param(S(42))
    //         .erase();
    //     // You can easily store hh in any collection.
    //     // You can easily add a method to CommitProc.
    //     let mut erasable_commit_processor = h.by_id_mut(&hh.0).unwrap();
    //     let count = erasable_commit_processor.get_mut(hh.1).commit_count();
    //     assert_eq!(count, 0);
    // }
}
