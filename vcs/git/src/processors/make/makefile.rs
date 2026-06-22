use git2::{Oid, Repository};

use crate::preprocessed::RepositoryProcessor;
use crate::processing::ParametrizedProcessor2Handle as PCP2Handle;
use crate::processing::erased::ParametrizedCommitProcessorHandle as PCPHandle;
use crate::processing::{CacheHolding, ObjectName};

use super::{MakeModuleAcc, MakeProc};

#[derive(Clone, PartialEq, Eq, Default)]
pub struct Parameter {}

pub struct MakefileProc(Option<Parameter>, crate::processing::caches::Makefile);

impl From<Parameter> for MakefileProc {
    fn from(value: Parameter) -> Self {
        MakefileProc(Some(value), Default::default())
    }
}

impl PartialEq<MakefileProc> for Parameter {
    fn eq(&self, other: &MakefileProc) -> bool {
        Some(self) == other.0.as_ref()
    }
}

impl From<PCP2Handle<MakeProc>> for PCP2Handle<MakefileProc> {
    fn from(value: PCP2Handle<MakeProc>) -> Self {
        PCP2Handle(value.0, std::marker::PhantomData)
    }
}

impl CacheHolding<crate::processing::caches::Makefile> for MakefileProc {
    fn get_caches_mut(&mut self) -> &mut crate::processing::caches::Makefile {
        &mut self.1
    }
    fn get_caches(&self) -> &crate::processing::caches::Makefile {
        &self.1
    }
}

// TODO should not have to impl this trait
impl crate::processing::erased::CommitProc for MakefileProc {
    fn prepare_processing(
        &self,
        _repository: &git2::Repository,
        _commit_builder: crate::preprocessed::CommitBuilder,
        _param_handle: PCPHandle,
    ) -> Box<dyn crate::processing::erased::PreparedCommitProc> {
        unimplemented!("required for processing at the root of a project")
    }

    fn get_commit(&self, _commit_oid: git2::Oid) -> Option<&crate::Commit> {
        unimplemented!("required for processing at the root of a project")
    }

    fn commit_count(&self) -> usize {
        unimplemented!()
    }
}

use hyperast_gen_ts_xml::legion::XmlTreeGen;
impl RepositoryProcessor {
    pub(super) fn help_handle_makefile(
        &mut self,
        oid: Oid,
        parent_acc: &mut MakeModuleAcc,
        name: ObjectName,
        repository: &Repository,
        parameters: PCP2Handle<MakefileProc>,
    ) -> Result<(), crate::ParseErr> {
        let x = self
            .processing_systems
            .caching_blob_handler::<crate::processing::file_sys::MakeFile>()
            .handle(oid, repository, &name, parameters, |_c, n, t| {
                super::handle_makefile_file(
                    &mut XmlTreeGen::bare(self.main_stores.mut_with_ts()),
                    n,
                    t,
                )
                .map_err(|_| crate::ParseErr::IllFormed)
            })?;
        let name = self.intern_object_name(&name);
        assert!(!parent_acc.primary.children_names.contains(&name));
        parent_acc.push_makefile(name, x);
        Ok(())
    }
}
