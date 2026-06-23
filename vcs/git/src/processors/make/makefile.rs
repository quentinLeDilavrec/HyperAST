use git2::{Oid, Repository};

use crate::preprocessed::RepositoryProcessor;
use crate::processing::ParametrizedProcessorHandle as PPHandle;
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

impl From<PPHandle<MakeProc>> for PPHandle<MakefileProc> {
    fn from(value: PPHandle<MakeProc>) -> Self {
        PPHandle(value.0, std::marker::PhantomData)
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

use hyperast_gen_ts_xml::legion::XmlTreeGen;
impl RepositoryProcessor {
    pub(super) fn help_handle_makefile(
        &mut self,
        parent_acc: &mut MakeModuleAcc,
        oid: Oid,
        name: ObjectName,
        repository: &Repository,
        parameters: PPHandle<MakefileProc>,
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
