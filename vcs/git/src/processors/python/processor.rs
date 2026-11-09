use std::iter::Peekable;
use std::path::Components;

use git2::{Oid, Repository};

use crate::preprocessed::RepositoryProcessor;
use crate::processing::ObjectName;
use crate::processing::ParametrizedProcessorHandle as PPHandle;
use crate::processors::SimpleProcessor;

use super::Acc;
use super::FullNode;
use super::Proc;

impl RepositoryProcessor {
    pub(crate) fn handle_python_blob(
        &mut self,
        oid: Oid,
        name: &ObjectName,
        repository: &Repository,
        parameters: PPHandle<Proc>,
    ) -> Result<FullNode, crate::ParseErr> {
        SimpleProcessor::<Acc, Proc>::handle_blob(self, oid, name, repository, parameters)
    }

    pub fn handle_python_directory<'b, 'd: 'b>(
        &mut self,
        repository: &Repository,
        dir_path: &'b mut Peekable<Components<'d>>,
        name: &ObjectName,
        oid: git2::Oid,
        handle: PPHandle<Proc>,
    ) -> FullNode {
        use crate::Processor as _;
        SimpleProcessor::<Acc, Proc>::prepare(repository, self, dir_path, name, oid, handle)
            .process()
    }
}
