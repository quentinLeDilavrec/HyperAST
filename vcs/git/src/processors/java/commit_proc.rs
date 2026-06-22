use std::marker::PhantomData;
use std::path::PathBuf;

use git2::{Oid, Repository};

use crate::Processor;
use crate::preprocessed::CommitBuilder;
use crate::preprocessed::RepositoryProcessor;
use crate::processing::ObjectName;
use crate::processing::ParametrizedProcessor2Handle as PCP2Handle;
use crate::processing::erased::ParametrizedCommitProc2;
use crate::processing::erased::ParametrizedCommitProcessorHandle;
use crate::processing::erased::PreparedCommitProc;

use super::JavaAcc;
use super::JavaProc;
use super::JavaProcessorHolder;
use super::processor::JavaProcessor;

impl crate::processing::erased::CommitProc for JavaProc {
    fn get_commit(&self, commit_oid: Oid) -> Option<&crate::Commit> {
        self.commits.get(&commit_oid)
    }

    fn commit_count(&self) -> usize {
        self.commits.len()
    }

    fn get_precomp_query(&self) -> Option<hyperast_tsquery::ZeroSepArrayStr> {
        self.parameter.query.clone()
    }

    fn prepare_processing<'repo>(
        &self,
        repository: &'repo Repository,
        commit_builder: CommitBuilder,
        handle: ParametrizedCommitProcessorHandle,
    ) -> Box<dyn PreparedCommitProc + 'repo> {
        Box::new(PreparedJavaCommitProc {
            repository,
            commit_builder,
            handle,
        })
    }
}

struct PreparedJavaCommitProc<'repo> {
    repository: &'repo Repository,
    commit_builder: CommitBuilder,
    pub(crate) handle: ParametrizedCommitProcessorHandle,
}

impl<'repo> PreparedCommitProc for PreparedJavaCommitProc<'repo> {
    fn process(
        self: Box<PreparedJavaCommitProc<'repo>>,
        prepro: &mut RepositoryProcessor,
    ) -> hyperast::store::defaults::NodeIdentifier {
        let dir_path = PathBuf::from("");
        let mut dir_path = dir_path.components().peekable();
        let name = ObjectName::from(b"");
        // TODO check parameter in self to know it is a recursive module search
        let root_full_node = JavaProcessor::<JavaAcc>::prepare(
            self.repository,
            prepro,
            &mut dir_path,
            &name,
            self.commit_builder.tree_oid(),
            &PCP2Handle(self.handle.1, PhantomData),
        )
        .process();
        let h = prepro
            .processing_systems
            .commit_proc_mut::<JavaProcessorHolder>();
        let handle = self.handle;
        let commit_oid = self.commit_builder.commit_oid();
        let commit = self.commit_builder.finish(root_full_node.id);
        h.with_parameters_mut(handle.1)
            .commits
            .insert(commit_oid, commit);
        root_full_node.id
    }
}
