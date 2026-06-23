use std::path::PathBuf;

use git2::{Oid, Repository};

use crate::Processor;
use crate::preprocessed::CommitBuilder;
use crate::preprocessed::RepositoryProcessor;
use crate::processing::ObjectName;
use crate::processing::ParametrizedProcessorHandle;
use crate::processing::erased::ParametrizedCommitProc2 as _;
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
        let java_handle = handle.try_into().unwrap_or_else(|err| {
            eprintln!("{}", err);
            unreachable!("caller of prepare_processing should properly dispatch")
        });
        Box::new(PreparedJavaCommitProc {
            repository,
            commit_builder,
            java_handle,
        })
    }
}

struct PreparedJavaCommitProc<'repo> {
    repository: &'repo Repository,
    commit_builder: CommitBuilder,
    pub(crate) java_handle: ParametrizedProcessorHandle<JavaProc>,
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
            self.java_handle,
        )
        .process();
        let h = prepro
            .processing_systems
            .commit_proc_mut::<JavaProcessorHolder>();
        let handle = self.java_handle;
        let oid = self.commit_builder.commit_oid();
        let commit = self.commit_builder.finish(root_full_node.id);
        h.with_parameters_mut(handle).commits.insert(oid, commit);
        root_full_node.id
    }
}
