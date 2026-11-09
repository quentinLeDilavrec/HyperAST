use git2::Repository;

use crate::preprocessed::CommitBuilder;
use crate::processing::erased::ParametrizedCommitProcessorHandle as PCPHandle;
use crate::processing::erased::PreparedCommitProc;

impl crate::processing::erased::CommitProc for super::Proc {
    fn prepare_processing_at_path<'repo>(
        &self,
        repository: &'repo Repository,
        commit_builder: CommitBuilder,
        path: std::path::PathBuf,
        handle: PCPHandle,
    ) -> Box<dyn PreparedCommitProc + 'repo> {
        Box::new(
            crate::processors::simple_helpers::PreparedSimpleCommitProc::<
                super::Acc,
                super::Parameter,
                super::TStore,
            >::new(repository, commit_builder, path, handle),
        )
    }

    fn get_commit(&self, commit_oid: git2::Oid) -> Option<&crate::Commit> {
        self.commits.get(&commit_oid)
    }

    fn commit_count(&self) -> usize {
        self.commits.len()
    }

    fn get_precomp_query(&self) -> Option<hyperast_tsquery::ZeroSepArrayStr> {
        dbg!(&self.parameter.query);
        self.parameter.query.clone()
    }
}
