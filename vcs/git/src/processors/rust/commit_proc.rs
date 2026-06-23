use git2::Repository;

use crate::preprocessed::{CommitBuilder, RepositoryProcessor};
use crate::processing::ObjectName;
use crate::processing::erased::ParametrizedCommitProcTyped as _;
use crate::processing::erased::ParametrizedCommitProcessorHandle as PCPHandle;
use crate::processing::erased::PreparedCommitProc;

use super::{RustProc, RustProcessorHolder};

impl crate::processing::erased::CommitProc for RustProc {
    fn prepare_processing_at_path<'repo>(
        &self,
        repository: &'repo Repository,
        commit_builder: CommitBuilder,
        path: std::path::PathBuf,
        handle: PCPHandle,
    ) -> Box<dyn PreparedCommitProc + 'repo> {
        Box::new(PreparedRustCommitProc {
            repository,
            commit_builder,
            dir_path: path,
            handle,
        })
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

struct PreparedRustCommitProc<'repo> {
    repository: &'repo Repository,
    commit_builder: CommitBuilder,
    dir_path: std::path::PathBuf,
    pub(crate) handle: PCPHandle,
}

impl<'repo> PreparedCommitProc for PreparedRustCommitProc<'repo> {
    fn process(
        self: Box<PreparedRustCommitProc<'repo>>,
        prepro: &mut RepositoryProcessor,
    ) -> hyperast::store::defaults::NodeIdentifier {
        let mut dir_path = self.dir_path.components().peekable();
        let name = ObjectName::from(b"");
        // TODO check parameter in self to know it is a recursive module search
        let root_full_node = prepro.handle_rust_directory(
            self.repository,
            &mut dir_path,
            &name,
            self.commit_builder.tree_oid(),
            self.handle.try_into().unwrap(),
        );
        let h = prepro
            .processing_systems
            .commit_proc_mut::<RustProcessorHolder>();
        let handle = self.handle.try_into().unwrap();
        let oid = self.commit_builder.commit_oid();
        let commit = self.commit_builder.finish(root_full_node.id);
        h.with_parameters_mut(handle).commits.insert(oid, commit);
        root_full_node.id
    }
}
