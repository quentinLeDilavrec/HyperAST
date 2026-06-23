use git2::Oid;
use git2::Repository;

use crate::preprocessed::CommitBuilder;
use crate::preprocessed::RepositoryProcessor;
use crate::processing::ObjectName;
use crate::processing::erased::ParametrizedCommitProc2 as _;
use crate::processing::erased::ParametrizedCommitProcessorHandle as PCPHandle;
use crate::processing::erased::PreparedCommitProc;

use super::CppProc;
use super::CppProcessorHolder;

impl crate::processing::erased::CommitProc for CppProc {
    fn prepare_processing<'repo>(
        &self,
        repository: &'repo Repository,
        commit_builder: CommitBuilder,
        handle: PCPHandle,
    ) -> Box<dyn PreparedCommitProc + 'repo> {
        Box::new(PreparedCppCommitProc {
            repository,
            commit_builder,
            handle,
        })
    }

    fn get_commit(&self, commit_oid: Oid) -> Option<&crate::Commit> {
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

struct PreparedCppCommitProc<'repo> {
    repository: &'repo Repository,
    commit_builder: CommitBuilder,
    pub(crate) handle: PCPHandle,
}

impl<'repo> PreparedCommitProc for PreparedCppCommitProc<'repo> {
    fn process(
        self: Box<PreparedCppCommitProc<'repo>>,
        prepro: &mut RepositoryProcessor,
    ) -> hyperast::store::defaults::NodeIdentifier {
        let dir_path = std::path::PathBuf::from("");
        let mut dir_path = dir_path.components().peekable();
        let name = ObjectName::from(b"");
        // TODO check parameter in self to know it is a recursive module search
        let root_full_node = prepro.handle_cpp_directory(
            self.repository,
            &mut dir_path,
            &name,
            self.commit_builder.tree_oid(),
            self.handle.try_into().unwrap(),
        );
        let h = prepro
            .processing_systems
            .commit_proc_mut::<CppProcessorHolder>();
        let handle = self.handle.try_into().unwrap();
        let oid = self.commit_builder.commit_oid();
        let commit = self.commit_builder.finish(root_full_node.id);
        h.with_parameters_mut(handle).commits.insert(oid, commit);
        root_full_node.id
    }
}
