use crate::Processor as _;
use crate::preprocessed::{CommitBuilder, RepositoryProcessor};
use crate::processing::erased::ParametrizedCommitProcTyped as _;
use crate::processing::erased::ParametrizedCommitProcessorHandle as PCPHandle;
use crate::processing::erased::PreparedCommitProc;
use crate::processors::file_sys::processor::FileSysProcessor;
use crate::processors::file_sys::{FileSysAcc, FileSysProc};

type FileSysProcessorHolder = crate::processing::ProcessorHolder<FileSysProc>;

struct PreparedMakeCommitProc<'repo> {
    repository: &'repo git2::Repository,
    commit_builder: CommitBuilder,
    pub(crate) handle: PCPHandle,
}

impl<'repo> PreparedCommitProc for PreparedMakeCommitProc<'repo> {
    fn process(
        self: Box<PreparedMakeCommitProc<'repo>>,
        prepro: &mut RepositoryProcessor,
    ) -> hyperast::store::defaults::NodeIdentifier {
        let dir_path = std::path::PathBuf::from("");
        let mut dir_path = dir_path.components().peekable();
        let name = b"";
        // TODO check parameter in self to know it is a recursive module search
        let root_full_node = FileSysProcessor::<true, false, FileSysAcc>::prepare(
            self.repository,
            prepro,
            &mut dir_path,
            name,
            self.commit_builder.tree_oid(),
            self.handle,
        )
        .process();
        let h = prepro
            .processing_systems
            .commit_proc_mut::<FileSysProcessorHolder>();
        let handle = self.handle.try_into().unwrap();
        let oid = self.commit_builder.commit_oid();
        let commit = self.commit_builder.finish(root_full_node.id);
        h.with_parameters_mut(handle).commits.insert(oid, commit);
        root_full_node.id
    }
}

impl crate::processing::erased::CommitProc for FileSysProc {
    fn prepare_processing<'repo>(
        &self,
        repository: &'repo git2::Repository,
        commit_builder: CommitBuilder,
        handle: PCPHandle,
    ) -> Box<dyn PreparedCommitProc + 'repo> {
        Box::new(PreparedMakeCommitProc {
            repository,
            commit_builder,
            handle,
        })
    }

    fn get_commit(&self, commit_oid: git2::Oid) -> Option<&crate::Commit> {
        self.commits.get(&commit_oid)
    }

    fn commit_count(&self) -> usize {
        self.commits.len()
    }

    fn get_lang_handle(&self, lang: &str) -> Option<PCPHandle> {
        dbg!(self.parameter.cpp_handle);
        if lang.eq_ignore_ascii_case("cpp") {
            Some(self.parameter.cpp_handle.erase())
        } else if lang.eq_ignore_ascii_case("java") {
            Some(self.parameter.java_handle.erase())
        } else if lang.eq_ignore_ascii_case("python") {
            Some(self.parameter.python_handle.erase())
        } else {
            None
        }
    }
}
