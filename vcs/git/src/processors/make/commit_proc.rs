use crate::Processor;
use crate::preprocessed::{CommitBuilder, RepositoryProcessor};
use crate::processing::ParametrizedProcessorHandle as PPHandle;
use crate::processing::erased::ParametrizedCommitProcTyped as _;
use crate::processing::erased::ParametrizedCommitProcessorHandle as PCPHandle;

use super::make_processor::MakeProcessor;
use super::{MakeModuleAcc, MakeProc, MakeProcessorHolder, Parameter};

pub(crate) struct PreparedMakeCommitProc<'repo> {
    repository: &'repo git2::Repository,
    commit_builder: CommitBuilder,
    dir_path: std::path::PathBuf,
    pub(crate) handle: PPHandle<MakeProc>,
    pub(crate) handles: Parameter,
}

impl<'repo> crate::processing::erased::PreparedCommitProc for PreparedMakeCommitProc<'repo> {
    fn process(
        self: Box<PreparedMakeCommitProc<'repo>>,
        prepro: &mut RepositoryProcessor,
    ) -> hyperast::store::defaults::NodeIdentifier {
        let mut dir_path = self.dir_path.components().peekable();
        let name = b"";
        // TODO check parameter in self to know it is a recursive module search
        let root_full_node = MakeProcessor::<true, false, MakeModuleAcc>::prepare(
            self.repository,
            prepro,
            &mut dir_path,
            name,
            self.commit_builder.tree_oid(),
            self.handle,
            self.handles,
        )
        .process();
        let h = prepro
            .processing_systems
            .commit_proc_mut::<MakeProcessorHolder>();
        let handle = self.handle.try_into().unwrap();
        let oid = self.commit_builder.commit_oid();
        let commit = self.commit_builder.finish(root_full_node.id);
        h.with_parameters_mut(handle).commits.insert(oid, commit);
        root_full_node.id
    }
}

impl MakeProc {
    pub(crate) fn prepare_processing<'repo>(
        &self,
        repository: &'repo git2::Repository,
        commit_builder: CommitBuilder,
        path: std::path::PathBuf,
        handle: PCPHandle,
    ) -> PreparedMakeCommitProc<'repo> {
        let handle = handle.try_into().unwrap_or_else(|err| {
            eprintln!("{}", err);
            unreachable!("caller of prepare_processing should properly dispatch")
        });
        PreparedMakeCommitProc {
            repository,
            commit_builder,
            dir_path: path,
            handle,
            handles: self.parameter.clone(),
        }
    }
}

impl crate::processing::erased::CommitProc for MakeProc {
    fn prepare_processing_at_path<'repo>(
        &self,
        repository: &'repo git2::Repository,
        commit_builder: CommitBuilder,
        path: std::path::PathBuf,
        handle: PCPHandle,
    ) -> Box<dyn crate::processing::erased::PreparedCommitProc + 'repo> {
        Box::new(self.prepare_processing(repository, commit_builder, path, handle))
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
            if cfg!(debug_assertions) {
                unimplemented!()
            }
            None
        } else {
            None
        }
    }
}
