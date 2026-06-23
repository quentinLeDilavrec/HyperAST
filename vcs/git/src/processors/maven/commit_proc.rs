use super::processor::MavenProcessor;
use super::{MavenModuleAcc, MavenProc, MavenProcessorHolder};
use crate::Processor as _;
use crate::preprocessed::{CommitBuilder, RepositoryProcessor};
use crate::processing::ParametrizedProcessorHandle as PPHandle;
use crate::processing::erased::CommitProc;
use crate::processing::erased::ParametrizedCommitProcTyped as _;
use crate::processing::erased::ParametrizedCommitProcessorHandle as PCPHandle;
use crate::processing::erased::PreparedCommitProc;
use crate::processors::java::JavaProc;
use crate::processors::maven::PomProc;

pub(super) struct PreparedMavenCommitProc<'repo> {
    repository: &'repo git2::Repository,
    commit_builder: CommitBuilder,
    dir_path: std::path::PathBuf,
    pub(crate) maven_handle: PPHandle<MavenProc>,
    pub(crate) pom_handle: PPHandle<PomProc>,
    pub(crate) java_handle: PPHandle<JavaProc>,
}

impl<'repo> PreparedCommitProc for PreparedMavenCommitProc<'repo> {
    fn process(
        self: Box<PreparedMavenCommitProc<'repo>>,
        prepro: &mut RepositoryProcessor,
    ) -> hyperast::store::defaults::NodeIdentifier {
        let mut dir_path = self.dir_path.components().peekable();
        let name = b"";
        // TODO check parameter in self to know it is a recursive module search
        let root_full_node = MavenProcessor::<true, false, MavenModuleAcc>::prepare(
            self.repository,
            prepro,
            &mut dir_path,
            name,
            self.commit_builder.tree_oid(),
            self.maven_handle,
            self.pom_handle,
            self.java_handle,
        )
        .process();
        let h = prepro
            .processing_systems
            .commit_proc_mut::<MavenProcessorHolder>();
        let handle = self.maven_handle;
        let oid = self.commit_builder.commit_oid();
        let commit = self.commit_builder.finish(root_full_node.id);
        h.with_parameters_mut(handle).commits.insert(oid, commit);
        root_full_node.id
    }
}

impl CommitProc for MavenProc {
    fn prepare_processing_at_path<'repo>(
        &self,
        repository: &'repo git2::Repository,
        commit_builder: CommitBuilder,
        path: std::path::PathBuf,
        handle: PCPHandle,
    ) -> Box<dyn PreparedCommitProc + 'repo> {
        let maven_handle = handle.try_into().unwrap_or_else(|err| {
            eprintln!("{}", err);
            unreachable!("caller of prepare_processing should properly dispatch")
        });
        let pom_handle = self.parameter.pom_handle;
        let java_handle = self.parameter.java_handle;
        Box::new(PreparedMavenCommitProc {
            repository,
            commit_builder,
            dir_path: path,
            maven_handle,
            pom_handle,
            java_handle,
        })
    }

    fn get_commit(&self, commit_oid: git2::Oid) -> Option<&crate::Commit> {
        self.commits.get(&commit_oid)
    }

    fn commit_count(&self) -> usize {
        self.commits.len()
    }

    fn get_lang_handle(&self, lang: &str) -> Option<PCPHandle> {
        if lang.eq_ignore_ascii_case("java") {
            Some(self.parameter.java_handle.erase())
        } else if lang.eq_ignore_ascii_case("cpp") {
            if cfg!(debug_assertions) {
                unimplemented!()
            }
            None
        } else {
            None
        }
    }
}
