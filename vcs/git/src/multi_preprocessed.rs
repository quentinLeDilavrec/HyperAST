use std::collections::HashMap;

use crate::Commit;
use crate::git::Repo;
use crate::preprocessed::RepositoryProcessor;
use crate::processing::erased::ParametrizedCommitProcessorHandle;
use crate::processing::{ConfiguredRepo2, ProcessorHolder};
use crate::processing::{ConfiguredRepoHandle2, RepoConfig};

#[cfg(feature = "java")]
use crate::processors::java::JavaProc;
#[cfg(feature = "java")]
type JavaProcessorHolder = ProcessorHolder<JavaProc>;
#[cfg(feature = "cpp")]
use crate::processors::cpp::CppProc;
#[cfg(feature = "cpp")]
type CppProcessorHolder = ProcessorHolder<CppProc>;
#[cfg(feature = "c")]
use crate::processors::c::CProc;
#[cfg(feature = "c")]
type CProcessorHolder = ProcessorHolder<CProc>;
#[cfg(feature = "python")]
use crate::processors::python::PythonProc;
#[cfg(feature = "python")]
type PythonProcessorHolder = ProcessorHolder<PythonProc>;
#[cfg(feature = "make_cpp")]
use crate::processors::make::MakeProc;
#[cfg(feature = "rust")]
use crate::processors::rust::RustProc;
#[cfg(feature = "rust")]
type RustProcessorHolder = ProcessorHolder<RustProc>;
#[cfg(feature = "typescript")]
use crate::processors::typescript::TypescriptProc;
#[cfg(feature = "typescript")]
type TypescriptProcessorHolder = ProcessorHolder<TypescriptProc>;
#[cfg(feature = "make_cpp")]
type MakeProcessorHolder = ProcessorHolder<MakeProc>;
#[cfg(feature = "maven_java")]
use crate::processors::maven::MavenProc;
#[cfg(feature = "maven_java")]
type MavenProcessorHolder = ProcessorHolder<MavenProc>;
#[cfg(feature = "maven_java")]
use crate::processors::maven::PomProc;
#[cfg(feature = "maven_java")]
type PomProcessorHolder = ProcessorHolder<PomProc>;
#[cfg(feature = "make_cpp")]
use crate::processors::make::makefile::MakefileProc;
#[cfg(feature = "make_cpp")]
type MakefileProcessorHolder = ProcessorHolder<MakefileProc>;
#[cfg(feature = "file_sys")]
use crate::processors::file_sys::FileSysProc;
#[cfg(feature = "file_sys")]
type FileSysProcessorHolder = ProcessorHolder<FileSysProc>;

/// Preprocess git repositories
/// share most components with PreProcessedRepository
#[derive(Default)]
pub struct PreProcessedRepositories {
    // pub commits: HashMap<RepoConfig, HashMap<git2::Oid, Commit>>,
    pub processor: RepositoryProcessor,
    // pub processing_ordered_commits: HashMap<String,Vec<git2::Oid>>,
    configs: HashMap<Repo, ParametrizedCommitProcessorHandle>,
}

// #[derive(Default)]
// pub struct CommitsPerSys {
//     pub maven: HashMap<git2::Oid, Commit>,
//     pub make: HashMap<git2::Oid, Commit>,
//     pub npm: HashMap<git2::Oid, Commit>,
//     pub any: HashMap<git2::Oid, Commit>,
// }

// impl CommitsPerSys {
//     // pub fn accessCommits<'a>(&'a self, sys: &BuildSystem) -> &'a HashMap<git2::Oid, Commit> {
//     //     match sys {
//     //         BuildSystem::Maven => &self.maven,
//     //         BuildSystem::Make => &self.make,
//     //         BuildSystem::Npm => &self.npm,
//     //         BuildSystem::None => &self.any,
//     //     }
//     // }
//     pub fn accessCommits<'a>(&'a self, sys: &RepoConfig) -> &'a HashMap<git2::Oid, Commit> {
//         match sys {
//             RepoConfig::JavaMaven => &self.maven,
//             RepoConfig::CppMake => &self.make,
//             RepoConfig::TsNpm => &self.npm,
//             RepoConfig::Any => &self.any,
//         }
//     }
// }

// pub(crate) struct CommitBuilder<'prepro, 'repo, Sys, CP: CommitProcessor<Sys>> {
//     pub commits: &'prepro mut HashMap<git2::Oid, Commit>,
//     pub processor: &'prepro mut CP,
//     repository: &'repo mut ConfiguredRepo,
//     phantom: PhantomData<Sys>,
// }
// impl<'prepro, 'repo, Sys, CP: CommitProcessor<Sys>> CommitBuilder<'prepro, 'repo, Sys, CP> {
//     // pub fn with_limit(
//     //     self,
//     //     before: &str,
//     //     after: &str,
//     //     dir_path: &str,
//     //     limit: usize,
//     // ) -> Result<Vec<git2::Oid>, git2::Error> {
//     //     log::info!(
//     //         "commits to process: {:?}",
//     //         all_commits_between(&self.repository.repo, before, after).map(|x| x.count())
//     //     );
//     //     Ok(all_commits_between(&self.repository.repo, before, after)?
//     //         .take(limit)
//     //         .map(|oid| {
//     //             let oid = oid.unwrap();
//     //             let c = self
//     //                 .processor
//     //                 .handle_commit::<true>(&self.repository.repo, dir_path, oid);
//     //             self.commits.insert(oid.clone(), c);
//     //             oid.clone()
//     //         })
//     //         .collect())
//     // }

//     // pub fn single(
//     //     &mut self,
//     //     repository: &mut Repository,
//     //     ref_or_commit: &str,
//     //     dir_path: &str,
//     // ) -> git2::Oid {
//     //     let oid = crate::git::retrieve_commit(repository, ref_or_commit)
//     //         .unwrap()
//     //         .id();
//     //     let c = self
//     //         .processor
//     //         .handle_commit::<true>(&repository, dir_path, oid);
//     //     self.commits.insert(oid.clone(), c);
//     //     oid
//     // }
// }

impl PreProcessedRepositories {
    pub fn purge_caches(&mut self) {
        self.processor.purge_caches()
    }

    pub fn commit_count(&self, config: &ParametrizedCommitProcessorHandle) -> usize {
        let proc = self
            .processor
            .processing_systems
            .by_id(*config)
            .unwrap()
            .get(*config);
        proc.commit_count()
    }

    pub fn get_commit(
        &self,
        config: &ParametrizedCommitProcessorHandle,
        commit_oid: &git2::Oid,
    ) -> Option<&Commit> {
        let proc = self
            .processor
            .processing_systems
            .by_id(*config)
            .unwrap()
            .get(*config);
        proc.get_commit(*commit_oid)
    }

    pub fn register_config(&mut self, spec: Repo, config: RepoConfig) -> ConfiguredRepoHandle2 {
        use crate::processing::erased::ProcessorMap;
        let pom = |pr: &mut ProcessorMap| {
            let t = crate::processors::maven::PomParameter::default();
            let h = pr.proc_mut::<PomProcessorHolder>();
            h.register_param(t)
        };
        let makefile = |pr: &mut ProcessorMap| {
            let t = crate::processors::make::makefile::Parameter::default();
            let h = pr.proc_mut::<MakefileProcessorHolder>();
            h.register_param(t)
        };
        let processor_map = &mut self.processor.processing_systems;
        let r = match config {
            #[cfg(feature = "maven_java")]
            RepoConfig::JavaMaven => {
                let java_handle = JavaProc::default_handle(processor_map);
                let pom_handle = pom(processor_map);
                let h = processor_map.commit_proc_mut::<MavenProcessorHolder>();
                let t = crate::processors::maven::Parameter {
                    java_handle,
                    pom_handle,
                };
                let config = h.register_param(t).erase();
                ConfiguredRepoHandle2 { spec, config }
            }
            #[cfg(feature = "java")]
            RepoConfig::Java => {
                let config = JavaProc::default_handle(processor_map).erase();
                ConfiguredRepoHandle2 { spec, config }
            }
            #[cfg(feature = "python")]
            RepoConfig::Python => {
                let config = PythonProc::default_handle(processor_map).erase();
                ConfiguredRepoHandle2 { spec, config }
            }
            #[cfg(feature = "make_cpp")]
            RepoConfig::CppMake => {
                let cpp_handle = CppProc::default_handle(processor_map);
                let makefile_handle = makefile(processor_map);
                let h = processor_map.commit_proc_mut::<MakeProcessorHolder>();
                let t = crate::processors::make::Parameter::new(makefile_handle, cpp_handle);
                let config = h.register_param(t).erase();
                ConfiguredRepoHandle2 { spec, config }
            }
            #[cfg(feature = "cpp")]
            RepoConfig::Cpp => {
                let config = CppProc::default_handle(processor_map).erase();
                ConfiguredRepoHandle2 { spec, config }
            }
            #[cfg(feature = "c")]
            RepoConfig::C => {
                let config = CProc::default_handle(processor_map).erase();
                ConfiguredRepoHandle2 { spec, config }
            }
            #[cfg(feature = "typescript")]
            RepoConfig::Typescript => {
                let config = TypescriptProc::default_handle(processor_map).erase();
                ConfiguredRepoHandle2 { spec, config }
            }
            #[cfg(feature = "rust")]
            RepoConfig::Rust => {
                let config = RustProc::default_handle(processor_map).erase();
                ConfiguredRepoHandle2 { spec, config }
            }
            #[cfg(feature = "file_sys")]
            RepoConfig::Any => {
                let t = crate::processors::file_sys::Parameter {
                    cpp_handle: CppProc::default_handle(processor_map),
                    rust_handle: RustProc::default_handle(processor_map),
                    java_handle: JavaProc::default_handle(processor_map),
                    python_handle: PythonProc::default_handle(processor_map),
                    typescript_handle: TypescriptProc::default_handle(processor_map),
                };
                let holder = processor_map.commit_proc_mut::<FileSysProcessorHolder>();
                let config = holder.register_param(t).erase();
                ConfiguredRepoHandle2 { spec, config }
            }
            _ => todo!(),
        };

        self.configs.insert(r.spec.clone(), r.config);
        r
    }

    pub fn register_config_with_prepro(
        &mut self,
        spec: Repo,
        config: RepoConfig,
        prepro: crate::Str,
    ) -> ConfiguredRepoHandle2 {
        use crate::processing::erased::ProcessorMap;
        let processor_map = &mut self.processor.processing_systems;
        let java = |pr: &mut ProcessorMap, prepro: crate::Str| {
            let h = pr.commit_proc_mut::<JavaProcessorHolder>();
            let t = crate::processors::java::Parameter {
                prepro: Some(prepro),
                ..Default::default()
            };
            h.register_param(t)
        };
        let cpp = |_pr: &mut ProcessorMap, _prepro: crate::Str| {
            todo!("implement scripted preprocessing for cpp");
            // let h = pr.mut_or_default::<CppProcessorHolder>();
            // let t = crate::processors::cpp::Parameter {
            //     // prepro: Some(prepro),
            //     ..Default::default()
            // };
            // h.register_param(t)
        };

        let makefile = |pr: &mut ProcessorMap| {
            let t = crate::processors::make::makefile::Parameter::default();
            let h = pr.proc_mut::<MakefileProcessorHolder>();
            h.register_param(t)
        };
        let pom = |pr: &mut ProcessorMap| {
            let t = crate::processors::maven::PomParameter {};
            let h = pr.proc_mut::<PomProcessorHolder>();
            h.register_param(t)
        };
        let r = match config {
            RepoConfig::JavaMaven => {
                let java_handle = java(processor_map, prepro);
                let pom_handle = pom(processor_map);
                let h = processor_map.commit_proc_mut::<MavenProcessorHolder>();
                let t = crate::processors::maven::Parameter {
                    java_handle,
                    pom_handle,
                };
                let config = h.register_param(t).erase();
                ConfiguredRepoHandle2 { spec, config }
            }
            RepoConfig::CppMake => {
                let cpp_handle = cpp(processor_map, prepro);
                let makefile_handle = makefile(processor_map);
                let h = processor_map.commit_proc_mut::<MakeProcessorHolder>();
                let t = crate::processors::make::Parameter::new(makefile_handle, cpp_handle);
                let config = h.register_param(t).erase();
                ConfiguredRepoHandle2 { spec, config }
            }
            _ => todo!(),
        };
        self.configs.insert(r.spec.clone(), r.config);
        r
    }

    pub fn register_config_with_prequeries(
        &mut self,
        spec: Repo,
        config: RepoConfig,
        query: &[&str],
    ) -> ConfiguredRepoHandle2 {
        use crate::processing::erased::ProcessorMap;
        let cpp = |pr: &mut ProcessorMap, q: hyperast_tsquery::ZeroSepArrayStr| {
            let t = crate::processors::cpp::Parameter::new(q);
            let h = pr.commit_proc_mut::<CppProcessorHolder>();
            h.register_param(t)
        };
        let c = |pr: &mut ProcessorMap, q: hyperast_tsquery::ZeroSepArrayStr| {
            let t = crate::processors::c::Parameter::new(q);
            let h = pr.commit_proc_mut::<CProcessorHolder>();
            h.register_param(t)
        };
        let java = |pr: &mut ProcessorMap, q: hyperast_tsquery::ZeroSepArrayStr| {
            let t = crate::processors::java::Parameter::with_query(q);
            let h = pr.commit_proc_mut::<JavaProcessorHolder>();
            h.register_param(t)
        };
        let python = |pr: &mut ProcessorMap, q: hyperast_tsquery::ZeroSepArrayStr| {
            let t = crate::processors::python::Parameter::new(q);
            let h = pr.commit_proc_mut::<PythonProcessorHolder>();
            h.register_param(t)
        };
        let typescript = |pr: &mut ProcessorMap, q: hyperast_tsquery::ZeroSepArrayStr| {
            let t = crate::processors::typescript::Parameter::new(q);
            let h = pr.commit_proc_mut::<TypescriptProcessorHolder>();
            h.register_param(t)
        };
        let rust = |pr: &mut ProcessorMap, q: hyperast_tsquery::ZeroSepArrayStr| {
            let t = crate::processors::rust::Parameter::new(q);
            let h = pr.commit_proc_mut::<RustProcessorHolder>();
            h.register_param(t)
        };
        let makefile = |pr: &mut ProcessorMap| {
            let t = crate::processors::make::makefile::Parameter::default();
            let h = pr.proc_mut::<MakefileProcessorHolder>();
            h.register_param(t)
        };
        let pom = |pr: &mut ProcessorMap| {
            let t = crate::processors::maven::PomParameter {};
            let h = pr.proc_mut::<PomProcessorHolder>();
            h.register_param(t)
        };
        let processor_map = &mut self.processor.processing_systems;
        let r = match config {
            RepoConfig::JavaMaven => {
                let java_handle = java(processor_map, query.into());
                let pom_handle = pom(processor_map);
                let h = processor_map.commit_proc_mut::<MavenProcessorHolder>();
                let t = crate::processors::maven::Parameter {
                    java_handle,
                    pom_handle,
                };
                let config = h.register_param(t).erase();
                ConfiguredRepoHandle2 { spec, config }
            }
            RepoConfig::Java => {
                let config = java(processor_map, query.into()).erase();
                ConfiguredRepoHandle2 { spec, config }
            }
            RepoConfig::CppMake => {
                let cpp_handle = cpp(processor_map, query.into());
                let makefile_handle = makefile(processor_map);
                let h = processor_map.commit_proc_mut::<MakeProcessorHolder>();
                let t = crate::processors::make::Parameter::new(makefile_handle, cpp_handle);
                let config = h.register_param(t).erase();
                ConfiguredRepoHandle2 { spec, config }
            }
            RepoConfig::Cpp => {
                let config = cpp(processor_map, query.into()).erase();
                ConfiguredRepoHandle2 { spec, config }
            }
            RepoConfig::Python => {
                let config = python(processor_map, query.into()).erase();
                ConfiguredRepoHandle2 { spec, config }
            }
            RepoConfig::Typescript => {
                let config = typescript(processor_map, query.into()).erase();
                ConfiguredRepoHandle2 { spec, config }
            }
            RepoConfig::Rust => {
                let config = rust(processor_map, query.into()).erase();
                ConfiguredRepoHandle2 { spec, config }
            }
            RepoConfig::C => {
                let config = c(processor_map, query.into()).erase();
                ConfiguredRepoHandle2 { spec, config }
            }
            _ => todo!(),
        };
        self.configs.insert(r.spec.clone(), r.config);
        r
    }

    pub fn register_config_with_tsg(
        &mut self,
        repo: Repo,
        config: RepoConfig,
        tsg: crate::Str,
    ) -> ConfiguredRepoHandle2 {
        use crate::processing::erased::ProcessorMap;
        let java = |pr: &mut ProcessorMap, tsg: crate::Str| {
            let t = crate::processors::java::Parameter::with_tsg(tsg);
            let h = pr.commit_proc_mut::<JavaProcessorHolder>();
            h.register_param(t)
        };
        let pom = |pr: &mut ProcessorMap| {
            let t = crate::processors::maven::PomParameter::default();
            let h = pr.proc_mut::<PomProcessorHolder>();
            h.register_param(t)
        };
        let r = match config {
            RepoConfig::JavaMaven => {
                let processor_map = &mut self.processor.processing_systems;
                let java_handle = java(processor_map, tsg.clone());
                let pom_handle = pom(processor_map);
                let h = processor_map.commit_proc_mut::<MavenProcessorHolder>();
                let t1 = crate::processors::maven::Parameter {
                    java_handle,
                    pom_handle,
                };
                let config = h.register_param(t1).erase();
                ConfiguredRepoHandle2 { spec: repo, config }
            }
            RepoConfig::CppMake => {
                unimplemented!()
            }
            _ => todo!(),
        };
        self.configs.insert(r.spec.clone(), r.config);
        r
    }

    pub fn get_config(&self, spec: Repo) -> Option<ConfiguredRepoHandle2> {
        self.configs
            .get(&spec)
            .map(|&config| ConfiguredRepoHandle2 { config, spec })
    }

    pub fn get_precomp_query(
        &self,
        handle: ParametrizedCommitProcessorHandle,
        lang: &str,
    ) -> Option<hyperast_tsquery::ZeroSepArrayStr> {
        let processor_map = &self.processor.processing_systems;
        let proc = processor_map.by_id(handle).unwrap().get(handle);
        let handle = proc.get_lang_handle(lang)?;
        let proc = processor_map.by_id(handle).unwrap().get(handle);
        proc.get_precomp_query()
    }

    pub fn pre_process_chunk(
        &mut self,
        rw: &mut impl Iterator<Item = git2::Oid>,
        repository: &ConfiguredRepo2,
        size: usize,
    ) -> Vec<git2::Oid> {
        self.processor.pre_pro(rw, repository, size)
    }

    pub fn pre_process_with_limit(
        &mut self,
        repository: &ConfiguredRepo2,
        before: &str,
        after: &str,
        // dir_path: &str,
        limit: usize,
    ) -> Result<Vec<git2::Oid>, git2::Error> {
        self.processor
            .pre_process_with_limit(repository, before, after, limit)
    }

    pub fn ensure_pre_processed_with_limit(
        &self,
        repository: &ConfiguredRepo2,
        before: &str,
        after: &str,
        // dir_path: &str,
        limit: usize,
    ) -> Result<Result<Vec<git2::Oid>, Vec<git2::Oid>>, git2::Error> {
        self.processor
            .ensure_pre_processed_with_limit(repository, before, after, limit)
    }
}
