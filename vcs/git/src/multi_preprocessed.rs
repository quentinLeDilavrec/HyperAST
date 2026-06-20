use std::collections::HashMap;

use crate::Commit;
use crate::git::Repo;
use crate::preprocessed::RepositoryProcessor;
use crate::processing::erased::{CommitProcExt, ParametrizedCommitProcessorHandle};
use crate::processing::{ConfiguredRepo2, ProcessorHolder};
use crate::processing::{ConfiguredRepoHandle2, RepoConfig};

use crate::processors::java::JavaProc;
type JavaProcessorHolder = ProcessorHolder<JavaProc>;
use crate::processors::cpp::CppProc;
type CppProcessorHolder = ProcessorHolder<CppProc>;
#[cfg(feature = "python")]
type PythonProcessorHolder = ProcessorHolder<crate::processors::python::PythonProc>;
use crate::processors::make::MakeProc;
type MakeProcessorHolder = ProcessorHolder<MakeProc>;
use crate::processors::maven::MavenProc;
type MavenProcessorHolder = ProcessorHolder<MavenProc>;
use crate::processors::maven::PomProc;
type PomProcessorHolder = ProcessorHolder<PomProc>;
// #[cfg(feature = "file_sys")]
// use crate::processors::file_sys::FileSysProc;
// #[cfg(feature = "file_sys")]
// type FileSysProcessorHolder = ProcessorHolder<FileSysProc>;

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
            .by_id(&config.0)
            .unwrap()
            .get(config.1);
        proc.commit_count()
    }

    pub fn get_commit(
        &self,
        config: &ParametrizedCommitProcessorHandle,
        commit_oid: &git2::Oid,
    ) -> std::option::Option<&Commit> {
        let proc = self
            .processor
            .processing_systems
            .by_id(&config.0)
            .unwrap()
            .get(config.1);
        proc.get_commit(*commit_oid)
    }

    pub fn register_config(&mut self, spec: Repo, config: RepoConfig) -> ConfiguredRepoHandle2 {
        use crate::processing::erased::ProcessorMap;
        let java = |pr: &mut ProcessorMap| {
            let t = crate::processors::java::Parameter::faster();
            let h = pr.mut_or_default::<JavaProcessorHolder>();
            CommitProcExt::register_param(h, t)
        };
        let pom = |pr: &mut ProcessorMap| {
            let t = crate::processors::maven::PomParameter::default();
            let h = pr.mut_or_default::<PomProcessorHolder>();
            CommitProcExt::register_param(h, t)
        };
        let cpp = |pr: &mut ProcessorMap| {
            let q = ["(translation_unit)"].as_slice();
            let t = crate::processors::cpp::Parameter::new(q);
            let h = pr.mut_or_default::<CppProcessorHolder>();
            CommitProcExt::register_param(h, t)
        };
        #[cfg(feature = "python")]
        let python = |pr: &mut ProcessorMap| {
            let t = crate::processors::python::Parameter::default();
            let h = pr.mut_or_default::<PythonProcessorHolder>();
            CommitProcExt::register_param(h, t)
        };
        let r = match config {
            RepoConfig::JavaMaven => {
                let processor_map = &mut self.processor.processing_systems;
                let java_handle = java(processor_map);
                let pom_handle = pom(processor_map);
                let h = processor_map.mut_or_default::<MavenProcessorHolder>();
                let t = crate::processors::maven::Parameter {
                    java_handle,
                    pom_handle,
                };
                let config = h.register_param(t).erase();
                ConfiguredRepoHandle2 { spec, config }
            }
            RepoConfig::Java => {
                let processor_map = &mut self.processor.processing_systems;
                let config = java(processor_map).erase();
                ConfiguredRepoHandle2 { spec, config }
            }
            #[cfg(feature = "python")]
            RepoConfig::Python => {
                let processor_map = &mut self.processor.processing_systems;
                let config = python(processor_map).erase();
                ConfiguredRepoHandle2 { spec, config }
            }
            RepoConfig::CppMake => {
                let processor_map = &mut self.processor.processing_systems;
                let cpp_handle = cpp(processor_map);
                let h = processor_map.mut_or_default::<MakeProcessorHolder>();
                let t = crate::processors::make::Parameter { cpp_handle };
                let config = h.register_param(t).erase();
                ConfiguredRepoHandle2 { spec, config }
            }
            // #[cfg(feature = "file_sys")]
            // RepoConfig::Any => {
            //     let processor_map = &mut self.processor.processing_systems;
            //     let t = crate::processors::file_sys::Parameter {
            //         cpp_handle: cpp(processor_map),
            //         java_handle: java(processor_map),
            //         python_handle: python(processor_map),
            //         // query: Some(q.into()),
            //     };
            //     let holder = processor_map.mut_or_default::<FileSysProcessorHolder>();
            //     let config = holder.register_param(t).erase();
            //     ConfiguredRepoHandle2 { spec, config }
            // }
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
        let r = match config {
            RepoConfig::JavaMaven => {
                let processor_map = &mut self.processor.processing_systems;
                let h_java = processor_map.mut_or_default::<JavaProcessorHolder>();
                let t = crate::processors::java::Parameter {
                    prepro: Some(prepro),
                    ..Default::default()
                };
                let java_handle = CommitProcExt::register_param(h_java, t);
                use crate::processors::maven::PomParameter;
                let h_pom = processor_map.mut_or_default::<PomProcessorHolder>();
                let pom_handle = CommitProcExt::register_param(h_pom, PomParameter {});
                let h = self
                    .processor
                    .processing_systems
                    .mut_or_default::<MavenProcessorHolder>();
                let t = crate::processors::maven::Parameter {
                    java_handle,
                    pom_handle,
                };
                let config = h.register_param(t).erase();
                ConfiguredRepoHandle2 { spec, config }
            }
            RepoConfig::CppMake => {
                let t = crate::processors::cpp::Parameter { query: None };
                let h_cpp = self
                    .processor
                    .processing_systems
                    .mut_or_default::<CppProcessorHolder>();
                let cpp_handle = CommitProcExt::register_param(h_cpp, t);
                let h = self
                    .processor
                    .processing_systems
                    .mut_or_default::<MakeProcessorHolder>();
                let t: crate::processors::make::Parameter = cpp_handle.into();
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
            let h = pr.mut_or_default::<CppProcessorHolder>();
            CommitProcExt::register_param(h, t)
        };
        let r = match config {
            RepoConfig::JavaMaven => {
                let processor_map = &mut self.processor.processing_systems;
                let h_java = processor_map.mut_or_default::<JavaProcessorHolder>();
                let t = crate::processors::java::Parameter {
                    query: Some(query.into()),
                    ..Default::default()
                };
                let java_handle = CommitProcExt::register_param(h_java, t);
                use crate::processors::maven::PomParameter;
                let h_pom = processor_map.mut_or_default::<PomProcessorHolder>();
                let pom_handle = CommitProcExt::register_param(h_pom, PomParameter {});
                let h = processor_map.mut_or_default::<MavenProcessorHolder>();
                let config = h
                    .register_param(crate::processors::maven::Parameter {
                        java_handle,
                        pom_handle,
                    })
                    .erase();
                ConfiguredRepoHandle2 { spec, config }
            }
            RepoConfig::Java => {
                let processor_map = &mut self.processor.processing_systems;
                let h_java = processor_map.mut_or_default::<JavaProcessorHolder>();
                let t = crate::processors::java::Parameter::with_query(query);
                let config = h_java.register_param(t).erase();
                ConfiguredRepoHandle2 { spec, config }
            }
            RepoConfig::CppMake => {
                let processor_map = &mut self.processor.processing_systems;
                let cpp_handle = cpp(processor_map, query.into());
                let t = crate::processors::make::Parameter { cpp_handle };
                let h = processor_map.mut_or_default::<MakeProcessorHolder>();
                let config = h.register_param(t).erase();
                ConfiguredRepoHandle2 { spec, config }
            }
            RepoConfig::Python => {
                let processor_map = &mut self.processor.processing_systems;
                let h_python = processor_map.mut_or_default::<PythonProcessorHolder>();
                let t = crate::processors::python::Parameter::with_query(query);
                let config = h_python.register_param(t).erase();
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
            let h = pr.mut_or_default::<JavaProcessorHolder>();
            CommitProcExt::register_param(h, t)
        };
        let pom = |pr: &mut ProcessorMap| {
            let t = crate::processors::maven::PomParameter::default();
            let h = pr.mut_or_default::<PomProcessorHolder>();
            CommitProcExt::register_param(h, t)
        };
        let r = match config {
            RepoConfig::JavaMaven => {
                let processor_map = &mut self.processor.processing_systems;
                let java_handle = java(processor_map, tsg.clone());
                let pom_handle = pom(processor_map);
                let h = processor_map.mut_or_default::<MavenProcessorHolder>();
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

    pub fn get_config(&self, repo: Repo) -> Option<ConfiguredRepoHandle2> {
        self.configs
            .get(&repo)
            .map(|&config| ConfiguredRepoHandle2 { config, spec: repo })
    }

    pub fn get_precomp_query(
        &self,
        handle: ParametrizedCommitProcessorHandle,
        lang: &str,
    ) -> Option<hyperast_tsquery::ZeroSepArrayStr> {
        let proc = self
            .processor
            .processing_systems
            .by_id(&handle.0)
            .unwrap()
            .get(handle.1);
        let handle = proc.get_lang_handle(lang)?;
        let proc = self
            .processor
            .processing_systems
            .by_id(&handle.0)
            .unwrap()
            .get(handle.1);
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

    // pub fn pre_process_with_config2(
    //     &mut self,
    //     repository: &mut ConfiguredRepo2,
    //     before: &str,
    //     after: &str,
    // ) -> Result<Vec<git2::Oid>, git2::Error> {
    //     assert!(!before.is_empty());
    //     self.processor.pre_process(repository, before, after)
    // }

    // fn pre_process_with_config(
    //     &mut self,
    //     repository: &mut ConfiguredRepo,
    //     before: &str,
    //     after: &str,
    // ) -> Result<Vec<git2::Oid>, git2::Error> {
    //     let config = &repository.config;
    //     let repository = &mut repository.repo;
    //     log::info!(
    //         "commits to process: {:?}",
    //         all_commits_between(&repository, before, after).map(|x| x.count())
    //     );
    //     let mut processing_ordered_commits = vec![];
    //     let rw = all_commits_between(&repository, before, after)?;
    //     todo!();
    //     // let config = config.into();
    //     // match config {
    //     //     ProcessingConfig::JavaMaven { limit, dir_path } => {
    //     //         let commits = self.commits.entry(RepoConfig::JavaMaven).or_default();
    //     //         rw
    //     //             // .skip(1500)release-1.0.0 refs/tags/release-3.3.2-RC4
    //     //             .take(limit) // TODO make a variable
    //     //             .for_each(|oid| {
    //     //                 let oid = oid.unwrap();
    //     //                 let c = CommitProcessor::<file_sys::Maven>::handle_commit::<true>(
    //     //                     &mut self.processor,
    //     //                     &repository,
    //     //                     dir_path,
    //     //                     oid,
    //     //                 );
    //     //                 processing_ordered_commits.push(oid.clone());
    //     //                 commits.insert(oid.clone(), c);
    //     //             });
    //     //     }
    //     //     ProcessingConfig::CppMake { limit, dir_path } => {
    //     //         let commits = self.commits.entry(RepoConfig::CppMake).or_default();
    //     //         rw
    //     //             // .skip(1500)release-1.0.0 refs/tags/release-3.3.2-RC4
    //     //             .take(limit) // TODO make a variable
    //     //             .for_each(|oid| {
    //     //                 let oid = oid.unwrap();
    //     //                 let c = CommitProcessor::<file_sys::Make>::handle_commit::<true>(
    //     //                     &mut self.processor,
    //     //                     &repository,
    //     //                     dir_path,
    //     //                     oid,
    //     //                 );
    //     //                 processing_ordered_commits.push(oid.clone());
    //     //                 commits.insert(oid.clone(), c);
    //     //             });
    //     //     }
    //     //     ProcessingConfig::TsNpm { limit, dir_path } => {
    //     //         let commits = self.commits.entry(RepoConfig::TsNpm).or_default();
    //     //         rw
    //     //             // .skip(1500)release-1.0.0 refs/tags/release-3.3.2-RC4
    //     //             .take(limit) // TODO make a variable
    //     //             .for_each(|oid| {
    //     //                 let oid = oid.unwrap();
    //     //                 let c = CommitProcessor::<file_sys::Npm>::handle_commit::<true>(
    //     //                     &mut self.processor,
    //     //                     &repository,
    //     //                     dir_path,
    //     //                     oid,
    //     //                 );
    //     //                 processing_ordered_commits.push(oid.clone());
    //     //                 commits.insert(oid.clone(), c);
    //     //             });
    //     //     }
    //     //     ProcessingConfig::Any { limit, dir_path } => {
    //     //         let commits = self.commits.entry(RepoConfig::Any).or_default();
    //     //         rw
    //     //             // .skip(1500)release-1.0.0 refs/tags/release-3.3.2-RC4
    //     //             .take(limit) // TODO make a variable
    //     //             .for_each(|oid| {
    //     //                 let oid = oid.unwrap();
    //     //                 let c = CommitProcessor::<file_sys::Any>::handle_commit::<true>(
    //     //                     &mut self.processor,
    //     //                     &repository,
    //     //                     dir_path,
    //     //                     oid,
    //     //                 );
    //     //                 processing_ordered_commits.push(oid.clone());
    //     //                 commits.insert(oid.clone(), c);
    //     //             });
    //     //     }
    //     // }
    //     Ok(processing_ordered_commits)
    // }
}
