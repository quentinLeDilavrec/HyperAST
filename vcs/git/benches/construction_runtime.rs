use criterion::{BatchSize, BenchmarkId, Criterion, criterion_group, criterion_main};

use hyperast_vcs_git::git::Forge;
use hyperast_vcs_git::multi_preprocessed::PreProcessedRepositories;
use hyperast_vcs_git::processing::RepoConfig;

fn construction_group(c: &mut Criterion) {
    let mut group = c.benchmark_group("HyperAST Construction");

    struct Input {
        repo: hyperast_vcs_git::git::Repo,
        commit: &'static str,
        config: RepoConfig,
        fetch: bool,
    }

    let inputs: &[Input] = &[
        // Input {
        //     repo: hyperast_vcs_git::git::Forge::Github.repo("chromium", "chromium"),
        //     commit: "f461f9752e5918c5c87f2e3767bcb24945ee0fa0",
        //     config: hyperast_vcs_git::processing::RepoConfig::CppMake,
        //     fetch: false,
        // },
        Input {
            repo: Forge::Github.repo("INRIA", "spoon"),
            commit: "56e12a0c0e0e69ea70863011b4f4ca3305e0542b",
            config: RepoConfig::JavaMaven,
            fetch: true,
        },
        Input {
            repo: Forge::Github.repo("INRIA", "spoon"),
            commit: "56e12a0c0e0e69ea70863011b4f4ca3305e0542b",
            config: RepoConfig::Java,
            fetch: true,
        },
        Input {
            repo: Forge::Github.repo("INRIA", "spoon"),
            commit: "56e12a0c0e0e69ea70863011b4f4ca3305e0542b",
            config: RepoConfig::Any,
            fetch: true,
        },
        Input {
            repo: Forge::Github.repo("pallets", "click"),
            commit: "8a1b1a33d739be05b7e91251e3c0dde77c5e152f",
            config: RepoConfig::Python,
            fetch: true,
        },
        Input {
            repo: Forge::Github.repo("pallets", "click"),
            commit: "8a1b1a33d739be05b7e91251e3c0dde77c5e152f",
            config: RepoConfig::Any,
            fetch: true,
        },
        Input {
            repo: Forge::Github.repo("pallets", "flask"),
            commit: "36e4a824f340fdee7ed50937ba8e7f6bc7d17f81",
            config: RepoConfig::Python,
            fetch: true,
        },
        Input {
            repo: Forge::Github.repo("pallets", "flask"),
            commit: "36e4a824f340fdee7ed50937ba8e7f6bc7d17f81",
            config: RepoConfig::Any,
            fetch: true,
        },
        Input {
            repo: Forge::Github.repo("malivinayak", "Multi-Programming"),
            commit: "5debb0d755d71f9714d2fcd3e11fdac66d055b4a",
            config: RepoConfig::Any,
            fetch: true,
        },
        Input {
            repo: Forge::Github.repo("numpy", "numpy"),
            commit: "706b1035187baf72959059cd35ac1f5266e1932c",
            config: RepoConfig::Any,
            fetch: true,
        },
        Input {
            repo: Forge::Github.repo("numpy", "numpy"),
            commit: "706b1035187baf72959059cd35ac1f5266e1932c",
            config: RepoConfig::Python,
            fetch: true,
        },
        Input {
            repo: Forge::Github.repo("numpy", "numpy"),
            commit: "706b1035187baf72959059cd35ac1f5266e1932c",
            config: RepoConfig::Cpp,
            fetch: true,
        },
        Input {
            repo: Forge::Github.repo("numpy", "numpy"),
            commit: "706b1035187baf72959059cd35ac1f5266e1932c",
            config: RepoConfig::C,
            fetch: true,
        },
    ];

    for p in inputs.iter() {
        group.throughput(criterion::Throughput::Elements(1));
        group.bench_with_input(
            BenchmarkId::new("HyperAST", format!("{}_{:?}", p.repo.name(), p.config)),
            &p,
            |b, p| {
                b.iter_batched(
                    || {
                        let mut repositories = PreProcessedRepositories::default();
                        repositories.register_config(p.repo.clone(), p.config);
                        let repo = repositories
                            .get_config(p.repo.clone())
                            .ok_or_else(|| "missing config for repository".to_string())
                            .unwrap();
                        let repository = if p.fetch {
                            repo.fetch()
                        } else {
                            repo.nofetch()
                        };
                        (repositories, repository)
                    },
                    |(mut repositories, repository)| {
                        let mut rw = single_commit(p.commit, &repository.repo).unwrap();
                        repositories.pre_process_chunk(&mut rw, &repository, usize::MAX)
                    },
                    BatchSize::PerIteration,
                )
            },
        );
    }
    group.finish()
}

fn single_commit<'repo>(
    commit: &str,
    repository: &'repo git2::Repository,
) -> Result<impl Iterator<Item = git2::Oid> + 'repo, git2::Error> {
    Ok(hyperast_vcs_git::git::Builder::new(repository)?
        .after(commit)?
        .first_parents()?
        .walk()?
        .take(1)
        .map(|x| x.expect("a valid commit oid")))
}

criterion_group!(
    name = construction;
    config = Criterion::default().sample_size(10).measurement_time(std::time::Duration::from_secs(10)).configure_from_args();
    targets = construction_group
);
criterion_main!(construction);
