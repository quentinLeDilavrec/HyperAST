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
        Input {
            repo: Forge::Github.repo("microsoft", "vscode"),
            commit: "12717879c47434790e4e956f8e211dc6b1948efa",
            config: RepoConfig::Typescript,
            fetch: true,
        },
        Input {
            repo: Forge::Github.repo("godotengine", "godot"),
            commit: "8222d0983aa49f0bd4d74dde05d1a801269231db",
            config: RepoConfig::Cpp,
            fetch: true,
        },
        Input {
            repo: Forge::Github.repo("microsoft", "TypeScript"),
            commit: "8ef3e2f3d43c8c92bda9510c47f7d4d2b3aeca33",
            config: RepoConfig::Typescript,
            fetch: true,
        },
        Input {
            repo: Forge::Github.repo("pola-rs", "polars"),
            commit: "5e9cf5c09d3c0c316cba1ced8282cb835f76df5f",
            config: RepoConfig::Any,
            fetch: true,
        },
        Input {
            repo: Forge::Github.repo("pandas-dev", "pandas"),
            commit: "d57b53693bb01efeba9e51281e6e676c226ba1ae",
            config: RepoConfig::Any,
            fetch: true,
        },
        Input {
            repo: Forge::Github.repo("fastapi", "fastapi"),
            commit: "a497a025e7114ca442478ed28da7e0a1cdc6177a",
            config: RepoConfig::Any,
            fetch: true,
        },
        Input {
            repo: Forge::Github.repo("scikit-learn", "scikit-learn"),
            commit: "aeadb51af96556dd0f884c0037c9dc9993449538",
            config: RepoConfig::Any,
            fetch: true,
        },
        Input {
            repo: Forge::Github.repo("denoland", "deno"),
            commit: "dbcd1a9194f4a69b2ac96d925c90dcf9bf5a50cc",
            config: RepoConfig::Any,
            fetch: true,
        },
        Input {
            repo: Forge::Github.repo("denoland", "deno"),
            commit: "dbcd1a9194f4a69b2ac96d925c90dcf9bf5a50cc",
            config: RepoConfig::Typescript,
            fetch: true,
        },
    ];

    for p in inputs.iter() {
        group.throughput(criterion::Throughput::Elements(1));
        group.bench_with_input_prepared(
            BenchmarkId::new("HyperAST", format!("{}_{:?}", p.repo.name(), p.config)),
            &mut &p,
            |_, p| {
                if p.fetch {
                    p.repo
                        .fetch_with_cb(|s| {
                            println!("{}", s);
                        })
                        .unwrap();
                };
            },
            |b, (p, _)| {
                b.iter_batched(
                    || {
                        let mut repositories = PreProcessedRepositories::default();
                        repositories.register_config(p.repo.clone(), p.config);
                        let repo = repositories
                            .get_config(p.repo.clone())
                            .ok_or_else(|| "missing config for repository".to_string())
                            .unwrap();
                        (repositories, repo.nofetch())
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
