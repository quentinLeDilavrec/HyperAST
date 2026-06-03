//! Benchmarks for bottom-up runtime over multiple individual commit pairs.
use criterion::measurement::{Measurement, WallTime};
use criterion::{BatchSize, BenchmarkId, Criterion, Throughput};
use criterion::{criterion_group, criterion_main};

use hyperast_vcs_git::multi_preprocessed::PreProcessedRepositories;

use hyperast_benchmark_diffs::bottom_up_routines::WithSetup;
use hyperast_benchmark_diffs::{Input, OwnedLazyMapping, Prep, ThroughputKind};
use hyperast_benchmark_diffs::{initial_lazy_mapping, prep_commit_pair_after};

#[repr(transparent)]
struct B<'b, Mea: Measurement>(criterion::Bencher<'b, Mea>);

impl<'b, Mea: Measurement> WithSetup for B<'b, Mea> {
    fn run<I, O, S: FnMut() -> I, R: FnMut(I) -> O>(&mut self, s: S, r: R) {
        self.0.iter_batched(s, r, BatchSize::LargeInput);
    }
}

fn bottomup_group(c: &mut Criterion) {
    let only_summarize = c.resummarize;
    let Ok(throughput_kind) = c.throughput.parse();
    let mut group = c.benchmark_group("BottomUp_runtime_commits");
    // some filtering to avoid cluttering summary
    let depth = 10; // nb of diffs to compute
    let take_inputs = 30;
    let take_routines = ["Simple", "Hybrid_100", "Greedy_100"];

    let take_repo_config = hyperast_vcs_git::processing::RepoConfig::JavaMaven;

    let inputs = hyperast_benchmark_diffs::REPOSITORIES;

    let inputs = (inputs.iter().cloned().enumerate())
        .filter(|x| x.1.config == take_repo_config)
        .take(take_inputs)
        .map(|(i, x)| x.with(i, depth))
        .collect::<Vec<_>>();
    let alt_name = format!(
        "{}_{throughput_kind}_{}",
        take_routines.join("_"),
        inputs.len()
    );
    let take_routines = |name: &str| {
        take_routines.is_empty()
            || take_routines
                .iter()
                .any(|p| name.matches(p).next().is_some())
    };
    let inputs = inputs
        .into_iter()
        .filter_map(|i| i.try_fetch())
        .collect::<Vec<_>>();
    let mut p = Bench {
        group: &mut group,
        repositories: PreProcessedRepositories::default(),
        i: inputs.iter().next().unwrap(),
        name: None,
        prep: Prep::None,
        throughput_kind,
    };
    for i in inputs.iter() {
        let mut repositories = PreProcessedRepositories::default();
        repositories.register_config(i.gh(), i.config);
        p.repositories = repositories;
        p.i = i;
        use hyperast_benchmark_diffs::bottom_up_routines::Routines;
        let routines = Routines::lazy_to_lazy::<0, 2>()
            .chain(Routines::lazy_to_lazy::<100, 2>())
            .chain(Routines::lazy_to_lazy::<200, 2>())
            .chain(Routines::lazy_to_lazy::<400, 2>())
            .chain(Routines::lazy_to_complete::<0, 2>())
            .chain(Routines::lazy_to_complete::<100, 2>())
            .chain(Routines::lazy_to_complete::<200, 2>())
            .chain(Routines::lazy_to_complete::<400, 2>());
        for r in routines {
            if !take_routines(&r.name) {
                continue;
            }
            p.name = Some(r.name.clone());
            p.prep = r.prep;
            (match p.prep {
                Prep::None => todo!(),
                _ if only_summarize => only_set_throughput,
                _ => prep_subtree_and_bench,
            })(&mut p, |b, (repositories, (owned, mappings))| {
                // let h = &hyperast_vcs_git::no_space::as_nospaces(&repositories.processor.main_stores);
                let h = &repositories.processor.main_stores;
                // SAFETY: transparent wrapper
                // NOTE: it was needed to avoid issues with invariant lifetimes occurring with `B<'a,'b, Mea>(&mut Bencher<'b`
                let b: &mut B<WallTime> = unsafe { std::mem::transmute(b) };
                (r.routine)(b, h, (owned.clone(), mappings.clone()));
            });
        }
    }
    group.finish_with_alt_summary_save(&alt_name);
}

struct Bench<'a, G> {
    group: &'a mut G,
    repositories: PreProcessedRepositories,
    i: &'a Input,
    name: Option<String>,
    prep: Prep,
    throughput_kind: ThroughputKind,
}

fn only_set_throughput<Mea: Measurement>(
    p: &mut Bench<'_, criterion::BenchmarkGroup<'_, Mea>>,
    _: impl FnMut(&mut criterion::Bencher<'_, Mea>, &(&PreProcessedRepositories, &OwnedLazyMapping))
    + Clone,
) {
    let name = p.name.clone().unwrap();
    for i in 0..p.i.depth {
        let (src, dst) = prep_commit_pair_after(&p.i.repo, &mut p.repositories, 1);
        // let hyperast = hyperast_vcs_git::no_space::as_nospaces(&p.repositories.processor.main_stores);
        let hyperast = &p.repositories.processor.main_stores;

        use ThroughputKind::*;
        let throughput = match p.throughput_kind {
            SnapshotSize => ThroughputKind::size(hyperast, src, dst),
            UnmappedNodesGiven => {
                let owned = initial_lazy_mapping(hyperast, src, dst, &p.prep);
                ThroughputKind::unmapped_nodes_given(&owned.1[0])
            }
            MappedNodes => todo!(),
        };

        eprintln!(
            "set throughput of {}/{} at {}",
            p.i.user, p.i.name, throughput
        );
        p.group.throughput(Throughput::Elements(throughput));
        p.group
            .only_resumarize(BenchmarkId::new(&name, format!("{}/{}", p.i.name, i)));
    }
}

/// Note: Inline if you want to control HyperAST construction and preparatory mapping computation
fn prep_subtree_and_bench<Mea: Measurement>(
    p: &mut Bench<'_, criterion::BenchmarkGroup<'_, Mea>>,
    f: impl FnMut(&mut criterion::Bencher<'_, Mea>, &(&PreProcessedRepositories, &OwnedLazyMapping))
    + Clone,
) {
    let name = p.name.clone().unwrap();
    for i in 0..p.i.depth {
        p.group.bench_with_input_prepared(
            BenchmarkId::new(&name, format!("{}/{}", p.i.name, i)),
            &mut p.repositories,
            |group, repositories| {
                let (src, dst) = prep_commit_pair_after(&p.i.repo, repositories, 1);
                // let hyperast = hyperast_vcs_git::no_space::as_nospaces(&repositories.processor.main_stores);
                let hyperast = &repositories.processor.main_stores;
                let owned = initial_lazy_mapping(hyperast, src, dst, &p.prep);
                use ThroughputKind::*;
                if let SnapshotSize = p.throughput_kind {
                    let throughput = ThroughputKind::size(hyperast, src, dst);
                    group.throughput(Throughput::Elements(throughput));
                } else if let UnmappedNodesGiven = p.throughput_kind {
                    let throughput = ThroughputKind::unmapped_nodes_given(&owned.1[0]);
                    group.throughput(Throughput::Elements(throughput));
                }
                owned
            },
            f.clone(),
        );
    }
}

#[cfg(target_os = "linux")]
criterion_group!(
    name = bottomup;
    config = Criterion::default()
        .sample_size(10)
        .measurement_time(std::time::Duration::from_secs(10))
        .with_measurement(criterion_perf_events::Perf::new(
            perfcnt::linux::PerfCounterBuilderLinux::from_hardware_event(
                perfcnt::linux::HardwareEventType::Instructions
            )
        ))
        .configure_from_args();
    targets = bottomup_group
);
#[cfg(not(target_os = "linux"))]
criterion_group!(
    name = bottomup2;
    config = Criterion::default()
        .sample_size(10)
        .measurement_time(std::time::Duration::from_secs(10))
        .configure_from_args();
    targets = bottomup_group
);
criterion_main!(bottomup2);
