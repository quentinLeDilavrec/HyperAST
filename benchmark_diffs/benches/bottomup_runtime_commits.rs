use criterion::measurement::{Measurement, WallTime};
use criterion::{BatchSize, BenchmarkId, Criterion, Throughput};
use criterion::{criterion_group, criterion_main};

use hyper_diff::decompressed_tree_store::lazy_post_order::LazyPostOrder;
use hyper_diff::mappings::{MappingStore as _, VecStore};
use hyper_diff::matchers::{Decompressible, Mapper};
use hyperast::store::nodes::legion::NodeIdentifier;
use hyperast::types::{HyperAST as _, HyperASTShared, WithStats as _};
use hyperast_benchmark_diffs::bottom_up_routines::WithSetup;
use hyperast_vcs_git::multi_preprocessed::PreProcessedRepositories;

use hyperast_benchmark_diffs::{Input, Prep, ThroughputKind, prep_commit_pair_after};

#[allow(type_alias_bounds)]
type DS<HAST: HyperASTShared> = Decompressible<HAST, LazyPostOrder<HAST::IdN, u32>>;
type M = hyper_diff::mappings::VecStore<u32>;
type MM = hyper_diff::mappings::DefaultMultiMappingStore<u32>;

#[repr(transparent)]
struct B<'b, Mea: Measurement>(criterion::Bencher<'b, Mea>);

impl<'b, Mea: Measurement> WithSetup for B<'b, Mea> {
    fn run<I, O, S: FnMut() -> I, R: FnMut(I) -> O>(&mut self, s: S, r: R) {
        self.0.iter_batched(s, r, BatchSize::LargeInput);
    }
}

fn bottomup_group(c: &mut Criterion) {
    let only_sumarize = c.resummarize;
    let Ok(throughput_kind) = c.throughput.parse();
    let mut group = c.benchmark_group("BottomUp_runtime_commits");
    // some filtering to avoid cluttering summary
    let depth = 3; // nb of diffs to compute
    let take_inputs = 3;
    let take_routines = ["Simple", "Hybrid_100"];
    let take_repoconfig = hyperast_vcs_git::processing::RepoConfig::JavaMaven;

    let inputs = hyperast_benchmark_diffs::REPOSITORIES;

    let inputs = (inputs.iter().cloned().enumerate())
        .filter(|x| x.1.config == take_repoconfig)
        .take(take_inputs)
        .map(|(i, x)| x.with(i, depth))
        .collect::<Vec<_>>();
    let alt_name = format!(
        "{}_{throughput_kind}_{}",
        take_routines.join("_"),
        inputs.len()
    );
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
        let routines = Routines::lazy_to_lazy::<0>()
            .chain(Routines::lazy_to_lazy::<100>())
            .chain(Routines::lazy_to_lazy::<200>())
            .chain(Routines::lazy_to_lazy::<400>())
            .chain(Routines::lazy_to_complete::<0>())
            .chain(Routines::lazy_to_complete::<100>())
            .chain(Routines::lazy_to_complete::<200>())
            .chain(Routines::lazy_to_complete::<400>());
        for r in routines {
            if !take_routines.is_empty()
                && !take_routines
                    .iter()
                    .any(|p| r.name.matches(p).next().is_some())
            {
                continue;
            }
            p.name = Some(r.name.clone());
            p.prep = r.prep;
            (match p.prep {
                Prep::None => todo!(),
                _ if only_sumarize => only_set_throughput,
                _ => prep_gt_subtree_and_bench,
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

type OwnedLazyMapping = (
    (
        LazyPostOrder<NodeIdentifier, u32>,
        LazyPostOrder<NodeIdentifier, u32>,
    ),
    VecStore<u32>,
);

macro_rules! subtree_matching {
    ($mapper:expr, $prep:expr) => {
        match $prep {
            Prep::GT => {
                use hyper_diff::matchers::heuristic::gt;

                use gt::lazy_greedy_subtree_matcher::LazyGreedySubtreeMatcher;
                LazyGreedySubtreeMatcher::<_>::match_it::<MM>($mapper)
            }
            Prep::CD => {
                use hyper_diff::matchers::heuristic::cd;

                use cd::lazy_leaves_matcher::LazyLeavesMatcher;
                LazyLeavesMatcher::<_>::match_it($mapper)
            }
            Prep::None => panic!(),
        }
    };
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

        let throughput = if let ThroughputKind::SnapshotSize = p.throughput_kind {
            (hyperast.resolve(&src).size() + hyperast.resolve(&dst).size()).div_ceil(2) as u64
        } else if let ThroughputKind::UnmappedNodesGiven = p.throughput_kind {
            let mut mapper_owned: (DS<_>, DS<_>) = hyperast.decompress_pair(&src, &dst).1;
            let mapper = Mapper::with_mut_decompressible(&mut mapper_owned, M::default());
            let mapper = subtree_matching!(mapper, p.prep);
            (mapper.mappings.capacity().0 - mapper.mappings.len()) as u64
                + (mapper.mappings.capacity().1 - mapper.mappings.len()) as u64
            // (hyperast.resolve(&src).size() + hyperast.resolve(&dst).size()).div_ceil(2) as u64 + 1
        } else if let ThroughputKind::MappedNodes = p.throughput_kind {
            todo!()
        } else {
            panic!()
        };
        eprintln!(
            "set throughput of {}/{} at {}",
            p.i.user, p.i.name, throughput
        );
        p.group.throughput(Throughput::Elements(throughput));
        p.group
            .only_resumarize(BenchmarkId::new(&name, format!("{}/{}", p.i.id, i)));
    }
}

/// Note: Inline if you want to control HyperAST construction and preparatory mapping computation
fn prep_gt_subtree_and_bench<Mea: Measurement>(
    p: &mut Bench<'_, criterion::BenchmarkGroup<'_, Mea>>,
    f: impl FnMut(&mut criterion::Bencher<'_, Mea>, &(&PreProcessedRepositories, &OwnedLazyMapping))
    + Clone,
) {
    let name = p.name.clone().unwrap();
    for i in 0..p.i.depth {
        p.group.bench_with_input_prepared(
            BenchmarkId::new(&name, format!("{}/{}", p.i.id, i)),
            &mut p.repositories,
            |group, repositories| {
                let (src, dst) = prep_commit_pair_after(&p.i.repo, repositories, 1);
                // let hyperast = hyperast_vcs_git::no_space::as_nospaces(&repositories.processor.main_stores);
                let hyperast = &repositories.processor.main_stores;
                group.throughput(Throughput::Elements(
                    (hyperast.resolve(&src).size() + hyperast.resolve(&dst).size()).div_ceil(2)
                        as u64,
                ));
                let mut mapper_owned: (DS<_>, DS<_>) = hyperast.decompress_pair(&src, &dst).1;
                let mapper = Mapper::with_mut_decompressible(&mut mapper_owned, M::default());
                let mapper = subtree_matching!(mapper, p.prep);
                let mappings = mapper.mapping.mappings.clone();
                ((mapper_owned.0.decomp, mapper_owned.1.decomp), mappings)
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
