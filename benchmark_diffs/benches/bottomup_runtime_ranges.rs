//! Benchmarks for bottom-up runtime over ranges of commits (not one commit pair by batch).

use const_chunks::IteratorConstChunks;
use criterion::measurement::{Measurement, WallTime};
use criterion::{BatchSize, BenchmarkId, Criterion, Throughput};
use criterion::{criterion_group, criterion_main};

use hyper_diff::decompressed_tree_store::lazy_post_order::LazyPostOrder;
use hyper_diff::matchers::{Decompressible, Mapper};
use hyperast::store::nodes::legion::NodeIdentifier;
use hyperast::types::{DecompressedFrom, HyperAST as _};
use hyperast::types::{HyperASTShared, WithStats as _};
use hyperast_vcs_git::multi_preprocessed::PreProcessedRepositories;

use hyperast_benchmark_diffs::bottom_up_routines::{OwnedLazyMapping, WithSetup};
use hyperast_benchmark_diffs::initial_lazy_mapping;
use hyperast_benchmark_diffs::{Input, Prep, ThroughputKind, prep_commits};

#[allow(type_alias_bounds)]
type DS<HAST: HyperASTShared> = Decompressible<HAST, LazyPostOrder<HAST::IdN, u32>>;
type M = hyper_diff::mappings::VecStore<u32>;

#[repr(transparent)]
struct B<'b, Mea: Measurement>(criterion::Bencher<'b, Mea>);

impl<'b, Mea: Measurement> WithSetup for B<'b, Mea> {
    fn run<I, O, S: FnMut() -> I, R: FnMut(I) -> O>(&mut self, s: S, r: R) {
        self.0.iter_batched(s, r, BatchSize::LargeInput);
    }
}

const RANGE: usize = 10;

fn bottomup_group(c: &mut Criterion) {
    let only_summarize = c.resummarize;
    let Ok(throughput_kind) = c.throughput.parse();
    let mut group = c.benchmark_group("BottomUp_runtime_ranges");
    // some filtering to avoid cluttering summary
    let take_inputs = 30;
    let take_routines = ["Greedy_100", "Simple", "Hybrid_100"]; //["Simple", "Hybrid_100"]; //,
    let take_repo_config = hyperast_vcs_git::processing::RepoConfig::JavaMaven;

    let inputs = hyperast_benchmark_diffs::REPOSITORIES;

    let inputs = (inputs.iter().cloned().enumerate())
        .filter(|x| x.1.config == take_repo_config)
        .take(take_inputs)
        .map(|(i, x)| x.with(i, 1))
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
        let routines = Routines::lazy_to_lazy::<0, RANGE>()
            .chain(Routines::lazy_to_lazy::<100, RANGE>())
            .chain(Routines::lazy_to_lazy::<200, RANGE>())
            .chain(Routines::lazy_to_lazy::<400, RANGE>())
            .chain(Routines::lazy_to_complete::<0, RANGE>())
            .chain(Routines::lazy_to_complete::<100, RANGE>())
            .chain(Routines::lazy_to_complete::<200, RANGE>())
            .chain(Routines::lazy_to_complete::<400, RANGE>());
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

macro_rules! subtree_matching {
    ($mapper:expr, $prep:expr) => {
        match $prep {
            Prep::GT => {
                use hyper_diff::matchers::heuristic::gt;

                use gt::lazy_greedy_subtree_matcher::LazyGreedySubtreeMatcher;
                type MM = hyper_diff::mappings::DefaultMultiMappingStore<u32>;
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
    _: impl FnMut(
        &mut criterion::Bencher<'_, Mea>,
        &(
            &PreProcessedRepositories,
            &OwnedLazyMapping<NodeIdentifier, RANGE>,
        ),
    ) + Clone,
) {
    let name = p.name.clone().unwrap();
    let mut throughput = 0;
    let list = prep_commits(&p.i.repo, &mut p.repositories, RANGE + 1);
    // let hyperast = hyperast_vcs_git::no_space::as_nospaces(&p.repositories.processor.main_stores);
    let hyperast = &p.repositories.processor.main_stores;
    list.into_iter()
        .fold(vec![], |mut acc, dst: _| {
            let Some(src) = acc.pop() else {
                throughput += hyperast.resolve(&dst).size() as u64;
                acc.push(dst);
                return acc;
            };

            use ThroughputKind::*;
            if let SnapshotSize = p.throughput_kind {
                throughput += hyperast.resolve(&dst).size() as u64;
            } else if let UnmappedNodesGiven = p.throughput_kind {
                let owned = initial_lazy_mapping(hyperast, src, dst, &p.prep);
                throughput += ThroughputKind::unmapped_nodes_given(&owned.1[0]);
            }
            acc.push(src);
            acc.push(dst);
            acc
        })
        .into_iter()
        .const_chunks::<RANGE>()
        .next()
        .unwrap();

    eprintln!(
        "set throughput of {}/{} at {}",
        p.i.user, p.i.name, throughput
    );
    p.group.throughput(Throughput::Elements(throughput));
    p.group
        .only_resumarize(BenchmarkId::new(&name, format!("{}", p.i.name)));
}

/// Note: Inline if you want to control HyperAST construction and preparatory mapping computation
fn prep_subtree_and_bench<Mea: Measurement>(
    p: &mut Bench<'_, criterion::BenchmarkGroup<'_, Mea>>,
    f: impl FnMut(
        &mut criterion::Bencher<'_, Mea>,
        &(
            &PreProcessedRepositories,
            &OwnedLazyMapping<NodeIdentifier, RANGE>,
        ),
    ) + Clone,
) {
    let name = p.name.clone().unwrap();
    p.group.bench_with_input_prepared(
        BenchmarkId::new(&name, format!("{}", p.i.name)),
        &mut p.repositories,
        |group, repositories| {
            let mut throughput = 0;
            let list = prep_commits(&p.i.repo, repositories, RANGE + 1);
            // let hyperast = hyperast_vcs_git::no_space::as_nospaces(&repositories.processor.main_stores);
            let hyperast = &repositories.processor.main_stores;
            let mut mappings = std::array::from_fn::<M, RANGE, _>(|_| M::default());
            let owneds = list
                .into_iter()
                .map(|x| <DS<_> as DecompressedFrom<_>>::decompress(hyperast, &x))
                .fold(vec![], |mut acc, mut dst: DS<_>| {
                    let Some(mut src) = acc.pop() else {
                        throughput += dst.len() as u64;
                        acc.push(dst);
                        return acc;
                    };
                    let mapper = Mapper {
                        hyperast,
                        mapping: hyper_diff::matchers::Mapping {
                            src_arena: src.as_mut(),
                            dst_arena: dst.as_mut(),
                            mappings: &mut mappings[acc.len()],
                        },
                    };
                    let mapper = subtree_matching!(mapper, p.prep);

                    use ThroughputKind::*;
                    if let SnapshotSize = p.throughput_kind {
                        throughput += mapper.dst_arena.len() as u64;
                    } else if let UnmappedNodesGiven = p.throughput_kind {
                        throughput += ThroughputKind::unmapped_nodes_given(&mapper.mappings);
                    }
                    acc.push(src);
                    acc.push(dst);
                    acc
                })
                .into_iter()
                .map(|x| x.decomp)
                .const_chunks::<RANGE>()
                .next()
                .unwrap();
            group.throughput(Throughput::Elements(throughput));
            // hyper_diff::matchers::heuristic::gt::lazy_greedy_subtree_matcher::LazyGreedySubtreeMatcher
            // let owned = {
            //     // let mut owned: DS<_> = hyperast.decompress(&src).1;
            //     let mapper = Mapper::with_mut_decompressible(&mut mapper_owned, M::default());
            //     let mapper = subtree_matching!(mapper, p.prep);
            //     let mappings = mapper.mapping.mappings;
            //     (
            //         [mapper_owned.0.decomp, mapper_owned.1.decomp],
            //         [mappings, std::default::Default::default()],
            //     )
            // };
            (owneds, mappings)
        },
        f.clone(),
    );
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
