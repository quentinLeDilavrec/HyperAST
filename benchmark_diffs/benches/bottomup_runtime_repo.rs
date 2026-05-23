use criterion::measurement::Measurement;
use criterion::{BatchSize, BenchmarkId, Criterion, Throughput};
use criterion::{criterion_group, criterion_main};
use std::hint::black_box;

use hyper_diff::decompressed_tree_store::CompletePostOrder;
use hyper_diff::decompressed_tree_store::lazy_post_order::LazyPostOrder;
use hyper_diff::mappings::MappingStore;
use hyper_diff::mappings::VecStore;
use hyper_diff::matchers::{Decompressible, Mapper};
use hyperast::store::nodes::legion::NodeIdentifier;
use hyperast::types::{HyperAST as _, HyperASTShared, WithStats as _};
use hyperast_vcs_git::multi_preprocessed::PreProcessedRepositories;

use hyperast_benchmark_diffs::{Input, prep_commits};

#[allow(type_alias_bounds)]
type DS<HAST: HyperASTShared> = Decompressible<HAST, LazyPostOrder<HAST::IdN, u32>>;
#[allow(type_alias_bounds)]
type CDS<HAST: HyperASTShared> = Decompressible<HAST, CompletePostOrder<HAST::IdN, u32>>;
type M = hyper_diff::mappings::VecStore<u32>;
type MM = hyper_diff::mappings::DefaultMultiMappingStore<u32>;

fn bottomup_group(c: &mut Criterion) {
    let mut group = c.benchmark_group("Gumtree_BottomUp_runtime");

    let inputs: &[Input] = &[
        // Input {
        //     repo: hyperast_vcs_git::git::Forge::Github.repo("chromium", "chromium"),
        //     commit: "f461f9752e5918c5c87f2e3767bcb24945ee0fa0",
        //     config: hyperast_vcs_git::processing::RepoConfig::CppMake,
        //     fetch: false,
        // },
        //
        Input {
            repo: hyperast_vcs_git::git::Forge::Github.repo("apache", "maven"),
            commit: "c3cf29438e3d65d6ee5c5726f8611af99d9a649a",
            config: hyperast_vcs_git::processing::RepoConfig::JavaMaven,
            fetch: true,
        },
        Input {
            repo: hyperast_vcs_git::git::Forge::Github.repo("INRIA", "spoon"),
            commit: "56e12a0c0e0e69ea70863011b4f4ca3305e0542b",
            config: hyperast_vcs_git::processing::RepoConfig::JavaMaven,
            fetch: true,
        },
        Input {
            repo: hyperast_vcs_git::git::Forge::Github.repo("apache", "hadoop"),
            commit: "b69ede7154d44538a4a66824c34f7ba143deef25",
            config: hyperast_vcs_git::processing::RepoConfig::JavaMaven,
            fetch: true,
        },
    ];
    let mut repositories = PreProcessedRepositories::default();
    for p in inputs.iter() {
        repositories.register_config(p.repo.clone(), p.config);
    }
    for p in inputs.iter() {
        bench_xy(&mut group, &mut repositories, p);
        bench_lazy_xy(&mut group, &mut repositories, p);
        bench_greedy::<100>(&mut group, &mut repositories, p);
        bench_greedy::<200>(&mut group, &mut repositories, p);
        bench_greedy::<400>(&mut group, &mut repositories, p);
        bench_lazy_greedy::<100>(&mut group, &mut repositories, p);
        bench_lazy_greedy::<200>(&mut group, &mut repositories, p);
        bench_lazy_greedy::<400>(&mut group, &mut repositories, p);
        bench_simple(&mut group, &mut repositories, p);
        bench_lazy_simple(&mut group, &mut repositories, p);
        bench_hybrid::<100>(&mut group, &mut repositories, p);
        bench_hybrid::<200>(&mut group, &mut repositories, p);
        bench_hybrid::<400>(&mut group, &mut repositories, p);
        bench_lazy_hybrid::<100>(&mut group, &mut repositories, p);
        bench_lazy_hybrid::<200>(&mut group, &mut repositories, p);
        bench_lazy_hybrid::<400>(&mut group, &mut repositories, p);
        bench_stable::<100>(&mut group, &mut repositories, p);
        bench_stable::<200>(&mut group, &mut repositories, p);
        bench_stable::<400>(&mut group, &mut repositories, p);
        bench_lazy_stable::<100>(&mut group, &mut repositories, p);
        bench_lazy_stable::<200>(&mut group, &mut repositories, p);
        bench_lazy_stable::<400>(&mut group, &mut repositories, p);
        bench_lazy_stable_simple(&mut group, &mut repositories, p);
        bench_lazy_stable_hybrid::<100>(&mut group, &mut repositories, p);
        bench_lazy_stable_hybrid::<200>(&mut group, &mut repositories, p);
        bench_lazy_stable_hybrid::<400>(&mut group, &mut repositories, p);
    }
    group.finish();

    let mut group = c.benchmark_group("ChangDistiller_BottomUp_runtime");

    for p in inputs.iter() {
        bench_cd::<100>(&mut group, &mut repositories, p);
        bench_cd::<200>(&mut group, &mut repositories, p);
        bench_cd::<400>(&mut group, &mut repositories, p);
        bench_lazy_cd::<100>(&mut group, &mut repositories, p);
        bench_lazy_cd::<200>(&mut group, &mut repositories, p);
        bench_lazy_cd::<400>(&mut group, &mut repositories, p);
    }
    group.finish();
}

fn bench_xy(
    group: &mut criterion::BenchmarkGroup<'_, impl Measurement>,
    repositories: &mut PreProcessedRepositories,
    p: &Input,
) {
    use hyper_diff::matchers::heuristic::xy_bottom_up_matcher;
    prep_gt_subtree_and_bench(
        group,
        repositories,
        p,
        BenchmarkId::new("Xy", p.repo.name()),
        |b, (repositories, (owned, mappings))| {
            let hyperast = &repositories.processor.main_stores;
            b.iter_batched(
                || Mapper::prep(hyperast, mappings.clone(), owned.clone()),
                |mapper| {
                    let mapper = mapper.map(CDS::from, CDS::from);
                    use xy_bottom_up_matcher::XYBottomUpMatcher;
                    let mapper_bottom_up = XYBottomUpMatcher::<_>::match_it(mapper);
                    black_box(mapper_bottom_up);
                },
                BatchSize::SmallInput,
            );
        },
    );
}

fn bench_lazy_xy(
    group: &mut criterion::BenchmarkGroup<'_, impl Measurement>,
    repositories: &mut PreProcessedRepositories,
    p: &Input,
) {
    use hyper_diff::matchers::heuristic::lazy_xy_bottom_up_matcher;
    prep_gt_subtree_and_bench(
        group,
        repositories,
        p,
        BenchmarkId::new(format!("LazyXY"), p.repo.name()),
        |b, (repositories, (owned, mappings))| {
            let hyperast = &repositories.processor.main_stores;
            b.iter_batched(
                || Mapper::prep(hyperast, mappings.clone(), owned.clone()),
                |mut mapper| {
                    let mapper = mapper.mut_decompressible();
                    use lazy_xy_bottom_up_matcher::LazyXYBottomUpMatcher;
                    let mapper_bottom_up = LazyXYBottomUpMatcher::<_>::match_it(mapper);
                    black_box(mapper_bottom_up);
                },
                BatchSize::SmallInput,
            );
        },
    );
}

fn bench_lazy_greedy<const MAX_SIZE: usize>(
    group: &mut criterion::BenchmarkGroup<'_, impl Measurement>,
    repositories: &mut PreProcessedRepositories,
    p: &Input,
) {
    use hyper_diff::matchers::heuristic::gt;
    prep_gt_subtree_and_bench(
        group,
        repositories,
        p,
        BenchmarkId::new(format!("LazyGreedy_{}", MAX_SIZE), p.repo.name()),
        |b, (repositories, (owned, mappings))| {
            let hyperast = &repositories.processor.main_stores;
            b.iter_batched(
                || Mapper::prep(hyperast, mappings.clone(), owned.clone()),
                |mut mapper| {
                    let mapper = mapper.mut_decompressible();
                    use gt::lazy_greedy_bottom_up_matcher::LazyGreedyBottomUpMatcher;
                    let mapper_bottom_up =
                        LazyGreedyBottomUpMatcher::<_, M, MAX_SIZE>::match_it(mapper);
                    black_box(mapper_bottom_up);
                },
                BatchSize::SmallInput,
            );
        },
    );
}

fn bench_greedy<const MAX_SIZE: usize>(
    group: &mut criterion::BenchmarkGroup<'_, impl Measurement>,
    repositories: &mut PreProcessedRepositories,
    p: &Input,
) {
    use hyper_diff::matchers::heuristic::gt;
    prep_gt_subtree_and_bench(
        group,
        repositories,
        p,
        BenchmarkId::new(format!("Greedy_{}", MAX_SIZE), p.repo.name()),
        |b, (repositories, (owned, mappings))| {
            let hyperast = &repositories.processor.main_stores;
            b.iter_batched(
                || Mapper::prep(hyperast, mappings.clone(), owned.clone()),
                |mapper| {
                    let mapper = mapper.map(CDS::from, CDS::from);
                    use gt::greedy_bottom_up_matcher::GreedyBottomUpMatcher;
                    let mapper_bottom_up = GreedyBottomUpMatcher::<_, MAX_SIZE>::match_it(mapper);
                    black_box(mapper_bottom_up);
                },
                BatchSize::SmallInput,
            );
        },
    );
}

fn bench_hybrid<const MAX_SIZE: usize>(
    group: &mut criterion::BenchmarkGroup<'_, impl Measurement>,
    repositories: &mut PreProcessedRepositories,
    p: &Input,
) {
    use hyper_diff::matchers::heuristic::gt;
    prep_gt_subtree_and_bench(
        group,
        repositories,
        p,
        BenchmarkId::new(format!("Hybrid_{}", MAX_SIZE), p.repo.name()),
        |b, (repositories, (owned, mappings))| {
            let hyperast = &repositories.processor.main_stores;
            b.iter_batched(
                || Mapper::prep(hyperast, mappings.clone(), owned.clone()),
                |mapper| {
                    let mapper = mapper.map(CDS::from, CDS::from);
                    use gt::hybrid_bottom_up_matcher::HybridBottomUpMatcher;
                    let mapper_bottom_up =
                        HybridBottomUpMatcher::<_, M, MAX_SIZE>::match_it(mapper);
                    black_box(mapper_bottom_up);
                },
                BatchSize::SmallInput,
            );
        },
    );
}

fn bench_lazy_hybrid<const MAX_SIZE: usize>(
    group: &mut criterion::BenchmarkGroup<'_, impl Measurement>,
    repositories: &mut PreProcessedRepositories,
    p: &Input,
) {
    use hyper_diff::matchers::heuristic::gt;
    prep_gt_subtree_and_bench(
        group,
        repositories,
        p,
        BenchmarkId::new(format!("LazyHybrid_{}", MAX_SIZE), p.repo.name()),
        |b, (repositories, (owned, mappings))| {
            let hyperast = &repositories.processor.main_stores;
            b.iter_batched(
                || Mapper::prep(hyperast, mappings.clone(), owned.clone()),
                |mut mapper| {
                    let mapper = mapper.mut_decompressible();
                    use gt::lazy_hybrid_bottom_up_matcher::LazyHybridBottomUpMatcher;
                    let mapper_bottom_up =
                        LazyHybridBottomUpMatcher::<_, M, MAX_SIZE>::match_it(mapper);
                    black_box(mapper_bottom_up);
                },
                BatchSize::SmallInput,
            );
        },
    );
}

fn bench_stable<const MAX_SIZE: usize>(
    group: &mut criterion::BenchmarkGroup<'_, impl Measurement>,
    repositories: &mut PreProcessedRepositories,
    p: &Input,
) {
    use hyper_diff::matchers::heuristic::gt;
    prep_gt_subtree_and_bench(
        group,
        repositories,
        p,
        BenchmarkId::new(format!("Stable_{}", MAX_SIZE), p.repo.name()),
        |b, (repositories, (owned, mappings))| {
            let hyperast = &repositories.processor.main_stores;
            b.iter_batched(
                || Mapper::prep(hyperast, mappings.clone(), owned.clone()),
                |mapper| {
                    let mapper = mapper.map(CDS::from, CDS::from);
                    use gt::marriage_bottom_up_matcher::MarriageBottomUpMatcher;
                    let mapper_bottom_up =
                        MarriageBottomUpMatcher::<_, M, MAX_SIZE>::match_it(mapper);
                    black_box(mapper_bottom_up);
                },
                BatchSize::SmallInput,
            );
        },
    );
}

fn bench_lazy_stable_simple(
    group: &mut criterion::BenchmarkGroup<'_, impl Measurement>,
    repositories: &mut PreProcessedRepositories,
    p: &Input,
) {
    use hyper_diff::matchers::heuristic::gt;
    prep_gt_subtree_and_bench(
        group,
        repositories,
        p,
        BenchmarkId::new("LazyStableSimple", p.repo.name()),
        |b, (repositories, (owned, mappings))| {
            let hyperast = &repositories.processor.main_stores;
            b.iter_batched(
                || Mapper::prep(hyperast, mappings.clone(), owned.clone()),
                |mut mapper| {
                    let mapper = mapper.mut_decompressible();
                    use gt::lazy_simple_marriage_bottom_up_matcher::LazySimpleMarriageBottomUpMatcher;
                    let mapper_bottom_up =
                        LazySimpleMarriageBottomUpMatcher::<_>::match_it(mapper);
                    black_box(mapper_bottom_up);
                },
                BatchSize::SmallInput,
            );
        },
    );
}

fn bench_lazy_stable_hybrid<const MAX_SIZE: usize>(
    group: &mut criterion::BenchmarkGroup<'_, impl Measurement>,
    repositories: &mut PreProcessedRepositories,
    p: &Input,
) {
    use hyper_diff::matchers::heuristic::gt;
    prep_gt_subtree_and_bench(
        group,
        repositories,
        p,
        BenchmarkId::new(format!("LazyStableHybrid_{}", MAX_SIZE), p.repo.name()),
        |b, (repositories, (owned, mappings))| {
            let hyperast = &repositories.processor.main_stores;
            b.iter_batched(
                || Mapper::prep(hyperast, mappings.clone(), owned.clone()),
                |mut mapper| {
                    let mapper = mapper.mut_decompressible();
                    use gt::lazy_hybrid_marriage_bottom_up_matcher::LazyHybridMarriageBottomUpMatcher;
                    let mapper_bottom_up =
                        LazyHybridMarriageBottomUpMatcher::<_, M, MAX_SIZE>::match_it(mapper);
                    black_box(mapper_bottom_up);
                },
                BatchSize::SmallInput,
            );
        },
    );
}

fn bench_lazy_stable<const MAX_SIZE: usize>(
    group: &mut criterion::BenchmarkGroup<'_, impl Measurement>,
    repositories: &mut PreProcessedRepositories,
    p: &Input,
) {
    use hyper_diff::matchers::heuristic::gt;
    prep_gt_subtree_and_bench(
        group,
        repositories,
        p,
        BenchmarkId::new(format!("LazyStable_{}", MAX_SIZE), p.repo.name()),
        |b, (repositories, (owned, mappings))| {
            let hyperast = &repositories.processor.main_stores;
            b.iter_batched(
                || Mapper::prep(hyperast, mappings.clone(), owned.clone()),
                |mut mapper| {
                    let mapper = mapper.mut_decompressible();
                    use gt::lazy_marriage_bottom_up_matcher::LazyMarriageBottomUpMatcher;
                    let mapper_bottom_up =
                        LazyMarriageBottomUpMatcher::<_, M, MAX_SIZE>::match_it(mapper);
                    black_box(mapper_bottom_up);
                },
                BatchSize::SmallInput,
            );
        },
    );
}

fn bench_simple(
    group: &mut criterion::BenchmarkGroup<'_, impl Measurement>,
    repositories: &mut PreProcessedRepositories,
    p: &Input,
) {
    use hyper_diff::matchers::heuristic::gt;
    prep_gt_subtree_and_bench(
        group,
        repositories,
        p,
        BenchmarkId::new("Simple", p.repo.name()),
        |b, (repositories, (owned, mappings))| {
            let hyperast = &repositories.processor.main_stores;
            b.iter_batched(
                || Mapper::prep(hyperast, mappings.clone(), owned.clone()),
                |mapper| {
                    let mapper = mapper.map(CDS::from, CDS::from);
                    use gt::simple_bottom_up_matcher::SimpleBottomUpMatcher;
                    let mapper_bottom_up = SimpleBottomUpMatcher::<_>::match_it(mapper);
                    black_box(mapper_bottom_up);
                },
                BatchSize::SmallInput,
            );
        },
    );
}

fn bench_lazy_simple(
    group: &mut criterion::BenchmarkGroup<'_, impl Measurement>,
    repositories: &mut PreProcessedRepositories,
    p: &Input,
) {
    use hyper_diff::matchers::heuristic::gt;
    prep_gt_subtree_and_bench(
        group,
        repositories,
        p,
        BenchmarkId::new("LazySimple", p.repo.name()),
        |b, (repositories, (owned, mappings))| {
            let hyperast = &repositories.processor.main_stores;
            b.iter_batched(
                || Mapper::prep(hyperast, mappings.clone(), owned.clone()),
                |mut mapper| {
                    let mapper = mapper.mut_decompressible();
                    use gt::lazy_simple_bottom_up_matcher::LazySimpleBottomUpMatcher;
                    let mapper_bottom_up = LazySimpleBottomUpMatcher::<_>::match_it(mapper);
                    black_box(mapper_bottom_up);
                },
                BatchSize::SmallInput,
            );
        },
    );
}

fn bench_cd<const MAX_SIZE: usize>(
    group: &mut criterion::BenchmarkGroup<'_, impl Measurement>,
    repositories: &mut PreProcessedRepositories,
    p: &Input,
) {
    use hyper_diff::matchers::heuristic::cd;
    prep_cd_subtree_and_bench(
        group,
        repositories,
        p,
        BenchmarkId::new(format!("Baseline {}", MAX_SIZE), p.repo.name()),
        |b, (repositories, (owned, mappings))| {
            let hyperast = &repositories.processor.main_stores;
            b.iter_batched(
                || Mapper::prep(hyperast, mappings.clone(), owned.clone()),
                |mapper| {
                    dbg!(mapper.mappings.len());
                    let mapper = mapper.map(CDS::from, CDS::from);
                    use cd::bottom_up_matcher::BottomUpMatcher;
                    let mapper = BottomUpMatcher::<_, MAX_SIZE>::match_it(mapper);
                    dbg!(mapper.mappings.len(), mapper.mappings.capacity());
                    black_box(mapper);
                },
                BatchSize::SmallInput,
            );
        },
    );
}

fn bench_lazy_cd<const MAX_SIZE: usize>(
    group: &mut criterion::BenchmarkGroup<'_, impl Measurement>,
    repositories: &mut PreProcessedRepositories,
    p: &Input,
) {
    use hyper_diff::matchers::heuristic::cd;
    prep_cd_subtree_and_bench(
        group,
        repositories,
        p,
        BenchmarkId::new(format!("Lazy {}", MAX_SIZE), p.repo.name()),
        |b, (repositories, (owned, mappings))| {
            let hyperast = &repositories.processor.main_stores;
            b.iter_batched(
                || Mapper::prep(hyperast, mappings.clone(), owned.clone()),
                |mut mapper| {
                    dbg!(mapper.mappings.len());
                    let mapper = mapper.mut_decompressible();
                    use cd::lazy_bottom_up_matcher::BottomUpMatcher;
                    let mapper = BottomUpMatcher::<_, MAX_SIZE>::match_it(mapper);
                    dbg!(mapper.mappings.len(), mapper.mappings.capacity());
                    black_box(mapper);
                },
                BatchSize::SmallInput,
            );
        },
    );
}

type OwnedLazyMapping = (
    (
        LazyPostOrder<NodeIdentifier, u32>,
        LazyPostOrder<NodeIdentifier, u32>,
    ),
    VecStore<u32>,
);

fn prep_gt_subtree_and_bench<Mea: Measurement>(
    group: &mut criterion::BenchmarkGroup<'_, Mea>,
    repositories: &mut PreProcessedRepositories,
    p: &Input,
    bid: BenchmarkId,
    f: impl FnMut(&mut criterion::Bencher<'_, Mea>, &(&PreProcessedRepositories, &OwnedLazyMapping)),
) {
    group.bench_with_input_prepared(
        bid,
        repositories,
        |group, repositories| {
            let (src, dst) = prep_commits(p, repositories);
            // let hyperast = hyperast_vcs_git::no_space::as_nospaces2(&repositories.processor.main_stores);
            let hyperast = &repositories.processor.main_stores;
            group.throughput(Throughput::Elements(
                (hyperast.node_store().resolve(src).size()
                    + hyperast.node_store().resolve(dst).size())
                .div_ceil(2) as u64,
            ));
            let mut mapper_owned: (DS<_>, DS<_>) = hyperast.decompress_pair(&src, &dst).1;
            let mapper = Mapper::with_mut_decompressible(&mut mapper_owned, M::default());
            use hyper_diff::matchers::heuristic::gt::lazy_greedy_subtree_matcher::LazyGreedySubtreeMatcher;
            let mapper = LazyGreedySubtreeMatcher::<_>::match_it::<MM>(mapper);
            let mappings = mapper.mapping.mappings.clone();
            ((mapper_owned.0.decomp, mapper_owned.1.decomp), mappings)
        },
        f
    );
}

fn prep_cd_subtree_and_bench<Mea: Measurement>(
    group: &mut criterion::BenchmarkGroup<'_, Mea>,
    repositories: &mut PreProcessedRepositories,
    p: &Input,
    bid: BenchmarkId,
    f: impl FnMut(&mut criterion::Bencher<'_, Mea>, &(&PreProcessedRepositories, &OwnedLazyMapping)),
) {
    group.bench_with_input_prepared(
        bid,
        repositories,
        |group, repositories| {
            let (src, dst) = prep_commits(p, repositories);
            let hyperast = &repositories.processor.main_stores;
            group.throughput(Throughput::Elements(
                (hyperast.node_store().resolve(src).size()
                    + hyperast.node_store().resolve(dst).size())
                .div_ceil(2) as u64,
            ));
            let mut mapper_owned: (DS<_>, DS<_>) = hyperast.decompress_pair(&src, &dst).1;
            let mapper = Mapper::with_mut_decompressible(&mut mapper_owned, M::default());

            use cd::lazy_leaves_matcher::LazyLeavesMatcher;
            use hyper_diff::matchers::heuristic::cd;
            dbg!();
            let mapper = LazyLeavesMatcher::<_>::match_it(mapper);
            dbg!();
            let mappings = mapper.mapping.mappings;
            ((mapper_owned.0.decomp, mapper_owned.1.decomp), mappings)
        },
        f,
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
    name = bottomup;
    config = Criterion::default()
        .sample_size(10)
        .measurement_time(std::time::Duration::from_secs(10))
        .configure_from_args();
    targets = bottomup_group
);
criterion_main!(bottomup);
