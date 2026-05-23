use criterion::measurement::Measurement;
use criterion::{BenchmarkId, Criterion, Throughput};
use criterion::{criterion_group, criterion_main};
use std::hint::black_box;

use hyper_diff::decompressed_tree_store::{CompletePostOrder, lazy_post_order::LazyPostOrder};
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

fn mapping_group(c: &mut Criterion) {
    let mut group = c.benchmark_group("Mapping_runtime");

    let inputs: &[Input] = &[
        // Input {
        //     repo: hyperast_vcs_git::git::Forge::Github.repo("chromium", "chromium"),
        //     commit: "f461f9752e5918c5c87f2e3767bcb24945ee0fa0",
        //     config: hyperast_vcs_git::processing::RepoConfig::CppMake,
        //     fetch: false,
        // },
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
            repo: hyperast_vcs_git::git::Forge::Github.repo("javaparser", "javaparser"),
            commit: "046bf8be251189452ad6b25bf9107a1a2167ce6f",
            config: hyperast_vcs_git::processing::RepoConfig::JavaMaven,
            fetch: true,
        },
        Input {
            repo: hyperast_vcs_git::git::Forge::Github.repo("apache", "dubbo"),
            commit: "e831b464837ae5d2afac9841559420aeaef6c52b",
            config: hyperast_vcs_git::processing::RepoConfig::JavaMaven,
            fetch: true,
        },
        Input {
            repo: hyperast_vcs_git::git::Forge::Github.repo("netty", "netty"),
            commit: "c2b846750dd2131d65aa25c8cf66bf3649b248f9",
            config: hyperast_vcs_git::processing::RepoConfig::JavaMaven,
            fetch: true,
        },
        Input {
            repo: hyperast_vcs_git::git::Forge::Github.repo("apache", "hadoop"),
            commit: "d5e97fe4d6baf43a5576cbd1700c22b788dba01e",
            config: hyperast_vcs_git::processing::RepoConfig::JavaMaven,
            fetch: true,
        },
    ];
    let mut repositories = PreProcessedRepositories::default();
    for p in inputs.iter() {
        repositories.register_config(p.repo.clone(), p.config);
    }
    for p in inputs.iter() {
        no_size_threshold(&mut group, &mut repositories, p);
        no_sim_threshold::<200>(&mut group, &mut repositories, p);
        no_size_threshold_with_sim_threshold::<1, 2>(&mut group, &mut repositories, p);
        with_sim_threshold::<200, 1, 2>(&mut group, &mut repositories, p);
        with_second_sim_threshold::<200, 4, 10, 6, 10>(&mut group, &mut repositories, p);
    }
    group.finish()
}

fn no_size_threshold(
    group: &mut criterion::BenchmarkGroup<'_, impl Measurement>,
    repositories: &mut PreProcessedRepositories,
    p: &Input,
) {
    use hyper_diff::matchers::heuristic::gt;
    prep_and_bench(
        group,
        repositories,
        p,
        BenchmarkId::new("Xy", p.repo.name()),
        |b, (repositories, (src, dst))| {
            b.iter(|| {
                // let hyperast = hyperast_vcs_git::no_space::as_nospaces2(&repositories.processor.main_stores);
                let hyperast = &repositories.processor.main_stores;
                let mapper_owned: (CDS<_>, CDS<_>) = hyperast.decompress_pair(src, dst).1;
                let mapper = Mapper::new(hyperast, VecStore::default(), mapper_owned);

                use gt::greedy_subtree_matcher::GreedySubtreeMatcher;
                let mapper = GreedySubtreeMatcher::<_>::match_it::<MM>(mapper);
                use hyper_diff::matchers::heuristic::xy_bottom_up_matcher::XYBottomUpMatcher;
                let mapper_bottom_up = XYBottomUpMatcher::<_>::match_it(mapper);
                black_box(mapper_bottom_up);
            });
        },
    );
    prep_and_bench(
        group,
        repositories,
        p,
        BenchmarkId::new("PartialLazyXy", p.repo.name()),
        |b, (repositories, (src, dst))| {
            b.iter(|| {
                let hyperast = &repositories.processor.main_stores;
                let mut mapper_owned: (DS<_>, DS<_>) = hyperast.decompress_pair(src, dst).1;
                let mapper = Mapper::with_mut_decompressible(&mut mapper_owned, M::default());

                use gt::lazy_greedy_subtree_matcher::LazyGreedySubtreeMatcher;
                let mapper = LazyGreedySubtreeMatcher::<_>::match_it::<MM>(mapper);
                let mappings = mapper.mapping.mappings;
                let mapper = Mapper::new(hyperast, mappings, mapper_owned);
                let mapper = mapper.map(CDS::from, CDS::from);
                use hyper_diff::matchers::heuristic::xy_bottom_up_matcher::XYBottomUpMatcher;
                let mapper_bottom_up = XYBottomUpMatcher::<_>::match_it(mapper);
                black_box(mapper_bottom_up);
            });
        },
    );
    prep_and_bench(
        group,
        repositories,
        p,
        BenchmarkId::new("LazyXy", p.repo.name()),
        |b, (repositories, (src, dst))| {
            b.iter(|| {
                let hyperast = &repositories.processor.main_stores;
                let mut mapper_owned: (DS<_>, DS<_>) = hyperast.decompress_pair(src, dst).1;
                let mapper = Mapper::with_mut_decompressible(
                    &mut mapper_owned,
                    M::default(),
                );

                use gt::lazy_greedy_subtree_matcher::LazyGreedySubtreeMatcher;
                let mapper = LazyGreedySubtreeMatcher::<_>::match_it::<MM>(mapper);
                use hyper_diff::matchers::heuristic::lazy_xy_bottom_up_matcher::LazyXYBottomUpMatcher;
                let mapper_bottom_up = LazyXYBottomUpMatcher::<_>::match_it(mapper);
                black_box(mapper_bottom_up);
            });
        },
    );
}

fn no_sim_threshold<const MAX_SIZE: usize>(
    group: &mut criterion::BenchmarkGroup<'_, impl Measurement>,
    repositories: &mut PreProcessedRepositories,
    p: &Input,
) {
    use hyper_diff::matchers::heuristic::gt;

    prep_and_bench(
        group,
        repositories,
        p,
        BenchmarkId::new(
            format!("PartialLazyHybridGumtree {}", MAX_SIZE),
            p.repo.name(),
        ),
        |b, (repositories, (src, dst))| {
            b.iter(|| {
                let hyperast = &repositories.processor.main_stores;
                let mut mapper_owned: (DS<_>, DS<_>) = hyperast.decompress_pair(src, dst).1;
                let mapper = Mapper::with_mut_decompressible(&mut mapper_owned, M::default());

                use gt::lazy_greedy_subtree_matcher::LazyGreedySubtreeMatcher;
                let mapper = LazyGreedySubtreeMatcher::<_>::match_it::<MM>(mapper);
                let mappings = mapper.mapping.mappings;
                let mapper = Mapper::new(hyperast, mappings, mapper_owned);
                let mapper = mapper.map(CDS::from, CDS::from);
                use gt::hybrid_bottom_up_matcher::HybridBottomUpMatcher;
                let mapper_bottom_up = HybridBottomUpMatcher::<_, M, MAX_SIZE>::match_it(mapper);
                black_box(mapper_bottom_up);
            });
        },
    );
    prep_and_bench(
        group,
        repositories,
        p,
        BenchmarkId::new(format!("LazyHybridGumtree {}", MAX_SIZE), p.repo.name()),
        |b, (repositories, (src, dst))| {
            b.iter(|| {
                let hyperast = &repositories.processor.main_stores;
                let mut mapper_owned: (DS<_>, DS<_>) = hyperast.decompress_pair(src, dst).1;
                let mapper = Mapper::with_mut_decompressible(&mut mapper_owned, M::default());

                use gt::lazy_greedy_subtree_matcher::LazyGreedySubtreeMatcher;
                let mapper = LazyGreedySubtreeMatcher::<_>::match_it::<MM>(mapper);

                use gt::lazy_hybrid_bottom_up_matcher::LazyHybridBottomUpMatcher;
                let mapper_bottom_up =
                    LazyHybridBottomUpMatcher::<_, M, MAX_SIZE>::match_it(mapper);
                black_box(mapper_bottom_up);
            });
        },
    );
}

fn no_size_threshold_with_sim_threshold<const NUM: u64, const DEN: u64>(
    group: &mut criterion::BenchmarkGroup<'_, impl Measurement>,
    repositories: &mut PreProcessedRepositories,
    p: &Input,
) {
    use hyper_diff::matchers::heuristic::gt;
    prep_and_bench(
        group,
        repositories,
        p,
        BenchmarkId::new(
            format!("PartialLazySimpleGumtree {}/{}", NUM, DEN),
            p.repo.name(),
        ),
        |b, (repositories, (src, dst))| {
            b.iter(|| {
                let hyperast = &repositories.processor.main_stores;
                let mut mapper_owned: (DS<_>, DS<_>) = hyperast.decompress_pair(src, dst).1;
                let mapper = Mapper::with_mut_decompressible(&mut mapper_owned, M::default());

                use gt::lazy_greedy_subtree_matcher::LazyGreedySubtreeMatcher;
                let mapper = LazyGreedySubtreeMatcher::<_>::match_it::<MM>(mapper);
                let mappings = mapper.mapping.mappings;
                let mapper = Mapper::new(hyperast, mappings, mapper_owned);
                let mapper = mapper.map(CDS::from, CDS::from);
                use gt::simple_bottom_up_matcher::SimpleBottomUpMatcher;
                let mapper_bottom_up = SimpleBottomUpMatcher::<_, NUM, DEN>::match_it(mapper);
                black_box(mapper_bottom_up);
            });
        },
    );
    prep_and_bench(
        group,
        repositories,
        p,
        BenchmarkId::new(format!("LazySimpleGumtree {}/{}", NUM, DEN), p.repo.name()),
        |b, (repositories, (src, dst))| {
            b.iter(|| {
                let hyperast = &repositories.processor.main_stores;
                let mut mapper_owned: (DS<_>, DS<_>) = hyperast.decompress_pair(src, dst).1;
                let mapper = Mapper::with_mut_decompressible(&mut mapper_owned, M::default());

                use gt::lazy_greedy_subtree_matcher::LazyGreedySubtreeMatcher;
                let mapper = LazyGreedySubtreeMatcher::<_>::match_it::<MM>(mapper);

                use gt::lazy_simple_bottom_up_matcher::LazySimpleBottomUpMatcher;
                let mapper_bottom_up = LazySimpleBottomUpMatcher::<_, NUM, DEN>::match_it(mapper);
                black_box(mapper_bottom_up);
            });
        },
    );
}

fn with_sim_threshold<const MAX_SIZE: usize, const NUM: u64, const DEN: u64>(
    group: &mut criterion::BenchmarkGroup<'_, impl Measurement>,
    repositories: &mut PreProcessedRepositories,
    p: &Input,
) {
    use hyper_diff::matchers::heuristic::gt;

    prep_and_bench(
        group,
        repositories,
        p,
        BenchmarkId::new(
            format!("GreedyGumtree {} {}/{}", MAX_SIZE, NUM, DEN),
            p.repo.name(),
        ),
        |b, (repositories, (src, dst))| {
            b.iter(|| {
                // let hyperast = hyperast_vcs_git::no_space::as_nospaces2(&repositories.processor.main_stores);
                let hyperast = &repositories.processor.main_stores;
                let mapper_owned: (CDS<_>, CDS<_>) = hyperast.decompress_pair(src, dst).1;
                let mapper = Mapper::new(hyperast, VecStore::default(), mapper_owned);

                use gt::greedy_subtree_matcher::GreedySubtreeMatcher;
                let mapper = GreedySubtreeMatcher::<_>::match_it::<MM>(mapper);
                use gt::greedy_bottom_up_matcher::GreedyBottomUpMatcher;
                let mapper_bottom_up =
                    GreedyBottomUpMatcher::<_, MAX_SIZE, NUM, DEN>::match_it(mapper);
                black_box(mapper_bottom_up);
            });
        },
    );
    prep_and_bench(
        group,
        repositories,
        p,
        BenchmarkId::new(
            format!("PartialLazyGreedyGumtree {} {}/{}", MAX_SIZE, NUM, DEN),
            p.repo.name(),
        ),
        |b, (repositories, (src, dst))| {
            b.iter(|| {
                let hyperast = &repositories.processor.main_stores;
                let mut mapper_owned: (DS<_>, DS<_>) = hyperast.decompress_pair(src, dst).1;
                let mapper = Mapper::with_mut_decompressible(&mut mapper_owned, M::default());

                use gt::lazy_greedy_subtree_matcher::LazyGreedySubtreeMatcher;
                let mapper = LazyGreedySubtreeMatcher::<_>::match_it::<MM>(mapper);
                let mappings = mapper.mapping.mappings;
                let mapper = Mapper::new(hyperast, mappings, mapper_owned);
                let mapper = mapper.map(CDS::from, CDS::from);
                use gt::greedy_bottom_up_matcher::GreedyBottomUpMatcher;
                let mapper_bottom_up =
                    GreedyBottomUpMatcher::<_, MAX_SIZE, NUM, DEN>::match_it(mapper);
                black_box(mapper_bottom_up);
            });
        },
    );
    prep_and_bench(
        group,
        repositories,
        p,
        BenchmarkId::new(
            format!("LazyGreedyGumtree {} {}/{}", MAX_SIZE, NUM, DEN),
            p.repo.name(),
        ),
        |b, (repositories, (src, dst))| {
            b.iter(|| {
                let hyperast = &repositories.processor.main_stores;
                let mut mapper_owned: (DS<_>, DS<_>) = hyperast.decompress_pair(src, dst).1;
                let mapper = Mapper::with_mut_decompressible(&mut mapper_owned, M::default());

                use gt::lazy_greedy_subtree_matcher::LazyGreedySubtreeMatcher;
                let mapper = LazyGreedySubtreeMatcher::<_>::match_it::<MM>(mapper);

                use gt::lazy_greedy_bottom_up_matcher::LazyGreedyBottomUpMatcher;
                let mapper_bottom_up =
                    LazyGreedyBottomUpMatcher::<_, M, MAX_SIZE, NUM, DEN>::match_it(mapper);
                black_box(mapper_bottom_up);
            });
        },
    );
    prep_and_bench(
        group,
        repositories,
        p,
        BenchmarkId::new(
            format!("PartialLazyStableGumtree {} {}/{}", MAX_SIZE, NUM, DEN),
            p.repo.name(),
        ),
        |b, (repositories, (src, dst))| {
            b.iter(|| {
                let hyperast = &repositories.processor.main_stores;
                let mut mapper_owned: (DS<_>, DS<_>) = hyperast.decompress_pair(src, dst).1;
                let mapper = Mapper::with_mut_decompressible(&mut mapper_owned, M::default());

                use gt::lazy_greedy_subtree_matcher::LazyGreedySubtreeMatcher;
                let mapper = LazyGreedySubtreeMatcher::<_>::match_it::<MM>(mapper);
                let mappings = mapper.mapping.mappings;
                let mapper = Mapper::new(hyperast, mappings, mapper_owned);
                let mapper = mapper.map(CDS::from, CDS::from);
                use gt::marriage_bottom_up_matcher::MarriageBottomUpMatcher;
                let mapper_bottom_up = MarriageBottomUpMatcher::<_, M, 200>::match_it(mapper);
                black_box(mapper_bottom_up);
            });
        },
    );
    prep_and_bench(
        group,
        repositories,
        p,
        BenchmarkId::new(
            format!("LazyStableGumtree {} {}/{}", MAX_SIZE, NUM, DEN),
            p.repo.name(),
        ),
        |b, (repositories, (src, dst))| {
            b.iter(|| {
                let hyperast = &repositories.processor.main_stores;
                let mut mapper_owned: (DS<_>, DS<_>) = hyperast.decompress_pair(src, dst).1;
                let mapper = Mapper::with_mut_decompressible(&mut mapper_owned, M::default());

                use gt::lazy_greedy_subtree_matcher::LazyGreedySubtreeMatcher;
                let mapper = LazyGreedySubtreeMatcher::<_>::match_it::<MM>(mapper);

                use gt::lazy_marriage_bottom_up_matcher::LazyMarriageBottomUpMatcher;
                let mapper_bottom_up = LazyMarriageBottomUpMatcher::<_, M, 200>::match_it(mapper);
                black_box(mapper_bottom_up);
            });
        },
    );
}

fn with_second_sim_threshold<
    const MAX_SIZE: usize,
    const NUM: u64,
    const DEN: u64,
    const NUM2: u64,
    const DEN2: u64,
>(
    group: &mut criterion::BenchmarkGroup<'_, impl Measurement>,
    repositories: &mut PreProcessedRepositories,
    p: &Input,
) {
    use hyper_diff::matchers::heuristic::cd;
    prep_and_bench(
        group,
        repositories,
        p,
        BenchmarkId::new(
            format!(
                "ChangeDistiller {} {}/{} {}/{}",
                MAX_SIZE, NUM, DEN, NUM2, DEN2
            ),
            p.repo.name(),
        ),
        |b, (repositories, (src, dst))| {
            b.iter(|| {
                let hyperast = &repositories.processor.main_stores;
                let mapper_owned: (CDS<_>, CDS<_>) = hyperast.decompress_pair(src, dst).1;
                let mapper = Mapper::new(hyperast, VecStore::default(), mapper_owned);

                use cd::leaves_matcher::LeavesMatcher;
                let mapper = LeavesMatcher::<_>::match_it(mapper);
                use cd::bottom_up_matcher::BottomUpMatcher;
                let mapper_bottom_up =
                    BottomUpMatcher::<_, MAX_SIZE, NUM, DEN, NUM2, DEN2>::match_it(mapper);
                black_box(mapper_bottom_up);
            });
        },
    );
    prep_and_bench(
        group,
        repositories,
        p,
        BenchmarkId::new(
            format!(
                "PartialLazyChangeDistiller {} {}/{} {}/{}",
                MAX_SIZE, NUM, DEN, NUM2, DEN2
            ),
            p.repo.name(),
        ),
        |b, (repositories, (src, dst))| {
            b.iter(|| {
                let hyperast = &repositories.processor.main_stores;
                let mut mapper_owned: (DS<_>, DS<_>) = hyperast.decompress_pair(src, dst).1;
                let mapper = Mapper::with_mut_decompressible(&mut mapper_owned, M::default());
                use cd::lazy_leaves_matcher::LazyLeavesMatcher;
                let mapper = LazyLeavesMatcher::<_>::match_it(mapper);
                let mappings = mapper.mapping.mappings;
                let mapper = Mapper::new(hyperast, mappings, mapper_owned);
                let mapper = mapper.map(CDS::from, CDS::from);
                use cd::bottom_up_matcher::BottomUpMatcher;
                let mapper_bottom_up =
                    BottomUpMatcher::<_, MAX_SIZE, NUM, DEN, NUM2, DEN2>::match_it(mapper);
                black_box(mapper_bottom_up);
            });
        },
    );
    prep_and_bench(
        group,
        repositories,
        p,
        BenchmarkId::new(
            format!(
                "LazyChangeDistiller {} {}/{} {}/{}",
                MAX_SIZE, NUM, DEN, NUM2, DEN2
            ),
            p.repo.name(),
        ),
        |b, (repositories, (src, dst))| {
            b.iter(|| {
                let hyperast = &repositories.processor.main_stores;
                let mut mapper_owned: (DS<_>, DS<_>) = hyperast.decompress_pair(src, dst).1;
                let mapper = Mapper::with_mut_decompressible(&mut mapper_owned, M::default());
                use cd::lazy_leaves_matcher::LazyLeavesMatcher;
                let mapper = LazyLeavesMatcher::<_>::match_it(mapper);

                use cd::lazy_bottom_up_matcher::BottomUpMatcher;
                let mapper_bottom_up =
                    BottomUpMatcher::<_, MAX_SIZE, NUM, DEN, NUM2, DEN2>::match_it(mapper);
                black_box(mapper_bottom_up);
            });
        },
    );
}

fn prep_and_bench<Mea: Measurement>(
    group: &mut criterion::BenchmarkGroup<'_, Mea>,
    repositories: &mut PreProcessedRepositories,
    p: &Input,
    bid: BenchmarkId,
    f: impl FnMut(
        &mut criterion::Bencher<'_, Mea>,
        &(&PreProcessedRepositories, &(NodeIdentifier, NodeIdentifier)),
    ),
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
            (src, dst)
        },
        f,
    );
}

#[cfg(target_os = "linux")]
criterion_group!(
    name = mapping;
    config = Criterion::default()
        .sample_size(10)
        .measurement_time(std::time::Duration::from_secs(10))
        .with_measurement(criterion_perf_events::Perf::new(
            perfcnt::linux::PerfCounterBuilderLinux::from_hardware_event(
                perfcnt::linux::HardwareEventType::Instructions
            )
        ))
        .configure_from_args();
    targets = mapping_group
);
#[cfg(not(target_os = "linux"))]
criterion_group!(
    name = mapping;
    config = Criterion::default()
        .sample_size(10)
        .measurement_time(std::time::Duration::from_secs(10))
        .configure_from_args();
    targets = mapping_group
);
criterion_main!(mapping);
