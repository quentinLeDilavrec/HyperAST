//! benchmarks for the whole matching process, including the subtree matcher

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

use hyperast_benchmark_diffs::{InputRepo, prep_commit_pair};

#[allow(type_alias_bounds)]
type DS<HAST: HyperASTShared> = Decompressible<HAST, LazyPostOrder<HAST::IdN, u32>>;
#[allow(type_alias_bounds)]
type CDS<HAST: HyperASTShared> = Decompressible<HAST, CompletePostOrder<HAST::IdN, u32>>;
type M = hyper_diff::mappings::VecStore<u32>;
type MM = hyper_diff::mappings::DefaultMultiMappingStore<u32>;

fn mapping_group(c: &mut Criterion) {
    let mut group = c.benchmark_group("Mapping_runtime");
    let inputs = hyperast_benchmark_diffs::REPOSITORIES;

    for p in inputs.iter() {
        let mut repositories = PreProcessedRepositories::default();
        repositories.register_config(p.gh(), p.config);
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
    p: &InputRepo,
) {
    use hyper_diff::matchers::heuristic::gt;
    prep_and_bench(
        group,
        repositories,
        p,
        BenchmarkId::new("Xy", p.name),
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
        BenchmarkId::new("PartialLazyXy", p.name),
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
        BenchmarkId::new("LazyXy", p.name),
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
    p: &InputRepo,
) {
    use hyper_diff::matchers::heuristic::gt;

    prep_and_bench(
        group,
        repositories,
        p,
        BenchmarkId::new(format!("PartialLazyHybridGumtree {}", MAX_SIZE), p.name),
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
        BenchmarkId::new(format!("LazyHybridGumtree {}", MAX_SIZE), p.name),
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
    p: &InputRepo,
) {
    use hyper_diff::matchers::heuristic::gt;
    prep_and_bench(
        group,
        repositories,
        p,
        BenchmarkId::new(format!("PartialLazySimpleGumtree {}/{}", NUM, DEN), p.name),
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
        BenchmarkId::new(format!("LazySimpleGumtree {}/{}", NUM, DEN), p.name),
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
    p: &InputRepo,
) {
    use hyper_diff::matchers::heuristic::gt;

    prep_and_bench(
        group,
        repositories,
        p,
        BenchmarkId::new(
            format!("GreedyGumtree {} {}/{}", MAX_SIZE, NUM, DEN),
            p.name,
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
                    GreedyBottomUpMatcher::<_, M, MAX_SIZE, NUM, DEN>::match_it(mapper);
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
            p.name,
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
                    GreedyBottomUpMatcher::<_, M, MAX_SIZE, NUM, DEN>::match_it(mapper);
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
            p.name,
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
            p.name,
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
            p.name,
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
    p: &InputRepo,
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
            p.name,
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
            p.name,
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
            p.name,
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
    p: &InputRepo,
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
            let (src, dst) = prep_commit_pair(p, repositories);
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
