use criterion::measurement::Measurement;
use criterion::{BatchSize, BenchmarkId, Criterion, Throughput};
use criterion::{criterion_group, criterion_main};

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
    let mut p = Bench {
        group: &mut group,
        repositories: &mut repositories,
        i: inputs.iter().next().unwrap(),
        name: None,
        prep: Prep::None,
    };
    for i in inputs.iter() {
        p.i = i;
        bench_xy(&mut p);
        bench_lazy_xy(&mut p);
        bench_greedy::<100>(&mut p);
        bench_greedy::<200>(&mut p);
        bench_greedy::<400>(&mut p);
        bench_lazy_greedy::<100>(&mut p);
        bench_lazy_greedy::<200>(&mut p);
        bench_lazy_greedy::<400>(&mut p);
        bench_simple(&mut p);
        bench_lazy_simple(&mut p);
        bench_hybrid::<100>(&mut p);
        bench_hybrid::<200>(&mut p);
        bench_hybrid::<400>(&mut p);
        bench_lazy_hybrid::<100>(&mut p);
        bench_lazy_hybrid::<200>(&mut p);
        bench_lazy_hybrid::<400>(&mut p);
        bench_stable::<100>(&mut p);
        bench_stable::<200>(&mut p);
        bench_stable::<400>(&mut p);
        bench_lazy_stable::<100>(&mut p);
        bench_lazy_stable::<200>(&mut p);
        bench_lazy_stable::<400>(&mut p);
        bench_lazy_stable_simple(&mut p);
        bench_lazy_stable_hybrid::<100>(&mut p);
        bench_lazy_stable_hybrid::<200>(&mut p);
        bench_lazy_stable_hybrid::<400>(&mut p);
    }
    group.finish();

    let mut group = c.benchmark_group("ChangDistiller_BottomUp_runtime");

    let mut p = Bench {
        group: &mut group,
        repositories: &mut repositories,
        i: inputs.iter().next().unwrap(),
        name: None,
        prep: Prep::None,
    };

    for i in inputs.iter() {
        p.i = i;
        bench_cd::<100>(&mut p);
        bench_cd::<200>(&mut p);
        bench_cd::<400>(&mut p);
        bench_lazy_cd::<100>(&mut p);
        bench_lazy_cd::<200>(&mut p);
        bench_lazy_cd::<400>(&mut p);
    }
    group.finish();
}

fn bench_xy(p: &mut impl Runner) {
    use hyper_diff::matchers::heuristic::xy_bottom_up_matcher;
    let name = format!("Xy");
    p.name(name).prep(Prep::GT).routine(|mapper| {
        let mapper = mapper.map(CDS::from, CDS::from);
        use xy_bottom_up_matcher::XYBottomUpMatcher;
        XYBottomUpMatcher::<_>::match_it(mapper)
    });
}

fn bench_lazy_xy(p: &mut impl Runner) {
    use hyper_diff::matchers::heuristic::lazy_xy_bottom_up_matcher;
    let name = format!("LazyXY");
    p.name(name).prep(Prep::GT).routine(|mut mapper| {
        let mpr = mapper.mut_decompressible();
        use lazy_xy_bottom_up_matcher::LazyXYBottomUpMatcher;
        let _mpr = LazyXYBottomUpMatcher::<_>::match_it(mpr);
        mapper
    });
}

fn bench_lazy_greedy<const MAX_SIZE: usize>(p: &mut impl Runner) {
    use hyper_diff::matchers::heuristic::gt;
    let name = format!("LazyGreedy_{}", MAX_SIZE);
    p.name(name).prep(Prep::GT).routine(|mut mapper| {
        let mpr = mapper.mut_decompressible();
        use gt::lazy_greedy_bottom_up_matcher::LazyGreedyBottomUpMatcher;
        let _mpr = LazyGreedyBottomUpMatcher::<_, M, MAX_SIZE>::match_it(mpr);
        mapper
    });
}

fn bench_greedy<const MAX_SIZE: usize>(p: &mut impl Runner) {
    use hyper_diff::matchers::heuristic::gt;
    let name = format!("Greedy_{}", MAX_SIZE);
    p.name(name).prep(Prep::GT).routine(|mapper| {
        let mapper = mapper.map(CDS::from, CDS::from);
        use gt::greedy_bottom_up_matcher::GreedyBottomUpMatcher;
        let mapper = GreedyBottomUpMatcher::<_, MAX_SIZE>::match_it(mapper);
        mapper
    });
}

fn bench_hybrid<const MAX_SIZE: usize>(p: &mut impl Runner) {
    use hyper_diff::matchers::heuristic::gt;
    let name = format!("Hybrid_{}", MAX_SIZE);
    p.name(name).prep(Prep::GT).routine(|mapper| {
        let mapper = mapper.map(CDS::from, CDS::from);
        use gt::hybrid_bottom_up_matcher::HybridBottomUpMatcher;
        let mapper = HybridBottomUpMatcher::<_, M, MAX_SIZE>::match_it(mapper);
        mapper
    });
}

fn bench_lazy_hybrid<const MAX_SIZE: usize>(p: &mut impl Runner) {
    use hyper_diff::matchers::heuristic::gt;
    let name = format!("LazyHybrid_{}", MAX_SIZE);
    p.name(name).prep(Prep::GT).routine(|mut mapper| {
        let mpr = mapper.mut_decompressible();
        use gt::lazy_hybrid_bottom_up_matcher::LazyHybridBottomUpMatcher;
        let _mpr = LazyHybridBottomUpMatcher::<_, M, MAX_SIZE>::match_it(mpr);
        mapper
    });
}

fn bench_stable<const MAX_SIZE: usize>(p: &mut impl Runner) {
    use hyper_diff::matchers::heuristic::gt;
    let name = format!("Stable_{}", MAX_SIZE);
    p.name(name).prep(Prep::GT).routine(|mapper| {
        let mapper = mapper.map(CDS::from, CDS::from);
        use gt::marriage_bottom_up_matcher::MarriageBottomUpMatcher;
        let mapper = MarriageBottomUpMatcher::<_, M, MAX_SIZE>::match_it(mapper);
        mapper
    });
}

fn bench_lazy_stable_simple(p: &mut impl Runner) {
    use hyper_diff::matchers::heuristic::gt;
    let name = format!("LazyStableSimple");
    p.name(name).prep(Prep::GT).routine(|mut mapper| {
        let mpr = mapper.mut_decompressible();
        use gt::lazy_simple_marriage_bottom_up_matcher::LazySimpleMarriageBottomUpMatcher;
        let _mpr = LazySimpleMarriageBottomUpMatcher::<_>::match_it(mpr);
        mapper
    });
}

fn bench_lazy_stable_hybrid<const MAX_SIZE: usize>(p: &mut impl Runner) {
    use hyper_diff::matchers::heuristic::gt;
    let name = format!("LazyStableHybrid_{}", MAX_SIZE);
    p.name(name).prep(Prep::GT).routine(|mut mapper| {
        let mpr = mapper.mut_decompressible();
        use gt::lazy_hybrid_marriage_bottom_up_matcher::LazyHybridMarriageBottomUpMatcher;
        let _mpr = LazyHybridMarriageBottomUpMatcher::<_, M, MAX_SIZE>::match_it(mpr);
        mapper
    });
}

fn bench_lazy_stable<const MAX_SIZE: usize>(p: &mut impl Runner) {
    use hyper_diff::matchers::heuristic::gt;
    let name = format!("LazyStable_{}", MAX_SIZE);
    p.name(name).prep(Prep::GT).routine(|mut mapper| {
        let mpr = mapper.mut_decompressible();
        use gt::lazy_marriage_bottom_up_matcher::LazyMarriageBottomUpMatcher;
        let _mpr = LazyMarriageBottomUpMatcher::<_, M, MAX_SIZE>::match_it(mpr);
        mapper
    });
}

fn bench_simple(p: &mut impl Runner) {
    use hyper_diff::matchers::heuristic::gt;
    let name = format!("Simple");
    p.name(name).prep(Prep::GT).routine(|mapper| {
        let mapper = mapper.map(CDS::from, CDS::from);
        use gt::simple_bottom_up_matcher::SimpleBottomUpMatcher;
        let mapper = SimpleBottomUpMatcher::<_>::match_it(mapper);
        mapper
    });
}

fn bench_lazy_simple(p: &mut impl Runner) {
    use hyper_diff::matchers::heuristic::gt;
    let name = format!("LazySimple");
    p.name(name).prep(Prep::GT).routine(|mut mapper| {
        let mpr = mapper.mut_decompressible();
        use gt::lazy_simple_bottom_up_matcher::LazySimpleBottomUpMatcher;
        let _mpr = LazySimpleBottomUpMatcher::<_>::match_it(mpr);
        mapper
    });
}

fn bench_cd<const MAX_SIZE: usize>(p: &mut impl Runner) {
    use hyper_diff::matchers::heuristic::cd;
    let name = format!("Baseline {}", MAX_SIZE);
    p.name(name).prep(Prep::CD).routine(|mapper| {
        let mapper = mapper.map(CDS::from, CDS::from);
        use cd::bottom_up_matcher::BottomUpMatcher;
        let mapper = BottomUpMatcher::<_, MAX_SIZE>::match_it(mapper);
        mapper
    });
}

fn bench_lazy_cd<const MAX_SIZE: usize>(p: &mut impl Runner) {
    use hyper_diff::matchers::heuristic::cd;
    let name = format!("Lazy {}", MAX_SIZE);
    p.name(name).prep(Prep::CD).routine(|mut mapper| {
        dbg!(mapper.mappings.len());
        let mpr = mapper.mut_decompressible();
        use cd::lazy_bottom_up_matcher::BottomUpMatcher;
        let mpr = BottomUpMatcher::<_, MAX_SIZE>::match_it(mpr);
        dbg!(mpr.mappings.len(), mpr.mappings.capacity());
        mapper
    });
}

trait Runner {
    fn name(&mut self, name: String) -> &mut Self;
    fn prep(&mut self, prep: Prep) -> &mut Self;
    type Output;
    fn routine<D1, D2>(
        &mut self,
        f: impl for<'a> FnMut(Mpr<&'a HAST>) -> Mpr<&'a HAST, D1, D2> + Clone,
    ) -> Self::Output;
}

enum Prep {
    None,
    GT,
    CD,
}

struct Bench<'a, G> {
    group: &'a mut G,
    repositories: &'a mut PreProcessedRepositories,
    i: &'a Input,
    name: Option<String>,
    prep: Prep,
}

impl<Mea: Measurement> Runner for Bench<'_, criterion::BenchmarkGroup<'_, Mea>> {
    fn name(&mut self, name: String) -> &mut Self {
        self.name = Some(name);
        self
    }

    fn prep(&mut self, prep: Prep) -> &mut Self {
        self.prep = prep;
        self
    }

    type Output = ();

    fn routine<D1, D2>(
        &mut self,
        f: impl for<'a> FnMut(Mpr<&'a HAST>) -> Mpr<&'a HAST, D1, D2> + Clone,
    ) -> Self::Output {
        match self.prep {
            Prep::None => todo!(),
            Prep::GT => prep_gt_subtree_and_bench_batched(self, f),
            Prep::CD => prep_cd_subtree_and_bench_batched(self, f),
        }
    }
}

type OwnedLazyMapping = (
    (
        LazyPostOrder<NodeIdentifier, u32>,
        LazyPostOrder<NodeIdentifier, u32>,
    ),
    VecStore<u32>,
);

/// Note: Inline if you want to control mapper preparation in the setup phase
fn prep_gt_subtree_and_bench_batched<Mea: Measurement, D1, D2>(
    p: &mut Bench<'_, criterion::BenchmarkGroup<'_, Mea>>,
    f: impl for<'a> FnMut(Mpr<&'a HAST>) -> Mpr<&'a HAST, D1, D2> + Clone,
) {
    prep_gt_subtree_and_bench(p, move |b, (repositories, (owned, mappings))| {
        let hyperast = &repositories.processor.main_stores;
        b.iter_batched(
            || Mapper::prep(hyperast, mappings.clone(), owned.clone()),
            f.clone(),
            BatchSize::LargeInput,
        );
    });
}

/// Note: Inline if you want to control HyperAST construction and preparatory mapping computation
fn prep_gt_subtree_and_bench<Mea: Measurement>(
    p: &mut Bench<'_, criterion::BenchmarkGroup<'_, Mea>>,
    f: impl FnMut(&mut criterion::Bencher<'_, Mea>, &(&PreProcessedRepositories, &OwnedLazyMapping)),
) {
    p.group.bench_with_input_prepared(
        BenchmarkId::new(&p.name.clone().unwrap(), p.i.repo.name()),
        p.repositories,
        |group, repositories| {
            let (src, dst) = prep_commits(p.i, repositories);
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

type HAST = hyperast::store::SimpleStores<hyperast_vcs_git::TStore>;
type Mpr<HAST, D1 = LazyPostOrder<NodeIdentifier, u32>, D2 = LazyPostOrder<NodeIdentifier, u32>> =
    Mapper<HAST, Decompressible<HAST, D1>, Decompressible<HAST, D2>, VecStore<u32>>;

fn prep_cd_subtree_and_bench_batched<Mea: Measurement, D1, D2>(
    p: &mut Bench<'_, criterion::BenchmarkGroup<'_, Mea>>,
    f: impl for<'a> FnMut(Mpr<&'a HAST>) -> Mpr<&'a HAST, D1, D2> + Clone,
) {
    prep_cd_subtree_and_bench(p, move |b, (repositories, (owned, mappings))| {
        let hyperast = &repositories.processor.main_stores;
        b.iter_batched(
            || Mapper::prep(hyperast, mappings.clone(), owned.clone()),
            f.clone(),
            BatchSize::LargeInput,
        );
    });
}

fn prep_cd_subtree_and_bench<Mea: Measurement>(
    p: &mut Bench<'_, criterion::BenchmarkGroup<'_, Mea>>,
    f: impl FnMut(&mut criterion::Bencher<'_, Mea>, &(&PreProcessedRepositories, &OwnedLazyMapping)),
) {
    p.group.bench_with_input_prepared(
        BenchmarkId::new(&p.name.clone().unwrap(), p.i.repo.name()),
        p.repositories,
        |group, repositories| {
            let (src, dst) = prep_commits(p.i, repositories);
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
            let mapper = LazyLeavesMatcher::<_>::match_it(mapper);
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
