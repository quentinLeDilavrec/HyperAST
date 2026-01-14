use hyperast::position::position_accessors::SolvedPosition;
use hyperast::store::defaults::NodeIdentifier;
use hyperast_gen_ts_tsquery::code2query::QueryLattice;

pub struct SynthConfig {
    /// patterns with more nodes than size_threshold only produce pattern shrinking enough
    pub size_threshold: usize,
    /// the shrink factor in percent
    pub shrink_threshold_factor: usize,
}

pub fn synth<'a, P: SolvedPosition<NodeIdentifier>>(
    timeout: &crate::Timeout,
    inst: &'a [P],
    stores: &hyperast::store::SimpleStores<hyperast_gen_ts_java::types::TStore>,
    meta_gen: &str,
    meta_simp: &str,
    config: &SynthConfig,
) -> QueryLattice<P>
where
    P: Sync + Send + Eq + Copy,
{
    assert!(!inst.is_empty(), "No instances provided");
    assert!(!meta_gen.is_empty());
    assert!(!meta_simp.is_empty());
    use hyperast_gen_ts_java as ts_gen;
    let lang = ts_gen::language();
    // generate and refine patterns
    let meta_gen = hyperast_tsquery::Query::new(meta_gen, lang.clone()).unwrap();
    // .map_err(|e| format!("error in meta_gen: {e}"))?;

    let meta_simp =
        hyperast_tsquery::Query::new(meta_simp, hyperast_gen_ts_tsquery::language()).unwrap();
    // .map_err(|e| format!("error in meta_simp: {e}"))?;

    use hyperast_gen_ts_tsquery::code2query::QueryLattice;
    let query_poset = {
        let b = QueryLattice::builder::<ts_gen::types::TStore, ts_gen::types::TIdN<_>, _>(
            stores,
            inst.iter().copied(),
            &meta_gen,
            &meta_simp,
            &|x| (x.local.metrics.size, x.local.metrics.hashs.label),
        );
        if !inst.is_empty() {
            assert!(!b.dedup.is_empty());
        }
        let mut b = b.dedup_leaf_queries(|from: Vec<(_, (_, (u32, u32)))>| {
            hyperast_gen_ts_tsquery::code2query::group_by_size(from)
        });
        if !inst.is_empty() {
            assert!(!b.dedup.is_empty());
        }

        let start = std::time::Instant::now();
        let timeout = timeout.0;
        let mut timeouted = false;
        let mut timeout = || {
            if start.elapsed() > timeout {
                log::warn!("Timeout reached");
                timeouted = true;
                return true;
            }
            false
        };

        let size_threshold = |s| {
            config
                .size_threshold
                .max(s * config.shrink_threshold_factor / 100)
        };
        hyperast_gen_ts_tsquery::code2query::semi_interactive_poset_build(
            &mut b,
            &meta_simp,
            &mut timeout,
            size_threshold,
        );
        if timeouted {
            log::trace!(
                "timeouted lattice size: {}",
                b.dedup.iter().map(|x| x.len()).sum::<usize>()
            );
            // TIP simplify more aggressively
        } else {
            log::trace!(
                "final lattice size: {}",
                b.dedup.iter().map(|x| x.len()).sum::<usize>()
            );
        }
        b.post();
        b.build()
    };

    query_poset
}

pub use hyperast_gen_ts_tsquery::lattice_graph::Prep;
