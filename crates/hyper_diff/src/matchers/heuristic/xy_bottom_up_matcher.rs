use crate::matchers::Mapper;
use crate::matchers::mapping_store::MonoMappingStore;
use crate::matchers::similarity_metrics;
use hyperast::PrimInt;
use hyperast::types::{HyperAST, LendT, NodeId, NodeStore, Tree, WithHashs};
use std::collections::HashMap;
use std::fmt::Debug;

use super::factorized_bounds::DecompTreeBounds;

pub struct XYBottomUpMatcher<
    Mpr,
    const SIM_THRESHOLD_NUM: u64 = 1,
    const SIM_THRESHOLD_DEN: u64 = 2,
> {
    _phantom: std::marker::PhantomData<*const Mpr>,
}

impl<
    Dsrc: DecompTreeBounds<HAST, M::Src>,
    Ddst: DecompTreeBounds<HAST, M::Dst>,
    HAST: HyperAST + Copy,
    M: MonoMappingStore,
    const SIM_THRESHOLD_NUM: u64,
    const SIM_THRESHOLD_DEN: u64,
> XYBottomUpMatcher<Mapper<HAST, Dsrc, Ddst, M>, SIM_THRESHOLD_NUM, SIM_THRESHOLD_DEN>
where
    for<'t> LendT<'t, HAST>: WithHashs,
    M::Src: PrimInt,
    M::Dst: PrimInt,
    HAST::Label: Eq,
    HAST::IdN: Debug,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
{
    pub fn match_it(
        mut mapper: crate::matchers::Mapper<HAST, Dsrc, Ddst, M>,
    ) -> crate::matchers::Mapper<HAST, Dsrc, Ddst, M> {
        mapper.mapping.mappings.topit(
            mapper.mapping.src_arena.len(),
            mapper.mapping.dst_arena.len(),
        );
        Self::execute(&mut mapper);
        mapper
    }

    pub fn execute(mapper: &mut Mapper<HAST, Dsrc, Ddst, M>) {
        for a in mapper.src_arena.iter_df_post::<false>() {
            if !(mapper.mappings.is_src(&a) || !Self::src_has_children(mapper, a)) {
                let candidates = mapper.get_dst_candidates(&a);
                let mut best = None;
                let mut max: f64 = -1.;
                for cand in candidates {
                    let sim = similarity_metrics::SimilarityMeasure::range(
                        &mapper.src_arena.descendants_range(&a),
                        &mapper.dst_arena.descendants_range(&cand),
                        &mapper.mappings,
                    )
                    .jaccard();
                    if sim > max && sim >= SIM_THRESHOLD_NUM as f64 / SIM_THRESHOLD_DEN as f64 {
                        max = sim;
                        best = Some(cand);
                    }
                }

                if let Some(best) = best {
                    Self::last_chance_match(mapper, a, best);
                    mapper.mappings.link(a, best);
                }
            }
        }
        // for root
        mapper.mapping.mappings.link(
            mapper.mapping.src_arena.root(),
            mapper.mapping.dst_arena.root(),
        );
        Self::last_chance_match(mapper, mapper.src_arena.root(), mapper.dst_arena.root());
    }

    fn src_has_children(mapper: &mut Mapper<HAST, Dsrc, Ddst, M>, src: M::Src) -> bool {
        use num_traits::ToPrimitive;
        let r = mapper
            .hyperast
            .node_store()
            .resolve(&mapper.src_arena.original(&src))
            .has_children();
        debug_assert_eq!(
            r,
            mapper.src_arena.lld(&src) < src,
            "{:?} {:?}",
            mapper.src_arena.lld(&src),
            src.to_usize()
        );
        r
    }
    fn last_chance_match(mapper: &mut Mapper<HAST, Dsrc, Ddst, M>, src: M::Src, dst: M::Dst) {
        let mut src_types: HashMap<_, Vec<M::Src>> = HashMap::new();
        let mut dst_types: HashMap<_, Vec<M::Dst>> = HashMap::new();

        for src_child in mapper.src_arena.children(&src) {
            let original = mapper.src_arena.original(&src_child);
            let src_type = mapper.hyperast.resolve_type(&original);
            src_types.entry(src_type).or_default().push(src_child);
        }

        for dst_child in mapper.dst_arena.children(&dst) {
            let original = mapper.dst_arena.original(&dst_child);
            let dst_type = mapper.hyperast.resolve_type(&original);
            dst_types.entry(dst_type).or_default().push(dst_child);
        }

        for (src_type, src_list) in src_types.iter() {
            // TODO same thing use an Option instead of a Vec
            if src_list.len() == 1 {
                if let Some(dst_list) = dst_types.get(src_type) {
                    if dst_list.len() == 1
                        && !mapper.mappings.is_src(&src_list[0])
                        && !mapper.mappings.is_dst(&dst_list[0])
                    {
                        mapper.mappings.link(src_list[0], dst_list[0]);
                    }
                }
            }
        }
    }
}
