use axum::{Json, response::IntoResponse};
use enumset::{EnumSet, EnumSetType};
use hyper_diff::matchers::Mapping;
use hyperast::PrimInt;
use hyperast_vcs_git::multi_preprocessed::PreProcessedRepositories;
use serde::{Deserialize, Serialize};
use serde_aux::prelude::deserialize_bool_from_anything;
use std::{fmt::Debug, thread::sleep, time::Duration};
use tokio::time::Instant;

use hyper_diff::decompressed_tree_store::ShallowDecompressedTreeStore;
use hyper_diff::decompressed_tree_store::lazy_post_order::LazyPostOrder;
use hyper_diff::matchers::Mapper;
use hyper_diff::matchers::mapping_store;
use hyperast::store::defaults::NodeIdentifier as IdN;
use hyperast::types::{Childrn, HyperAST, NodeStore, WithChildren};
use mapping_store::MappingStore;

type IdD = u32;
type LazyVecMapping =
    Mapping<LazyPostOrder<IdN, IdD>, LazyPostOrder<IdN, IdD>, mapping_store::VecStore<IdD>>;
type LazyRefMut<'a> = clashmap::mapref::one::RefMut<'a, IdN, LazyPostOrder<IdN, IdD>>;

// WARN lazy subtrees are not complete
fn lazy_mapping<'a>(
    repositories: &'a PreProcessedRepositories,
    mappings: &'a crate::MappingCache,
    src_tr: IdN,
    dst_tr: IdN,
) -> dashmap::mapref::one::RefMut<'a, (IdN, IdN), LazyVecMapping> {
    use mapping_store::DefaultMappingStore as M;
    use mapping_store::DefaultMultiMappingStore as MM;

    use hyper_diff::matchers::heuristic::gt;

    use gt::lazy_greedy_bottom_up_matcher::LazyGreedyBottomUpMatcher as BottomupMatcher;
    use gt::lazy_greedy_subtree_matcher::LazyGreedySubtreeMatcher as SubtreeMatcher;
    mappings.entry((src_tr, dst_tr)).or_insert_with(|| {
        let hyperast = &repositories.processor.main_stores;
        let src = &src_tr;
        let dst = &dst_tr;
        let now = Instant::now();
        let mut _mapper: Mapper<_, LazyPostOrder<_, IdD>, LazyPostOrder<_, IdD>, M<_>> =
            hyperast.decompress_pair(src, dst).into();
        // TODO factor

        let mapper = _mapper.split_mut();
        let subtree_prepare_t = now.elapsed().as_secs_f64();
        let now = Instant::now();
        let mapper = SubtreeMatcher::<_>::match_it::<MM<_>>(mapper);
        let subtree_matcher_t = now.elapsed().as_secs_f64();
        let subtree_mappings_s = mapper.mappings().len();
        dbg!(&subtree_matcher_t, &subtree_mappings_s);
        let bottomup_prepare_t = 0.;
        let now = Instant::now();
        let mapper = BottomupMatcher::<_, M<_>>::match_it(mapper);
        dbg!(&now.elapsed().as_secs_f64());
        let bottomup_matcher_t = now.elapsed().as_secs_f64();
        let bottomup_mappings_s = mapper.mappings().len();
        dbg!(&bottomup_matcher_t, &bottomup_mappings_s);
        // let now = Instant::now();

        // NOTE could also have completed trees
        // let node_store = hyperast.node_store();
        // let mapper = mapper.map(
        //     |src_arena| CompletePostOrder::from(src_arena.complete(node_store)),
        //     |dst_arena| {
        //         let complete = CompletePostOrder::from(dst_arena.complete(node_store));
        //         SimpleBfsMapper::from(node_store, complete)
        //     },
        // );

        // NOTE we do not use edit scripts here
        // let prepare_gen_t = now.elapsed().as_secs_f64();
        // let now = Instant::now();
        // let actions = ScriptGenerator::compute_actions(mapper.hyperast, &mapper.mapping).ok();
        // let gen_t = now.elapsed().as_secs_f64();
        // dbg!(gen_t);
        // let mapper = mapper.map(|x| x, |dst_arena| dst_arena.back);
        // Mapper::<_, LazyPostOrder<_, _>, LazyPostOrder<_, _>, _>::persist(mapper)
        Mapping {
            mappings: _mapper.mapping.mappings,
            src_arena: _mapper.mapping.src_arena,
            dst_arena: _mapper.mapping.dst_arena,
        }
    })
}

// WARN lazy subtrees are not complete
fn lazy_subtree_mapping<'a>(
    repositories: &PreProcessedRepositories,
    partial_comp_cache: &'a crate::PartialDecompCache,
    src_tr: IdN,
    dst_tr: IdN,
) -> Mapping<LazyRefMut<'a>, LazyRefMut<'a>, mapping_store::MultiVecStore<IdD>> {
    use gt::lazy_greedy_subtree_matcher::LazyGreedySubtreeMatcher as SubtreeMatcher;
    use hyper_diff::decompressed_tree_store::lazy_post_order::LazyPostOrder;
    use hyper_diff::matchers::heuristic::gt;
    use mapping_store::DefaultMultiMappingStore as MM;
    use mapping_store::VecStore as M;

    let hyperast = &repositories.processor.main_stores;
    let src = &src_tr;
    let dst = &dst_tr;
    let now = Instant::now();
    assert_ne!(src, dst);
    use hyperast::types::DecompressedFrom;
    let cached_decomp = |id: &IdN| -> Option<_> {
        let decompress = partial_comp_cache
            .try_entry(*id)?
            .or_insert_with(|| LazyPostOrder::<_, u32>::decompress(hyperast, id));
        Some(decompress)
    };
    let (mut decompress_src, mut decompress_dst) = loop {
        match (cached_decomp(src), cached_decomp(dst)) {
            (Some(src), Some(dst)) => {
                break (src, dst);
            }
            (None, None) => {
                dbg!();
            }
            _ => {
                dbg!(
                    partial_comp_cache.hash_usize(src),
                    partial_comp_cache.hash_usize(dst)
                );
                dbg!(
                    partial_comp_cache.determine_shard(partial_comp_cache.hash_usize(src)),
                    partial_comp_cache.determine_shard(partial_comp_cache.hash_usize(dst))
                );
            }
        }
        sleep(Duration::from_secs(2));
    };

    let mappings = M::default();
    let owned = (decompress_src.value_mut(), decompress_dst.value_mut());
    let mut mapper = Mapper::prep(hyperast, mappings, owned);
    mapper.mapping.mappings.topit(
        mapper.mapping.src_arena.len(),
        mapper.mapping.dst_arena.len(),
    );
    dbg!();

    let mm = SubtreeMatcher::<_>::compute_multi_mapping::<MM<_>>(&mut mapper);
    dbg!();

    Mapping {
        src_arena: decompress_src,
        dst_arena: decompress_dst,
        mappings: mm,
    }
}

pub(super) fn shift_piece(
    commit: hyperast_vcs_git::git::Oid,
    source: Option<super::PieceOfCode<IdN, u16>>,
    src: super::LocalPieceOfCode<IdN, u16>,
    spec: &hyperast_vcs_git::git::Repo,
) -> (
    super::PieceOfCode<IdN, u16>,
    Option<super::PieceOfCode<IdN, u16>>,
) {
    let new = src.globalize(spec, commit);
    if let Some(src) = source {
        (src, Some(new))
    } else {
        (new, None)
    }
}

pub fn repo_config_error(now: Instant) -> super::TrackingError {
    super::TrackingError {
        compute_time: now.elapsed().as_secs_f64(),
        commits_processed: 0,
        node_processed: 0,
        message: "missing config for repository".to_string(),
    }
}

use hyperast::types::NodeId;
use hyperast::types::TypeStore;

pub fn child_by_type<HAST: HyperAST>(
    stores: &HAST,
    d: HAST::IdN,
    t: &<HAST::TS as TypeStore>::Ty,
) -> Option<(HAST::IdN, usize)>
where
    HAST::IdN: NodeId<IdN = HAST::IdN>,
{
    let n = stores.node_store().resolve(&d);
    let children = n.children().unwrap();
    let mut cs_by_idx = children.iter_children().enumerate();
    cs_by_idx
        .find(|(_, x)| stores.resolve_type(x).eq(t))
        .map(|(i, x)| (x, i))
}

#[derive(Clone)]
pub struct TargetCodeElement<IdN, Idx> {
    pub start: usize,
    pub end: usize,
    pub path: Vec<Idx>,
    pub path_no_spaces: Vec<Idx>,
    pub root: IdN,
    pub node: IdN,
}

use hyperast::position::position_accessors;

pub trait WithPreOrderOffsetsNoSpaces: position_accessors::WithOffsets {
    type It<'a>: Iterator<Item = &'a Self::Idx>
    where
        Self: 'a,
        Self::Idx: 'a;
    fn iter_offsets_nospaces(&self) -> Self::It<'_>;
}

impl<IdN: Clone, Idx> position_accessors::SolvedPosition<IdN> for TargetCodeElement<IdN, Idx> {
    fn node(&self) -> IdN {
        self.node.clone()
    }
}

impl<IdN: Clone, Idx> position_accessors::RootedPosition<IdN> for TargetCodeElement<IdN, Idx> {
    fn root(&self) -> IdN {
        self.root.clone()
    }
}

impl<IdN, Idx: PrimInt> position_accessors::WithPath<IdN> for TargetCodeElement<IdN, Idx> {}

impl<IdN, Idx: PrimInt> position_accessors::WithPreOrderOffsets for TargetCodeElement<IdN, Idx> {
    type It<'a>
        = std::iter::Copied<std::slice::Iter<'a, Idx>>
    where
        Idx: 'a,
        Self: 'a;

    fn iter_offsets(&self) -> Self::It<'_> {
        self.path.iter().copied()
    }
}

impl<IdN, Idx: PrimInt> position_accessors::WithPreOrderPath<IdN> for TargetCodeElement<IdN, Idx> {
    type ItPath = std::vec::IntoIter<(Idx, IdN)>;

    fn iter_offsets_and_nodes(&self) -> Self::ItPath {
        todo!()
    }
}

impl<IdN, Idx: PrimInt> position_accessors::WithOffsets for TargetCodeElement<IdN, Idx> {
    type Idx = Idx;
}

impl<IdN, Idx> position_accessors::OffsetPostionT<IdN> for TargetCodeElement<IdN, Idx> {
    type IdO = usize;

    fn offset(&self) -> Self::IdO {
        self.start
    }

    fn len(&self) -> Self::IdO {
        self.end - self.start
    }

    fn start(&self) -> Self::IdO {
        self.start
    }

    fn end(&self) -> Self::IdO {
        self.end
    }
}

impl<IdN, Idx: PrimInt> WithPreOrderOffsetsNoSpaces for TargetCodeElement<IdN, Idx> {
    type It<'a>
        = std::slice::Iter<'a, Idx>
    where
        Idx: 'a,
        Self: 'a;

    fn iter_offsets_nospaces(&self) -> Self::It<'_> {
        self.path_no_spaces.iter()
    }
}
