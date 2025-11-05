use serde::{Deserialize, Serialize};
use std::fmt::Debug;

use hyper_diff::decompressed_tree_store::ShallowDecompressedTreeStore;
use hyper_diff::matchers::{Decompressible, Mapper};
use hyperast::store::defaults::NodeIdentifier;
use hyperast::types::{Childrn, HyperAST, HyperType, WithChildren, WithStats};
use hyperast_vcs_git::{SimpleStores, processing::ConfiguredRepoTrait};

use crate::{MappingStage, matching, no_space};

pub(crate) type NoSpaceStore<'a, 'store> = hyperast::store::SimpleStores<
    hyperast_vcs_git::TStore,
    no_space::NoSpaceNodeStoreWrapper<'store>,
    &'a hyperast::store::labels::LabelStore,
>;

#[derive(Deserialize, Serialize, Debug)]
pub struct SrcChanges {
    pub user: String,
    pub name: String,
    pub commit: String,
    /// Global position of deleted elements
    pub deletions: Vec<u32>, // TODO diff encode
}
#[derive(Deserialize, Serialize, Debug)]
pub struct DstChanges {
    pub user: String,
    pub name: String,
    pub commit: String,
    /// Global position of added elements
    pub additions: Vec<u32>, // TODO diff encode
}

type RepoConfig = hyperast_vcs_git::processing::ParametrizedCommitProcessorHandle;

pub(crate) const UPD: bool = true;

pub(crate) fn added_deleted(
    state: std::sync::Arc<crate::AppState>,
    repo_handle: &impl ConfiguredRepoTrait<Config = RepoConfig>,
    src_oid: hyperast_vcs_git::git::Oid,
    dst_oid: hyperast_vcs_git::git::Oid,
) -> Result<(SrcChanges, DstChanges), String> {
    let repositories = state.repositories.read().unwrap();
    let commit_src = repositories
        .get_commit(repo_handle.config(), &src_oid)
        .unwrap();
    let src_tr = commit_src.ast_root;
    let commit_dst = repositories
        .get_commit(repo_handle.config(), &dst_oid)
        .unwrap();
    let dst_tr = commit_dst.ast_root;
    let with_spaces_stores = &repositories.processor.main_stores;
    let stores = &no_space::as_nospaces(with_spaces_stores);

    log::info!("added_deleted {} {}", src_oid, dst_oid);
    if src_tr == dst_tr {
        return Ok((
            SrcChanges {
                user: repo_handle.spec().user().to_string(),
                name: repo_handle.spec().name().to_string(),
                commit: src_oid.to_string(),
                deletions: Default::default(),
            },
            DstChanges {
                user: repo_handle.spec().user().to_string(),
                name: repo_handle.spec().name().to_string(),
                commit: dst_oid.to_string(),
                additions: Default::default(),
            },
        ));
    }
    // // enable considering updates added/removed (for visualization for example)
    // // TODO make a special category for updates
    // if UPD {
    //     // NOTE I don't know why yet but in some cases the UPD feature seems to require to compute the mappings in both ways
    //     // It happens when going through the greedy tracking
    //     // maybe an issue with DashMaps ?
    //     compute_and_cache_full_diff(state.clone(), repo_handle.config(), dst_oid, src_oid);
    // }
    // NOTE the above trick has been moved to the greedy tracking branch

    let binding = crate::utils::bind_tree_pair(&state.partial_decomps, &src_tr, &dst_tr);

    let mut updated = hashbrown::HashSet::<u32>::default();
    let mapped = {
        let mut locked = binding.lock();
        let tree_pair = locked.as_mut(stores);

        let mappings = hyper_diff::matchers::mapping_store::VecStore::default();
        let mut mapper = hyper_diff::matchers::Mapper::prep(stores, mappings, tree_pair);
        use hyper_diff::matchers::mapping_store::DefaultMultiMappingStore as MM;
        let mapped = crate::changes::continue_compute_mappings_full::<_, _, MM<_>>(
            &state.mappings_alone,
            &mut mapper,
            None,
        );
        let vec_store = &mapped.1;
        if UPD {
            for (i, x) in vec_store.src_to_dst.iter().enumerate() {
                if *x == 0 {
                    continue;
                }
                let s = mapper.src_arena.original(&(i as u32));
                use hyperast::types::Labeled;
                let s = stores.resolve(&s);
                let s = s.try_get_label();
                let d = mapper.dst_arena.original(&(x - 1));
                let d = stores.resolve(&d);
                let d = d.try_get_label();
                if s != d {
                    updated.insert(i as u32);
                }
            }
        }
        log::info!("mapped len: {}", mapped.1.len());
        mapped
    };

    let dst_to_src = &mapped.1.dst_to_src;
    let src_to_dst = &mapped.1.src_to_dst;
    let unmapped_dst: Vec<_> = global_pos_with_spaces(
        &repositories.processor.main_stores,
        dst_tr,
        (dst_to_src.iter().enumerate()).filter_map(|(i, x)| {
            if *x == 0 {
                Some(i as u32)
            } else if UPD && updated.contains(&(x - 1)) {
                Some(i as u32)
            } else {
                None
            }
        }),
    );
    let unmapped_src: Vec<_> = global_pos_with_spaces(
        &repositories.processor.main_stores,
        src_tr,
        (src_to_dst.iter().enumerate()).filter_map(|(i, x)| {
            if *x == 0 {
                Some(i as u32)
            } else if UPD && updated.contains(&(i as u32)) {
                Some(i as u32)
            } else {
                None
            }
        }),
    );

    Ok((
        SrcChanges {
            user: repo_handle.spec().user().to_string(),
            name: repo_handle.spec().name().to_string(),
            commit: src_oid.to_string(),
            deletions: unmapped_src,
        },
        DstChanges {
            user: repo_handle.spec().user().to_string(),
            name: repo_handle.spec().name().to_string(),
            commit: dst_oid.to_string(),
            additions: unmapped_dst,
        },
    ))
}

// TODO try to move it in hyperast::position
/// no_spaces gives topolgical indexes, topologically ordered,
/// it maps onto a tree without spaces
pub fn global_pos_with_spaces<It: Iterator<Item = u32>>(
    stores: &SimpleStores,
    root: NodeIdentifier,
    // increasing order
    mut no_spaces: It,
) -> Vec<It::Item> {
    #[derive(Debug)]
    struct Ele {
        id: NodeIdentifier,
        i_no_s: u32,
        i_w_s: u32,
        idx: usize,
        children: Vec<NodeIdentifier>,
        d1_no_s: u32,
    }
    let mut res = vec![];
    let mut stack = {
        let b = stores.node_store().resolve(root);
        let cs = b.children().unwrap();
        let children = cs.iter_children().collect();
        let i_no_s = b.size_no_spaces() as u32;
        let i_w_s = b.size() as u32;
        vec![Ele {
            id: root,
            i_no_s,
            i_w_s,
            idx: 0,
            children,
            d1_no_s: 0,
        }]
    };
    let mut index_with_spaces: u32 = 0;
    let mut index_no_spaces: u32 = 0;
    for curr_no_space in no_spaces {
        loop {
            // dbg!(stack.len());
            let mut ele = stack.pop().unwrap();
            // dbg!(
            //     curr_no_space,
            //     index_with_spaces,
            //     index_no_spaces,
            //     &ele
            // );
            // TODO add debug assertion about size_no_space and is_space being compatible
            assert!(index_no_spaces <= index_with_spaces);
            if curr_no_space < index_no_spaces {
                panic!()
            } else if curr_no_space < ele.i_no_s {
                // need to go down
                let Some(&id) = ele.children.get(ele.idx) else {
                    for x in ele.children {
                        let b = stores.node_store().resolve(x);
                        dbg!(stores.resolve_type(&x));
                        dbg!(b.size_no_spaces());
                    }
                    panic!()
                };
                let b = stores.node_store().resolve(id);
                if stores.resolve_type(&id).is_spaces() {
                    ele.idx += 1;
                    ele.d1_no_s += b.size_no_spaces() as u32;
                    stack.push(ele);
                    index_with_spaces += 1;
                    // dbg!(b.size_no_spaces());
                    continue;
                }
                let cs = b.children();
                let value = if let Some(cs) = cs {
                    // dbg!(b.size_no_spaces(), b.size());
                    Ele {
                        id,
                        children: cs.iter_children().collect(),
                        i_no_s: index_no_spaces + b.size_no_spaces() as u32 - 1,
                        i_w_s: index_with_spaces + b.size() as u32 - 1,
                        idx: 0,
                        d1_no_s: index_no_spaces,
                    }
                } else {
                    // dbg!();
                    Ele {
                        id,
                        children: vec![],
                        i_no_s: index_no_spaces,
                        i_w_s: index_with_spaces,
                        idx: 0,
                        d1_no_s: index_no_spaces,
                    }
                };
                ele.idx += 1;
                if ele.idx >= ele.children.len() {
                    // dbg!(ele.idx);
                }
                stack.push(ele);
                stack.push(value);
            } else if curr_no_space == ele.i_no_s {
                let b = stores.node_store().resolve(ele.id);
                if stores.resolve_type(&ele.id).is_spaces() {
                    panic!();
                }
                res.push(index_with_spaces);
                if let Some(e) = stack.last_mut() {
                    e.d1_no_s += b.size_no_spaces() as u32;
                }
                index_no_spaces = ele.i_no_s + 1;
                index_with_spaces = ele.i_w_s + 1;
                break;
            } else {
                // index_no_spaces + ele.size_no_s < curr_no_space
                // we can skip the current node
                // we already poped ele
                index_no_spaces = ele.i_no_s + 1;
                index_with_spaces = ele.i_w_s + 1;
                // dbg!();
                let b = stores.node_store().resolve(ele.id);
                if let Some(e) = stack.last_mut() {
                    e.d1_no_s += b.size_no_spaces() as u32;
                }
            }
        }
    }
    res
}

use crate::MappingAloneCacheRef;
use hyper_diff::decompressed_tree_store::Shallow;
use hyper_diff::decompressed_tree_store::lazy_post_order::LazyPostOrder;
use hyper_diff::matchers::mapping_store::MonoMappingStore;
use hyper_diff::matchers::mapping_store::MultiMappingStore;
use hyper_diff::matchers::mapping_store::{self, MappingStore};
use hyperast::PrimInt;
use hyperast::types;
use types::WithHashs;

pub fn continue_compute_mappings_full<'alone, 'tree, HAST: HyperAST + Copy, M, MM>(
    mappings_alone: &'alone crate::MappingAloneCache<HAST::IdN, M>,
    mapper: &mut hyper_diff::matchers::Mapper<
        HAST,
        Decompressible<HAST, &'tree mut LazyPostOrder<HAST::IdN, M::Src>>,
        Decompressible<HAST, &'tree mut LazyPostOrder<HAST::IdN, M::Dst>>,
        M,
    >,
    partial: Option<MM>,
) -> MappingAloneCacheRef<'alone, HAST::IdN, M>
where
    for<'t> types::LendT<'t, HAST>: WithHashs + WithStats,
    HAST::IdN: Clone + Eq,
    HAST::Label: Clone + Eq,
    HAST::IdN: types::NodeId<IdN = HAST::IdN>,
    M: MonoMappingStore + Clone + Default,
    MM: MultiMappingStore<Src = M::Src, Dst = M::Dst> + Default,
    M::Src: PrimInt + Shallow<M::Src>,
    M::Dst: PrimInt + Shallow<M::Dst>,
    M::Src: std::hash::Hash + Debug,
    M::Dst: std::hash::Hash + Debug,
    HAST::IdN: std::hash::Hash + Debug,
{
    use dashmap::mapref::entry::Entry;
    use hyper_diff::matchers::heuristic::gt as matching;
    match mappings_alone.entry((
        mapper.src_arena.original(&mapper.src_arena.root()),
        mapper.dst_arena.original(&mapper.dst_arena.root()),
    )) {
        Entry::Occupied(mut entry) => {
            use mapping_store::MappingStore;
            mapper.mapping.mappings.topit(
                mapper.mapping.src_arena.len(),
                mapper.mapping.dst_arena.len(),
            );
            let mm = if entry.get().0 == MappingStage::Bottomup {
                return entry.into_ref().downgrade();
            } else if let Some(mm) = partial {
                mm
            } else {
                if entry.get().0 == MappingStage::Subtree {
                    log::warn!("cannot store the multimappings in MappingAloneCache: wrong type");
                }
                let now = std::time::Instant::now();
                let mm = LazyGreedySubtreeMatcher::<_>::compute_multi_mapping::<MM>(mapper);
                let compute_multi_mapping_t = now.elapsed().as_secs_f64();
                dbg!(compute_multi_mapping_t);
                mm
            };

            let now = std::time::Instant::now();

            // matching::bottom_up_hiding(hyperast, &mm, mapper);

            use hyper_diff::matchers::heuristic::gt;

            use gt::lazy_greedy_subtree_matcher::LazyGreedySubtreeMatcher;
            LazyGreedySubtreeMatcher::<_>::filter_mappings(mapper, &mm);

            use gt::lazy_hybrid_bottom_up_matcher::LazyHybridBottomUpMatcher;
            LazyHybridBottomUpMatcher::<_, M, 200>::execute(mapper);
            let bottom_up_t = now.elapsed().as_secs_f64();
            log::info!("bottom_up_t: {}", bottom_up_t);

            let value = (
                crate::MappingStage::Bottomup,
                mapper.mapping.mappings.clone(),
            );
            entry.insert(value);
            entry.into_ref().downgrade()
        }
        Entry::Vacant(entry) => {
            use mapping_store::MappingStore;
            mapper.mapping.mappings.topit(
                mapper.mapping.src_arena.len(),
                mapper.mapping.dst_arena.len(),
            );
            let mm = if let Some(mm) = partial {
                mm
            } else {
                let now = std::time::Instant::now();
                let mm = LazyGreedySubtreeMatcher::<_>::compute_multi_mapping::<MM>(mapper);
                let compute_multi_mapping_t = now.elapsed().as_secs_f64();
                dbg!(compute_multi_mapping_t);
                mm
            };

            let now = std::time::Instant::now();

            // matching::bottom_up_hiding(hyperast, &mm, mapper);

            use hyper_diff::matchers::heuristic::gt;

            use gt::lazy_greedy_subtree_matcher::LazyGreedySubtreeMatcher;
            LazyGreedySubtreeMatcher::<_>::filter_mappings(mapper, &mm);

            use gt::lazy_hybrid_bottom_up_matcher::LazyHybridBottomUpMatcher;
            LazyHybridBottomUpMatcher::<_, M, 200>::execute(mapper);
            let bottom_up_t = now.elapsed().as_secs_f64();
            log::info!("bottom_up_t: {}", bottom_up_t);

            let value = (
                crate::MappingStage::Bottomup,
                mapper.mapping.mappings.clone(),
            );
            entry.insert(value).downgrade()
        }
    }
}

fn compute_and_cache_full_diff(
    state: crate::SharedState,
    config: &hyperast_vcs_git::processing::ParametrizedCommitProcessorHandle,
    src_oid: hyperast_vcs_git::git::Oid,
    dst_oid: hyperast_vcs_git::git::Oid,
) {
    let repositories = state.repositories.read().unwrap();
    let commit_src = repositories.get_commit(&config, &src_oid).unwrap();
    let src_tr = commit_src.ast_root;
    let commit_dst = repositories.get_commit(&config, &dst_oid).unwrap();
    let dst_tr = commit_dst.ast_root;
    let with_spaces_stores = &repositories.processor.main_stores;
    let stores = &hyperast_vcs_git::no_space::as_nospaces(with_spaces_stores);
    let binding = crate::utils::bind_tree_pair(&state.partial_decomps, &src_tr, &dst_tr);
    let mut locked = binding.lock();
    let tree_pair = locked.as_mut(stores);

    let mappings = hyper_diff::matchers::mapping_store::VecStore::default();
    let mut mapper = hyper_diff::matchers::Mapper::prep(stores, mappings, tree_pair);
    use hyper_diff::matchers::mapping_store::DefaultMultiMappingStore as MM;
    crate::changes::continue_compute_mappings_full::<_, _, MM<_>>(
        &state.mappings_alone,
        &mut mapper,
        None,
    );
}
