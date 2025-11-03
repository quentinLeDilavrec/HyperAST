use hyper_diff::{
    decompressed_tree_store::{
        Shallow,
        lazy_post_order::{self, LazyPostOrder},
    },
    matchers::Decompressible,
};
use hyperast::position::position_accessors::{self, SolvedPosition};
use mapping_store::MappingStore;

use crate::MappingAloneCacheRef;

use super::*;

type IdD = u32;

type DecompressedTree<IdN = super::IdN, IdD = self::IdD> = LazyPostOrder<IdN, IdD>;

struct MappingTracker<'store, HAST> {
    stores: &'store HAST,
    // no_spaces_path_to_target: Vec<HAST::Idx>,
}

impl<'store, HAST> MappingTracker<'store, HAST> {
    fn new(stores: &'store HAST) -> Self {
        Self { stores }
    }
    fn size(&self, other_tr: &HAST::IdN, current_tr: &HAST::IdN) -> usize
    where
        HAST: HyperAST,
        for<'t> <HAST as hyperast::types::AstLending<'t>>::RT: types::WithStats,
    {
        let node_store = self.stores.node_store();
        let src_size: usize = node_store.resolve(other_tr).size();
        let dst_size: usize = node_store.resolve(current_tr).size();
        src_size + dst_size
    }
}

pub trait WithPreOrderOffsetsNoSpaces: WithOffsets {
    // type Path: Iterator;
    // fn path(&self) -> Self::Path;
    type It<'a>: Iterator<Item = &'a Self::Idx>
    where
        Self: 'a,
        Self::Idx: 'a;
    fn iter_offsets_nospaces(&self) -> Self::It<'_>;
}

pub(super) fn do_tracking<P, C>(
    repositories: &multi_preprocessed::PreProcessedRepositories,
    partial_decomps: &PartialDecompCache,
    mappings_alone: &MappingAloneCache,
    flags: &Flags,
    target: &P,
    other_tr: super::IdN,
    postprocess_matching: &impl Fn(LocalPieceOfCode<super::IdN, super::Idx>) -> C,
) -> MappingResult<super::IdN, super::Idx, C>
where
    P: position_accessors::SolvedPosition<super::IdN>
        + position_accessors::RootedPosition<super::IdN>
        + position_accessors::WithPreOrderOffsets<Idx = super::Idx>
        + position_accessors::OffsetPostionT<super::IdN, IdO = usize>
        + self::WithPreOrderOffsetsNoSpaces,
{
    let with_spaces_stores = &repositories.processor.main_stores;
    let tracker_nospace = MappingTracker {
        stores: &hyperast_vcs_git::no_space::as_nospaces(with_spaces_stores),
    };

    let target_tr = target.root();

    // NOTE: persists mappings, could also easily persist diffs,
    // but some compression on mappings could help
    // such as, not storing the decompression arenas
    // or encoding mappings more efficiently considering that most slices could simply by represented as ranges (ie. mapped identical subtrees)
    // or only storing the mapping costly to compute

    if target_tr == other_tr {
        // if both tree are identical lets just take a shortcut
        let nodes = tracker_nospace.size(&target_tr, &other_tr);
        let src = compute_local2(target, with_spaces_stores);
        // the shortcut, it's ok because roots/trees are identical
        let matches = vec![src.clone()];
        let matches = matches.into_iter().map(postprocess_matching).collect();
        return if flags.some() {
            let next = matches;
            MappingResult::Skipped { nodes, src, next }
        } else {
            MappingResult::Direct { src, matches }
        };
    }
    let stores = &no_space::as_nospaces(with_spaces_stores);
    let binding = crate::utils::bind_tree_pair(partial_decomps, &target_tr, &other_tr);
    let mut locked = binding.lock();
    let tree_pair = locked.as_mut(stores);

    let mappings = mapping_store::VecStore::default();
    let mut mapper = Mapper::prep(stores, mappings, tree_pair);
    let fuller_mappings = if flags.some() {
        // case where we want to track through multiple commits
        let subtree_mappings = matching::top_down(
            stores,
            &mut mapper.mapping.src_arena,
            &mut mapper.mapping.dst_arena,
        );
        dbg!();
        // let mut mapper = mapper.mirror();
        if let Some(value) = track_greedy(
            with_spaces_stores,
            stores,
            &mut mapper.mapping.src_arena,
            &mut mapper.mapping.dst_arena,
            &subtree_mappings,
            flags,
            target,
            postprocess_matching,
        ) {
            dbg!();
            return value;
        }
        // let mut mapper = mapper.mirror();
        use mapping_store::DefaultMultiMappingStore as MM;
        compute_mappings_full::<_, _, MM<_>>(mappings_alone, &mut mapper, Some(subtree_mappings))
    } else {
        use mapping_store::DefaultMultiMappingStore as MM;
        compute_mappings_full::<_, _, MM<_>>(mappings_alone, &mut mapper, None)
    };
    let fuller_mappings = &fuller_mappings.1;

    let mapping_target = {
        let root = mapper.src_arena.root();
        let p = target.iter_offsets_nospaces().copied();
        mapper.src_arena.child_decompressed(&root, p)
    };

    // let mut mapper = mapper.mirror();
    if let Some(mapped) = fuller_mappings.get_dst(&mapping_target) {
        dbg!();
        return track_with_mappings(
            with_spaces_stores,
            &mut mapper,
            flags,
            target,
            mapping_target,
            mapped,
            postprocess_matching,
        );
    }

    for parent_target in mapper.mapping.src_arena.parents(mapping_target) {
        let Some(mapped) = fuller_mappings.get_dst(&parent_target) else {
            continue;
        };
        let fallback = post_process_mapped2::<P, C>(
            other_tr,
            postprocess_matching,
            with_spaces_stores,
            &mut mapper.mapping.dst_arena,
            mapped,
        );
        // assert_eq!(Some(&mapped_node), path_ids.last().or(Some(&other_tr)), "{:?} {:?} {:?} {:?}", mapped_node, other_tr, path, path_ids); // if it holds then ok to take the ids from the nospace repr.
        // TODO WARN there is an issue there. Entity(2148976) Entity(2149024) [0, 38, 2] [Entity(2148976), Entity(2148992), Entity(2149008)]
        // the list of ids is I believe sorted in reverse compered to the list of offsets,
        // but as you can see the mapped node is the same (but at the begining of the array) so it should be correct to  use the path from the nospace repr.
        let src = compute_local2(target, with_spaces_stores);
        // TODO add fallback_src
        return MappingResult::Missing { src, fallback };
    }
    // lets try
    unreachable!("At least roots should have been mapped")
    // RATIONAL: For now I consider that mapping roots is part of the hypothesis when tracking a value between a tree pair,
    //           it is not necessary for all mapping algorithms,
    //           but providing a fallback is still useful,
    //           so that the user can descide if the element is really not there.
    //           The disaperance of an element should probably be descibed using a window of versions.
    //           Actually, relaxing the mapping process could always find a match for a given code element.
    //           Moreover, a mapping algorithm does not give an absolute result (would not mean much),
    //           it is just a process that minimizes the number of actions to go from one version to the other.
    //           In this context falling back to a clone detection approach seem more adapted.

    // let path = path_to_target.clone();
    // let (target_pos, target_path_ids) = compute_position_and_nodes(
    //     other_tr,
    //     &mut path_to_target.iter().copied(),
    //     with_spaces_stores,
    // );
    // // TODO what should be done if there is no match ?
    // MappingResult::Direct {
    //     src: LocalPieceOfCode::from_file_and_range(
    //         target_pos.file(),
    //         target_range,
    //         path,
    //         target_path_ids,
    //     ),
    //     matches: vec![],
    // }
}

fn post_process_mapped2<P, C>(
    other_tr: super::IdN,
    postprocess_matching: &impl Fn(LocalPieceOfCode<super::IdN, super::Idx>) -> C,
    with_spaces_stores: &SimpleStores<TStore>,
    dst_tree: &mut Decompressible<&NoSpaceStore<'_, '_>, &mut LazyPostOrder<super::IdN, IdD>>,
    mapped: u32,
) -> C
where
    P: position_accessors::SolvedPosition<super::IdN>
        + position_accessors::RootedPosition<super::IdN>
        + position_accessors::WithPreOrderOffsets<Idx = super::Idx>
        + position_accessors::OffsetPostionT<super::IdN, IdO = usize>
        + self::WithPreOrderOffsetsNoSpaces,
{
    let (path, path_ids) = {
        let mapped_parent = dst_tree.decompress_to(&mapped);
        assert_eq!(other_tr, dst_tree.original(&dst_tree.root()));
        let path = dst_tree.path(&dst_tree.root(), &mapped_parent);
        let mut path_ids = vec![dst_tree.original(&mapped_parent)];
        path_ids.extend(
            dst_tree
                .parents(mapped_parent)
                .map(|i| dst_tree.original(&i)),
        );
        path_ids.pop();
        assert_eq!(path.len(), path_ids.len());
        dbg!(&path_ids);
        let path = path_with_spaces(other_tr, &mut path.iter().copied(), with_spaces_stores).0;
        (path, path_ids)
    };
    let (pos, _) = compute_position(other_tr, &mut path.iter().copied(), with_spaces_stores);
    let fallback = LocalPieceOfCode::from_position(&pos, path, path_ids);
    let fallback = postprocess_matching(fallback);
    fallback
}

fn track_with_mappings<P, C, M: MappingStore>(
    with_spaces_stores: &SimpleStores<TStore>,
    mapper: &mut Mapper<
        &NoSpaceStore<'_, '_>,
        Decompressible<&NoSpaceStore<'_, '_>, &mut LazyPostOrder<super::IdN, M::Src>>,
        Decompressible<&NoSpaceStore<'_, '_>, &mut LazyPostOrder<super::IdN, M::Dst>>,
        M,
    >,
    flags: &Flags,
    target: &P,
    mapping_target: M::Src,
    mapped: M::Dst,
    postprocess_matching: &impl Fn(LocalPieceOfCode<super::IdN, super::Idx>) -> C,
) -> MappingResult<super::IdN, super::Idx, C>
where
    P: SolvedPosition<super::IdN>
        + position_accessors::RootedPosition<super::IdN>
        + position_accessors::WithPreOrderOffsets<Idx = super::Idx>
        + position_accessors::OffsetPostionT<super::IdN, IdO = usize>,
    M::Src: PrimInt,
    M::Dst: PrimInt + Shallow<M::Dst>,
{
    let mapped_node = mapper.dst_arena.original(&mapped);

    let mut flagged = false;
    let mut triggered = false;
    if flags.exact_child {
        flagged = true;
        dbg!();
        triggered |= target.node() != mapped_node;
    }
    if flags.child || flags.sim_child {
        flagged = true;
        dbg!();

        let target_node = mapper.hyperast.node_store.resolve(target.node());
        let mapped_node = mapper.hyperast.node_store.resolve(mapped_node);
        if flags.sim_child {
            triggered |= target_node.hash(&types::HashKind::structural())
                != mapped_node.hash(&types::HashKind::structural());
        } else {
            triggered |= target_node.hash(&types::HashKind::label())
                != mapped_node.hash(&types::HashKind::label());
        }
    }
    if flags.upd {
        flagged = true;
        dbg!();
        // TODO need role name
        // let target_ident = child_by_type(stores, target_node, &Type::Identifier);
        // let mapped_ident = child_by_type(stores, mapped_node, &Type::Identifier);
        // if let (Some(target_ident), Some(mapped_ident)) = (target_ident, mapped_ident) {
        //     let target_node = stores.node_store.resolve(target_ident.0);
        //     let target_ident = target_node.try_get_label();
        //     let mapped_node = stores.node_store.resolve(mapped_ident.0);
        //     let mapped_ident = mapped_node.try_get_label();
        //     triggered |= target_ident != mapped_ident;
        // }
    }

    if flags.parent {
        flagged = true;
        dbg!();
        let target_parent = mapper.src_arena.parent(&mapping_target);
        let target_parent = target_parent.map(|x| mapper.src_arena.original(&x));
        let mapped_parent = mapper.dst_arena.parent(&mapped);
        let mapped_parent = mapped_parent.map(|x| mapper.dst_arena.original(&x));
        triggered |= target_parent != mapped_parent;
    }
    // TODO add flags for artefacts (tests, prod code, build, lang, misc)
    // TODO add flags for similarity comps

    let matched = reconstruct_mapped(with_spaces_stores, &mut mapper.mapping.dst_arena, mapped);
    let matched = postprocess_matching(matched);

    let matches = vec![matched];
    let src = compute_local2(target, with_spaces_stores);
    dbg!(target.root(), &src.path_ids);
    if flagged && !triggered {
        let other_tr = mapper.dst_arena.original(&mapper.dst_arena.root());
        let nodes = MappingTracker::new(mapper.hyperast).size(&other_tr, &target.root());
        MappingResult::Skipped {
            nodes,
            src,
            next: matches,
        }
    } else {
        MappingResult::Direct { src, matches }
    }
}

fn reconstruct_mapped<IdD>(
    with_spaces_stores: &SimpleStores<TStore>,
    arena: &mut Decompressible<&NoSpaceStore<'_, '_>, &mut LazyPostOrder<super::IdN, IdD>>,
    mapped: IdD,
) -> LocalPieceOfCode<super::IdN, Idx>
where
    IdD: PrimInt + Shallow<IdD>,
{
    let tr = arena.original(&arena.root());
    let matched = arena.decompress_to(&mapped);
    let dst_tree = &arena;
    let path_no_spaces = dst_tree.path_rooted(&matched);

    dbg!(dst_tree.original(&matched));
    let mut path_ids = vec![dst_tree.original(&matched)];
    dbg!(
        &(dst_tree.parents(matched))
            .map(|i| dst_tree.original(&i))
            .collect::<Vec<_>>()
    );
    path_ids.extend(dst_tree.parents(matched).map(|i| dst_tree.original(&i)));
    path_ids.pop();

    assert_eq!(path_no_spaces.len(), path_ids.len());
    let (path, _) = path_with_spaces(tr, &mut path_no_spaces.iter().copied(), with_spaces_stores);
    let offsets = &mut path.iter().copied();
    let (pos, mapped_node) = compute_position(tr, offsets, with_spaces_stores);
    LocalPieceOfCode::from_position(&pos, path.clone(), path_ids.clone())
}

type NoSpaceStore<'a, 'store> = hyperast::store::SimpleStores<
    TStore,
    no_space::NoSpaceNodeStoreWrapper<'store>,
    &'a hyperast::store::labels::LabelStore,
>;

fn compute_mappings_full<'store, 'alone, 'tree, 'rest, 's: 'tree, HAST: HyperAST + Copy, M, MM>(
    mappings_alone: &'alone MappingAloneCache<HAST::IdN, M>,
    mapper: &mut Mapper<
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
    let entry = match mappings_alone.entry((
        mapper.src_arena.original(&mapper.src_arena.root()),
        mapper.dst_arena.original(&mapper.dst_arena.root()),
    )) {
        Entry::Occupied(occ) => return occ.into_ref().downgrade(),
        Entry::Vacant(vac) => vac,
    };
    let mm = if let Some(mm) = partial {
        use mapping_store::MappingStore;
        mapper.mapping.mappings.topit(
            mapper.mapping.src_arena.len(),
            mapper.mapping.dst_arena.len(),
        );
        mm
    } else {
        use mapping_store::MappingStore;
        use mapping_store::VecStore;
        mapper.mapping.mappings.topit(
            mapper.mapping.src_arena.len(),
            mapper.mapping.dst_arena.len(),
        );

        let now = std::time::Instant::now();
        let mm = matching::LazyGreedySubtreeMatcher::<_>::compute_multi_mapping::<MM>(mapper);
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

const CONST_NODE_COUNTING: Option<usize> = Some(500_000);

fn track_greedy<'s, C, P>(
    with_spaces_stores: &'s SimpleStores<TStore>,
    stores: &'s NoSpaceStore<'_, '_>,
    src_tree: &mut DecompressedTree,
    dst_tree: &mut DecompressedTree,
    subtree_mappings: &mapping_store::MultiVecStore<IdD>,
    flags: &Flags,
    target: &P,
    postprocess_matching: &impl Fn(LocalPieceOfCode<super::IdN, super::Idx>) -> C,
) -> Option<MappingResult<super::IdN, super::Idx, C>>
where
    P: position_accessors::SolvedPosition<super::IdN>
        + position_accessors::RootedPosition<super::IdN>
        + position_accessors::WithPreOrderOffsets<Idx = super::Idx>
        + position_accessors::OffsetPostionT<super::IdN, IdO = usize>
        + self::WithPreOrderOffsetsNoSpaces,
{
    let dst_tree = Decompressible {
        hyperast: stores,
        decomp: dst_tree,
    };
    let current_tr = target.root();
    let other_tr = dst_tree.original(&dst_tree.root());
    assert_eq!(current_tr, src_tree.original(&src_tree.root()));
    let node_store = &stores.node_store;
    let tracker_nospace = MappingTracker {
        stores: &hyperast_vcs_git::no_space::as_nospaces(with_spaces_stores),
    };
    let mut curr = src_tree.root();
    let path = target.iter_offsets_nospaces().copied().collect::<Vec<_>>();
    let mut path = &path[..];
    let flags: EnumSet<_> = flags.into();
    loop {
        // dbg!(path);
        let dsts = subtree_mappings.get_dsts(&curr);
        if let Some(value) = track_greedy_aux(
            with_spaces_stores,
            flags,
            target,
            &dst_tree,
            current_tr,
            other_tr,
            &tracker_nospace,
            dsts,
            path,
            postprocess_matching,
        ) {
            // found
            return Some(value);
        }

        let Some(i) = path.first() else {
            break;
        };
        path = &path[1..];
        let cs = Decompressible {
            hyperast: stores,
            decomp: &mut *src_tree,
        }
        .decompress_children(&curr);
        if cs.is_empty() {
            break;
        }
        curr = cs[*i as usize];
    }
    None
}

fn track_greedy_aux<'s, C, P>(
    with_spaces_stores: &'s SimpleStores<TStore>,
    flags: EnumSet<FlagsE>,
    target: &P,
    dst_tree: &Decompressible<&NoSpaceStore<'_, '_>, &mut DecompressedTree>,
    current_tr: super::IdN,
    other_tr: super::IdN,
    tracker_nospace: &MappingTracker<'s, NoSpaceStore<'_, '_>>,
    mappeds: &[IdD],
    // remaining path to `target`
    path: &[super::Idx],
    postprocess_matching: &impl Fn(LocalPieceOfCode<super::IdN, super::Idx>) -> C,
) -> Option<MappingResult<super::IdN, super::Idx, C>>
where
    P: position_accessors::SolvedPosition<super::IdN>
        + position_accessors::RootedPosition<super::IdN>
        + position_accessors::WithPreOrderOffsets<Idx = super::Idx>
        + position_accessors::OffsetPostionT<super::IdN, IdO = usize>
        + self::WithPreOrderOffsetsNoSpaces,
{
    let curr_flags = FlagsE::Upd | FlagsE::Child | FlagsE::SimChild;
    //  | FlagsE::ExactChild
    let parent_flags = curr_flags | FlagsE::Parent | FlagsE::SimParent;
    //  | FlagsE::ExactParent
    if mappeds.is_empty() {
        // no mappeds
        // continue through path_to_target
        // dbg!(curr);
        return None;
    }
    if path.is_empty() && flags.is_subset(curr_flags) {
        // only trigger on curr and children changed
        let nodes =
            CONST_NODE_COUNTING.unwrap_or_else(|| tracker_nospace.size(&other_tr, &current_tr));

        let next = mappeds
            .iter()
            .map(|x| {
                let path_dst = dst_tree.path_rooted(x);
                let (path_dst, _) =
                    path_with_spaces(other_tr, &mut path_dst.iter().copied(), with_spaces_stores);
                postprocess_matching(compute_local(other_tr, &path_dst, with_spaces_stores))
            })
            .collect();
        let src = compute_local2(target, with_spaces_stores);
        // src: {
        //     let (pos, path_ids) = compute_position_and_nodes(
        //         current_tr,
        //         &mut target.iter_offsets(),
        //         with_spaces_stores,
        //     );

        //     LocalPieceOfCode::from_file_and_range(
        //         pos.file(),
        //         target.start()..target.end(),
        //         target.iter_offsets().collect(),
        //         path_ids,
        //     )
        // },
        return Some(MappingResult::Skipped { nodes, src, next });
        // also the type of src and dsts
        // also check it file path changed
        // can we test if parent changed ? at least we can check some attributes
    }
    if !flags.is_subset(parent_flags) {
        // nothing to do on parents
        return None;
    }
    if path.len() == 1 {
        // only trigger on parent, curr and children changed
        let nodes =
            CONST_NODE_COUNTING.unwrap_or_else(|| tracker_nospace.size(&other_tr, &current_tr));
        let next = mappeds
            .iter()
            .map(|x| {
                let mut path_dst = dst_tree.path_rooted(x);
                path_dst.extend(path); // WARN with similarity it might not be possible to simply concat path...
                let (path_dst, _) =
                    path_with_spaces(other_tr, &mut path_dst.iter().copied(), with_spaces_stores);
                let local = compute_local(other_tr, &path_dst, with_spaces_stores);
                dbg!(&local.path_ids);
                postprocess_matching(local)
            })
            .collect();
        let src = compute_local2(target, with_spaces_stores);
        // src: {
        //     let (pos, path_ids) = compute_position_and_nodes(
        //         current_tr,
        //         &mut target.iter_offsets(),
        //         with_spaces_stores,
        //     );

        //     let path = target.iter_offsets().collect();
        //     LocalPieceOfCode::from_position(&pos, path, path_ids)
        // },
        return Some(MappingResult::Skipped { nodes, src, next });
        // also the type of src and dsts
        // also check if file path changed
        // can we test if parent changed ? at least we can ckeck some attributes
    }
    // need to check the type of src and dsts
    // only trigger on parent, curr and children changed
    let nodes = CONST_NODE_COUNTING.unwrap_or_else(|| tracker_nospace.size(&other_tr, &current_tr));
    let next = mappeds
        .iter()
        .map(|x| {
            let mut path_dst = dst_tree.path_rooted(x);
            path_dst.extend(path); // WARN with similarity it might not be possible to simply concat path...
            let (path_dst, _) =
                path_with_spaces(other_tr, &mut path_dst.iter().copied(), with_spaces_stores);
            postprocess_matching(compute_local(other_tr, &path_dst, with_spaces_stores))
        })
        .collect();
    let src = compute_local2(target, with_spaces_stores);
    return Some(MappingResult::Skipped { nodes, src, next });
    // also check if file path changed
    // can we test if parent changed ? at least we can ckeck some attributes
}

fn compute_local(
    tr: super::IdN,
    path: &[super::Idx],
    with_spaces_stores: &SimpleStores<TStore>,
) -> LocalPieceOfCode<super::IdN, super::Idx> {
    let (pos, path_ids) =
        compute_position_and_nodes(tr, &mut path.iter().copied(), with_spaces_stores);
    let path = path.to_vec();
    LocalPieceOfCode::from_position(&pos, path, path_ids)
}

fn compute_local2<P>(
    path: &P,
    store: &SimpleStores<TStore>,
) -> LocalPieceOfCode<super::IdN, super::Idx>
where
    P: position_accessors::WithPreOrderOffsets<Idx = super::Idx>
        + position_accessors::RootedPosition<super::IdN>,
{
    let tr = path.root();
    let (pos, path_ids) = compute_position_and_nodes(tr, &mut path.iter_offsets(), store);
    let path = path.iter_offsets().collect();
    LocalPieceOfCode::from_position(&pos, path, path_ids)
}
