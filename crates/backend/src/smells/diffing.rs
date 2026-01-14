use super::Diff;
use super::Idx;
use super::Pos;
use super::*;
use hyper_diff::actions::action_tree::ActionsTree;
use hyper_diff::actions::action_vec::ActionsVec;
use hyper_diff::actions::script_generator2::Act;
use hyper_diff::actions::script_generator2::ScriptGenerator;
use hyper_diff::actions::script_generator2::SimpleAction;
use hyper_diff::algorithms::DiffResult;
use hyper_diff::decompressed_tree_store::bfs_wrapper::SimpleBfsMapper;
use hyper_diff::decompressed_tree_store::complete_post_order_ref;
use hyper_diff::matchers::Decompressible;
use hyper_diff::tree::tree_path::CompressedTreePath;
use hyperast::store::defaults::LabelIdentifier;
use hyperast::store::defaults::NodeIdentifier;
use hyperast::store::labels::LabelStore;
use hyperast::types::HyperAST;
use hyperast_vcs_git::no_space::NoSpaceWrapper;

pub(crate) struct T;

impl hyperast::types::Node for T {}

impl hyperast::types::Stored for T {
    type TreeId = NodeIdentifier;
}

impl<'a> hyperast::types::CLending<'a, u16, NodeIdentifier> for T {
    type Children = hyperast::types::ChildrenSlice<'a, NodeIdentifier>;
}

impl hyperast::types::WithChildren for T {
    type ChildIdx = u16;

    // type Children<'a>
    //     = hyperast::types::MySlice<NodeIdentifier>
    // where
    //     Self: 'a;

    fn child_count(&self) -> Self::ChildIdx {
        todo!()
    }

    fn child(
        &self,
        idx: &Self::ChildIdx,
    ) -> Option<<Self::TreeId as hyperast::types::NodeId>::IdN> {
        todo!()
    }

    fn child_rev(
        &self,
        idx: &Self::ChildIdx,
    ) -> Option<<Self::TreeId as hyperast::types::NodeId>::IdN> {
        todo!()
    }

    fn children(
        &self,
    ) -> Option<
        hyperast::types::LendC<
            '_,
            Self,
            Self::ChildIdx,
            <Self::TreeId as hyperast::types::NodeId>::IdN,
        >,
    > {
        todo!()
    }
}

impl hyperast::types::Labeled for T {
    type Label = LabelIdentifier;

    fn get_label_unchecked(&self) -> &Self::Label {
        todo!()
    }

    fn try_get_label(&self) -> Option<&Self::Label> {
        todo!()
    }
}

pub(crate) fn diff(
    state: std::sync::Arc<crate::AppState>,
    repo_handle: &impl hyperast_vcs_git::processing::ConfiguredRepoTrait<
        Config = hyperast_vcs_git::processing::ParametrizedCommitProcessorHandle,
    >,
    src_oid: hyperast_vcs_git::git::Oid,
    dst_oid: hyperast_vcs_git::git::Oid,
) -> Result<Diff, String> {
    let repositories = state.repositories.read().unwrap();
    let commit_src = repositories
        .get_commit(repo_handle.config(), &src_oid)
        .unwrap();
    let src_tr = commit_src.ast_root;
    let commit_dst = repositories
        .get_commit(repo_handle.config(), &dst_oid)
        .unwrap();
    let dst_tr = commit_dst.ast_root;

    if src_tr == dst_tr {
        return Ok(Diff {
            focuses: Default::default(),
            deletes: Default::default(),
            inserts: Default::default(),
            moves: Default::default(),
        });
    }

    let with_spaces_stores = &repositories.processor.main_stores;
    let stores = &hyperast_vcs_git::no_space::as_nospaces(with_spaces_stores);

    let diff = hyper_diff::algorithms::gumtree_stable_hybrid_lazy::diff(
        // hyper_diff::algorithms::gumtree_stable_lazy::diff(
        stores, &src_tr, &dst_tr,
    );
    let mapper = diff.mapper;
    let actions = diff.actions.unwrap();
    // let actions = _diff(&state, src_tr, dst_tr, with_spaces_stores, stores)?
    //     .actions
    //     .unwrap();
    dbg!(&actions.len());

    enum Choice {
        Del,
        Mov,
        Mov2,
        Ins,
        Upd,
        Mov2Del,
    }
    let choice = Choice::Mov2Del;
    let mut focuses = vec![];
    let mut deletes = vec![];
    let mut inserts = vec![];
    let moves = if let Choice::Del = choice {
        extract_deletes(with_spaces_stores, stores, src_tr, dst_tr, &actions).collect()
    } else if let Choice::Ins = choice {
        extract_inserts(with_spaces_stores, stores, src_tr, dst_tr, &actions).collect()
    } else if let Choice::Upd = choice {
        extract_updates(with_spaces_stores, stores, src_tr, dst_tr, &actions).collect()
    } else if let Choice::Mov = choice {
        extract_moves(with_spaces_stores, stores, src_tr, dst_tr, &actions).collect()
    } else if let Choice::Mov2 = choice {
        extract_moves2(with_spaces_stores, stores, src_tr, dst_tr, &actions).collect()
    } else if let Choice::Mov2Del = choice {
        let foc = extract_focuses(with_spaces_stores, stores, src_tr, dst_tr, &actions);
        focuses = foc.collect();
        let dels = extract_deletes(with_spaces_stores, stores, src_tr, dst_tr, &actions);
        deletes = dels.map(|x| x.0).collect();
        let ins = extract_inserts(with_spaces_stores, stores, src_tr, dst_tr, &actions);
        inserts = ins.map(|x| x.1).collect();
        let movs = extract_moves2(with_spaces_stores, stores, src_tr, dst_tr, &actions);
        movs.collect()
    } else {
        unreachable!()
    };

    Ok(Diff {
        focuses,
        deletes,
        inserts,
        moves,
    })
}

fn _diff(
    state: &crate::AppState,
    src_tr: NodeIdentifier,
    dst_tr: NodeIdentifier,
    with_spaces_stores: &hyperast::store::SimpleStores<hyperast_vcs_git::TStore>,
    stores: &hyperast::store::SimpleStores<
        hyperast_vcs_git::TStore,
        hyperast_vcs_git::no_space::NoSpaceNodeStoreWrapper<'_>,
        &LabelStore,
    >,
) -> Result<
    DiffResult<SimpleAction<LabelIdentifier, CompressedTreePath<u16>, NodeIdentifier>, (), ()>,
    // ActionsVec<SimpleAction<LabelIdentifier, CompressedTreePath<u16>, NodeIdentifier>>,
    String,
> {
    let binding = crate::utils::bind_tree_pair(&state.partial_decomps, &src_tr, &dst_tr);
    use hyper_diff::decompressed_tree_store::ShallowDecompressedTreeStore;
    use hyperast::types::WithStats;
    let mapped = {
        let mappings_cache = &state.mappings_alone;
        use hyper_diff::matchers::mapping_store;
        use mapping_store::MappingStore;
        use mapping_store::VecStore;

        let hyperast = stores;
        use hyper_diff::matchers::Mapping;

        dbg!();
        match mappings_cache.entry((src_tr, dst_tr)) {
            dashmap::mapref::entry::Entry::Occupied(entry) => entry.into_ref().downgrade(),
            dashmap::mapref::entry::Entry::Vacant(entry) => {
                // std::collections::hash_map::Entry::Vacant(entry) => {
                let mappings = VecStore::default();
                let mut locked = binding.lock();
                let (src_arena, dst_arena) = locked.as_mut(stores);
                dbg!(src_arena.len());
                dbg!(dst_arena.len());
                let src_size = stores.node_store.resolve(src_tr).size();
                let dst_size = stores.node_store.resolve(dst_tr).size();
                dbg!(src_size);
                dbg!(dst_size);
                let mut mapper = hyper_diff::matchers::Mapper {
                    hyperast,
                    mapping: Mapping {
                        src_arena: Decompressible {
                            hyperast,
                            decomp: src_arena,
                        },
                        dst_arena: Decompressible {
                            hyperast,
                            decomp: dst_arena,
                        },
                        mappings,
                    },
                };
                dbg!();
                dbg!(mapper.mapping.src_arena.len());
                dbg!(mapper.mapping.dst_arena.len());
                mapper.mapping.mappings.topit(
                    mapper.mapping.src_arena.len(),
                    mapper.mapping.dst_arena.len(),
                );
                crate::matching::full2(&mut mapper);

                // TODO match decls by sig/path

                let vec_store = mapper.mappings.clone();

                dbg!();
                entry
                    .insert((crate::MappingStage::Bottomup, vec_store))
                    .downgrade()
            }
        }
    };
    let mut locked = binding.lock();
    let (src_arena, dst_arena) = locked.as_mut(stores);
    dbg!();
    let mut src_arena = Decompressible {
        hyperast: stores,
        decomp: src_arena,
    };
    let mut dst_arena = Decompressible {
        hyperast: stores,
        decomp: dst_arena,
    };
    src_arena.complete_subtree(&src_arena.root());
    let src_arena = complete_post_order_ref::CompletePostOrder::from(&*src_arena.decomp);
    dbg!();
    dst_arena.complete_subtree(&dst_arena.root());
    let dst_arena = complete_post_order_ref::CompletePostOrder::from(&*dst_arena.decomp);
    dbg!();
    let dst_arena = Decompressible {
        hyperast: stores,
        decomp: dst_arena,
    };
    let dst_arena = SimpleBfsMapper::with_store(stores, dst_arena);
    dbg!();
    let ms = &mapped.1;
    let src_arena = Decompressible {
        hyperast: stores,
        decomp: src_arena,
    };
    let mapping = hyper_diff::matchers::Mapping {
        src_arena,
        dst_arena,
        mappings: ms.clone(),
    };

    let mut this = ScriptGenerator::new(stores, &mapping.src_arena, &mapping.dst_arena)
        .init_cpy(&mapping.mappings);
    this.auxilary_ins_mov_upd(&|w, x| assert_eq!(stores.resolve_type(w), stores.resolve_type(x)))?;
    this.del();

    Ok(DiffResult {
        mapper: (),
        actions: Some(this.actions),
        exec_data: (),
    })
}

pub(crate) type A = SimpleAction<LabelIdentifier, CompressedTreePath<u16>, NodeIdentifier>;

pub(crate) fn extract_moves<'a>(
    with_spaces_stores: &'a hyperast::store::SimpleStores<hyperast_vcs_git::TStore>,
    stores: &'a Stores,
    src_tr: NodeIdentifier,
    dst_tr: NodeIdentifier,
    actions: &'a ActionsVec<A>,
) -> impl Iterator<Item = (Pos, Pos)> + 'a {
    let mut result = vec![];
    let mut a_tree = ActionsTree::new();
    for a in actions.0.iter() {
        if let Act::Move { from } = &a.action {
            dbg!(from.ori.iter().count(), a.path.ori.iter().count());
            let (_, w) = hyperast::position::path_with_spaces(
                src_tr,
                &mut from.ori.iter(),
                with_spaces_stores,
            );
            let (_, x) = hyperast::position::path_with_spaces(
                dst_tr,
                &mut a.path.ori.iter(),
                with_spaces_stores,
            );
            assert_eq!(
                hyperast::types::HyperAST::resolve_type(stores, &w),
                hyperast::types::HyperAST::resolve_type(stores, &x)
            );

            a_tree.merge_ori(a);
        }
    }
    // eprintln!("{:?}", a_tree.inspect());
    use hyperast::types::HyperType;
    go_to_files(
        stores,
        &a_tree.atomics,
        hyperast::position::StructuralPosition::new(dst_tr),
        &mut |p, nn, n, id| {
            let t = hyperast::types::HyperAST::resolve_type(stores, &id);
            // dbg!(t.as_static_str(), p);
            // if t.is_hidden() {
            //     return false
            // }
            let Act::Move { from } = &n.action.action else {
                unreachable!();
            };
            dbg!(from.ori.iter().count(), p.iter_offsets().count());
            result.push((p.clone(), from.ori.clone()));
            false
        },
    );

    dbg!(&result.len());

    result.into_iter().filter_map(move |(to, from)| {
        dbg!(&from);
        dbg!(from.iter().count());
        let (from_path, f_id) =
            hyperast::position::path_with_spaces(src_tr, &mut from.iter(), with_spaces_stores);
        dbg!(from_path.len());
        let (from, _from) = hyperast::position::compute_position(
            src_tr,
            &mut from_path.iter().copied(),
            with_spaces_stores,
        );
        dbg!(f_id);

        dbg!(to.node());
        let t_t = hyperast::types::HyperAST::resolve_type(stores, &to.node());
        let tr = to.root();
        dbg!(&to);
        let t0 = to.iter_offsets().count();
        let to_path =
            hyperast::position::path_with_spaces(tr, &mut to.iter_offsets(), with_spaces_stores).0;
        let t1 = to_path.len();
        let (to, _to) = hyperast::position::compute_position(
            tr,
            &mut to_path.iter().copied(),
            with_spaces_stores,
        );
        dbg!(_to);

        let t_f = hyperast::types::HyperAST::resolve_type(stores, &f_id);
        dbg!(t0, t1);
        assert_eq!(t_f, t_t);

        let t_f = hyperast::types::HyperAST::resolve_type(stores, &_from);
        let t_t = hyperast::types::HyperAST::resolve_type(stores, &_to);
        if t_f != t_t {
            dbg!(t_f.as_static_str(), t_t.as_static_str());
            return None;
        }
        Some(((to, to_path), (from, from_path)))
    })
}

pub(crate) fn extract_moves2<'a>(
    with_spaces_stores: &'a hyperast::store::SimpleStores<hyperast_vcs_git::TStore>,
    stores: &'a Stores,
    src_tr: NodeIdentifier,
    dst_tr: NodeIdentifier,
    actions: &'a ActionsVec<A>,
) -> impl Iterator<Item = (Pos, Pos)> + 'a {
    actions.0.iter().filter_map(move |a| {
        let from = match &a.action {
            Act::Move { from } => from,
            Act::MovUpd { from, .. } => from,
            Act::Update { before, .. } => before,
            // Act::Insert { sub } => todo!(),
            _ => return None,
        };
        let (from_path, w) =
            hyperast::position::path_with_spaces(src_tr, &mut from.ori.iter(), with_spaces_stores);
        let t = hyperast::types::HyperAST::resolve_type(stores, &w);
        use hyperast::types::HyperType;
        if t.is_file() || t.is_directory() {
            return None;
        }
        // if t.is_hidden() || !t.is_named() || {
        //     dbg!(t.as_static_str());
        //     return None;
        // }
        // if t.as_static_str() != "method_declaration" && t.as_static_str() != "_method_header" {
        //     dbg!(t.as_static_str());
        //     return None;
        // }
        let (to_path, x) = hyperast::position::path_with_spaces(
            dst_tr,
            &mut a.path.ori.iter(),
            with_spaces_stores,
        );
        let (from, _from) = hyperast::position::compute_position(
            src_tr,
            &mut from_path.iter().copied(),
            with_spaces_stores,
        );
        assert_eq!(w, _from);

        let (to, _to) = hyperast::position::compute_position(
            dst_tr,
            &mut to_path.iter().copied(),
            with_spaces_stores,
        );
        assert_eq!(x, _to);

        let t_f = hyperast::types::HyperAST::resolve_type(stores, &_from);

        if t_f.is_syntax() {
            return None;
        }
        // if t_f.is_supertype() {
        //     return None;
        // }
        let t_t = hyperast::types::HyperAST::resolve_type(stores, &_to);
        if t_f != t_t {
            dbg!(t_f.as_static_str(), t_t.as_static_str());
            return None;
        }
        Some(((to, to_path), (from, from_path)))
    })
}

pub(crate) fn extract_updates<'a>(
    with_spaces_stores: &'a hyperast::store::SimpleStores<hyperast_vcs_git::TStore>,
    stores: &'a Stores,
    _src_tr: NodeIdentifier,
    dst_tr: NodeIdentifier,
    actions: &'a ActionsVec<A>,
) -> impl Iterator<Item = (Pos, Pos)> + 'a {
    let mut result = vec![];
    let mut a_tree = ActionsTree::new();
    for a in actions.0.iter() {
        if let Act::Update { .. } = &a.action {
            a_tree.merge_ori(a);
        }
    }
    // eprintln!("{:?}", a_tree.inspect());
    use hyperast::types::HyperType;
    go_to_files(
        stores,
        &a_tree.atomics,
        hyperast::position::StructuralPosition::new(dst_tr),
        &mut |p, nn, n, id| {
            let t = stores.resolve_type(&id);
            dbg!(t.as_static_str(), p);
            result.push(p.clone());
            false
        },
    );

    dbg!(&result.len());

    result
        .into_iter()
        .map(move |path| {
            let tr = path.root();
            let path = hyperast::position::path_with_spaces(
                tr,
                &mut path.iter_offsets(),
                with_spaces_stores,
            )
            .0;
            let (pos, _) = hyperast::position::compute_position(
                tr,
                &mut path.iter().copied(),
                with_spaces_stores,
            );
            (pos, path)
        })
        .map(|x| (x.clone(), x))
}
pub(crate) fn extract_updates2<'a>(
    with_spaces_stores: &'a hyperast::store::SimpleStores<hyperast_vcs_git::TStore>,
    stores: &'a Stores,
    src_tr: NodeIdentifier,
    dst_tr: NodeIdentifier,
    actions: &'a ActionsVec<A>,
) -> impl Iterator<Item = (Pos, Pos)> + 'a {
    actions.0.iter().filter_map(move |a| {
        let from = match &a.action {
            Act::Update { .. } => (),
            _ => return None,
        };
        let (from_path, w) = hyperast::position::path_with_spaces(
            src_tr,
            &mut a.path.ori.iter(),
            with_spaces_stores,
        );
        let t = hyperast::types::HyperAST::resolve_type(stores, &w);
        use hyperast::types::HyperType;
        if t.is_file() || t.is_directory() {
            return None;
        }
        // if t.is_hidden() || !t.is_named() || {
        //     dbg!(t.as_static_str());
        //     return None;
        // }
        // if t.as_static_str() != "method_declaration" && t.as_static_str() != "_method_header" {
        //     dbg!(t.as_static_str());
        //     return None;
        // }
        let (to_path, x) = hyperast::position::path_with_spaces(
            dst_tr,
            &mut a.path.ori.iter(),
            with_spaces_stores,
        );
        let (from, _from) = hyperast::position::compute_position(
            src_tr,
            &mut from_path.iter().copied(),
            with_spaces_stores,
        );
        assert_eq!(w, _from);

        let (to, _to) = hyperast::position::compute_position(
            dst_tr,
            &mut to_path.iter().copied(),
            with_spaces_stores,
        );
        // dbg!(_to);
        assert_eq!(x, _to);

        let t_f = hyperast::types::HyperAST::resolve_type(stores, &_from);
        let t_t = hyperast::types::HyperAST::resolve_type(stores, &_to);
        if t_f != t_t {
            dbg!(t_f.as_static_str(), t_t.as_static_str());
            return None;
        }
        // dbg!(t_f.as_static_str());
        Some(((to, to_path), (from, from_path)))
    })
}

pub(crate) fn extract_inserts<'a>(
    with_spaces_stores: &'a hyperast::store::SimpleStores<hyperast_vcs_git::TStore>,
    stores: &'a Stores,
    _src_tr: NodeIdentifier,
    dst_tr: NodeIdentifier,
    actions: &'a ActionsVec<A>,
) -> impl Iterator<Item = (Pos, Pos)> + 'a {
    let mut result = vec![];
    let mut a_tree = ActionsTree::new();
    for a in actions.0.iter() {
        if let Act::Insert { .. } = &a.action {
            a_tree.merge_ori(a);
        }
    }
    use hyperast::types::HyperType;
    go_to_files(
        stores,
        &a_tree.atomics,
        hyperast::position::StructuralPosition::new(dst_tr),
        &mut |p, nn, n, id| {
            result.push(p.clone());
            false
        },
    );

    dbg!(&result.len());

    result
        .into_iter()
        .map(move |path| {
            let tr = path.root();
            let path = hyperast::position::path_with_spaces(
                tr,
                &mut path.iter_offsets(),
                with_spaces_stores,
            )
            .0;
            let (pos, _) = hyperast::position::compute_position(
                tr,
                &mut path.iter().copied(),
                with_spaces_stores,
            );
            (pos, path)
        })
        .map(|x| (x.clone(), x))
}

pub(crate) fn extract_deletes<'a>(
    with_spaces_stores: &'a hyperast::store::SimpleStores<hyperast_vcs_git::TStore>,
    stores: &'a Stores,
    src_tr: NodeIdentifier,
    _dst_tr: NodeIdentifier,
    actions: &'a ActionsVec<A>,
) -> impl Iterator<Item = (Pos, Pos)> + 'a {
    let mut result = vec![];
    let mut a_tree = ActionsTree::new();
    for a in actions.0.iter().rev() {
        if let Act::Delete { .. } = &a.action {
            // let (to_path, x) = hyperast::position::path_with_spaces(
            //     src_tr,
            //     &mut a.path.ori.iter(),
            //     with_spaces_stores,
            // );
            // let t = with_spaces_stores.resolve_type(&x);
            // dbg!(t);
            // let del = hyperast::nodes::TextSerializer::new(with_spaces_stores, x).to_string();
            // println!("ddel: {}", del);
            a_tree.merge_ori(a);
        }
    }
    // eprintln!("{:?}", a_tree.inspect());
    use hyperast::types::HyperType;
    go_to_files(
        stores,
        &a_tree.atomics, // , &mapping
        hyperast::position::StructuralPosition::new(src_tr),
        &mut |p, nn, n, id| {
            let t = stores.resolve_type(&id);
            if t.is_syntax() {
                return false;
            }
            // dbg!(t);
            // let del = hyperast::nodes::TextSerializer::new(with_spaces_stores, id);
            // println!("del : {}", del);
            result.push(p.clone());
            false
        },
    );

    dbg!(&result.len());

    result
        .into_iter()
        .map(move |path| {
            let tr = path.root();
            let path = hyperast::position::path_with_spaces(
                tr,
                &mut path.iter_offsets(),
                with_spaces_stores,
            )
            .0;
            let (pos, _) = hyperast::position::compute_position(
                tr,
                &mut path.iter().copied(),
                with_spaces_stores,
            );
            (pos, path)
        })
        .map(|x| (x.clone(), x))
}

pub(crate) fn extract_focuses<'a>(
    with_spaces_stores: &'a hyperast::store::SimpleStores<hyperast_vcs_git::TStore>,
    stores: &'a Stores,
    src_tr: NodeIdentifier,
    dst_tr: NodeIdentifier,
    actions: &'a ActionsVec<A>,
) -> impl Iterator<Item = (Pos, Pos)> + 'a {
    let mut a_tree = ActionsTree::new();
    for a in actions.0.iter().rev() {
        let from = match &a.action {
            Act::Delete { .. } => a_tree.merge_ori(a),
            Act::Move { .. } => a_tree.merge_ori(a),
            // Act::Move { from } | Act::MovUpd { from, .. } => a_tree.merge_ori(&SimpleAction {
            //     path: a.path.clone(),
            //     action: Act::Delete {},
            // }),
            _ => (),
        };
    }
    // eprintln!("{:?}", a_tree.inspect());
    use hyperast::types::HyperType;
    let composed = a_tree.composed;
    a_tree.atomics.into_iter().filter_map(move |atomic| {
        let mut already = None;
        let mut moves = vec![];
        go_to_files_aux(
            stores,
            &atomic,
            &hyperast::position::StructuralPosition::new(src_tr),
            &mut |p, nn, n, id| {
                if let Act::Delete { .. } = n.action.action {
                    if with_spaces_stores.resolve_type(&id).is_syntax() {
                        return false;
                    };
                    if already.is_none() {
                        already = Some(p.clone());
                    }
                    if already.is_some() && n.composed_offset != u32::MAX {
                        moves.push(n.composed_offset);
                    }
                    false
                } else {
                    false
                }
            },
        );
        let Some(already) = already else {
            return None;
        };
        let mut agg_pos = None;
        let mut first_path = None;
        for m in moves {
            let a = &composed[m as usize];
            let (to_path, x) = hyperast::position::path_with_spaces(
                dst_tr,
                &mut a.path.ori.iter(),
                with_spaces_stores,
            );

            let (to, _to) = hyperast::position::compute_position(
                dst_tr,
                &mut to_path.iter().copied(),
                with_spaces_stores,
            );
            if first_path.is_none() {
                first_path = Some(to_path);
                agg_pos = Some(to);
            } else if let Some(agg_pos) = &mut agg_pos {
                agg_pos.try_merge(to)
            }
        }
        let already = {
            let path = already;
            let tr = path.root();
            let path = hyperast::position::path_with_spaces(
                tr,
                &mut path.iter_offsets(),
                with_spaces_stores,
            )
            .0;
            let (pos, _) = hyperast::position::compute_position(
                tr,
                &mut path.iter().copied(),
                with_spaces_stores,
            );
            (pos, path)
        };
        if let Some((first_path, agg_pos)) = first_path.zip(agg_pos) {
            Some(((agg_pos, first_path), already))
        } else {
            Some((already.clone(), already))
        }
    })

    // dbg!(&result.len());

    // result
    //     .into_iter()
    //     .map(move |path| {
    //         let tr = path.root();
    //         let path = hyperast::position::path_with_spaces(
    //             tr,
    //             &mut path.iter_offsets(),
    //             with_spaces_stores,
    //         )
    //         .0;
    //         let (pos, _) = hyperast::position::compute_position(
    //             tr,
    //             &mut path.iter().copied(),
    //             with_spaces_stores,
    //         );
    //         (pos, path)
    //     })
    //     .map(|x| (x.clone(), x))
}

pub(crate) type _R = hyperast::position::structural_pos::StructuralPosition<NodeIdentifier, u16>;

pub(crate) type Stores<'a> = hyperast::store::SimpleStores<
    hyperast_vcs_git::TStore,
    hyperast_vcs_git::no_space::NoSpaceNodeStoreWrapper<'a>,
    &'a LabelStore,
>;

pub(crate) type N = hyper_diff::actions::action_tree::Node<
    SimpleAction<LabelIdentifier, CompressedTreePath<Idx>, NodeIdentifier>,
>;

pub(crate) type P = hyperast::position::StructuralPosition;

pub(crate) fn go_to_files<F>(stores: &Stores, cs: &[N], path: P, result: &mut F)
where
    F: FnMut(&P, &NoSpaceWrapper<NodeIdentifier>, &N, NodeIdentifier) -> bool,
{
    for n in cs {
        go_to_files_aux(stores, n, &path, result);
    }
}

pub(crate) fn go_to_files_aux<F>(stores: &Stores, n: &N, path: &P, result: &mut F)
where
    F: FnMut(&P, &NoSpaceWrapper<NodeIdentifier>, &N, NodeIdentifier) -> bool,
{
    let mut path = path.clone();
    let mut p_it = n.action.path.ori.iter();
    loop {
        let Some(p) = p_it.next() else {
            break;
        };
        let id = path.node();
        let nn = stores.node_store.resolve(id);
        use hyperast::types::TypeStore;
        let t = stores.resolve_type(&id);
        use hyperast::types::HyperType;
        // dbg!(t.as_static_str());
        if t.is_file() {
            got_through(stores, n, path.clone(), p, p_it, 0, result);
            // got_through_file(stores, n, path.clone(), p, p_it, 0, result);
            return;
        }
        use hyperast::types::WithChildren;
        let cs = nn.children().unwrap();
        let node = cs.get(p).unwrap();
        path.goto(*node, p);
    }

    go_to_files(stores, &n.children, path, result);
}

pub(crate) fn got_through<F>(
    stores: &Stores,
    n: &N,
    mut path: hyperast::position::StructuralPosition,
    mut p: u16,
    mut p_it: impl std::iter::Iterator<Item = u16> + Clone,
    mut d: usize,
    result: &mut F,
) where
    F: FnMut(&P, &NoSpaceWrapper<NodeIdentifier>, &N, NodeIdentifier) -> bool,
{
    let mut id = path.node();
    let mut nn = stores.node_store.resolve(id);
    loop {
        use hyperast::types::WithChildren;
        let Some(cs) = nn.children() else {
            return; // NOTE should not happen
        };

        let Some(node) = cs.get(p) else {
            return; // NOTE should not happen
        };
        path.goto(*node, p);
        d += 1;

        id = *node;
        nn = stores.node_store.resolve(*node);

        let Some(_p) = p_it.next() else {
            break;
        };
        p = _p;
    }
    if result(&path, &nn, n, id) {
        return;
    }
    for n in &n.children {
        let mut p_it = n.action.path.ori.iter();
        // always at least one element in an action path
        let p = p_it.next().unwrap();
        got_through(stores, n, path.clone(), p, p_it, d, result);
    }
}

#[cfg(test)]
mod tests {
    use hyperast::types::{HyperType, WithChildren};
    use hyperast_vcs_git::preprocessed::child_by_name_with_idx;

    use super::*;

    #[ignore]
    #[test_log::test]
    fn test_finding_smells_gson_try_fail_catch()
    -> std::result::Result<(), Box<dyn std::error::Error>> {
        let user = "Marcono1234";
        let name = "gson";
        let repo_spec = hyperast_vcs_git::git::Forge::Github.repo(user, name);
        let config = hyperast_vcs_git::processing::RepoConfig::JavaMaven;
        let commit = "3d241ca0a6435cbf1fa1cdaed2af8480b99fecde";
        let language = "Java";

        let state = crate::AppState::default();
        state
            .repositories
            .write()
            .unwrap()
            .register_config(repo_spec.clone(), config);
        let state = std::sync::Arc::new(state);

        let repo_handle = state
            .repositories
            .write()
            .unwrap()
            .get_config(repo_spec)
            .ok_or_else(|| "missing config for repository".to_string())?;
        let mut repository = repo_handle.fetch();
        log::warn!("done cloning {}", repository.spec);

        let commits = state
            .repositories
            .write()
            .unwrap()
            .pre_process_with_limit(&mut repository, "", &commit, 4)
            .map_err(|e| e.to_string())?;
        let now = Instant::now();
        log::warn!(
            "done construction of {commits:?} in {}",
            repository.spec.user()
        );
        let src_oid = commits[0];
        let dst_oid = commits[1];
        // let diff = diff(state, &repository, dst_oid, src_oid).map_err(|e| e.to_string())?;

        let repositories = state.repositories.read().unwrap();
        let commit_src = repositories
            .get_commit(&repository.config, &src_oid)
            .unwrap();
        let src_tr = commit_src.ast_root;
        let commit_dst = repositories
            .get_commit(&repository.config, &dst_oid)
            .unwrap();
        let dst_tr = commit_dst.ast_root;

        let stores = &repositories.processor.main_stores;

        let src_tr = child_by_name_with_idx(stores, src_tr, "gson").unwrap().0;
        let dst_tr = child_by_name_with_idx(stores, dst_tr, "gson").unwrap().0;
        let src_tr = child_by_name_with_idx(stores, src_tr, "src").unwrap().0;
        let dst_tr = child_by_name_with_idx(stores, dst_tr, "src").unwrap().0;
        let src_tr = child_by_name_with_idx(stores, src_tr, "test").unwrap().0;
        let dst_tr = child_by_name_with_idx(stores, dst_tr, "test").unwrap().0;
        let src_tr = child_by_name_with_idx(stores, src_tr, "java").unwrap().0;
        let dst_tr = child_by_name_with_idx(stores, dst_tr, "java").unwrap().0;
        let src_tr = child_by_name_with_idx(stores, src_tr, "com").unwrap().0;
        let dst_tr = child_by_name_with_idx(stores, dst_tr, "com").unwrap().0;
        let src_tr = child_by_name_with_idx(stores, src_tr, "google").unwrap().0;
        let dst_tr = child_by_name_with_idx(stores, dst_tr, "google").unwrap().0;
        let src_tr = child_by_name_with_idx(stores, src_tr, "gson").unwrap().0;
        let dst_tr = child_by_name_with_idx(stores, dst_tr, "gson").unwrap().0;
        let src_tr = child_by_name_with_idx(stores, src_tr, "ToNumberPolicyTest.java")
            .unwrap()
            .0;
        let dst_tr = child_by_name_with_idx(stores, dst_tr, "ToNumberPolicyTest.java")
            .unwrap()
            .0;

        // gson/src/test/java/com/google/gson/ToNumberPolicyTest.java

        let with_spaces_stores = &repositories.processor.main_stores;
        let stores = &hyperast_vcs_git::no_space::as_nospaces(with_spaces_stores);

        // let diff = hyper_diff::algorithms::gumtree_hybrid_lazy::diff(stores, &src_tr, &dst_tr);
        let diff = hyper_diff::algorithms::gumtree_hybrid_lazy::diff_with_hyperparameters::<
            _,
            3,
            300,
            1,
            2,
        >(stores, &src_tr, &dst_tr);

        // let diff = _diff(&state, src_tr, dst_tr, with_spaces_stores, stores).unwrap();
        println!("Diff: {diff}");

        use hyper_diff::decompressed_tree_store::PostOrderIterable;

        for x in diff.mapper.mapping.src_arena.iter_df_post::<true>() {
            use hyper_diff::decompressed_tree_store::ShallowDecompressedTreeStore;
            use hyper_diff::matchers::mapping_store::MappingStore;
            let s = diff.mapper.mapping.src_arena.original(&x);
            // let t = stores.resolve_type(&s);

            let mut fmtd_ty = String::new();
            let mut n = s;
            loop {
                let t = stores.resolve_type(&n);
                if !t.is_supertype() {
                    fmtd_ty.push_str(t.as_static_str());
                    break;
                }
                fmtd_ty.push_str(t.as_static_str());
                fmtd_ty.push_str("/");
                use hyperast::types::WithChildren;
                n = stores.resolve(&n).child(&0).unwrap();
            }
            use hyperast::types::HyperType;
            if diff.mapper.mappings.is_src(&x) {
                println!("mapped {}", fmtd_ty);
            } else {
                println!("unmapped {}", fmtd_ty);
            }
        }
        Ok(())
    }

    #[ignore]
    #[test_log::test]
    fn test_wrong_diff() -> std::result::Result<(), Box<dyn std::error::Error>> {
        let mut stores =
            hyperast::store::SimpleStores::<hyperast_gen_ts_java::types::TStore>::default();
        let mut md_cache = Default::default();
        let mut java_tree_gen =
            hyperast_gen_ts_java::legion_with_refs::JavaTreeGen::new(&mut stores, &mut md_cache);
        let tree =
            match hyperast_gen_ts_java::legion_with_refs::tree_sitter_parse(BEFORE2.as_bytes()) {
                Ok(t) => t,
                Err(t) => t,
            };
        let src_tr = java_tree_gen
            .generate_file("".as_bytes(), BEFORE2.as_bytes(), tree.walk())
            .local
            .compressed_node;
        let tree =
            match hyperast_gen_ts_java::legion_with_refs::tree_sitter_parse(AFTER2.as_bytes()) {
                Ok(t) => t,
                Err(t) => t,
            };
        let dst_tr = java_tree_gen
            .generate_file("".as_bytes(), AFTER2.as_bytes(), tree.walk())
            .local
            .compressed_node;

        // let stores = stores.change_type_store::<hyperast_gen_ts_java::types::TStore>();
        let stores = &hyperast_vcs_git::no_space::as_nospaces(&stores);

        let mut diff = hyper_diff::algorithms::gumtree_stable_hybrid_lazy::diff(
            // hyper_diff::algorithms::gumtree_stable_lazy::diff(
            stores, &src_tr, &dst_tr,
        );
        type Pos2<IdN, Idx> = hyperast::position::offsets_and_nodes::StructuralPosition<IdN, Idx>;
        (diff.actions.as_ref().unwrap().iter()).for_each(|a| match &a.action {
            Act::Delete {} => eprint!("del "),
            Act::Update { .. } => eprint!("upd "),
            Act::Move { from } => {
                eprint!("mov ");
                let of =
                    hyperast::position::Offsets::from_iterator(from.ori.iter()).with_root(src_tr);
                let p = hyperast::position::PositionConverter::new(&of)
                    .with_stores(&stores)
                    .compute_pos_pre_order::<_, Pos2<IdN, Idx>>();
                eprintln!("{}", TextSerializer::new(stores, p.node()));
            }
            Act::MovUpd { from, new } => eprint!("mup "),
            Act::Insert { sub } => eprint!("ins "),
        });
        eprintln!();

        // (diff.actions.as_mut().unwrap().iter_mut()).for_each(|a| match &a.action {
        //     Act::Insert { sub } => {
        //         type Pos = hyperast::position::file_and_range::Position<std::path::PathBuf, usize>;
        //         let t = stores.resolve_type(sub);
        //         println!("{}", t.as_static_str());
        //         let mut curr = dst_tr;
        //         for x in a.path.ori.iter() {
        //             let t = stores.resolve_type(&curr);
        //             println!("{}", t.as_static_str());
        //             println!("{x}");
        //             let n = stores.node_store.resolve(curr);
        //             println!("cs count {}", n.child_count());
        //             let children = n.children().unwrap();
        //             let child = children.get(x).unwrap();
        //             curr = *child;
        //         }
        //         println!();
        //         let p = {
        //             let of = hyperast::position::Offsets::from_iterator(a.path.ori.iter())
        //                 .with_root(dst_tr);
        //             let p = hyperast::position::PositionConverter::new(&of)
        //                 .with_stores(&stores)
        //                 .compute_pos_pre_order::<_, Pos>();
        //             p.range()
        //         };
        //     }
        //     _ => {}
        // });
        eprintln!("{diff}");
        Ok(())
    }
    const BEFORE: &str = r#"
/*
 * Copyright (C) 2010 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.google.gson.stream;

import static com.google.gson.stream.JsonScope.DANGLING_NAME;
import static com.google.gson.stream.JsonScope.EMPTY_ARRAY;
import static com.google.gson.stream.JsonScope.EMPTY_DOCUMENT;
import static com.google.gson.stream.JsonScope.EMPTY_OBJECT;
import static com.google.gson.stream.JsonScope.NONEMPTY_ARRAY;
import static com.google.gson.stream.JsonScope.NONEMPTY_DOCUMENT;
import static com.google.gson.stream.JsonScope.NONEMPTY_OBJECT;

import com.google.errorprone.annotations.CanIgnoreReturnValue;
import java.io.Closeable;
import java.io.Flushable;
import java.io.IOException;
import java.io.Writer;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.regex.Pattern;

import com.google.gson.FormattingStyle;

/**
 * Writes a JSON (<a href="http://www.ietf.org/rfc/rfc7159.txt">RFC 7159</a>)
 * encoded value to a stream, one token at a time. The stream includes both
 * literal values (strings, numbers, booleans and nulls) as well as the begin
 * and end delimiters of objects and arrays.
 *
 * <h2>Encoding JSON</h2>
 * To encode your data as JSON, create a new {@code JsonWriter}. Call methods
 * on the writer as you walk the structure's contents, nesting arrays and objects
 * as necessary:
 * <ul>
 *   <li>To write <strong>arrays</strong>, first call {@link #beginArray()}.
 *       Write each of the array's elements with the appropriate {@link #value}
 *       methods or by nesting other arrays and objects. Finally close the array
 *       using {@link #endArray()}.
 *   <li>To write <strong>objects</strong>, first call {@link #beginObject()}.
 *       Write each of the object's properties by alternating calls to
 *       {@link #name} with the property's value. Write property values with the
 *       appropriate {@link #value} method or by nesting other objects or arrays.
 *       Finally close the object using {@link #endObject()}.
 * </ul>
 *
 * <h2>Example</h2>
 * Suppose we'd like to encode a stream of messages such as the following: <pre> {@code
 * [
 *   {
 *     "id": 912345678901,
 *     "text": "How do I stream JSON in Java?",
 *     "geo": null,
 *     "user": {
 *       "name": "json_newb",
 *       "followers_count": 41
 *      }
 *   },
 *   {
 *     "id": 912345678902,
 *     "text": "@json_newb just use JsonWriter!",
 *     "geo": [50.454722, -104.606667],
 *     "user": {
 *       "name": "jesse",
 *       "followers_count": 2
 *     }
 *   }
 * ]}</pre>
 * This code encodes the above structure: <pre>   {@code
 *   public void writeJsonStream(OutputStream out, List<Message> messages) throws IOException {
 *     JsonWriter writer = new JsonWriter(new OutputStreamWriter(out, "UTF-8"));
 *     writer.setIndent("    ");
 *     writeMessagesArray(writer, messages);
 *     writer.close();
 *   }
 *
 *   public void writeMessagesArray(JsonWriter writer, List<Message> messages) throws IOException {
 *     writer.beginArray();
 *     for (Message message : messages) {
 *       writeMessage(writer, message);
 *     }
 *     writer.endArray();
 *   }
 *
 *   public void writeMessage(JsonWriter writer, Message message) throws IOException {
 *     writer.beginObject();
 *     writer.name("id").value(message.getId());
 *     writer.name("text").value(message.getText());
 *     if (message.getGeo() != null) {
 *       writer.name("geo");
 *       writeDoublesArray(writer, message.getGeo());
 *     } else {
 *       writer.name("geo").nullValue();
 *     }
 *     writer.name("user");
 *     writeUser(writer, message.getUser());
 *     writer.endObject();
 *   }
 *
 *   public void writeUser(JsonWriter writer, User user) throws IOException {
 *     writer.beginObject();
 *     writer.name("name").value(user.getName());
 *     writer.name("followers_count").value(user.getFollowersCount());
 *     writer.endObject();
 *   }
 *
 *   public void writeDoublesArray(JsonWriter writer, List<Double> doubles) throws IOException {
 *     writer.beginArray();
 *     for (Double value : doubles) {
 *       writer.value(value);
 *     }
 *     writer.endArray();
 *   }}</pre>
 *
 * <p>Each {@code JsonWriter} may be used to write a single JSON stream.
 * Instances of this class are not thread safe. Calls that would result in a
 * malformed JSON string will fail with an {@link IllegalStateException}.
 *
 * @author Jesse Wilson
 * @since 1.6
 */
public class JsonWriter implements Closeable, Flushable {

  // Syntax as defined by https://datatracker.ietf.org/doc/html/rfc8259#section-6
  private static final Pattern VALID_JSON_NUMBER_PATTERN = Pattern.compile("-?(?:0|[1-9][0-9]*)(?:\\.[0-9]+)?(?:[eE][-+]?[0-9]+)?");

  /*
   * From RFC 7159, "All Unicode characters may be placed within the
   * quotation marks except for the characters that must be escaped:
   * quotation mark, reverse solidus, and the control characters
   * (U+0000 through U+001F)."
   *
   * We also escape '\u2028' and '\u2029', which JavaScript interprets as
   * newline characters. This prevents eval() from failing with a syntax
   * error. http://code.google.com/p/google-gson/issues/detail?id=341
   */
  private static final String[] REPLACEMENT_CHARS;
  private static final String[] HTML_SAFE_REPLACEMENT_CHARS;
  static {
    REPLACEMENT_CHARS = new String[128];
    for (int i = 0; i <= 0x1f; i++) {
      REPLACEMENT_CHARS[i] = String.format("\\u%04x", i);
    }
    REPLACEMENT_CHARS['"'] = "\\\"";
    REPLACEMENT_CHARS['\\'] = "\\\\";
    REPLACEMENT_CHARS['\t'] = "\\t";
    REPLACEMENT_CHARS['\b'] = "\\b";
    REPLACEMENT_CHARS['\n'] = "\\n";
    REPLACEMENT_CHARS['\r'] = "\\r";
    REPLACEMENT_CHARS['\f'] = "\\f";
    HTML_SAFE_REPLACEMENT_CHARS = REPLACEMENT_CHARS.clone();
    HTML_SAFE_REPLACEMENT_CHARS['<'] = "\\u003c";
    HTML_SAFE_REPLACEMENT_CHARS['>'] = "\\u003e";
    HTML_SAFE_REPLACEMENT_CHARS['&'] = "\\u0026";
    HTML_SAFE_REPLACEMENT_CHARS['='] = "\\u003d";
    HTML_SAFE_REPLACEMENT_CHARS['\''] = "\\u0027";
  }

  /** The JSON output destination */
  private final Writer out;

  private int[] stack = new int[32];
  private int stackSize = 0;
  {
    push(EMPTY_DOCUMENT);
  }

  /**
   * The settings used for pretty printing, or null for no pretty printing.
   */
  private FormattingStyle formattingStyle;

  /**
   * The name/value separator; either ":" or ": ".
   */
  private String separator = ":";

  private boolean lenient;

  private boolean htmlSafe;

  private String deferredName;

  private boolean serializeNulls = true;

  /**
   * Creates a new instance that writes a JSON-encoded stream to {@code out}.
   * For best performance, ensure {@link Writer} is buffered; wrapping in
   * {@link java.io.BufferedWriter BufferedWriter} if necessary.
   */
  public JsonWriter(Writer out) {
    this.out = Objects.requireNonNull(out, "out == null");
  }

  /**
   * Sets the indentation string to be repeated for each level of indentation
   * in the encoded document. If {@code indent.isEmpty()} the encoded document
   * will be compact. Otherwise the encoded document will be more
   * human-readable.
   *
   * @param indent a string containing only whitespace.
   */
  public final void setIndent(String indent) {
    if (indent.isEmpty()) {
      setFormattingStyle(null);
    } else {
      setFormattingStyle(FormattingStyle.DEFAULT.withIndent(indent));
    }
  }

  /**
   * Sets the pretty printing style to be used in the encoded document.
   * No pretty printing is done if the given style is {@code null}.
   *
   * <p>Sets the various attributes to be used in the encoded document.
   * For example the indentation string to be repeated for each level of indentation.
   * Or the newline style, to accommodate various OS styles.</p>
   *
   * <p>Has no effect if the serialized format is a single line.</p>
   *
   * @param formattingStyle the style used for pretty printing, no pretty printing if {@code null}.
   * @since $next-version$
   */
  public final void setFormattingStyle(FormattingStyle formattingStyle) {
    this.formattingStyle = formattingStyle;
    if (formattingStyle == null) {
      this.separator = ":";
    } else {
      this.separator = ": ";
    }
  }

  /**
   * Returns the pretty printing style used by this writer.
   *
   * @return the {@code FormattingStyle} that will be used.
   * @since $next-version$
   */
  public final FormattingStyle getFormattingStyle() {
    return formattingStyle;
  }

  /**
   * Configure this writer to relax its syntax rules. By default, this writer
   * only emits well-formed JSON as specified by <a
   * href="http://www.ietf.org/rfc/rfc7159.txt">RFC 7159</a>. Setting the writer
   * to lenient permits the following:
   * <ul>
   *   <li>Numbers may be {@link Double#isNaN() NaNs} or {@link
   *       Double#isInfinite() infinities}.
   * </ul>
   */
  public final void setLenient(boolean lenient) {
    this.lenient = lenient;
  }

  /**
   * Returns true if this writer has relaxed syntax rules.
   */
  public boolean isLenient() {
    return lenient;
  }

  /**
   * Configure this writer to emit JSON that's safe for direct inclusion in HTML
   * and XML documents. This escapes the HTML characters {@code <}, {@code >},
   * {@code &} and {@code =} before writing them to the stream. Without this
   * setting, your XML/HTML encoder should replace these characters with the
   * corresponding escape sequences.
   */
  public final void setHtmlSafe(boolean htmlSafe) {
    this.htmlSafe = htmlSafe;
  }

  /**
   * Returns true if this writer writes JSON that's safe for inclusion in HTML
   * and XML documents.
   */
  public final boolean isHtmlSafe() {
    return htmlSafe;
  }

  /**
   * Sets whether object members are serialized when their value is null.
   * This has no impact on array elements. The default is true.
   */
  public final void setSerializeNulls(boolean serializeNulls) {
    this.serializeNulls = serializeNulls;
  }

  /**
   * Returns true if object members are serialized when their value is null.
   * This has no impact on array elements. The default is true.
   */
  public final boolean getSerializeNulls() {
    return serializeNulls;
  }

  /**
   * Begins encoding a new array. Each call to this method must be paired with
   * a call to {@link #endArray}.
   *
   * @return this writer.
   */
  @CanIgnoreReturnValue
  public JsonWriter beginArray() throws IOException {
    writeDeferredName();
    return open(EMPTY_ARRAY, '[');
  }

  /**
   * Ends encoding the current array.
   *
   * @return this writer.
   */
  @CanIgnoreReturnValue
  public JsonWriter endArray() throws IOException {
    return close(EMPTY_ARRAY, NONEMPTY_ARRAY, ']');
  }

  /**
   * Begins encoding a new object. Each call to this method must be paired
   * with a call to {@link #endObject}.
   *
   * @return this writer.
   */
  @CanIgnoreReturnValue
  public JsonWriter beginObject() throws IOException {
    writeDeferredName();
    return open(EMPTY_OBJECT, '{');
  }

  /**
   * Ends encoding the current object.
   *
   * @return this writer.
   */
  @CanIgnoreReturnValue
  public JsonWriter endObject() throws IOException {
    return close(EMPTY_OBJECT, NONEMPTY_OBJECT, '}');
  }

  /**
   * Enters a new scope by appending any necessary whitespace and the given
   * bracket.
   */
  @CanIgnoreReturnValue
  private JsonWriter open(int empty, char openBracket) throws IOException {
    beforeValue();
    push(empty);
    out.write(openBracket);
    return this;
  }

  /**
   * Closes the current scope by appending any necessary whitespace and the
   * given bracket.
   */
  @CanIgnoreReturnValue
  private JsonWriter close(int empty, int nonempty, char closeBracket)
      throws IOException {
    int context = peek();
    if (context != nonempty && context != empty) {
      throw new IllegalStateException("Nesting problem.");
    }
    if (deferredName != null) {
      throw new IllegalStateException("Dangling name: " + deferredName);
    }

    stackSize--;
    if (context == nonempty) {
      newline();
    }
    out.write(closeBracket);
    return this;
  }

  private void push(int newTop) {
    if (stackSize == stack.length) {
      stack = Arrays.copyOf(stack, stackSize * 2);
    }
    stack[stackSize++] = newTop;
  }

  /**
   * Returns the value on the top of the stack.
   */
  private int peek() {
    if (stackSize == 0) {
      throw new IllegalStateException("JsonWriter is closed.");
    }
    return stack[stackSize - 1];
  }

  /**
   * Replace the value on the top of the stack with the given value.
   */
  private void replaceTop(int topOfStack) {
    stack[stackSize - 1] = topOfStack;
  }

  /**
   * Encodes the property name.
   *
   * @param name the name of the forthcoming value. May not be null.
   * @return this writer.
   */
  @CanIgnoreReturnValue
  public JsonWriter name(String name) throws IOException {
    Objects.requireNonNull(name, "name == null");
    if (deferredName != null) {
      throw new IllegalStateException();
    }
    if (stackSize == 0) {
      throw new IllegalStateException("JsonWriter is closed.");
    }
    deferredName = name;
    return this;
  }

  private void writeDeferredName() throws IOException {
    if (deferredName != null) {
      beforeName();
      string(deferredName);
      deferredName = null;
    }
  }

  /**
   * Encodes {@code value}.
   *
   * @param value the literal string value, or null to encode a null literal.
   * @return this writer.
   */
  @CanIgnoreReturnValue
  public JsonWriter value(String value) throws IOException {
    if (value == null) {
      return nullValue();
    }
    writeDeferredName();
    beforeValue();
    string(value);
    return this;
  }

  /**
   * Writes {@code value} directly to the writer without quoting or
   * escaping. This might not be supported by all implementations, if
   * not supported an {@code UnsupportedOperationException} is thrown.
   *
   * @param value the literal string value, or null to encode a null literal.
   * @return this writer.
   * @throws UnsupportedOperationException if this writer does not support
   *    writing raw JSON values.
   * @since 2.4
   */
  @CanIgnoreReturnValue
  public JsonWriter jsonValue(String value) throws IOException {
    if (value == null) {
      return nullValue();
    }
    writeDeferredName();
    beforeValue();
    out.append(value);
    return this;
  }

  /**
   * Encodes {@code null}.
   *
   * @return this writer.
   */
  @CanIgnoreReturnValue
  public JsonWriter nullValue() throws IOException {
    if (deferredName != null) {
      if (serializeNulls) {
        writeDeferredName();
      } else {
        deferredName = null;
        return this; // skip the name and the value
      }
    }
    beforeValue();
    out.write("null");
    return this;
  }

  /**
   * Encodes {@code value}.
   *
   * @return this writer.
   */
  @CanIgnoreReturnValue
  public JsonWriter value(boolean value) throws IOException {
    writeDeferredName();
    beforeValue();
    out.write(value ? "true" : "false");
    return this;
  }

  /**
   * Encodes {@code value}.
   *
   * @return this writer.
   * @since 2.7
   */
  @CanIgnoreReturnValue
  public JsonWriter value(Boolean value) throws IOException {
    if (value == null) {
      return nullValue();
    }
    writeDeferredName();
    beforeValue();
    out.write(value ? "true" : "false");
    return this;
  }

  /**
   * Encodes {@code value}.
   *
   * @param value a finite value, or if {@link #setLenient(boolean) lenient},
   *     also {@link Float#isNaN() NaN} or {@link Float#isInfinite()
   *     infinity}.
   * @return this writer.
   * @throws IllegalArgumentException if the value is NaN or Infinity and this writer is not {@link
   *     #setLenient(boolean) lenient}.
   * @since 2.9.1
   */
  @CanIgnoreReturnValue
  public JsonWriter value(float value) throws IOException {
    writeDeferredName();
    if (!lenient && (Float.isNaN(value) || Float.isInfinite(value))) {
      throw new IllegalArgumentException("Numeric values must be finite, but was " + value);
    }
    beforeValue();
    out.append(Float.toString(value));
    return this;
  }

  /**
   * Encodes {@code value}.
   *
   * @param value a finite value, or if {@link #setLenient(boolean) lenient},
   *     also {@link Double#isNaN() NaN} or {@link Double#isInfinite() infinity}.
   * @return this writer.
   * @throws IllegalArgumentException if the value is NaN or Infinity and this writer is
   *     not {@link #setLenient(boolean) lenient}.
   */
  @CanIgnoreReturnValue
  public JsonWriter value(double value) throws IOException {
    writeDeferredName();
    if (!lenient && (Double.isNaN(value) || Double.isInfinite(value))) {
      throw new IllegalArgumentException("Numeric values must be finite, but was " + value);
    }
    beforeValue();
    out.append(Double.toString(value));
    return this;
  }

  /**
   * Encodes {@code value}.
   *
   * @return this writer.
   */
  @CanIgnoreReturnValue
  public JsonWriter value(long value) throws IOException {
    writeDeferredName();
    beforeValue();
    out.write(Long.toString(value));
    return this;
  }

  /**
   * Returns whether the {@code toString()} of {@code c} can be trusted to return
   * a valid JSON number.
   */
  private static boolean isTrustedNumberType(Class<? extends Number> c) {
    // Note: Don't consider LazilyParsedNumber trusted because it could contain
    // an arbitrary malformed string
    return c == Integer.class || c == Long.class || c == Double.class || c == Float.class || c == Byte.class || c == Short.class
        || c == BigDecimal.class || c == BigInteger.class || c == AtomicInteger.class || c == AtomicLong.class;
  }

  /**
   * Encodes {@code value}. The value is written by directly writing the {@link Number#toString()}
   * result to JSON. Implementations must make sure that the result represents a valid JSON number.
   *
   * @param value a finite value, or if {@link #setLenient(boolean) lenient},
   *     also {@link Double#isNaN() NaN} or {@link Double#isInfinite() infinity}.
   * @return this writer.
   * @throws IllegalArgumentException if the value is NaN or Infinity and this writer is
   *     not {@link #setLenient(boolean) lenient}; or if the {@code toString()} result is not a
   *     valid JSON number.
   */
  @CanIgnoreReturnValue
  public JsonWriter value(Number value) throws IOException {
    if (value == null) {
      return nullValue();
    }

    writeDeferredName();
    String string = value.toString();
    if (string.equals("-Infinity") || string.equals("Infinity") || string.equals("NaN")) {
      if (!lenient) {
        throw new IllegalArgumentException("Numeric values must be finite, but was " + string);
      }
    } else {
      Class<? extends Number> numberClass = value.getClass();
      // Validate that string is valid before writing it directly to JSON output
      if (!isTrustedNumberType(numberClass) && !VALID_JSON_NUMBER_PATTERN.matcher(string).matches()) {
        throw new IllegalArgumentException("String created by " + numberClass + " is not a valid JSON number: " + string);
      }
    }

    beforeValue();
    out.append(string);
    return this;
  }

  /**
   * Ensures all buffered data is written to the underlying {@link Writer}
   * and flushes that writer.
   */
  @Override public void flush() throws IOException {
    if (stackSize == 0) {
      throw new IllegalStateException("JsonWriter is closed.");
    }
    out.flush();
  }

  /**
   * Flushes and closes this writer and the underlying {@link Writer}.
   *
   * @throws IOException if the JSON document is incomplete.
   */
  @Override public void close() throws IOException {
    out.close();

    int size = stackSize;
    if (size > 1 || (size == 1 && stack[size - 1] != NONEMPTY_DOCUMENT)) {
      throw new IOException("Incomplete document");
    }
    stackSize = 0;
  }

  private void string(String value) throws IOException {
    String[] replacements = htmlSafe ? HTML_SAFE_REPLACEMENT_CHARS : REPLACEMENT_CHARS;
    out.write('\"');
    int last = 0;
    int length = value.length();
    for (int i = 0; i < length; i++) {
      char c = value.charAt(i);
      String replacement;
      if (c < 128) {
        replacement = replacements[c];
        if (replacement == null) {
          continue;
        }
      } else if (c == '\u2028') {
        replacement = "\\u2028";
      } else if (c == '\u2029') {
        replacement = "\\u2029";
      } else {
        continue;
      }
      if (last < i) {
        out.write(value, last, i - last);
      }
      out.write(replacement);
      last = i + 1;
    }
    if (last < length) {
      out.write(value, last, length - last);
    }
    out.write('\"');
  }

  private void newline() throws IOException {
    if (formattingStyle == null) {
      return;
    }

    out.write(formattingStyle.getNewline());
    for (int i = 1, size = stackSize; i < size; i++) {
      out.write(formattingStyle.getIndent());
    }
  }

  /**
   * Inserts any necessary separators and whitespace before a name. Also
   * adjusts the stack to expect the name's value.
   */
  private void beforeName() throws IOException {
    int context = peek();
    if (context == NONEMPTY_OBJECT) { // first in object
      out.write(',');
    } else if (context != EMPTY_OBJECT) { // not in an object!
      throw new IllegalStateException("Nesting problem.");
    }
    newline();
    replaceTop(DANGLING_NAME);
  }

  /**
   * Inserts any necessary separators and whitespace before a literal value,
   * inline array, or inline object. Also adjusts the stack to expect either a
   * closing bracket or another element.
   */
  @SuppressWarnings("fallthrough")
  private void beforeValue() throws IOException {
    switch (peek()) {
    case NONEMPTY_DOCUMENT:
      if (!lenient) {
        throw new IllegalStateException(
            "JSON must have only one top-level value.");
      }
      // fall-through
    case EMPTY_DOCUMENT: // first in document
      replaceTop(NONEMPTY_DOCUMENT);
      break;

    case EMPTY_ARRAY: // first in array
      replaceTop(NONEMPTY_ARRAY);
      newline();
      break;

    case NONEMPTY_ARRAY: // another in array
      out.append(',');
      newline();
      break;

    case DANGLING_NAME: // value for name
      out.append(separator);
      replaceTop(NONEMPTY_OBJECT);
      break;

    default:
      throw new IllegalStateException("Nesting problem.");
    }
  }
}
"#;
    const AFTER: &str = r#"
/*
 * Copyright (C) 2010 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.google.gson.stream;

import static com.google.gson.stream.JsonScope.DANGLING_NAME;
import static com.google.gson.stream.JsonScope.EMPTY_ARRAY;
import static com.google.gson.stream.JsonScope.EMPTY_DOCUMENT;
import static com.google.gson.stream.JsonScope.EMPTY_OBJECT;
import static com.google.gson.stream.JsonScope.NONEMPTY_ARRAY;
import static com.google.gson.stream.JsonScope.NONEMPTY_DOCUMENT;
import static com.google.gson.stream.JsonScope.NONEMPTY_OBJECT;

import com.google.errorprone.annotations.CanIgnoreReturnValue;
import com.google.gson.FormattingStyle;
import java.io.Closeable;
import java.io.Flushable;
import java.io.IOException;
import java.io.Writer;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.regex.Pattern;

/**
 * Writes a JSON (<a href="http://www.ietf.org/rfc/rfc7159.txt">RFC 7159</a>)
 * encoded value to a stream, one token at a time. The stream includes both
 * literal values (strings, numbers, booleans and nulls) as well as the begin
 * and end delimiters of objects and arrays.
 *
 * <h2>Encoding JSON</h2>
 * To encode your data as JSON, create a new {@code JsonWriter}. Call methods
 * on the writer as you walk the structure's contents, nesting arrays and objects
 * as necessary:
 * <ul>
 *   <li>To write <strong>arrays</strong>, first call {@link #beginArray()}.
 *       Write each of the array's elements with the appropriate {@link #value}
 *       methods or by nesting other arrays and objects. Finally close the array
 *       using {@link #endArray()}.
 *   <li>To write <strong>objects</strong>, first call {@link #beginObject()}.
 *       Write each of the object's properties by alternating calls to
 *       {@link #name} with the property's value. Write property values with the
 *       appropriate {@link #value} method or by nesting other objects or arrays.
 *       Finally close the object using {@link #endObject()}.
 * </ul>
 *
 * <h2>Example</h2>
 * Suppose we'd like to encode a stream of messages such as the following: <pre> {@code
 * [
 *   {
 *     "id": 912345678901,
 *     "text": "How do I stream JSON in Java?",
 *     "geo": null,
 *     "user": {
 *       "name": "json_newb",
 *       "followers_count": 41
 *      }
 *   },
 *   {
 *     "id": 912345678902,
 *     "text": "@json_newb just use JsonWriter!",
 *     "geo": [50.454722, -104.606667],
 *     "user": {
 *       "name": "jesse",
 *       "followers_count": 2
 *     }
 *   }
 * ]}</pre>
 * This code encodes the above structure: <pre>   {@code
 *   public void writeJsonStream(OutputStream out, List<Message> messages) throws IOException {
 *     JsonWriter writer = new JsonWriter(new OutputStreamWriter(out, "UTF-8"));
 *     writer.setIndent("    ");
 *     writeMessagesArray(writer, messages);
 *     writer.close();
 *   }
 *
 *   public void writeMessagesArray(JsonWriter writer, List<Message> messages) throws IOException {
 *     writer.beginArray();
 *     for (Message message : messages) {
 *       writeMessage(writer, message);
 *     }
 *     writer.endArray();
 *   }
 *
 *   public void writeMessage(JsonWriter writer, Message message) throws IOException {
 *     writer.beginObject();
 *     writer.name("id").value(message.getId());
 *     writer.name("text").value(message.getText());
 *     if (message.getGeo() != null) {
 *       writer.name("geo");
 *       writeDoublesArray(writer, message.getGeo());
 *     } else {
 *       writer.name("geo").nullValue();
 *     }
 *     writer.name("user");
 *     writeUser(writer, message.getUser());
 *     writer.endObject();
 *   }
 *
 *   public void writeUser(JsonWriter writer, User user) throws IOException {
 *     writer.beginObject();
 *     writer.name("name").value(user.getName());
 *     writer.name("followers_count").value(user.getFollowersCount());
 *     writer.endObject();
 *   }
 *
 *   public void writeDoublesArray(JsonWriter writer, List<Double> doubles) throws IOException {
 *     writer.beginArray();
 *     for (Double value : doubles) {
 *       writer.value(value);
 *     }
 *     writer.endArray();
 *   }}</pre>
 *
 * <p>Each {@code JsonWriter} may be used to write a single JSON stream.
 * Instances of this class are not thread safe. Calls that would result in a
 * malformed JSON string will fail with an {@link IllegalStateException}.
 *
 * @author Jesse Wilson
 * @since 1.6
 */
public class JsonWriter implements Closeable, Flushable {

  // Syntax as defined by https://datatracker.ietf.org/doc/html/rfc8259#section-6
  private static final Pattern VALID_JSON_NUMBER_PATTERN = Pattern.compile("-?(?:0|[1-9][0-9]*)(?:\\.[0-9]+)?(?:[eE][-+]?[0-9]+)?");

  /*
   * From RFC 7159, "All Unicode characters may be placed within the
   * quotation marks except for the characters that must be escaped:
   * quotation mark, reverse solidus, and the control characters
   * (U+0000 through U+001F)."
   *
   * We also escape '\u2028' and '\u2029', which JavaScript interprets as
   * newline characters. This prevents eval() from failing with a syntax
   * error. http://code.google.com/p/google-gson/issues/detail?id=341
   */
  private static final String[] REPLACEMENT_CHARS;
  private static final String[] HTML_SAFE_REPLACEMENT_CHARS;
  static {
    REPLACEMENT_CHARS = new String[128];
    for (int i = 0; i <= 0x1f; i++) {
      REPLACEMENT_CHARS[i] = String.format("\\u%04x", i);
    }
    REPLACEMENT_CHARS['"'] = "\\\"";
    REPLACEMENT_CHARS['\\'] = "\\\\";
    REPLACEMENT_CHARS['\t'] = "\\t";
    REPLACEMENT_CHARS['\b'] = "\\b";
    REPLACEMENT_CHARS['\n'] = "\\n";
    REPLACEMENT_CHARS['\r'] = "\\r";
    REPLACEMENT_CHARS['\f'] = "\\f";
    HTML_SAFE_REPLACEMENT_CHARS = REPLACEMENT_CHARS.clone();
    HTML_SAFE_REPLACEMENT_CHARS['<'] = "\\u003c";
    HTML_SAFE_REPLACEMENT_CHARS['>'] = "\\u003e";
    HTML_SAFE_REPLACEMENT_CHARS['&'] = "\\u0026";
    HTML_SAFE_REPLACEMENT_CHARS['='] = "\\u003d";
    HTML_SAFE_REPLACEMENT_CHARS['\''] = "\\u0027";
  }

  /** The JSON output destination */
  private final Writer out;

  private int[] stack = new int[32];
  private int stackSize = 0;
  {
    push(EMPTY_DOCUMENT);
  }

  private FormattingStyle formattingStyle;
  // These fields cache data derived from the formatting style, to avoid having to
  // re-evaluate it every time something is written
  private String formattedColon;
  private String formattedComma;
  private boolean usesEmptyNewlineAndIndent;

  private boolean lenient;

  private boolean htmlSafe;

  private String deferredName;

  private boolean serializeNulls = true;

  /**
   * Creates a new instance that writes a JSON-encoded stream to {@code out}.
   * For best performance, ensure {@link Writer} is buffered; wrapping in
   * {@link java.io.BufferedWriter BufferedWriter} if necessary.
   */
  public JsonWriter(Writer out) {
    this.out = Objects.requireNonNull(out, "out == null");
    setFormattingStyle(FormattingStyle.COMPACT);
  }

  /**
   * Sets the indentation string to be repeated for each level of indentation
   * in the encoded document. If {@code indent.isEmpty()} the encoded document
   * will be compact. Otherwise the encoded document will be more
   * human-readable.
   *
   * <p>This is a convenience method which overwrites any previously
   * {@linkplain #setFormattingStyle(FormattingStyle) set formatting style} with
   * either {@link FormattingStyle#COMPACT} if the given indent string is
   * empty, or {@link FormattingStyle#PRETTY} with the given indent if
   * not empty.
   *
   * @param indent a string containing only whitespace.
   */
  public final void setIndent(String indent) {
    if (indent.isEmpty()) {
      setFormattingStyle(FormattingStyle.COMPACT);
    } else {
      setFormattingStyle(FormattingStyle.PRETTY.withIndent(indent));
    }
  }

  /**
   * Sets the formatting style to be used in the encoded document.
   *
   * <p>The formatting style specifies for example the indentation string to be
   * repeated for each level of indentation, or the newline style, to accommodate
   * various OS styles.</p>
   *
   * @param formattingStyle the formatting style to use, must not be {@code null}.
   * @since $next-version$
   */
  public final void setFormattingStyle(FormattingStyle formattingStyle) {
    this.formattingStyle = Objects.requireNonNull(formattingStyle);

    this.formattedComma = ",";
    if (this.formattingStyle.usesSpaceAfterSeparators()) {
      this.formattedColon = ": ";

      // Only add space if no newline is written
      if (this.formattingStyle.getNewline().isEmpty()) {
        this.formattedComma = ", ";
      }
    } else {
      this.formattedColon = ":";
    }

    this.usesEmptyNewlineAndIndent = this.formattingStyle.getNewline().isEmpty()
        && this.formattingStyle.getIndent().isEmpty();
  }

  /**
   * Returns the pretty printing style used by this writer.
   *
   * @return the {@code FormattingStyle} that will be used.
   * @since $next-version$
   */
  public final FormattingStyle getFormattingStyle() {
    return formattingStyle;
  }

  /**
   * Configure this writer to relax its syntax rules. By default, this writer
   * only emits well-formed JSON as specified by <a
   * href="http://www.ietf.org/rfc/rfc7159.txt">RFC 7159</a>. Setting the writer
   * to lenient permits the following:
   * <ul>
   *   <li>Numbers may be {@link Double#isNaN() NaNs} or {@link
   *       Double#isInfinite() infinities}.
   * </ul>
   */
  public final void setLenient(boolean lenient) {
    this.lenient = lenient;
  }

  /**
   * Returns true if this writer has relaxed syntax rules.
   */
  public boolean isLenient() {
    return lenient;
  }

  /**
   * Configure this writer to emit JSON that's safe for direct inclusion in HTML
   * and XML documents. This escapes the HTML characters {@code <}, {@code >},
   * {@code &} and {@code =} before writing them to the stream. Without this
   * setting, your XML/HTML encoder should replace these characters with the
   * corresponding escape sequences.
   */
  public final void setHtmlSafe(boolean htmlSafe) {
    this.htmlSafe = htmlSafe;
  }

  /**
   * Returns true if this writer writes JSON that's safe for inclusion in HTML
   * and XML documents.
   */
  public final boolean isHtmlSafe() {
    return htmlSafe;
  }

  /**
   * Sets whether object members are serialized when their value is null.
   * This has no impact on array elements. The default is true.
   */
  public final void setSerializeNulls(boolean serializeNulls) {
    this.serializeNulls = serializeNulls;
  }

  /**
   * Returns true if object members are serialized when their value is null.
   * This has no impact on array elements. The default is true.
   */
  public final boolean getSerializeNulls() {
    return serializeNulls;
  }

  /**
   * Begins encoding a new array. Each call to this method must be paired with
   * a call to {@link #endArray}.
   *
   * @return this writer.
   */
  @CanIgnoreReturnValue
  public JsonWriter beginArray() throws IOException {
    writeDeferredName();
    return open(EMPTY_ARRAY, '[');
  }

  /**
   * Ends encoding the current array.
   *
   * @return this writer.
   */
  @CanIgnoreReturnValue
  public JsonWriter endArray() throws IOException {
    return close(EMPTY_ARRAY, NONEMPTY_ARRAY, ']');
  }

  /**
   * Begins encoding a new object. Each call to this method must be paired
   * with a call to {@link #endObject}.
   *
   * @return this writer.
   */
  @CanIgnoreReturnValue
  public JsonWriter beginObject() throws IOException {
    writeDeferredName();
    return open(EMPTY_OBJECT, '{');
  }

  /**
   * Ends encoding the current object.
   *
   * @return this writer.
   */
  @CanIgnoreReturnValue
  public JsonWriter endObject() throws IOException {
    return close(EMPTY_OBJECT, NONEMPTY_OBJECT, '}');
  }

  /**
   * Enters a new scope by appending any necessary whitespace and the given
   * bracket.
   */
  @CanIgnoreReturnValue
  private JsonWriter open(int empty, char openBracket) throws IOException {
    beforeValue();
    push(empty);
    out.write(openBracket);
    return this;
  }

  /**
   * Closes the current scope by appending any necessary whitespace and the
   * given bracket.
   */
  @CanIgnoreReturnValue
  private JsonWriter close(int empty, int nonempty, char closeBracket)
      throws IOException {
    int context = peek();
    if (context != nonempty && context != empty) {
      throw new IllegalStateException("Nesting problem.");
    }
    if (deferredName != null) {
      throw new IllegalStateException("Dangling name: " + deferredName);
    }

    stackSize--;
    if (context == nonempty) {
      newline();
    }
    out.write(closeBracket);
    return this;
  }

  private void push(int newTop) {
    if (stackSize == stack.length) {
      stack = Arrays.copyOf(stack, stackSize * 2);
    }
    stack[stackSize++] = newTop;
  }

  /**
   * Returns the value on the top of the stack.
   */
  private int peek() {
    if (stackSize == 0) {
      throw new IllegalStateException("JsonWriter is closed.");
    }
    return stack[stackSize - 1];
  }

  /**
   * Replace the value on the top of the stack with the given value.
   */
  private void replaceTop(int topOfStack) {
    stack[stackSize - 1] = topOfStack;
  }

  /**
   * Encodes the property name.
   *
   * @param name the name of the forthcoming value. May not be {@code null}.
   * @return this writer.
   */
  @CanIgnoreReturnValue
  public JsonWriter name(String name) throws IOException {
    Objects.requireNonNull(name, "name == null");
    if (deferredName != null) {
      throw new IllegalStateException();
    }
    if (stackSize == 0) {
      throw new IllegalStateException("JsonWriter is closed.");
    }
    deferredName = name;
    return this;
  }

  private void writeDeferredName() throws IOException {
    if (deferredName != null) {
      beforeName();
      string(deferredName);
      deferredName = null;
    }
  }

  /**
   * Encodes {@code value}.
   *
   * @param value the literal string value, or null to encode a null literal.
   * @return this writer.
   */
  @CanIgnoreReturnValue
  public JsonWriter value(String value) throws IOException {
    if (value == null) {
      return nullValue();
    }
    writeDeferredName();
    beforeValue();
    string(value);
    return this;
  }

  /**
   * Writes {@code value} directly to the writer without quoting or
   * escaping. This might not be supported by all implementations, if
   * not supported an {@code UnsupportedOperationException} is thrown.
   *
   * @param value the literal string value, or null to encode a null literal.
   * @return this writer.
   * @throws UnsupportedOperationException if this writer does not support
   *    writing raw JSON values.
   * @since 2.4
   */
  @CanIgnoreReturnValue
  public JsonWriter jsonValue(String value) throws IOException {
    if (value == null) {
      return nullValue();
    }
    writeDeferredName();
    beforeValue();
    out.append(value);
    return this;
  }

  /**
   * Encodes {@code null}.
   *
   * @return this writer.
   */
  @CanIgnoreReturnValue
  public JsonWriter nullValue() throws IOException {
    if (deferredName != null) {
      if (serializeNulls) {
        writeDeferredName();
      } else {
        deferredName = null;
        return this; // skip the name and the value
      }
    }
    beforeValue();
    out.write("null");
    return this;
  }

  /**
   * Encodes {@code value}.
   *
   * @return this writer.
   */
  @CanIgnoreReturnValue
  public JsonWriter value(boolean value) throws IOException {
    writeDeferredName();
    beforeValue();
    out.write(value ? "true" : "false");
    return this;
  }

  /**
   * Encodes {@code value}.
   *
   * @return this writer.
   * @since 2.7
   */
  @CanIgnoreReturnValue
  public JsonWriter value(Boolean value) throws IOException {
    if (value == null) {
      return nullValue();
    }
    writeDeferredName();
    beforeValue();
    out.write(value ? "true" : "false");
    return this;
  }

  /**
   * Encodes {@code value}.
   *
   * @param value a finite value, or if {@link #setLenient(boolean) lenient},
   *     also {@link Float#isNaN() NaN} or {@link Float#isInfinite()
   *     infinity}.
   * @return this writer.
   * @throws IllegalArgumentException if the value is NaN or Infinity and this writer is not {@link
   *     #setLenient(boolean) lenient}.
   * @since 2.9.1
   */
  @CanIgnoreReturnValue
  public JsonWriter value(float value) throws IOException {
    writeDeferredName();
    if (!lenient && (Float.isNaN(value) || Float.isInfinite(value))) {
      throw new IllegalArgumentException("Numeric values must be finite, but was " + value);
    }
    beforeValue();
    out.append(Float.toString(value));
    return this;
  }

  /**
   * Encodes {@code value}.
   *
   * @param value a finite value, or if {@link #setLenient(boolean) lenient},
   *     also {@link Double#isNaN() NaN} or {@link Double#isInfinite() infinity}.
   * @return this writer.
   * @throws IllegalArgumentException if the value is NaN or Infinity and this writer is
   *     not {@link #setLenient(boolean) lenient}.
   */
  @CanIgnoreReturnValue
  public JsonWriter value(double value) throws IOException {
    writeDeferredName();
    if (!lenient && (Double.isNaN(value) || Double.isInfinite(value))) {
      throw new IllegalArgumentException("Numeric values must be finite, but was " + value);
    }
    beforeValue();
    out.append(Double.toString(value));
    return this;
  }

  /**
   * Encodes {@code value}.
   *
   * @return this writer.
   */
  @CanIgnoreReturnValue
  public JsonWriter value(long value) throws IOException {
    writeDeferredName();
    beforeValue();
    out.write(Long.toString(value));
    return this;
  }

  /**
   * Returns whether the {@code toString()} of {@code c} can be trusted to return
   * a valid JSON number.
   */
  private static boolean isTrustedNumberType(Class<? extends Number> c) {
    // Note: Don't consider LazilyParsedNumber trusted because it could contain
    // an arbitrary malformed string
    return c == Integer.class || c == Long.class || c == Double.class || c == Float.class || c == Byte.class || c == Short.class
        || c == BigDecimal.class || c == BigInteger.class || c == AtomicInteger.class || c == AtomicLong.class;
  }

  /**
   * Encodes {@code value}. The value is written by directly writing the {@link Number#toString()}
   * result to JSON. Implementations must make sure that the result represents a valid JSON number.
   *
   * @param value a finite value, or if {@link #setLenient(boolean) lenient},
   *     also {@link Double#isNaN() NaN} or {@link Double#isInfinite() infinity}.
   * @return this writer.
   * @throws IllegalArgumentException if the value is NaN or Infinity and this writer is
   *     not {@link #setLenient(boolean) lenient}; or if the {@code toString()} result is not a
   *     valid JSON number.
   */
  @CanIgnoreReturnValue
  public JsonWriter value(Number value) throws IOException {
    if (value == null) {
      return nullValue();
    }

    writeDeferredName();
    String string = value.toString();
    if (string.equals("-Infinity") || string.equals("Infinity") || string.equals("NaN")) {
      if (!lenient) {
        throw new IllegalArgumentException("Numeric values must be finite, but was " + string);
      }
    } else {
      Class<? extends Number> numberClass = value.getClass();
      // Validate that string is valid before writing it directly to JSON output
      if (!isTrustedNumberType(numberClass) && !VALID_JSON_NUMBER_PATTERN.matcher(string).matches()) {
        throw new IllegalArgumentException("String created by " + numberClass + " is not a valid JSON number: " + string);
      }
    }

    beforeValue();
    out.append(string);
    return this;
  }

  /**
   * Ensures all buffered data is written to the underlying {@link Writer}
   * and flushes that writer.
   */
  @Override public void flush() throws IOException {
    if (stackSize == 0) {
      throw new IllegalStateException("JsonWriter is closed.");
    }
    out.flush();
  }

  /**
   * Flushes and closes this writer and the underlying {@link Writer}.
   *
   * @throws IOException if the JSON document is incomplete.
   */
  @Override public void close() throws IOException {
    out.close();

    int size = stackSize;
    if (size > 1 || (size == 1 && stack[size - 1] != NONEMPTY_DOCUMENT)) {
      throw new IOException("Incomplete document");
    }
    stackSize = 0;
  }

  private void string(String value) throws IOException {
    String[] replacements = htmlSafe ? HTML_SAFE_REPLACEMENT_CHARS : REPLACEMENT_CHARS;
    out.write('\"');
    int last = 0;
    int length = value.length();
    for (int i = 0; i < length; i++) {
      char c = value.charAt(i);
      String replacement;
      if (c < 128) {
        replacement = replacements[c];
        if (replacement == null) {
          continue;
        }
      } else if (c == '\u2028') {
        replacement = "\\u2028";
      } else if (c == '\u2029') {
        replacement = "\\u2029";
      } else {
        continue;
      }
      if (last < i) {
        out.write(value, last, i - last);
      }
      out.write(replacement);
      last = i + 1;
    }
    if (last < length) {
      out.write(value, last, length - last);
    }
    out.write('\"');
  }

  private void newline() throws IOException {
    if (usesEmptyNewlineAndIndent) {
      return;
    }

    out.write(formattingStyle.getNewline());
    for (int i = 1, size = stackSize; i < size; i++) {
      out.write(formattingStyle.getIndent());
    }
  }

  /**
   * Inserts any necessary separators and whitespace before a name. Also
   * adjusts the stack to expect the name's value.
   */
  private void beforeName() throws IOException {
    int context = peek();
    if (context == NONEMPTY_OBJECT) { // first in object
      out.write(formattedComma);
    } else if (context != EMPTY_OBJECT) { // not in an object!
      throw new IllegalStateException("Nesting problem.");
    }
    newline();
    replaceTop(DANGLING_NAME);
  }

  /**
   * Inserts any necessary separators and whitespace before a literal value,
   * inline array, or inline object. Also adjusts the stack to expect either a
   * closing bracket or another element.
   */
  @SuppressWarnings("fallthrough")
  private void beforeValue() throws IOException {
    switch (peek()) {
    case NONEMPTY_DOCUMENT:
      if (!lenient) {
        throw new IllegalStateException(
            "JSON must have only one top-level value.");
      }
      // fall-through
    case EMPTY_DOCUMENT: // first in document
      replaceTop(NONEMPTY_DOCUMENT);
      break;

    case EMPTY_ARRAY: // first in array
      replaceTop(NONEMPTY_ARRAY);
      newline();
      break;

    case NONEMPTY_ARRAY: // another in array
      out.append(formattedComma);
      newline();
      break;

    case DANGLING_NAME: // value for name
      out.append(formattedColon);
      replaceTop(NONEMPTY_OBJECT);
      break;

    default:
      throw new IllegalStateException("Nesting problem.");
    }
  }
}
"#;

    const BEFORE2: &str = r#"
/*
 * Copyright (C) 2021 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.google.gson;

import static com.google.common.truth.Truth.assertThat;
import static org.junit.Assert.fail;

import com.google.gson.internal.LazilyParsedNumber;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.MalformedJsonException;
import java.io.IOException;
import java.io.StringReader;
import java.math.BigDecimal;
import org.junit.Test;

public class ToNumberPolicyTest {
  @Test
  public void testDouble() throws IOException {
    ToNumberStrategy strategy = ToNumberPolicy.DOUBLE;
    assertThat(strategy.readNumber(fromString("10.1"))).isEqualTo(10.1);
    assertThat(strategy.readNumber(fromString("3.141592653589793238462643383279"))).isEqualTo(3.141592653589793D);
    try {
      strategy.readNumber(fromString("1e400"));
      fail();
    } catch (MalformedJsonException expected) {
      assertThat(expected).hasMessageThat().isEqualTo(
          "JSON forbids NaN and infinities: Infinity at line 1 column 6 path $"
          + "\nSee https://github.com/google/gson/blob/main/Troubleshooting.md#malformed-json");
    }
    try {
      strategy.readNumber(fromString("\"not-a-number\""));
      fail();
    } catch (NumberFormatException expected) {
    }
  }

  @Test
  public void testLazilyParsedNumber() throws IOException {
    ToNumberStrategy strategy = ToNumberPolicy.LAZILY_PARSED_NUMBER;
    assertThat(strategy.readNumber(fromString("10.1"))).isEqualTo(new LazilyParsedNumber("10.1"));
    assertThat(strategy.readNumber(fromString("3.141592653589793238462643383279"))).isEqualTo(new LazilyParsedNumber("3.141592653589793238462643383279"));
    assertThat(strategy.readNumber(fromString("1e400"))).isEqualTo(new LazilyParsedNumber("1e400"));
  }

  @Test
  public void testLongOrDouble() throws IOException {
    ToNumberStrategy strategy = ToNumberPolicy.LONG_OR_DOUBLE;
    assertThat(strategy.readNumber(fromString("10"))).isEqualTo(10L);
    assertThat(strategy.readNumber(fromString("10.1"))).isEqualTo(10.1);
    assertThat(strategy.readNumber(fromString("3.141592653589793238462643383279"))).isEqualTo(3.141592653589793D);
    try {
      strategy.readNumber(fromString("1e400"));
      fail();
    } catch (MalformedJsonException expected) {
      assertThat(expected).hasMessageThat().isEqualTo("JSON forbids NaN and infinities: Infinity; at path $");
    }
    try {
      strategy.readNumber(fromString("\"not-a-number\""));
      fail();
    } catch (JsonParseException expected) {
      assertThat(expected).hasMessageThat().isEqualTo("Cannot parse not-a-number; at path $");
    }

    assertThat(strategy.readNumber(fromStringLenient("NaN"))).isEqualTo(Double.NaN);
    assertThat(strategy.readNumber(fromStringLenient("Infinity"))).isEqualTo(Double.POSITIVE_INFINITY);
    assertThat(strategy.readNumber(fromStringLenient("-Infinity"))).isEqualTo(Double.NEGATIVE_INFINITY);
    try {
      strategy.readNumber(fromString("NaN"));
      fail();
    } catch (MalformedJsonException expected) {
      assertThat(expected).hasMessageThat().isEqualTo(
          "Use JsonReader.setLenient(true) to accept malformed JSON at line 1 column 1 path $"
          + "\nSee https://github.com/google/gson/blob/main/Troubleshooting.md#malformed-json");
    }
    try {
      strategy.readNumber(fromString("Infinity"));
      fail();
    } catch (MalformedJsonException expected) {
      assertThat(expected).hasMessageThat().isEqualTo(
          "Use JsonReader.setLenient(true) to accept malformed JSON at line 1 column 1 path $"
          + "\nSee https://github.com/google/gson/blob/main/Troubleshooting.md#malformed-json");
    }
    try {
      strategy.readNumber(fromString("-Infinity"));
      fail();
    } catch (MalformedJsonException expected) {
      assertThat(expected).hasMessageThat().isEqualTo(
          "Use JsonReader.setLenient(true) to accept malformed JSON at line 1 column 1 path $"
          + "\nSee https://github.com/google/gson/blob/main/Troubleshooting.md#malformed-json");
    }
  }

  @Test
  public void testBigDecimal() throws IOException {
    ToNumberStrategy strategy = ToNumberPolicy.BIG_DECIMAL;
    assertThat(strategy.readNumber(fromString("10.1"))).isEqualTo(new BigDecimal("10.1"));
    assertThat(strategy.readNumber(fromString("3.141592653589793238462643383279"))).isEqualTo(new BigDecimal("3.141592653589793238462643383279"));
    assertThat(strategy.readNumber(fromString("1e400"))).isEqualTo(new BigDecimal("1e400"));

    try {
      strategy.readNumber(fromString("\"not-a-number\""));
      fail();
    } catch (JsonParseException expected) {
      assertThat(expected).hasMessageThat().isEqualTo("Cannot parse not-a-number; at path $");
    }
  }

  @Test
  public void testNullsAreNeverExpected() throws IOException {
    try {
      ToNumberPolicy.DOUBLE.readNumber(fromString("null"));
      fail();
    } catch (IllegalStateException expected) {
      assertThat(expected).hasMessageThat().isEqualTo("Expected a double but was NULL at line 1 column 5 path $"
          + "\nSee https://github.com/google/gson/blob/main/Troubleshooting.md#adapter-not-null-safe");
    }
    try {
      ToNumberPolicy.LAZILY_PARSED_NUMBER.readNumber(fromString("null"));
      fail();
    } catch (IllegalStateException expected) {
      assertThat(expected).hasMessageThat().isEqualTo("Expected a string but was NULL at line 1 column 5 path $"
          + "\nSee https://github.com/google/gson/blob/main/Troubleshooting.md#adapter-not-null-safe");
    }
    try {
      ToNumberPolicy.LONG_OR_DOUBLE.readNumber(fromString("null"));
      fail();
    } catch (IllegalStateException expected) {
      assertThat(expected).hasMessageThat().isEqualTo("Expected a string but was NULL at line 1 column 5 path $"
          + "\nSee https://github.com/google/gson/blob/main/Troubleshooting.md#adapter-not-null-safe");
    }
    try {
      ToNumberPolicy.BIG_DECIMAL.readNumber(fromString("null"));
      fail();
    } catch (IllegalStateException expected) {
      assertThat(expected).hasMessageThat().isEqualTo("Expected a string but was NULL at line 1 column 5 path $"
          + "\nSee https://github.com/google/gson/blob/main/Troubleshooting.md#adapter-not-null-safe");
    }
  }

  private static JsonReader fromString(String json) {
    return new JsonReader(new StringReader(json));
  }

  private static JsonReader fromStringLenient(String json) {
    JsonReader jsonReader = fromString(json);
    jsonReader.setLenient(true);
    return jsonReader;
  }
}
"#;

    const AFTER2: &str = r#"
/*
 * Copyright (C) 2021 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.google.gson;

import static com.google.common.truth.Truth.assertThat;
import static org.junit.Assert.assertThrows;

import com.google.gson.internal.LazilyParsedNumber;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.MalformedJsonException;
import java.io.IOException;
import java.io.StringReader;
import java.math.BigDecimal;
import org.junit.Test;

public class ToNumberPolicyTest {
  @Test
  public void testDouble() throws IOException {
    ToNumberStrategy strategy = ToNumberPolicy.DOUBLE;
    assertThat(strategy.readNumber(fromString("10.1"))).isEqualTo(10.1);
    assertThat(strategy.readNumber(fromString("3.141592653589793238462643383279"))).isEqualTo(3.141592653589793D);

    MalformedJsonException e = assertThrows(MalformedJsonException.class, () -> strategy.readNumber(fromString("1e400")));
    assertThat(e).hasMessageThat().isEqualTo(
        "JSON forbids NaN and infinities: Infinity at line 1 column 6 path $"
        + "\nSee https://github.com/google/gson/blob/main/Troubleshooting.md#malformed-json"
    );

    assertThrows(NumberFormatException.class, () -> strategy.readNumber(fromString("\"not-a-number\"")));
  }

  @Test
  public void testLazilyParsedNumber() throws IOException {
    ToNumberStrategy strategy = ToNumberPolicy.LAZILY_PARSED_NUMBER;
    assertThat(strategy.readNumber(fromString("10.1"))).isEqualTo(new LazilyParsedNumber("10.1"));
    assertThat(strategy.readNumber(fromString("3.141592653589793238462643383279"))).isEqualTo(new LazilyParsedNumber("3.141592653589793238462643383279"));
    assertThat(strategy.readNumber(fromString("1e400"))).isEqualTo(new LazilyParsedNumber("1e400"));
  }

  @Test
  public void testLongOrDouble() throws IOException {
    ToNumberStrategy strategy = ToNumberPolicy.LONG_OR_DOUBLE;
    assertThat(strategy.readNumber(fromString("10"))).isEqualTo(10L);
    assertThat(strategy.readNumber(fromString("10.1"))).isEqualTo(10.1);
    assertThat(strategy.readNumber(fromString("3.141592653589793238462643383279"))).isEqualTo(3.141592653589793D);

    Exception e = assertThrows(MalformedJsonException.class, () -> strategy.readNumber(fromString("1e400")));
    assertThat(e).hasMessageThat().isEqualTo("JSON forbids NaN and infinities: Infinity; at path $");

    e = assertThrows(JsonParseException.class, () -> strategy.readNumber(fromString("\"not-a-number\"")));
    assertThat(e).hasMessageThat().isEqualTo("Cannot parse not-a-number; at path $");

    assertThat(strategy.readNumber(fromStringLenient("NaN"))).isEqualTo(Double.NaN);
    assertThat(strategy.readNumber(fromStringLenient("Infinity"))).isEqualTo(Double.POSITIVE_INFINITY);
    assertThat(strategy.readNumber(fromStringLenient("-Infinity"))).isEqualTo(Double.NEGATIVE_INFINITY);

    e = assertThrows(MalformedJsonException.class, () -> strategy.readNumber(fromString("NaN")));
    assertThat(e).hasMessageThat().isEqualTo(
        "Use JsonReader.setLenient(true) to accept malformed JSON at line 1 column 1 path $"
        + "\nSee https://github.com/google/gson/blob/main/Troubleshooting.md#malformed-json");

    e = assertThrows(MalformedJsonException.class, () -> strategy.readNumber(fromString("Infinity")));
    assertThat(e).hasMessageThat().isEqualTo(
        "Use JsonReader.setLenient(true) to accept malformed JSON at line 1 column 1 path $"
        + "\nSee https://github.com/google/gson/blob/main/Troubleshooting.md#malformed-json");

    e = assertThrows(MalformedJsonException.class, () -> strategy.readNumber(fromString("-Infinity")));
    assertThat(e).hasMessageThat().isEqualTo(
        "Use JsonReader.setLenient(true) to accept malformed JSON at line 1 column 1 path $"
        + "\nSee https://github.com/google/gson/blob/main/Troubleshooting.md#malformed-json");
  }

  @Test
  public void testBigDecimal() throws IOException {
    ToNumberStrategy strategy = ToNumberPolicy.BIG_DECIMAL;
    assertThat(strategy.readNumber(fromString("10.1"))).isEqualTo(new BigDecimal("10.1"));
    assertThat(strategy.readNumber(fromString("3.141592653589793238462643383279"))).isEqualTo(new BigDecimal("3.141592653589793238462643383279"));
    assertThat(strategy.readNumber(fromString("1e400"))).isEqualTo(new BigDecimal("1e400"));

    JsonParseException e = assertThrows(JsonParseException.class, () -> strategy.readNumber(fromString("\"not-a-number\"")));
    assertThat(e).hasMessageThat().isEqualTo("Cannot parse not-a-number; at path $");
  }

  @Test
  public void testNullsAreNeverExpected() throws IOException {
    IllegalStateException e = assertThrows(IllegalStateException.class, () -> ToNumberPolicy.DOUBLE.readNumber(fromString("null")));
    assertThat(e).hasMessageThat().isEqualTo("Expected a double but was NULL at line 1 column 5 path $"
        + "\nSee https://github.com/google/gson/blob/main/Troubleshooting.md#adapter-not-null-safe");

    e = assertThrows(IllegalStateException.class, () -> ToNumberPolicy.LAZILY_PARSED_NUMBER.readNumber(fromString("null")));
    assertThat(e).hasMessageThat().isEqualTo("Expected a string but was NULL at line 1 column 5 path $"
        + "\nSee https://github.com/google/gson/blob/main/Troubleshooting.md#adapter-not-null-safe");

    e = assertThrows(IllegalStateException.class, () -> ToNumberPolicy.LONG_OR_DOUBLE.readNumber(fromString("null")));
    assertThat(e).hasMessageThat().isEqualTo("Expected a string but was NULL at line 1 column 5 path $"
        + "\nSee https://github.com/google/gson/blob/main/Troubleshooting.md#adapter-not-null-safe");

    e = assertThrows(IllegalStateException.class, () -> ToNumberPolicy.BIG_DECIMAL.readNumber(fromString("null")));
    assertThat(e).hasMessageThat().isEqualTo("Expected a string but was NULL at line 1 column 5 path $"
        + "\nSee https://github.com/google/gson/blob/main/Troubleshooting.md#adapter-not-null-safe");
  }

  private static JsonReader fromString(String json) {
    return new JsonReader(new StringReader(json));
  }

  private static JsonReader fromStringLenient(String json) {
    JsonReader jsonReader = fromString(json);
    jsonReader.setLenient(true);
    return jsonReader;
  }
}
"#;
}
