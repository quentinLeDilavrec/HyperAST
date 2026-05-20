use hyperast::types::{Childrn, LendT, NodeStore, WithChildren};
use hyperast::types::{NodeId, UniformNodeId};

use super::factorized_bounds;

pub mod bottom_up_matcher;
pub mod greedy_bottom_up_matcher;
pub mod greedy_subtree_matcher;
pub mod hybrid_bottom_up_matcher;
pub mod marriage_bottom_up_matcher;
pub mod simple_bottom_up_matcher;

// lazy versions, that do not decompress directly subtrees
pub mod lazy_bottom_up_matcher;
pub mod lazy_greedy_bottom_up_matcher;
pub mod lazy_greedy_subtree_matcher;
pub mod lazy_hybrid_bottom_up_matcher;
pub mod lazy_hybrid_marriage_bottom_up_matcher;
pub mod lazy_marriage_bottom_up_matcher;
pub mod lazy_simple_bottom_up_matcher;
pub mod lazy_simple_marriage_bottom_up_matcher;
//pub mod lazy_xy_bottom_up_matcher;

pub fn size<'a, IdC: Clone + NodeId<IdN = IdC>, S>(store: &'a S, x: &IdC) -> usize
where
    S: NodeStore<IdC>,
    for<'t> hyperast::types::LendN<'t, S, IdC>: WithChildren<TreeId = IdC>,
{
    let node = store.resolve(x);
    let cs = node.children().unwrap();
    let mut z = 0;
    for x in cs.iter_children() {
        z += size(store, &x);
    }
    z + 1
}

/// TODO specialize with WithStats when specialization is stabilized
pub fn height<IdC: Clone + NodeId<IdN = IdC>, S>(store: &S, x: &IdC) -> usize
where
    S: NodeStore<IdC>,
    for<'t> hyperast::types::LendN<'t, S, IdC>: WithChildren<TreeId = IdC>,
{
    let node = store.resolve(x);
    let cs = node.children();
    let Some(cs) = cs else {
        return 0;
    };
    if cs.is_empty() {
        return 0;
    }
    let mut z = 0;
    for c in cs.iter_children() {
        z = z.max(height(store, &c));
    }
    z + 1
}

/// if H then test the hash otherwise do not test it,
/// considering hash collisions testing it should only be useful once.
pub(crate) fn isomorphic<HAST, const HASH: bool, const STRUCTURAL: bool>(
    hyperast: HAST,
    src: &HAST::IdN,
    dst: &HAST::IdN,
) -> bool
where
    HAST: hyperast::types::HyperAST + Copy,
    for<'t> LendT<'t, HAST>: hyperast::types::WithHashs,
    HAST::IdN: Clone + Eq,
    HAST::Label: Eq,
    HAST::IdN: UniformNodeId,
{
    use hyperast::types::HashKind;
    use hyperast::types::Labeled;
    use hyperast::types::WithHashs;
    if src == dst {
        return true;
    }

    let src_ = hyperast.node_store().resolve(src);
    let dst_ = hyperast.node_store().resolve(dst);
    if HASH && !STRUCTURAL {
        let src_hash = WithHashs::hash(&src_, &HashKind::label());
        let dst_hash = WithHashs::hash(&dst_, &HashKind::label());
        if src_hash != dst_hash {
            return false;
        }
    } else if HASH && STRUCTURAL {
        let src_hash = WithHashs::hash(&src_, &HashKind::structural());
        let dst_hash = WithHashs::hash(&dst_, &HashKind::structural());
        if src_hash != dst_hash {
            return false;
        }
    }

    let src_type = hyperast.resolve_type(src);
    let dst_type = hyperast.resolve_type(dst);
    if src_type != dst_type {
        return false;
    }

    if !STRUCTURAL {
        let src_label = src_.try_get_label();
        let dst_label = dst_.try_get_label();
        if src_label != dst_label {
            return false;
        }
    }

    let src_children: Option<Vec<_>> = src_.children().map(|x| x.iter_children().collect());
    let dst_children: Option<Vec<_>> = dst_.children().map(|x| x.iter_children().collect());
    match (src_children, dst_children) {
        (None, None) => true,
        (Some(src_c), Some(dst_c)) => {
            if src_c.len() != dst_c.len() {
                false
            } else {
                for (src, dst) in src_c.iter().zip(dst_c.iter()) {
                    if !isomorphic::<_, false, STRUCTURAL>(hyperast, src, dst) {
                        return false;
                    }
                }
                true
            }
        }
        _ => false,
    }
}
