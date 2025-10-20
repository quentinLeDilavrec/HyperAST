//! Utilities for simplified cursor
//!   * do not directly differentiate hidden and visible nodes

use hyperast::position::structural_pos;
use hyperast::types;

use types::{Childrn as _, NodeId};
use types::{HyperAST, HyperASTShared, LendT};
use types::{HyperType as _, Labeled as _, Tree as _};
use types::{RoleStore, TypeStore};
use types::{WithPrecompQueries, WithRoles};

use structural_pos::{CursorHead, CursorHeadMove};

use crate::{Symbol, TreeCursorStep};

use super::cursor_utils::{is_visible, kind, resolve, role};

pub(super) fn goto_parent<HAST: HyperAST>(
    stores: &HAST,
    pos: &mut impl CursorHead<HAST::IdN, HAST::Idx>,
) -> bool {
    loop {
        if !pos.up() {
            return false;
        }
        if is_visible(stores, pos) {
            return true;
        }
    }
}

#[inline(always)]
pub(super) fn goto_next_sibling_internal<HAST: HyperAST>(
    stores: &HAST,
    pos: &mut impl CursorHeadMove<HAST::IdN, HAST::Idx>,
) -> bool
where
    HAST::IdN: NodeId<IdN = HAST::IdN>,
    HAST::IdN: Copy,
{
    use hyperast::types::NodeStore;
    let Some(p) = pos.parent() else {
        return false;
    };
    let n = stores.node_store().resolve(&p);
    use hyperast::types::Children;
    use hyperast::types::WithChildren;
    let Some(node) = n.child(&(pos.offset() + num::one())) else {
        let k = decomp_t!(HAST::TS, &n);
        if k.is_hidden() {
            pos.up();
            return goto_next_sibling_internal(stores, pos);
        } else {
            return false;
        }
    };
    pos.inc(node);
    let k = kind(stores, pos);
    if k.is_spaces() {
        return goto_next_sibling_internal(stores, pos);
    }
    true
}

#[inline(always)]
pub(super) fn goto_first_child_internal<HAST: HyperAST>(
    stores: &HAST,
    pos: &mut impl CursorHeadMove<HAST::IdN, HAST::Idx>,
) -> bool
where
    HAST::IdN: Copy,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
{
    use hyperast::types::NodeStore;
    let n = stores.node_store().resolve(&pos.node());
    use hyperast::types::Children;
    use hyperast::types::WithChildren;
    let Some(cs) = n.children() else {
        return false;
    };
    let mut o = num::zero();
    for id in cs.iter_children() {
        let k = stores.resolve_type(&id);

        if !k.is_spaces() {
            pos.down(id, o);
            return true;
        }

        o += num::one();
    }
    unreachable!()
}

pub(super) fn has_child_with_field_id<'hast, HAST: HyperAST>(
    stores: &'hast HAST,
    pos: impl CursorHeadMove<HAST::IdN, HAST::Idx> + Clone + CursorHead<HAST::IdN, HAST::Idx>,
    field_id: <HAST::TS as RoleStore>::IdF,
) -> bool
where
    HAST::IdN: std::fmt::Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> LendT<'t, HAST>: WithRoles,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
{
    if field_id == Default::default() {
        return false;
    }
    let role = HAST::TS::resolve_field(kind(stores, &pos).get_lang(), field_id);
    let mut pos = pos;
    loop {
        if !kind(stores, &pos).is_supertype() {
            break;
        }
        if !goto_first_child_internal(stores, &mut pos) {
            panic!();
        }
        let k = kind(stores, &pos);
        if !k.is_hidden() {
            break;
        }
    }
    child_by_role(stores, &mut pos, role).is_some()
}

pub(super) fn child_by_role<'hast, HAST: HyperAST>(
    stores: &'hast HAST,
    pos: &mut (impl CursorHeadMove<HAST::IdN, HAST::Idx> + Clone),
    _role: <HAST::TS as RoleStore>::Role,
) -> Option<()>
where
    <HAST as HyperAST>::TS: RoleStore,
    <HAST as HyperASTShared>::IdN: Copy,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
    HAST::TS: RoleStore,
    for<'t> LendT<'t, HAST>: WithRoles,
{
    // TODO what about multiple children with same role?
    // NOTE treesitter uses a bin tree for repeats
    let visible = is_visible(stores, pos);
    if !goto_first_child_internal(stores, pos) {
        return None;
    }
    loop {
        if let Some(r) = role(stores, &mut pos.clone()) {
            if r == _role {
                return Some(());
            } else {
                if !goto_next_sibling_internal(stores, pos) {
                    return None;
                }
                continue;
            }
        }
        // do not go down
        if visible {
            if !goto_next_sibling_internal(stores, pos) {
                return None;
            }
        }
        // hidden node so can explore
        else {
            if child_by_role(stores, pos, _role).is_some() {
                return Some(());
            }
            if !goto_next_sibling_internal(stores, pos) {
                return None;
            }
        }
    }
}
