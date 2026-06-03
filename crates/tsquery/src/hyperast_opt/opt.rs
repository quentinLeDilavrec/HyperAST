use hyperast::position::structural_pos;
use hyperast::types;

use types::RoleStore;
use types::{HyperAST, HyperASTShared, LendT};
use types::{HyperType as _, NodeId};
use types::{WithPrecompQueries, WithRoles};

use structural_pos::CursorHead as _;
use structural_pos::{CursorWithPersistence, PersistedNode};

use crate::{BiCow, Point, Symbol, TreeCursorStep};
use crate::{StatusLending, TextLending};

use super::CursorStatus;
use super::{Node, NodeRef};

use super::cursor_utils::SuperTypeIter;

use super::cursor_utils::{goto_first_child_internal, goto_next_sibling_internal, goto_parent};
use super::cursor_utils::{is_visible, kind, role};

pub struct PersistCursor<IdN, Idx> {
    pub pos: CursorWithPersistence<IdN, Idx>,
    pub p: PersistedNode<IdN, Idx>,
}

#[allow(type_alias_bounds)]
pub type TreeCursor<'hast, HAST: HyperASTShared> =
    super::TreeCursor<'hast, HAST, PersistCursor<HAST::IdN, HAST::Idx>>;

pub fn tree_cursor<'hast, HAST: HyperAST>(
    stores: &'hast HAST,
    mut pos: CursorWithPersistence<HAST::IdN, HAST::Idx>,
) -> TreeCursor<'hast, HAST> {
    let p = pos.persist();
    let pos = PersistCursor { pos, p };
    TreeCursor::bind(pos, stores)
}

impl<IdN: Copy + PartialEq, Idx: Copy> Into<CursorWithPersistence<IdN, Idx>>
    for PersistCursor<IdN, Idx>
{
    fn into(mut self) -> CursorWithPersistence<IdN, Idx> {
        while self.pos.node() != self.p.node() {
            assert!(self.pos.up());
        }
        self.pos
    }
}

impl<'hast, HAST: HyperAST> From<(&'hast HAST, CursorWithPersistence<HAST::IdN, HAST::Idx>)>
    for TreeCursor<'hast, HAST>
{
    fn from(value: (&'hast HAST, CursorWithPersistence<HAST::IdN, HAST::Idx>)) -> Self {
        let (stores, pos) = value;
        tree_cursor(stores, pos)
    }
}

impl<'a, 'hast, HAST: HyperAST> crate::CNLending<'a> for self::TreeCursor<'hast, HAST>
where
    HAST::IdN: std::fmt::Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> LendT<'t, HAST>: WithRoles,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
{
    type NR = self::NodeRef<'a, 'hast, HAST>;
}

impl<'a, 'hast, HAST: HyperAST> crate::StatusLending<'a> for self::TreeCursor<'hast, HAST>
where
    HAST::TS: RoleStore,
{
    type Status = CursorStatus<<<HAST as HyperAST>::TS as RoleStore>::IdF>;
}

impl<'hast, HAST: HyperAST> crate::Cursor for self::TreeCursor<'hast, HAST>
where
    HAST::IdN: std::fmt::Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> LendT<'t, HAST>: WithRoles,
    for<'t> LendT<'t, HAST>: WithPrecompQueries,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
{
    type Node = self::Node<'hast, HAST>;

    fn goto_next_sibling_internal(&mut self) -> TreeCursorStep {
        if self.pos.p.ref_node().eq(&self.pos.pos.ref_node()) {
            return TreeCursorStep::None;
        }
        goto_next_sibling_internal(self.stores, &mut self.pos.pos)
    }

    fn goto_first_child_internal(&mut self) -> TreeCursorStep {
        goto_first_child_internal(self.stores, &mut self.pos.pos)
    }

    fn goto_parent(&mut self) -> bool {
        if self.pos.p.ref_node().eq(&self.pos.pos.ref_node()) {
            return false;
        }
        goto_parent(self.stores, &mut self.pos.pos)
    }

    fn current_node(&self) -> <Self as crate::CNLending<'_>>::NR {
        NodeRef {
            stores: self.stores,
            pos: self.pos.pos.ref_node(),
        }
    }

    fn parent_is_error(&self) -> bool {
        // NOTE: maybe more efficient impl
        let mut s = self.pos.pos.ref_node();
        if !goto_parent(self.stores, &mut s) {
            return false;
        }
        kind(self.stores, &s).is_error()
    }

    fn has_parent(&self) -> bool {
        let mut s = self.pos.pos.ref_node();
        goto_parent(self.stores, &mut s)
    }

    fn persist(&self) -> Self::Node {
        Node {
            stores: self.stores,
            pos: self.pos.pos.persist(),
        }
    }

    fn persist_parent(&self) -> Option<Self::Node> {
        Some(Node {
            stores: self.stores,
            pos: self.pos.pos.persist_parent()?,
        })
    }

    fn current_status(&self) -> <Self as StatusLending<'_>>::Status {
        current_status(self.stores, &self.pos.pos)
    }

    fn text_provider(&self) -> <Self::Node as crate::TextLending<'_>>::TP {
        self.stores.label_store()
    }

    fn is_visible_at_root(&self) -> bool {
        // assert!(self.pos.ref_parent().is_none());
        if self.pos.pos.ref_parent().is_none() {
            return true;
        }
        is_visible(self.stores, &self.pos.pos)
    }

    fn wont_match(&self, needed: crate::Precomps) -> bool {
        if needed == 0 {
            return false;
        }
        use hyperast::types::NodeStore;
        let id = self.pos.pos.node();
        let n = self.stores.node_store().resolve(&id);
        n.wont_match_given_precomputed_queries(needed)
    }
}

pub(super) fn current_status<'hast, HAST: HyperAST>(
    stores: &'hast HAST,
    pos: &CursorWithPersistence<HAST::IdN, HAST::Idx>,
) -> CursorStatus<<<HAST as HyperAST>::TS as RoleStore>::IdF>
where
    HAST::IdN: std::fmt::Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> LendT<'t, HAST>: WithRoles,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
{
    use crate::Cursor;
    let (_role, field_id) = NodeRef {
        stores,
        pos: pos.ref_node(),
    }
    .compute_current_role();
    let mut has_later_siblings = false;
    let mut has_later_named_siblings = false;
    let mut can_have_later_siblings_with_this_field = false;
    let supertypes = SuperTypeIter {
        stores,
        pos: pos.ref_node(),
    };
    let mut pos = pos.ext();
    loop {
        if let TreeCursorStep::None = goto_next_sibling_internal(stores, &mut pos) {
            break;
        }
        if _role.is_some() && role(stores, &mut pos.clone()) == _role {
            can_have_later_siblings_with_this_field = true;
        }
        let k = kind(stores, &pos);
        if k.is_spaces() {
            continue;
        }
        has_later_siblings = true;
        if k.is_supertype() {
            has_later_named_siblings = true;
        }
        if is_visible(stores, &pos) {
            has_later_siblings = true;
            if k.is_named() {
                has_later_named_siblings = true;
                break;
            }
        }
    }
    let supertypes = supertypes.collect();
    CursorStatus {
        has_later_siblings,
        has_later_named_siblings,
        can_have_later_siblings_with_this_field,
        field_id,
        supertypes,
    }
}
