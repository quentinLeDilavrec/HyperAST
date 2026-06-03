//! # Optimized cursor
//! nodes are only persisted when captured
//! * optimizes current_status
//! * optimizes some redundant subtree resolutions
//! * prepare memorization of current subtree in cursor
//! * remove unnecessary TreeCursorStep related instructions

use hyperast::position::structural_pos;
use hyperast::types;

use types::{Childrn, HyperAST, LangRef, NodeStore as _, TypeStore};
use types::{HyperASTShared, HyperType, LabelStore, Labeled, RoleStore, Tree};
use types::{LendT, NodeId};
use types::{WithChildren, WithPrecompQueries, WithRoles};

use structural_pos::{CursorHead, CursorHeadMove};
use structural_pos::{CursorWithPersistence, PersistedNode};

use super::CursorStatus;
use crate::{Symbol, TreeCursorStep};

use crate::StatusLending;

use super::cursor_utils::{kind, resolve, text};
use super::cursor_utils3::{goto_first_child_internal, goto_next_sibling_internal, goto_parent};

pub struct PersistCursor<'hast, HAST: HyperAST> {
    pub pos: CursorWithPersistence<HAST::IdN, HAST::Idx>,
    pub p: PersistedNode<HAST::IdN, HAST::Idx>,
    /// current subtree
    n: hyperast::types::LendT<'hast, HAST>,
}

#[allow(type_alias_bounds)]
pub type TreeCursor<'hast, HAST: HyperAST> =
    super::TreeCursor<'hast, HAST, PersistCursor<'hast, HAST>>;

pub fn tree_cursor<'hast, HAST: HyperAST>(
    stores: &'hast HAST,
    mut pos: CursorWithPersistence<HAST::IdN, HAST::Idx>,
) -> TreeCursor<'hast, HAST>
where
    HAST::IdN: Copy,
{
    let p = pos.persist();
    let n = stores.node_store().resolve(&pos.node());
    let pos = PersistCursor { pos, p, n };
    TreeCursor::bind(pos, stores)
}

impl<'a, 'hast, HAST: HyperAST> crate::CNLending<'a> for self::TreeCursor<'hast, HAST>
where
    HAST::IdN: std::fmt::Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> LendT<'t, HAST>: WithRoles,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
{
    type NR = super::NodeRefK<'a, 'hast, HAST>;
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
    type Node = super::Node<'hast, HAST>;

    fn goto_next_sibling_internal(&mut self) -> TreeCursorStep {
        if self.pos.p.ref_node().eq(&self.pos.pos.ref_node()) {
            return TreeCursorStep::None;
        }
        if !goto_next_sibling_internal(self.stores, &mut self.pos.pos) {
            return TreeCursorStep::None;
        }
        self.pos.n = self.stores.node_store().resolve(&self.pos.pos.node());
        let k = decomp_t!(HAST::TS, &self.pos.n);
        if !k.is_hidden() {
            TreeCursorStep::Visible
        } else {
            TreeCursorStep::Hidden
        }
    }

    #[inline(always)]
    fn goto_first_child_internal(&mut self) -> TreeCursorStep {
        if !goto_first_child_internal(self.stores, &mut self.pos.pos) {
            return TreeCursorStep::None;
        }
        self.pos.n = self.stores.node_store().resolve(&self.pos.pos.node());

        let k = decomp_t!(HAST::TS, &self.pos.n);
        if !k.is_hidden() {
            TreeCursorStep::Visible
        } else {
            TreeCursorStep::Hidden
        }
    }

    fn goto_parent(&mut self) -> bool {
        if self.pos.p.ref_node().eq(&self.pos.pos.ref_node()) {
            return false;
        }
        if !goto_parent(self.stores, &mut self.pos.pos) {
            return false;
        }
        self.pos.n = self.stores.node_store().resolve(&self.pos.pos.node());
        true
    }

    fn current_node(&self) -> <Self as crate::CNLending<'_>>::NR {
        let pos = self.pos.pos.ref_node();
        use hyperast::types::NodeStore;
        let kind = decomp_t!(HAST::TS, &self.pos.n);
        super::NodeRefK {
            stores: self.stores,
            pos,
            kind,
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
        let pos = self.pos.pos.persist();
        super::Node {
            stores: self.stores,
            pos,
        }
    }

    fn persist_parent(&self) -> Option<Self::Node> {
        let pos = self.pos.pos.persist_parent()?;
        Some(super::Node {
            stores: self.stores,
            pos,
        })
    }

    #[inline(always)]
    fn current_status(&self) -> <Self as StatusLending<'_>>::Status {
        current_status(self.stores, &self.pos.n, &self.pos.pos)
    }

    fn text_provider(&self) -> <Self::Node as crate::TextLending<'_>>::TP {
        self.stores.label_store()
    }

    fn is_visible_at_root(&self) -> bool {
        if self.pos.pos.ref_parent().is_none() {
            return true;
        }
        let t = decomp_t!(HAST::TS, &self.pos.n);
        !t.is_hidden()
    }

    fn wont_match(&self, needed: crate::Precomps) -> bool {
        if needed == 0 {
            return false;
        }
        use hyperast::types::NodeStore;
        let id = self.pos.pos.node();
        self.pos.n.wont_match_given_precomputed_queries(needed)
    }
}

pub(super) fn current_status<'hast, HAST: HyperAST>(
    stores: &'hast HAST,
    n: &LendT<'hast, HAST>,
    pos: &CursorWithPersistence<HAST::IdN, HAST::Idx>,
) -> CursorStatus<<<HAST as HyperAST>::TS as RoleStore>::IdF>
where
    HAST::IdN: std::fmt::Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> LendT<'t, HAST>: WithRoles,
    for<'t> LendT<'t, HAST>: WithPrecompQueries,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
{
    use crate::Cursor;
    let mut field_id = Default::default();
    let mut has_later_siblings = false;
    let mut has_later_named_siblings = false;
    let mut can_have_later_siblings_with_this_field = false;
    let mut supertypes = vec![];

    let mut role = None;
    let lang = decomp_t!(HAST::TS, n).get_lang();

    let mut pos = pos.ext();

    // go up until parent is visible
    loop {
        let o = pos.offset();
        if !pos.up() {
            break;
        };
        let n = resolve(stores, &pos);
        let k = decomp_t!(HAST::TS, &n);

        if k.is_supertype() {
            let symbol = lang.ts_symbol(k).into();
            supertypes.push(symbol);
        }

        if !has_later_siblings || !has_later_named_siblings {
            let mut o = o;
            // go right
            loop {
                o += num::one();
                use hyperast::types::Children;
                use hyperast::types::WithChildren;
                let Some(sib) = n.child(&o) else {
                    // dbg!();
                    break;
                };
                let k = stores.resolve_type(&sib);
                if k.is_spaces() {
                    continue;
                }
                has_later_siblings = true;

                if k.is_supertype() {
                    has_later_siblings = true;
                    has_later_named_siblings = true;
                    break;
                }
                if !k.is_hidden() {
                    has_later_siblings = true;
                    if k.is_named() {
                        has_later_named_siblings = true;
                        break;
                    }
                }
            }
        }

        if role.is_some() {
            // role was retrieved in descendant
            // without additional data I should assume there is a later sib with same field
            can_have_later_siblings_with_this_field = true;
        }

        // get field
        if role.is_none() {
            if !k.is_supertype() {
                role = n.role_at_and_has_later::<<HAST::TS as RoleStore>::Role>(o);
                if let Some((role, later)) = role {
                    field_id = HAST::TS::intern_role(lang, role);
                    can_have_later_siblings_with_this_field = later;
                }
            }
        }
        // // compute can_have_later_siblings_with_this_field
        // if role.is_some() {
        //     //   if same field_id later
        //     if todo!("can have node matching role after o") {
        //         // can_have_later_siblings_with_this_field = true;
        //     }
        // }
        if !k.is_hidden() {
            break;
        }
    }

    CursorStatus {
        has_later_siblings,
        has_later_named_siblings,
        can_have_later_siblings_with_this_field,
        field_id,
        supertypes,
    }
}
