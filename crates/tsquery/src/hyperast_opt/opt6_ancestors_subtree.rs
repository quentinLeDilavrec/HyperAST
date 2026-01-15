//! # Optimized cursor
//! nodes are only persisted when captured
//! * optimizes current_status
//! * optimizes some redundant subtree resolutions
//! * prepare memorization of current subtree in cursor
//! * remove unnecessary TreeCursorStep related instructions
//! * memorization of ancestors subtree in cursor
//! * stack of subtrees

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

use super::cursor_utils;
use cursor_utils::kind;

pub struct PersistCursor<'hast, HAST: HyperAST> {
    pub pos: CursorWithPersistence<HAST::IdN, HAST::Idx>,
    pub p: PersistedNode<HAST::IdN, HAST::Idx>,
    /// stack of ancestors' subtrees
    stack: Vec<LendT<'hast, HAST>>,
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
    let mut stack = vec![n];
    stack.reserve(30);
    let pos = PersistCursor { pos, p, stack };
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
        if !goto_next_sibling_internal(self.stores, &mut self.pos.stack, &mut self.pos.pos) {
            return TreeCursorStep::None;
        }
        let n = self.pos.stack.last().unwrap();
        debug_assert_eq!(decomp_t!(HAST::TS, n), kind(self.stores, &self.pos.pos));
        let k = decomp_t!(HAST::TS, n);
        if !k.is_hidden() {
            TreeCursorStep::Visible
        } else {
            TreeCursorStep::Hidden
        }
    }

    #[inline(always)]
    fn goto_first_child_internal(&mut self) -> TreeCursorStep {
        if !goto_first_child_internal(self.stores, &mut self.pos.stack, &mut self.pos.pos) {
            return TreeCursorStep::None;
        }
        let n = self.pos.stack.last().unwrap();
        debug_assert_eq!(decomp_t!(HAST::TS, n), kind(self.stores, &self.pos.pos));
        let k = decomp_t!(HAST::TS, n);
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
        if !goto_parent(self.stores, &mut self.pos.stack, &mut self.pos.pos) {
            return false;
        }
        let n = self.pos.stack.last().unwrap();
        debug_assert_eq!(decomp_t!(HAST::TS, n), kind(self.stores, &self.pos.pos));
        true
    }

    fn current_node(&self) -> <Self as crate::CNLending<'_>>::NR {
        let pos = self.pos.pos.ref_node();
        use hyperast::types::NodeStore;
        let n = self.pos.stack.last().unwrap();
        let kind = decomp_t!(HAST::TS, n);
        super::NodeRefK {
            stores: self.stores,
            pos,
            kind,
        }
    }

    fn parent_is_error(&self) -> bool {
        if self.pos.stack.len() == 1 {
            return false;
        }
        let mut s = self.pos.pos.ref_node();
        let n = self.pos.stack.last().unwrap();
        let Some(stack) = goto_parent_virt(self.stores, &self.pos.stack, &mut s) else {
            return false;
        };
        let n = stack.last().unwrap();
        let t = decomp_t!(HAST::TS, n);
        t.is_error()
    }

    fn has_parent(&self) -> bool {
        let mut s = self.pos.pos.ref_node();
        super::cursor_utils3::goto_parent(self.stores, &mut s)
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
        let n = self.pos.stack.last().unwrap();
        current_status(self.stores, n, &self.pos.pos)
    }

    fn text_provider(&self) -> <Self::Node as crate::TextLending<'_>>::TP {
        self.stores.label_store()
    }

    fn is_visible_at_root(&self) -> bool {
        // assert!(self.pos.pos.ref_parent().is_none());
        if self.pos.pos.ref_parent().is_none() {
            return true;
        }
        let n = self.pos.stack.last().unwrap();
        let t = decomp_t!(HAST::TS, n);
        !t.is_hidden()
    }

    fn wont_match(&self, needed: crate::Precomps) -> bool {
        if needed == 0 {
            return false;
        }
        use hyperast::types::NodeStore;
        let id = self.pos.pos.node();
        let n = self.pos.stack.last().unwrap();
        n.wont_match_given_precomputed_queries(needed)
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
        let n = cursor_utils::resolve(stores, &pos);
        let k = decomp_t!(HAST::TS, &n);
        // dbg!(k);
        // dbg!(o);
        // dbg!(n.child_count());

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
                // dbg!(k);
                has_later_siblings = true;

                if k.is_supertype() {
                    has_later_siblings = true;
                    has_later_named_siblings = true;
                    // dbg!(k);
                    break;
                }
                if !k.is_hidden() {
                    has_later_siblings = true;
                    if k.is_named() {
                        has_later_named_siblings = true;
                        // dbg!(k);
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
                    field_id = HAST::TS::intern_role(lang.clone(), role);
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

fn goto_parent<'hast, HAST: HyperAST>(
    stores: &'hast HAST,
    stack: &mut Vec<LendT<'hast, HAST>>,
    pos: &mut impl CursorHead<HAST::IdN, HAST::Idx>,
) -> bool
where
    HAST::IdN: NodeId<IdN = HAST::IdN>,
    HAST::IdN: Copy,
{
    loop {
        if !pos.up() {
            return false;
        }
        stack.pop().unwrap();
        let n = stack.last().unwrap();
        if !decomp_t!(HAST::TS, n).is_hidden() {
            return true;
        }
    }
}

fn goto_parent_virt<'a, 'hast, HAST: HyperAST>(
    stores: &'hast HAST,
    mut stack: &'a [LendT<'hast, HAST>],
    pos: &mut impl CursorHead<HAST::IdN, HAST::Idx>,
) -> Option<&'a [LendT<'hast, HAST>]>
where
    HAST::IdN: NodeId<IdN = HAST::IdN>,
    HAST::IdN: Copy,
{
    loop {
        if !pos.up() {
            return None;
        }
        stack = &stack[..stack.len() - 1];
        let n = stack.last().unwrap();
        if !decomp_t!(HAST::TS, n).is_hidden() {
            return Some(stack);
        }
    }
}

#[inline(always)]
fn goto_next_sibling_internal<'hast, HAST: HyperAST>(
    stores: &'hast HAST,
    stack: &mut Vec<LendT<'hast, HAST>>,
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
    let prev_n = stack.pop().unwrap();
    let n = stack.last().unwrap();
    use hyperast::types::Children;
    use hyperast::types::WithChildren;
    let Some(node) = n.child(&(pos.offset() + num::one())) else {
        // self.n = self.stores.node_store().resolve(&self.pos.node());
        let k = decomp_t!(HAST::TS, n);
        if k.is_hidden() {
            // dbg!(stores.resolve_type(&p));
            pos.up();
            return goto_next_sibling_internal(stores, stack, pos);
        } else {
            stack.push(prev_n);
            return false;
        }
    };
    pos.inc(node);
    let n = stores.node_store().resolve(&node);
    let k = decomp_t!(HAST::TS, &n);
    stack.push(n);
    if k.is_spaces() {
        return goto_next_sibling_internal(stores, stack, pos);
    }
    true
}

#[inline(always)]
fn goto_first_child_internal<'hast, HAST: HyperAST>(
    stores: &'hast HAST,
    stack: &mut Vec<LendT<'hast, HAST>>,
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
        let n = stores.node_store().resolve(&id);
        let k = decomp_t!(HAST::TS, &n);
        if !k.is_spaces() {
            pos.down(id, o);
            stack.push(n);
            return true;
        }

        o += num::one();
    }
    unreachable!()
}

// fn child_by_role<'hast, HAST: HyperAST>(
//     stores: &'hast HAST,
//     pos: &mut (impl BBB<HAST::IdN, HAST::Idx> + Clone),
//     _role: <HAST::TS as RoleStore>::Role,
// ) -> Option<()>
// where
//     <HAST as HyperAST>::TS: RoleStore,
//     <HAST as HyperASTShared>::IdN: Copy,
//     HAST::IdN: hyperast::types::NodeId<IdN = HAST::IdN>,
//     HAST::TS: RoleStore,
//     for<'t> hyperast::types::LendT<'t, HAST>: WithRoles,
// {
//     // TODO what about multiple children with same role?
//     // NOTE treesitter uses a bin tree for repeats
//     let visible = is_visible(stores, pos);
//     if !goto_first_child_internal(stores, pos) {
//         return None;
//     }
//     loop {
//         if let Some(r) = role(stores, &mut pos.clone()) {
//             if r == _role {
//                 return Some(());
//             } else {
//                 if !goto_next_sibling_internal(stores, pos) {
//                     return None;
//                 }
//                 continue;
//             }
//         }
//         // do not go down
//         if visible {
//             if !goto_next_sibling_internal(stores, pos) {
//                 return None;
//             }
//         }
//         // hidden node so can explore
//         else {
//             if child_by_role(stores, pos, _role).is_some() {
//                 return Some(());
//             }
//             if !goto_next_sibling_internal(stores, pos) {
//                 return None;
//             }
//         }
//     }
// }
