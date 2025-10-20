//! # Optimized cursor
//! nodes are only persisted when captured
//! * optimizes current_status
//! * optimizes some redundant subtree resolutions
//! * prepare memorization of current subtree in cursor
//! * remove unnecessary TreeCursorStep related instructions
//! * memorization of ancestors subtree in cursor
//! * stack of subtrees
use super::CursorStatus;
use super::hyperast_opt5::has_child_with_field_id;
use super::kind;
use super::resolve;
use super::text;
use crate::StatusLending;
use crate::{Status, Symbol, TreeCursorStep};
use hyperast::position::structural_pos;
use hyperast::types::{Childrn, HyperAST, LangRef, NodeStore as _, TypeStore};
use hyperast::types::{HyperASTShared, HyperType, LabelStore, Labeled, RoleStore, Tree};
use hyperast::types::{LendT, NodeId};
use hyperast::types::{WithChildren, WithPrecompQueries, WithRoles};
use structural_pos::{CursorHead, CursorHeadMove, CursorWithPersistence, PersistedNode};

pub struct TreeCursor<'hast, HAST: HyperAST> {
    pub stores: &'hast HAST,
    pub pos: CursorWithPersistence<HAST::IdN, HAST::Idx>,
    pub p: PersistedNode<HAST::IdN, HAST::Idx>,
    /// stack of subtrees
    stack: Vec<LendT<'hast, HAST>>,
}

macro_rules! decomp_t {
    ($T:ty, $n:expr) => {
        <$T as TypeStore>::decompress_type($n, std::any::TypeId::of::<<$T as TypeStore>::Ty>())
    };
}

impl<'hast, HAST: HyperAST> TreeCursor<'hast, HAST> {
    pub fn new(stores: &'hast HAST, mut pos: CursorWithPersistence<HAST::IdN, HAST::Idx>) -> Self
    where
        HAST::IdN: Copy,
    {
        let p = pos.persist();
        let n = stores.node_store().resolve(&pos.node());
        let mut stack = vec![n];
        stack.reserve(30);
        Self {
            stores,
            pos,
            p,
            stack,
        }
    }
}

impl<HAST: HyperAST> crate::WithField for self::TreeCursor<'_, HAST>
where
    HAST::TS: RoleStore,
{
    type IdF = <HAST::TS as RoleStore>::IdF;
}

impl<'a, 'hast, HAST: HyperAST> crate::CNLending<'a> for self::TreeCursor<'hast, HAST>
where
    HAST::IdN: std::fmt::Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> LendT<'t, HAST>: WithRoles,
    for<'t> LendT<'t, HAST>: WithPrecompQueries,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
{
    type NR = super::hyperast_opt4::NodeRef<'a, 'hast, HAST>;
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
    type Node = super::hyperast_opt4::Node<'hast, HAST>;

    fn goto_next_sibling_internal(&mut self) -> TreeCursorStep {
        if self.p.ref_node().eq(&self.pos.ref_node()) {
            return TreeCursorStep::None;
        }
        if !goto_next_sibling_internal(self.stores, &mut self.stack, &mut self.pos) {
            return TreeCursorStep::None;
        }
        // self.n = self.stores.node_store().resolve(&self.pos.node());
        let n = self.stack.last().unwrap();
        debug_assert_eq!(decomp_t!(HAST::TS, n), kind(self.stores, &self.pos));
        let k = decomp_t!(HAST::TS, n);
        if !k.is_hidden() {
            TreeCursorStep::Visible
        } else {
            TreeCursorStep::Hidden
        }
    }

    #[inline(always)]
    fn goto_first_child_internal(&mut self) -> TreeCursorStep {
        if !goto_first_child_internal(self.stores, &mut self.stack, &mut self.pos) {
            return TreeCursorStep::None;
        }
        // self.n = self.stores.node_store().resolve(&self.pos.node());
        let n = self.stack.last().unwrap();
        debug_assert_eq!(decomp_t!(HAST::TS, n), kind(self.stores, &self.pos));
        let k = decomp_t!(HAST::TS, n);
        if !k.is_hidden() {
            TreeCursorStep::Visible
        } else {
            TreeCursorStep::Hidden
        }
    }

    fn goto_parent(&mut self) -> bool {
        if self.p.ref_node().eq(&self.pos.ref_node()) {
            return false;
        }
        if !goto_parent(self.stores, &mut self.stack, &mut self.pos) {
            return false;
        }
        // self.n = self.stores.node_store().resolve(&self.pos.node());
        let n = self.stack.last().unwrap();
        debug_assert_eq!(decomp_t!(HAST::TS, n), kind(self.stores, &self.pos));
        true
    }

    fn current_node(&self) -> <Self as crate::CNLending<'_>>::NR {
        let pos = self.pos.ref_node();
        use hyperast::types::NodeStore;
        let n = self.stack.last().unwrap();
        let kind = decomp_t!(HAST::TS, n);
        super::hyperast_opt4::NodeRef {
            stores: self.stores,
            pos,
            kind,
        }
    }

    fn parent_is_error(&self) -> bool {
        if self.stack.len() == 1 {
            return false;
        }
        let mut s = self.pos.ref_node();
        let n = self.stack.last().unwrap();
        let Some(stack) = goto_parent_virt(self.stores, &*self.stack, &mut s) else {
            return false;
        };
        let n = stack.last().unwrap();
        let t = decomp_t!(HAST::TS, n);
        t.is_error()
    }

    fn has_parent(&self) -> bool {
        let mut s = self.pos.ref_node();
        super::hyperast_opt5::goto_parent(self.stores, &mut s)
    }

    fn persist(&self) -> Self::Node {
        let pos = self.pos.persist();
        super::hyperast_opt4::Node {
            stores: self.stores,
            pos,
        }
    }

    fn persist_parent(&self) -> Option<Self::Node> {
        let pos = self.pos.persist_parent()?;
        Some(super::hyperast_opt4::Node {
            stores: self.stores,
            pos,
        })
    }

    #[inline(always)]
    fn current_status(&self) -> <Self as StatusLending<'_>>::Status {
        let n = self.stack.last().unwrap();
        if cfg!(not(debug_assertions)) {
            return current_status(self.stores, n, &self.pos);
        }
        // let old = crate::current_status(self.stores, &self.pos);
        let new = current_status(self.stores, n, &self.pos);
        // if old.has_later_siblings != new.has_later_siblings
        //     || old.has_later_named_siblings != new.has_later_named_siblings
        //     || (old.can_have_later_siblings_with_this_field
        //         && !new.can_have_later_siblings_with_this_field)
        //     || old.field_id != new.field_id
        // {
        //     dbg!(old.has_later_siblings, new.has_later_siblings);
        //     dbg!(old.has_later_named_siblings, new.has_later_named_siblings);
        //     dbg!(
        //         old.can_have_later_siblings_with_this_field,
        //         new.can_have_later_siblings_with_this_field
        //     );
        //     let lang = kind(self.stores, &self.pos).get_lang();
        //     dbg!(old.field_id != new.field_id);
        //     // dbg!(
        //     //     HAST::TS::resolve_field(lang.clone(), old.field_id).,
        //     //     HAST::TS::resolve_field(lang.clone(), new.field_id)
        //     // );
        //     // let old_f: u16 = unsafe { std::mem::transmute(old.field_id) };
        //     // dbg!(old_f);
        //     // let new_f: u16 = unsafe { std::mem::transmute(new.field_id) };
        //     // dbg!(new_f);
        //     for t in old.supertypes {
        //         let old: u16 = unsafe { std::mem::transmute(t) };
        //         dbg!(old);
        //     }
        //     for t in new.supertypes {
        //         let new: u16 = unsafe { std::mem::transmute(t) };
        //         dbg!(new);
        //     }
        //     panic!()
        // }
        new
    }

    fn text_provider(&self) -> <Self::Node as crate::TextLending<'_>>::TP {
        self.stores.label_store()
    }

    fn is_visible_at_root(&self) -> bool {
        // assert!(self.pos.ref_parent().is_none());
        if self.pos.ref_parent().is_none() {
            return true;
        }
        let n = self.stack.last().unwrap();
        let t = decomp_t!(HAST::TS, n);
        !t.is_hidden()
    }

    fn wont_match(&self, needed: crate::Precomps) -> bool {
        if needed == 0 {
            return false;
        }
        use hyperast::types::NodeStore;
        let id = self.pos.node();
        let n = self.stack.last().unwrap();
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
        let n = resolve(stores, &pos);
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
