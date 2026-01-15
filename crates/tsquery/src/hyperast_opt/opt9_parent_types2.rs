//! # Optimized cursor
//! nodes are only persisted when captured
//! * optimizes current_status
//! * optimizes some redundant subtree resolutions
//! * prepare memorization of current subtree in cursor
//! * remove unnecessary TreeCursorStep related instructions
//! * memorization of ancestors subtree in cursor
//! * stack of subtrees
//! * stack of is_hidden
//! * use stack for status internal call to goto_parent
//! * stack for errors
//! * stack for types (and supertypes)
//! * further opt current_status
//! * remove the stack of nodes (benefit: no lifetime on HyperAST nodes)

use hyperast::position::structural_pos;
use hyperast::{store, types};

use types::{Childrn, HyperAST, LangRef, NodeStore as _, TypeStore};
use types::{HyperASTShared, HyperType, LabelStore, Labeled, RoleStore, Tree};
use types::{LendT, NodeId};
use types::{WithChildren, WithPrecompQueries, WithRoles};

use structural_pos::{CursorHead, CursorHeadMove};
use structural_pos::{CursorWithPersistence, PersistedNode};

use crate::{Symbol, TreeCursorStep};

use crate::StatusLending;

use super::BitSet as _;
use super::cursor_utils;
use super::opt8_parent_error::MiscStacks;
use cursor_utils::{kind, resolve};

pub struct PersistCursor<HAST: HyperAST> {
    pub pos: CursorWithPersistence<HAST::IdN, HAST::Idx>,
    pub p: PersistedNode<HAST::IdN, HAST::Idx>,
    misc_stacks: MiscStacks,
    types: Vec<<HAST::TS as TypeStore>::Ty>,
}

type Hiddens = usize;
type Errors = usize;

#[allow(type_alias_bounds)]
pub type TreeCursor<'hast, HAST: HyperAST> = super::TreeCursor<'hast, HAST, PersistCursor<HAST>>;

pub fn tree_cursor<'hast, HAST: HyperAST>(
    stores: &'hast HAST,
    mut pos: CursorWithPersistence<HAST::IdN, HAST::Idx>,
) -> TreeCursor<'hast, HAST>
where
    HAST::IdN: Copy,
{
    let p = pos.persist();
    let n = stores.node_store().resolve(&pos.node());
    let mut types = vec![decomp_t!(HAST::TS, &n)];
    types.reserve(30);
    // root node must be visible
    debug_assert!(!decomp_t!(HAST::TS, &n).is_hidden());
    // debug_assert!(!decomp_t!(HAST::TS, &n).is_error());
    let hiddens = 0;
    let errors = 0;
    let pos = PersistCursor {
        pos,
        p,
        misc_stacks: MiscStacks { hiddens, errors },
        types,
    };
    TreeCursor::bind(pos, stores)
}

impl<HAST: HyperAST> Into<CursorWithPersistence<HAST::IdN, HAST::Idx>> for PersistCursor<HAST>
where
    HAST::IdN: Copy,
    HAST::Idx: Copy,
{
    fn into(mut self) -> CursorWithPersistence<HAST::IdN, HAST::Idx> {
        while self.pos.node() != self.p.node() {
            assert!(self.pos.up());
        }
        self.pos
    }
}

pub struct CursorStatus<'a, IdF> {
    pub has_later_siblings: bool,
    pub has_later_named_siblings: bool,
    pub can_have_later_siblings_with_this_field: bool,
    pub field_id: IdF,
    pub supertypes: &'a [Symbol],
}

impl<'a, IdF: Copy> crate::Status for CursorStatus<'a, IdF> {
    type IdF = IdF;

    fn has_later_siblings(&self) -> bool {
        self.has_later_siblings
    }

    fn has_later_named_siblings(&self) -> bool {
        self.has_later_named_siblings
    }

    fn can_have_later_siblings_with_this_field(&self) -> bool {
        self.can_have_later_siblings_with_this_field
    }

    fn field_id(&self) -> Self::IdF {
        self.field_id
    }

    fn has_supertypes(&self) -> bool {
        !self.supertypes.is_empty()
    }

    fn contains_supertype(&self, sym: Symbol) -> bool {
        self.supertypes.contains(&sym)
    }
}

impl<'a, 'hast, HAST: HyperAST> crate::CNLending<'a> for self::TreeCursor<'hast, HAST>
where
    HAST::IdN: std::fmt::Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> LendT<'t, HAST>: WithRoles,
    HAST::IdN: hyperast::types::NodeId<IdN = HAST::IdN>,
{
    type NR = super::NodeRefK<'a, 'hast, HAST>;
}

impl<'a, 'hast, HAST: HyperAST> crate::StatusLending<'a> for self::TreeCursor<'hast, HAST>
where
    HAST::TS: RoleStore,
{
    type Status = CursorStatus<'a, <<HAST as HyperAST>::TS as RoleStore>::IdF>;
}

impl<'hast, HAST: HyperAST> crate::Cursor for self::TreeCursor<'hast, HAST>
where
    HAST::IdN: std::fmt::Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> LendT<'t, HAST>: WithRoles,
    for<'t> LendT<'t, HAST>: WithPrecompQueries,
    HAST::IdN: hyperast::types::NodeId<IdN = HAST::IdN>,
{
    type Node = super::Node<'hast, HAST>;

    #[inline(never)]
    fn goto_next_sibling_internal(&mut self) -> TreeCursorStep {
        if self.pos.p.ref_node().eq(&self.pos.pos.ref_node()) {
            return TreeCursorStep::None;
        }
        if !goto_next_sibling_internal(
            self.stores,
            &mut self.pos.types,
            &mut self.pos.misc_stacks,
            &mut self.pos.pos,
        ) {
            return TreeCursorStep::None;
        }
        if !self.pos.misc_stacks.hiddens.bit(self.pos.types.len() - 1) {
            TreeCursorStep::Visible
        } else {
            TreeCursorStep::Hidden
        }
    }

    #[inline(never)]
    fn goto_first_child_internal(&mut self) -> TreeCursorStep {
        if !goto_first_child_internal(
            self.stores,
            &mut self.pos.types,
            &mut self.pos.misc_stacks,
            &mut self.pos.pos,
        ) {
            return TreeCursorStep::None;
        }
        if !self.pos.misc_stacks.hiddens.bit(self.pos.types.len() - 1) {
            TreeCursorStep::Visible
        } else {
            TreeCursorStep::Hidden
        }
    }

    #[inline(never)]
    fn goto_parent(&mut self) -> bool {
        if self.pos.p.ref_node().eq(&self.pos.pos.ref_node()) {
            return false;
        }
        if !goto_parent(
            self.stores,
            &mut self.pos.types,
            &mut self.pos.misc_stacks,
            &mut self.pos.pos,
        ) {
            return false;
        }
        true
    }

    #[inline(never)]
    fn current_node(&self) -> <Self as crate::CNLending<'_>>::NR {
        let pos = self.pos.pos.ref_node();
        use hyperast::types::NodeStore;
        let kind = *self.pos.types.last().unwrap();
        super::NodeRefK {
            stores: self.stores,
            pos,
            kind,
        }
    }

    #[inline(never)]
    fn parent_is_error(&self) -> bool {
        if self.pos.types.len() == 1 {
            return false;
        }
        let mut s = self.pos.pos.ref_node();
        let Some(_) = goto_parent_virt(self.stores, &self.pos.types, &self.pos.misc_stacks, &mut s)
        else {
            return false;
        };
        self.pos.misc_stacks.errors.bit(self.pos.types.len() - 1)
    }

    #[inline(never)]
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

    #[inline(never)]
    fn current_status(&self) -> <Self as StatusLending<'_>>::Status {
        current_status(
            self.stores,
            &self.pos.types,
            &self.pos.misc_stacks,
            &self.pos.pos,
        )
    }

    fn text_provider(&self) -> <Self::Node as crate::TextLending<'_>>::TP {
        self.stores.label_store()
    }

    #[inline(never)]
    fn is_visible_at_root(&self) -> bool {
        if self.pos.pos.ref_parent().is_none() {
            return true;
        }
        !self.pos.misc_stacks.hiddens.bit(self.pos.types.len() - 1)
    }

    #[inline(never)]
    fn wont_match(&self, needed: crate::Precomps) -> bool {
        if needed == 0 {
            return false;
        }
        let n = resolve(self.stores, &self.pos.pos);
        n.wont_match_given_precomputed_queries(needed)
    }
}

#[inline(always)]
pub(super) fn current_status<'a, 'hast, HAST: HyperAST>(
    stores: &'hast HAST,
    types: &'a [<HAST::TS as TypeStore>::Ty],
    misc_stacks: &MiscStacks,
    pos: &CursorWithPersistence<HAST::IdN, HAST::Idx>,
) -> CursorStatus<'a, <<HAST as HyperAST>::TS as RoleStore>::IdF>
where
    HAST::IdN: std::fmt::Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> LendT<'t, HAST>: WithRoles,
    for<'t> LendT<'t, HAST>: WithPrecompQueries,
    HAST::IdN: hyperast::types::NodeId<IdN = HAST::IdN>,
{
    use crate::Cursor;
    let mut field_id = Default::default();
    let mut has_later_siblings = false;
    let mut has_later_named_siblings = false;
    let mut can_have_later_siblings_with_this_field = false;

    let mut role = None;
    let lang = types.last().unwrap().get_lang();

    let mut pos = pos.ext();
    let mut stack = types;
    let mut first_st = usize::MAX;

    // go up until parent is visible
    loop {
        let o = pos.offset();
        if !pos.up() {
            break;
        };
        stack = &stack[..stack.len() - 1];
        let Some(k) = stack.last() else {
            break;
        };
        let n = resolve(stores, &pos);
        // let k = types[stack.len() - 1];

        if k.is_supertype() {
            first_st = stack.len();
        }

        if !has_later_siblings || !has_later_named_siblings {
            let mut o = o;
            let cs = n.children().unwrap();
            // go right
            loop {
                o += num::one();
                use hyperast::types::Children;
                use hyperast::types::WithChildren;
                let Some(sib) = cs.get(o) else {
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
        if !misc_stacks.hiddens.bit(stack.len() - 1) {
            break;
        }
    }

    // field_id = if let Some(role) = role {
    //     HAST::TS::intern_role(lang.clone(), role);
    // } else { None };
    let supertypes = if first_st != usize::MAX {
        unsafe {
            std::mem::transmute::<&[<HAST::TS as hyperast::types::TypeStore>::Ty], &[Symbol]>(
                &types[stack.len() - 1..first_st],
            )
        }
    } else {
        &[]
    };
    CursorStatus {
        has_later_siblings,
        has_later_named_siblings,
        can_have_later_siblings_with_this_field,
        field_id,
        supertypes,
    }
}

fn role<'hast, HAST: HyperAST>(
    stores: &'hast HAST,
    pos: &mut impl CursorHead<HAST::IdN, HAST::Idx>,
) -> Option<<HAST::TS as RoleStore>::Role>
where
    HAST::TS: RoleStore,
    for<'t> LendT<'t, HAST>: WithRoles,
{
    let at = pos.offset();
    if !pos.up() {
        return None;
    }
    let n = cursor_utils::resolve(stores, pos);
    n.role_at::<<HAST::TS as RoleStore>::Role>(at)
}

fn goto_parent<HAST: HyperAST>(
    stores: &HAST,
    types: &mut Vec<<HAST::TS as TypeStore>::Ty>,
    misc_stacks: &mut MiscStacks,
    pos: &mut impl CursorHead<HAST::IdN, HAST::Idx>,
) -> bool
where
    HAST::IdN: hyperast::types::NodeId<IdN = HAST::IdN>,
    HAST::IdN: Copy,
{
    loop {
        if !pos.up() {
            return false;
        }
        types.pop().unwrap();
        if !misc_stacks.hiddens.bit(types.len() - 1) {
            return true;
        }
    }
}

fn goto_parent_virt<HAST: HyperAST>(
    stores: &HAST,
    mut types: &[<HAST::TS as TypeStore>::Ty],
    misc_stacks: &MiscStacks,
    pos: &mut impl CursorHead<HAST::IdN, HAST::Idx>,
) -> Option<()>
where
    HAST::IdN: hyperast::types::NodeId<IdN = HAST::IdN>,
    HAST::IdN: Copy,
{
    loop {
        if !pos.up() {
            return None;
        }
        types = &types[..types.len() - 1];
        if !misc_stacks.hiddens.bit(types.len() - 1) {
            return Some(());
        }
    }
}

fn goto_next_sibling_internal<HAST: HyperAST>(
    stores: &HAST,
    types: &mut Vec<<HAST::TS as TypeStore>::Ty>,
    misc_stacks: &mut MiscStacks,
    pos: &mut impl CursorHeadMove<HAST::IdN, HAST::Idx>,
) -> bool
where
    HAST::IdN: hyperast::types::NodeId<IdN = HAST::IdN>,
    HAST::IdN: Copy,
{
    use hyperast::types::NodeStore;
    let Some(p) = pos.parent() else {
        return false;
    };
    let prev_t = types.pop().unwrap();
    let Some(_) = types.last() else {
        return false;
    };
    let n = stores.node_store().resolve(&p);
    use hyperast::types::Children;
    use hyperast::types::WithChildren;
    let Some(node) = n.child(&(pos.offset() + num::one())) else {
        if misc_stacks.hiddens.bit(types.len() - 1) {
            pos.up();
            return goto_next_sibling_internal(stores, types, misc_stacks, pos);
        } else {
            types.push(prev_t);
            return false;
        }
    };
    pos.inc(node);
    let n = stores.node_store().resolve(&node);
    let k = decomp_t!(HAST::TS, &n);
    types.push(k);
    misc_stacks.hiddens.set(types.len() - 1, k.is_hidden());
    misc_stacks.errors.set(types.len() - 1, k.is_error());
    if k.is_spaces() {
        return goto_next_sibling_internal(stores, types, misc_stacks, pos);
    }
    true
}

fn goto_first_child_internal<HAST: HyperAST>(
    stores: &HAST,
    types: &mut Vec<<HAST::TS as TypeStore>::Ty>,
    misc_stacks: &mut MiscStacks,
    pos: &mut impl CursorHeadMove<HAST::IdN, HAST::Idx>,
) -> bool
where
    HAST::IdN: Copy,
    HAST::IdN: hyperast::types::NodeId<IdN = HAST::IdN>,
{
    use hyperast::types::{Children, NodeStore, WithChildren};
    let mut o = num::zero();
    let (id, n, k) = {
        let n = resolve(stores, pos);
        let Some(cs) = n.children() else {
            return false;
        };
        let mut cs = cs.iter_children();
        loop {
            let Some(id) = cs.next() else { unreachable!() };
            let n = stores.node_store().resolve(&id);
            let k = decomp_t!(HAST::TS, &n);
            if !k.is_spaces() {
                break (id, n, k);
            }
            o += num::one();
        }
    };
    pos.down(id, o);
    push_stacks::<HAST>(types, misc_stacks, n, k);
    return true;
}

pub(super) fn push_stacks<'hast, HAST: HyperAST>(
    types: &mut Vec<<HAST::TS as TypeStore>::Ty>,
    misc_stacks: &mut MiscStacks,
    n: LendT<'hast, HAST>,
    k: <<HAST as HyperAST>::TS as TypeStore>::Ty,
) {
    types.push(k);
    misc_stacks.hiddens.set(types.len() - 1, k.is_hidden());
    misc_stacks.errors.set(types.len() - 1, k.is_error());
}
