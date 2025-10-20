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
//! * precomputing later sibling info
use super::hyperast_opt5::has_child_with_field_id;
use super::hyperast_opt7::BitSet as _;
use super::hyperast_opt8::MiscStacks;
use super::hyperast_opt9::push_stacks;
use super::{kind, resolve, text};
use crate::StatusLending;
use crate::{Status, Symbol, TreeCursorStep};
use hyperast::position::structural_pos;
use hyperast::types::{Childrn, HyperAST, LangRef, LendT, NodeStore as _, TypeStore};
use hyperast::types::{HyperASTShared, HyperType, LabelStore, Labeled, RoleStore, Tree};
use hyperast::types::{WithChildren, WithPrecompQueries, WithRoles};
use structural_pos::{CursorHead, CursorHeadMove, CursorWithPersistence, PersistedNode};

pub struct TreeCursor<'hast, HAST: HyperAST> {
    pub stores: &'hast HAST,
    pub pos: CursorWithPersistence<HAST::IdN, HAST::Idx>,
    pub p: PersistedNode<HAST::IdN, HAST::Idx>,
    stack: Vec<LendT<'hast, HAST>>,
    misc_stacks: MiscStacks,
    types: Vec<<HAST::TS as TypeStore>::Ty>,
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
        let mut types = vec![decomp_t!(HAST::TS, &n)];
        types.reserve(30);
        // root node must be visible
        debug_assert!(!decomp_t!(HAST::TS, &n).is_hidden());
        // debug_assert!(!decomp_t!(HAST::TS, &n).is_error());
        let hiddens = 0;
        let errors = 0;
        let mut stack = vec![n];
        stack.reserve(30);
        Self {
            stores,
            pos,
            p,
            stack,
            misc_stacks: MiscStacks { hiddens, errors },
            types,
        }
    }
}

pub struct CursorStatus<'a, IdF> {
    pub has_later_siblings: bool,
    pub has_later_named_siblings: bool,
    pub can_have_later_siblings_with_this_field: bool,
    pub field_id: IdF,
    pub supertypes: &'a [Symbol],
}

impl<'a, IdF: Copy> Status for CursorStatus<'a, IdF> {
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
    HAST::IdN: hyperast::types::NodeId<IdN = HAST::IdN>,
{
    type NR = super::hyperast_opt4::NodeRef<'a, 'hast, HAST>;
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
    type Node = super::hyperast_opt4::Node<'hast, HAST>;

    #[inline(never)]
    fn goto_next_sibling_internal(&mut self) -> TreeCursorStep {
        if self.p.ref_node().eq(&self.pos.ref_node()) {
            return TreeCursorStep::TreeCursorStepNone;
        }
        if !goto_next_sibling_internal(
            self.stores,
            &mut self.stack,
            &mut self.types,
            &mut self.misc_stacks,
            &mut self.pos,
        ) {
            return TreeCursorStep::TreeCursorStepNone;
        }
        if !self.misc_stacks.hiddens.bit(self.stack.len() - 1) {
            TreeCursorStep::TreeCursorStepVisible
        } else {
            TreeCursorStep::TreeCursorStepHidden
        }
    }

    #[inline(never)]
    fn goto_first_child_internal(&mut self) -> TreeCursorStep {
        if !goto_first_child_internal(
            self.stores,
            &mut self.stack,
            &mut self.types,
            &mut self.misc_stacks,
            &mut self.pos,
        ) {
            return TreeCursorStep::TreeCursorStepNone;
        }
        if !self.misc_stacks.hiddens.bit(self.stack.len() - 1) {
            TreeCursorStep::TreeCursorStepVisible
        } else {
            TreeCursorStep::TreeCursorStepHidden
        }
    }

    #[inline(never)]
    fn goto_parent(&mut self) -> bool {
        if self.p.ref_node().eq(&self.pos.ref_node()) {
            return false;
        }
        if !goto_parent(
            self.stores,
            &mut self.stack,
            &mut self.misc_stacks,
            &mut self.pos,
        ) {
            return false;
        }
        true
    }

    #[inline(never)]
    fn current_node(&self) -> <Self as crate::CNLending<'_>>::NR {
        let pos = self.pos.ref_node();
        use hyperast::types::NodeStore;
        let kind = *self.types.last().unwrap();
        super::hyperast_opt4::NodeRef {
            stores: self.stores,
            pos,
            kind,
        }
    }

    #[inline(never)]
    fn parent_is_error(&self) -> bool {
        if self.stack.len() == 1 {
            return false;
        }
        let mut s = self.pos.ref_node();
        let n = self.stack.last().unwrap();
        let Some(stack) = goto_parent_virt(self.stores, &*self.stack, &self.misc_stacks, &mut s)
        else {
            return false;
        };
        self.misc_stacks.errors.bit(self.stack.len() - 1)
    }

    #[inline(never)]
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

    #[inline(never)]
    fn current_status(&self) -> <Self as StatusLending<'_>>::Status {
        current_status(
            self.stores,
            &self.types,
            &self.stack,
            &self.misc_stacks,
            &self.pos,
        )
    }

    fn text_provider(&self) -> <Self::Node as crate::TextLending<'_>>::TP {
        self.stores.label_store()
    }

    #[inline(never)]
    fn is_visible_at_root(&self) -> bool {
        if self.pos.ref_parent().is_none() {
            return true;
        }
        !self.misc_stacks.hiddens.bit(self.stack.len() - 1)
    }

    #[inline(never)]
    fn wont_match(&self, needed: crate::Precomps) -> bool {
        if needed == 0 {
            return false;
        }
        let n = self.stack.last().unwrap();
        n.wont_match_given_precomputed_queries(needed)
    }
}

#[inline(always)]
pub(super) fn current_status<'a, 'hast, HAST: HyperAST>(
    stores: &'hast HAST,
    types: &'a [<HAST::TS as TypeStore>::Ty],
    stack: &[LendT<'hast, HAST>],
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

    let mut pos = pos.ref_node();
    let mut stack = stack;
    let mut first_st = usize::MAX;

    // go up until parent is visible
    loop {
        let o = pos.offset();
        if !pos.up() {
            break;
        };
        stack = &stack[..stack.len() - 1];
        let Some(n) = stack.last() else {
            break;
        };
        let k = types[stack.len() - 1];

        if k.is_supertype() {
            first_st = stack.len();
            // let symbol = lang.ts_symbol(k).into();
            // supertypes.push(symbol);
        }

        if role.is_none() {
            role = n.role_at(o);
        }
        if !has_later_siblings
            || !has_later_named_siblings
            || !can_have_later_siblings_with_this_field
        {
            let (later, later_named, later_same_role) = n.later_sib_info(o, role);
            has_later_siblings |= later;
            has_later_named_siblings |= later_named;
            can_have_later_siblings_with_this_field |= later_same_role;
        }

        if !misc_stacks.hiddens.bit(stack.len() - 1) {
            break;
        }
    }

    field_id = if let Some(role) = role {
        HAST::TS::intern_role(lang.clone(), role)
    } else {
        Default::default()
    };
    let supertypes: &[Symbol] = if first_st != usize::MAX {
        unsafe { std::mem::transmute(&types[stack.len() - 1..first_st]) }
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

fn is_visible<HAST: HyperAST>(stores: &HAST, pos: &impl CursorHead<HAST::IdN, HAST::Idx>) -> bool {
    !kind(stores, pos).is_hidden()
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
    let n = resolve(stores, pos);
    n.role_at::<<HAST::TS as RoleStore>::Role>(at)
}

fn goto_parent<'hast, HAST: HyperAST>(
    stores: &'hast HAST,
    stack: &mut Vec<LendT<'hast, HAST>>,
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
        stack.pop().unwrap();
        if !misc_stacks.hiddens.bit(stack.len() - 1) {
            return true;
        }
    }
}

fn goto_parent_virt<'a, 'hast, HAST: HyperAST>(
    stores: &'hast HAST,
    mut stack: &'a [LendT<'hast, HAST>],
    misc_stacks: &MiscStacks,
    pos: &mut impl CursorHead<HAST::IdN, HAST::Idx>,
) -> Option<&'a [LendT<'hast, HAST>]>
where
    HAST::IdN: hyperast::types::NodeId<IdN = HAST::IdN>,
    HAST::IdN: Copy,
{
    loop {
        if !pos.up() {
            return None;
        }
        stack = &stack[..stack.len() - 1];
        if !misc_stacks.hiddens.bit(stack.len() - 1) {
            return Some(stack);
        }
    }
}

fn goto_next_sibling_internal<'hast, HAST: HyperAST>(
    stores: &'hast HAST,
    stack: &mut Vec<LendT<'hast, HAST>>,
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
    let prev_n = stack.pop().unwrap();
    let Some(n) = stack.last() else {
        return false;
    };
    use hyperast::types::Children;
    use hyperast::types::WithChildren;
    let Some(node) = n.child(&(pos.offset() + num::one())) else {
        if misc_stacks.hiddens.bit(stack.len() - 1) {
            pos.up();
            return goto_next_sibling_internal(stores, stack, types, misc_stacks, pos);
        } else {
            stack.push(prev_n);
            types.push(prev_t);
            return false;
        }
    };
    pos.inc(node);
    let n = stores.node_store().resolve(&node);
    let k = decomp_t!(HAST::TS, &n);
    push_stacks::<HAST>(stack, types, misc_stacks, n, k);
    if k.is_spaces() {
        return goto_next_sibling_internal(stores, stack, types, misc_stacks, pos);
    }
    true
}

fn goto_first_child_internal<'hast, HAST: HyperAST>(
    stores: &'hast HAST,
    stack: &mut Vec<LendT<'hast, HAST>>,
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
        let n = stack.last().unwrap();
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
    push_stacks::<HAST>(stack, types, misc_stacks, n, k);
    return true;
}
