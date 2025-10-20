use super::CursorStatus;
use super::text;
use crate::StatusLending;
use crate::{Status, Symbol, TreeCursorStep};
use hyperast::position::structural_pos::{self, CursorHead, CursorHeadMove};
use hyperast::types::{Childrn, HyperAST, LangRef, NodeStore as _, TypeStore};
use hyperast::types::{HyperASTShared, HyperType, LabelStore, Labeled, RoleStore, Tree};
use hyperast::types::{LendT, NodeId};
use hyperast::types::{WithChildren, WithPrecompQueries, WithRoles};

pub struct TreeCursor<HAST: HyperASTShared + Copy> {
    pub stores: HAST,
    pub pos: structural_pos::CursorWithPersistence<HAST::IdN, HAST::Idx>,
    pub p: structural_pos::PersistedNode<HAST::IdN, HAST::Idx>,
}

pub struct Node<HAST: HyperASTShared + Copy> {
    pub stores: HAST,
    pub pos: structural_pos::PersistedNode<HAST::IdN, HAST::Idx>,
}

pub struct NodeRef<'a, HAST: HyperASTShared + Copy> {
    pub stores: HAST,
    pub pos: structural_pos::RefNode<'a, HAST::IdN, HAST::Idx>,
}

impl<HAST: HyperAST + Copy> Clone for NodeRef<'_, HAST> {
    fn clone(&self) -> Self {
        Self {
            stores: self.stores,
            pos: self.pos.clone(),
        }
    }
}

#[cfg(feature = "tsg")]
impl<HAST: HyperAST + Copy> tree_sitter_graph::graph::SimpleNode for NodeRef<'_, HAST>
where
    <HAST as HyperASTShared>::IdN: std::hash::Hash + Copy,
    <HAST as HyperASTShared>::Idx: std::hash::Hash,
{
    fn id(&self) -> usize {
        use std::hash::Hash;
        use std::hash::Hasher;
        let mut hasher = std::hash::DefaultHasher::new();
        self.pos.hash(&mut hasher);
        hasher.finish() as usize
    }

    fn parent(&self) -> Option<Self>
    where
        Self: Sized,
    {
        let mut s = self.clone();
        if s.pos.up() { Some(s) } else { None }
    }
}

impl<HAST: HyperAST + Copy> TreeCursor<HAST> {
    pub fn new(
        stores: HAST,
        mut pos: structural_pos::CursorWithPersistence<HAST::IdN, HAST::Idx>,
    ) -> Self {
        let p = pos.persist();
        Self { stores, pos, p }
    }
}

impl<HAST: HyperAST + Copy> Clone for Node<HAST> {
    fn clone(&self) -> Self {
        Self {
            stores: self.stores,
            pos: self.pos.clone(),
        }
    }
}

impl<HAST: HyperAST + Copy> crate::WithField for self::TreeCursor<HAST>
where
    HAST::TS: RoleStore,
{
    type IdF = <HAST::TS as RoleStore>::IdF;
}

impl<'a, HAST: HyperAST + Copy> crate::CNLending<'a> for self::TreeCursor<HAST>
where
    HAST::IdN: std::fmt::Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> LendT<'t, HAST>: WithRoles,
    for<'t> LendT<'t, HAST>: WithPrecompQueries,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
{
    type NR = self::NodeRef<'a, HAST>;
}

impl<'a, 'hast, HAST: HyperAST + Copy> crate::StatusLending<'a> for self::TreeCursor<HAST>
where
    HAST::TS: RoleStore,
{
    type Status = CursorStatus<<<HAST as HyperAST>::TS as RoleStore>::IdF>;
}

impl<HAST: HyperAST + Copy> crate::Cursor for self::TreeCursor<HAST>
where
    HAST::IdN: std::fmt::Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> LendT<'t, HAST>: WithRoles,
    for<'t> LendT<'t, HAST>: WithPrecompQueries,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
{
    type Node = self::Node<HAST>;

    fn goto_next_sibling_internal(&mut self) -> TreeCursorStep {
        if self.p.ref_node().eq(&self.pos.ref_node()) {
            return TreeCursorStep::None;
        }
        goto_next_sibling_internal(self.stores, &mut self.pos)
    }

    #[inline(always)]
    fn goto_first_child_internal(&mut self) -> TreeCursorStep {
        goto_first_child_internal(self.stores, &mut self.pos)
    }

    fn goto_parent(&mut self) -> bool {
        if self.p.ref_node().eq(&self.pos.ref_node()) {
            return false;
        }
        goto_parent(self.stores, &mut self.pos)
    }

    fn current_node(&self) -> <Self as crate::CNLending<'_>>::NR {
        NodeRef {
            stores: self.stores,
            pos: self.pos.ref_node(),
        }
    }

    fn parent_is_error(&self) -> bool {
        // NOTE: maybe more efficient impl
        let mut s = self.pos.ref_node();
        if !goto_parent(self.stores, &mut s) {
            return false;
        }
        kind(self.stores, &s).is_error()
    }

    fn has_parent(&self) -> bool {
        let mut s = self.pos.ref_node();
        goto_parent(self.stores, &mut s)
    }

    fn persist(&self) -> Self::Node {
        Node {
            stores: self.stores,
            pos: self.pos.persist(),
        }
    }

    fn persist_parent(&self) -> Option<Self::Node> {
        Some(Node {
            stores: self.stores,
            pos: self.pos.persist_parent()?,
        })
    }

    #[inline(always)]
    fn current_status(&self) -> <Self as StatusLending<'_>>::Status {
        if cfg!(not(debug_assertions)) {
            return current_status(self.stores, &self.pos);
        }
        // let old = crate::current_status(self.stores, &self.pos);
        let new = current_status(self.stores, &self.pos);
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
        // self.stores
    }

    fn is_visible_at_root(&self) -> bool {
        // assert!(self.pos.ref_parent().is_none());
        if self.pos.ref_parent().is_none() {
            return true;
        }
        is_visible(self.stores, &self.pos)
    }

    fn wont_match(&self, needed: crate::Precomps) -> bool {
        if needed == 0 {
            return false;
        }
        use hyperast::types::NodeStore;
        let id = self.pos.node();
        let n = self.stores.node_store().resolve(&id);
        n.wont_match_given_precomputed_queries(needed)
    }
}

pub(super) fn current_status<HAST: HyperAST + Copy>(
    stores: HAST,
    pos: &structural_pos::CursorWithPersistence<HAST::IdN, HAST::Idx>,
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
    let lang = kind(stores, pos).get_lang();

    let mut pos = pos.ext();

    // go up until parent is visible
    loop {
        let o = pos.offset();
        if !pos.up() {
            break;
        };
        let n = {
            let n = pos.node();
            use hyperast::types::NodeStore;
            let n = stores.node_store().resolve(&n);
            n
        };
        let k = kind(stores, &pos);
        // dbg!(k);
        // dbg!(o);
        // dbg!(n.child_count());

        if k.is_supertype() {
            let symbol = symbol(stores, &pos);
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
                // NOTE semantic of TS (the weird `if (*has_later_named_siblings) break;` might be related to assembly opt):
                // if (sibling_metadata.visible) {
                //   *has_later_siblings = true;
                //   if (*has_later_named_siblings) break;
                //   if (sibling_metadata.named) {
                //     *has_later_named_siblings = true;
                //     break;
                //   }
                // } else if (ts_subtree_visible_child_count(sibling) > 0) {
                //   *has_later_siblings = true;
                //   if (*has_later_named_siblings) break;
                //   if (sibling.ptr->named_child_count > 0) {
                //     *has_later_named_siblings = true;
                //     break;
                //   }
                // }
            }
        }

        if role.is_some() {
            // role was retrieved in descendant
            // without additional data I should assume there is a later sib with same field
            // without hidden nodes other than supertypes, this statement should not even be executed
            can_have_later_siblings_with_this_field = true;
        }

        // get field
        if role.is_none() {
            if !k.is_supertype() {
                // VALIDITY It holds because we push the role to the parent if it is a supertype
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

#[doc(hidden)]
pub static mut ELAPSED_STATUS: std::time::Duration = std::time::Duration::ZERO;

impl<HAST: HyperAST + Copy> self::NodeRef<'_, HAST>
where
    HAST::IdN: std::fmt::Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> LendT<'t, HAST>: WithRoles,
    for<'t> LendT<'t, HAST>: WithPrecompQueries,
{
    #[inline(always)]
    fn compute_current_role(
        mut self,
    ) -> (
        Option<<<HAST as HyperAST>::TS as RoleStore>::Role>,
        <<HAST as HyperAST>::TS as RoleStore>::IdF,
    ) {
        let lang;
        let role = loop {
            let o = self.pos.offset();
            if !self.pos.up() {
                return (None, Default::default());
            };
            // dbg!(self.kind());
            let k = kind(self.stores, &self.pos);
            // dbg!(k);
            if k.is_supertype() {
                // dbg!();
                continue;
            }
            let n = {
                let pos = &self.pos;
                let n = pos.node();
                use hyperast::types::NodeStore;
                let n = self.stores.node_store().resolve(&n);
                n
            };
            if let Some(role) = n.role_at::<<HAST::TS as RoleStore>::Role>(o) {
                lang = k.get_lang();
                break Some(role);
            }
            if !k.is_hidden() {
                return (None, Default::default());
            }
        };
        let field_id = if let Some(role) = role {
            HAST::TS::intern_role(lang, role)
        } else {
            Default::default()
        };
        (role, field_id)
    }
}

impl<HAST: HyperAST + Copy> crate::TextLending<'_> for self::Node<HAST> {
    type TP = ();
}

impl<HAST: HyperAST + Copy> PartialEq for self::Node<HAST>
where
    HAST::IdN: std::fmt::Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> LendT<'t, HAST>: WithRoles,
    for<'t> LendT<'t, HAST>: WithPrecompQueries,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
{
    fn eq(&self, other: &Self) -> bool {
        self.pos == other.pos
    }
}

impl<HAST: HyperAST + Copy> crate::Node for self::Node<HAST>
where
    HAST::IdN: std::fmt::Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> LendT<'t, HAST>: WithRoles,
    for<'t> LendT<'t, HAST>: WithPrecompQueries,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
{
    fn symbol(&self) -> Symbol {
        let n = self.pos.node();
        let t = self.stores.resolve_type(&n);
        use hyperast::types::NodeStore;
        let n = self.stores.node_store().resolve(&n);
        let id = self.stores.resolve_lang(&n).ts_symbol(t);
        id.into()
    }

    fn is_named(&self) -> bool {
        self.kind().is_named()
    }

    fn str_symbol(&self) -> &str {
        self.kind().as_static_str()
    }

    fn start_point(&self) -> tree_sitter::Point {
        // TODO
        tree_sitter::Point { row: 0, column: 0 }
    }

    type IdF = <HAST::TS as RoleStore>::IdF;

    fn has_child_with_field_id(&self, field_id: Self::IdF) -> bool {
        has_child_with_field_id(self.stores, self.pos.ext(), field_id)
    }

    fn equal(&self, other: &Self, _text_provider: <Self as crate::TextLending<'_>>::TP) -> bool {
        self.pos.node() == other.pos.node()
    }

    fn compare(&self, other: &Self) -> std::cmp::Ordering {
        use std::cmp::Ordering::*;
        let left = self;
        let right = other;
        if left != right {
            return self.pos.cmp(&other.pos);
        }
        Equal
    }
    fn text<'s, 'l>(
        &'s self,
        text_provider: <Self as crate::TextLending<'l>>::TP,
    ) -> crate::BiCow<'s, 'l, str> {
        todo!()
        // text(self.stores, &self.pos)
    }
}

impl<HAST: HyperAST + Copy> crate::TextLending<'_> for self::NodeRef<'_, HAST> {
    type TP = ();
}

impl<HAST: HyperAST + Copy> PartialEq for self::NodeRef<'_, HAST>
where
    HAST::IdN: std::fmt::Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> LendT<'t, HAST>: WithRoles,
    for<'t> LendT<'t, HAST>: WithPrecompQueries,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
{
    fn eq(&self, other: &Self) -> bool {
        self.pos == other.pos
    }
}

impl<HAST: HyperAST + Copy> crate::Node for self::NodeRef<'_, HAST>
where
    HAST::IdN: std::fmt::Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> LendT<'t, HAST>: WithRoles,
    for<'t> LendT<'t, HAST>: WithPrecompQueries,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
{
    fn symbol(&self) -> Symbol {
        let n = self.pos.node();
        let t = self.stores.resolve_type(&n);
        use hyperast::types::NodeStore;
        let n = self.stores.node_store().resolve(&n);
        let id = self.stores.resolve_lang(&n).ts_symbol(t);
        id.into()
    }

    fn is_named(&self) -> bool {
        kind(self.stores, &self.pos).is_named()
    }

    fn str_symbol(&self) -> &str {
        kind(self.stores, &self.pos).as_static_str()
    }

    fn start_point(&self) -> tree_sitter::Point {
        // TODO
        tree_sitter::Point { row: 0, column: 0 }
    }

    type IdF = <HAST::TS as RoleStore>::IdF;

    fn has_child_with_field_id(&self, field_id: Self::IdF) -> bool {
        has_child_with_field_id(self.stores, self.pos.ext(), field_id)
    }

    fn equal(&self, other: &Self, _text_provider: <Self as crate::TextLending<'_>>::TP) -> bool {
        self.pos.node() == other.pos.node()
    }

    fn compare(&self, other: &Self) -> std::cmp::Ordering {
        self.pos.cmp(&other.pos)
    }

    fn text<'s, 'l>(
        &'s self,
        _text_provider: <Self as crate::TextLending<'l>>::TP,
    ) -> crate::BiCow<'s, 'l, str> {
        todo!()
        // text(self.stores, &self.pos)
    }
}

impl<HAST: HyperAST + Copy> Node<HAST>
where
    HAST::IdN: std::fmt::Debug + Copy,
{
    fn kind(&self) -> <HAST::TS as TypeStore>::Ty {
        kind(self.stores, &self.pos)
    }
}

fn kind<HAST: HyperAST + Copy>(
    stores: HAST,
    pos: &impl CursorHead<HAST::IdN, HAST::Idx>,
) -> <HAST::TS as TypeStore>::Ty {
    stores.resolve_type(&pos.node())
}

fn is_visible<HAST: HyperAST + Copy>(
    stores: HAST,
    pos: &impl CursorHead<HAST::IdN, HAST::Idx>,
) -> bool {
    !kind(stores, pos).is_hidden()
}

fn symbol<HAST: HyperAST + Copy>(
    stores: HAST,
    pos: &impl CursorHead<HAST::IdN, HAST::Idx>,
) -> Symbol {
    let n = pos.node();
    let t = stores.resolve_type(&n);
    use hyperast::types::NodeStore;
    let n = stores.node_store().resolve(&n);
    let id = stores.resolve_lang(&n).ts_symbol(t);
    id.into()
}

pub(super) fn role<HAST: HyperAST + Copy>(
    stores: HAST,
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
    let n = {
        let n = pos.node();
        use hyperast::types::NodeStore;
        let n = stores.node_store().resolve(&n);
        n
    };
    n.role_at::<<HAST::TS as RoleStore>::Role>(at)
}

fn goto_parent<HAST: HyperAST + Copy>(
    stores: HAST,
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
fn goto_next_sibling_internal<HAST: HyperAST + Copy>(
    stores: HAST,
    pos: &mut impl CursorHeadMove<HAST::IdN, HAST::Idx>,
) -> TreeCursorStep
where
    HAST::IdN: NodeId<IdN = HAST::IdN>,
    HAST::IdN: Copy,
{
    use hyperast::types::NodeStore;
    let Some(p) = pos.parent() else {
        return TreeCursorStep::None;
    };
    let n = stores.node_store().resolve(&p);
    use hyperast::types::Children;
    use hyperast::types::WithChildren;
    let Some(node) = n.child(&(pos.offset() + num::one())) else {
        if stores.resolve_type(&p).is_hidden() {
            // dbg!(stores.resolve_type(&p));
            pos.up();
            return goto_next_sibling_internal(stores, pos);
        } else {
            return TreeCursorStep::None;
        }
    };
    pos.inc(node);
    if kind(stores, pos).is_spaces() {
        return goto_next_sibling_internal(stores, pos);
    }
    if is_visible(stores, pos) {
        TreeCursorStep::Visible
    } else {
        TreeCursorStep::Hidden
    }
}

#[inline(always)]
fn goto_first_child_internal<HAST: HyperAST + Copy>(
    stores: HAST,
    pos: &mut impl CursorHeadMove<HAST::IdN, HAST::Idx>,
) -> TreeCursorStep
where
    HAST::IdN: Copy,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
{
    use hyperast::types::NodeStore;
    let n = stores.node_store().resolve(&pos.node());
    use hyperast::types::Children;
    use hyperast::types::WithChildren;
    let Some(node) = n.child(&num::zero()) else {
        return TreeCursorStep::None;
    };
    pos.down(node, num::zero());
    if kind(stores, pos).is_spaces() {
        return goto_next_sibling_internal(stores, pos);
    }
    if is_visible(stores, pos) {
        TreeCursorStep::Visible
    } else {
        TreeCursorStep::Hidden
    }
}

pub(super) fn has_child_with_field_id<HAST: HyperAST + Copy>(
    stores: HAST,
    pos: impl CursorHeadMove<HAST::IdN, HAST::Idx> + Clone + CursorHead<HAST::IdN, HAST::Idx>,
    field_id: <HAST::TS as RoleStore>::IdF,
) -> bool
where
    HAST::IdN: std::fmt::Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> LendT<'t, HAST>: WithRoles,
    HAST::IdN: hyperast::types::NodeId<IdN = HAST::IdN>,
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
        match goto_first_child_internal(stores, &mut pos) {
            TreeCursorStep::None => panic!(),
            TreeCursorStep::Hidden => (),
            TreeCursorStep::Visible => break,
        }
    }
    child_by_role(stores, &mut pos, role).is_some()
}

fn child_by_role<HAST: HyperAST + Copy>(
    stores: HAST,
    pos: &mut (impl CursorHeadMove<HAST::IdN, HAST::Idx> + Clone),
    _role: <HAST::TS as RoleStore>::Role,
) -> Option<()>
where
    <HAST as HyperAST>::TS: RoleStore,
    <HAST as HyperASTShared>::IdN: Copy,
    HAST::IdN: hyperast::types::NodeId<IdN = HAST::IdN>,
    HAST::TS: RoleStore,
    for<'t> LendT<'t, HAST>: WithRoles,
{
    // TODO what about multiple children with same role?
    // NOTE treesitter uses a bin tree for repeats
    let visible = is_visible(stores, pos);
    if let TreeCursorStep::None = goto_first_child_internal(stores, pos) {
        return None;
    }
    loop {
        if let Some(r) = role(stores, &mut pos.clone()) {
            if r == _role {
                return Some(());
            } else {
                if let TreeCursorStep::None = goto_next_sibling_internal(stores, pos) {
                    return None;
                }
                continue;
            }
        }
        // do not go down
        if visible {
            if let TreeCursorStep::None = goto_next_sibling_internal(stores, pos) {
                return None;
            }
        }
        // hidden node so can explore
        else {
            if child_by_role(stores, pos, _role).is_some() {
                return Some(());
            }
            if let TreeCursorStep::None = goto_next_sibling_internal(stores, pos) {
                return None;
            }
        }
    }
}
