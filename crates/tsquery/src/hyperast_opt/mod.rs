//! # Optimized cursor
//! nodes are only persisted when captured
use super::{Status, Symbol, TreeCursorStep};
use crate::StatusLending;
use hyperast::position::structural_pos;
use hyperast::types::{Childrn, HyperAST, LangRef, LendT, NodeStore as _, TypeStore};
use hyperast::types::{HyperASTShared, HyperType, LabelStore, Labeled, RoleStore, Tree};
use hyperast::types::{WithChildren, WithPrecompQueries, WithRoles};
use structural_pos::{CursorHead, CursorHeadMove, CursorWithPersistence, PersistedNode};

pub struct TreeCursor<'hast, HAST: HyperASTShared> {
    pub stores: &'hast HAST,
    pub pos: CursorWithPersistence<HAST::IdN, HAST::Idx>,
    pub p: PersistedNode<HAST::IdN, HAST::Idx>,
}

pub struct Node<'hast, HAST: HyperASTShared> {
    pub stores: &'hast HAST,
    pub pos: PersistedNode<HAST::IdN, HAST::Idx>,
}

pub struct NodeRef<'a, 'hast, HAST: HyperASTShared> {
    pub stores: &'hast HAST,
    pub pos: structural_pos::RefNode<'a, HAST::IdN, HAST::Idx>,
}

struct ExtNodeRef<'a, 'hast, HAST: HyperASTShared> {
    pub stores: &'hast HAST,
    pub pos: structural_pos::ExtRefNode<'a, HAST::IdN, HAST::Idx>,
}

impl<HAST: HyperAST> Clone for NodeRef<'_, '_, HAST> {
    fn clone(&self) -> Self {
        Self {
            stores: self.stores,
            pos: self.pos.clone(),
        }
    }
}

#[cfg(feature = "tsg")]
impl<HAST: HyperAST> tree_sitter_graph::graph::SimpleNode for NodeRef<'_, '_, HAST>
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

// impl<'hast, HAST: HyperAST> PartialEq for Node<'hast, HAST> {
//     fn eq(&self, other: &Self) -> bool {
//         self.pos == other.pos
//     }
// }

impl<'hast, HAST: HyperAST> TreeCursor<'hast, HAST> {
    pub fn new(stores: &'hast HAST, mut pos: CursorWithPersistence<HAST::IdN, HAST::Idx>) -> Self {
        let p = pos.persist();
        Self { stores, pos, p }
    }
}

impl<HAST: HyperAST> Clone for Node<'_, HAST> {
    fn clone(&self) -> Self {
        Self {
            stores: self.stores,
            pos: self.pos.clone(),
        }
    }
}

pub struct CursorStatus<IdF> {
    pub has_later_siblings: bool,
    pub has_later_named_siblings: bool,
    pub can_have_later_siblings_with_this_field: bool,
    pub field_id: IdF,
    pub supertypes: Vec<Symbol>,
}

impl<IdF: Copy> Status for CursorStatus<IdF> {
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
    type NR = self::NodeRef<'a, 'hast, HAST>;
}

impl<'a, 'hast, HAST: HyperAST> crate::StatusLending<'a> for self::TreeCursor<'hast, HAST>
where
    HAST::TS: RoleStore,
{
    type Status = CursorStatus<<<HAST as HyperAST>::TS as RoleStore>::IdF>;
}

impl<'hast, HAST: HyperAST> super::Cursor for self::TreeCursor<'hast, HAST>
where
    HAST::IdN: std::fmt::Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> LendT<'t, HAST>: WithRoles,
    for<'t> LendT<'t, HAST>: WithPrecompQueries,
    HAST::IdN: hyperast::types::NodeId<IdN = HAST::IdN>,
{
    type Node = self::Node<'hast, HAST>;

    fn goto_next_sibling_internal(&mut self) -> TreeCursorStep {
        if self.p.ref_node().eq(&self.pos.ref_node()) {
            return TreeCursorStep::TreeCursorStepNone;
        }
        goto_next_sibling_internal(self.stores, &mut self.pos)
    }

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
        symbol(self.stores, &s).is_error()
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

    fn current_status(&self) -> <Self as StatusLending<'_>>::Status {
        current_status(self.stores, &self.pos)
    }

    fn text_provider(&self) -> <Self::Node as super::TextLending<'_>>::TP {
        self.stores.label_store()
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

pub(super) fn current_status<'hast, HAST: HyperAST>(
    stores: &'hast HAST,
    pos: &CursorWithPersistence<HAST::IdN, HAST::Idx>,
) -> CursorStatus<<<HAST as HyperAST>::TS as RoleStore>::IdF>
where
    HAST::IdN: std::fmt::Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> LendT<'t, HAST>: WithRoles,
    for<'t> LendT<'t, HAST>: WithPrecompQueries,
    HAST::IdN: hyperast::types::NodeId<IdN = HAST::IdN>,
{
    use super::Cursor;
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
        if let TreeCursorStep::TreeCursorStepNone = goto_next_sibling_internal(stores, &mut pos) {
            break;
        }
        let time = std::time::Instant::now();
        if _role.is_some() && role(stores, &mut pos.clone()) == _role {
            can_have_later_siblings_with_this_field = true;
        }
        let k = kind(stores, &pos);
        if k.is_spaces() {
            continue;
        }
        // dbg!(k);
        if !has_later_siblings {
            // dbg!(k);
        }
        has_later_siblings = true;
        if k.is_supertype() {
            // dbg!();
            has_later_named_siblings = true;
        }
        if is_visible(stores, &pos) {
            has_later_siblings = true;
            if k.is_named() {
                // dbg!();
                has_later_named_siblings = true;
                break;
            }
        }
        let dur = time.elapsed();
        unsafe { ELAPSED_STATUS += dur };
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

#[doc(hidden)]
pub static mut ELAPSED_STATUS: std::time::Duration = std::time::Duration::ZERO;

impl<HAST: HyperAST> self::NodeRef<'_, '_, HAST>
where
    HAST::IdN: std::fmt::Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> LendT<'t, HAST>: WithRoles,
    for<'t> LendT<'t, HAST>: WithPrecompQueries,
{
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
            let n = resolve(self.stores, &self.pos);
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

impl<'hast, HAST: HyperAST> super::TextLending<'_> for self::Node<'hast, HAST> {
    type TP = &'hast <HAST as HyperAST>::LS;
}

impl<HAST: HyperAST> PartialEq for self::Node<'_, HAST>
where
    HAST::IdN: std::fmt::Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> LendT<'t, HAST>: WithRoles,
    for<'t> LendT<'t, HAST>: WithPrecompQueries,
    HAST::IdN: hyperast::types::NodeId<IdN = HAST::IdN>,
{
    fn eq(&self, other: &Self) -> bool {
        self.pos == other.pos
    }
}

impl<HAST: HyperAST> super::Node for self::Node<'_, HAST>
where
    HAST::IdN: std::fmt::Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> LendT<'t, HAST>: WithRoles,
    for<'t> LendT<'t, HAST>: WithPrecompQueries,
    HAST::IdN: hyperast::types::NodeId<IdN = HAST::IdN>,
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

    fn equal(&self, other: &Self, _text_provider: <Self as super::TextLending<'_>>::TP) -> bool {
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
        text_provider: <Self as super::TextLending<'l>>::TP,
    ) -> super::BiCow<'s, 'l, str> {
        text(self.stores, &self.pos)
    }
}

impl<'hast, HAST: HyperAST> super::TextLending<'_> for self::NodeRef<'_, 'hast, HAST> {
    type TP = &'hast <HAST as HyperAST>::LS;
}

impl<HAST: HyperAST> PartialEq for self::NodeRef<'_, '_, HAST>
where
    HAST::IdN: std::fmt::Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> LendT<'t, HAST>: WithRoles,
    for<'t> LendT<'t, HAST>: WithPrecompQueries,
    HAST::IdN: hyperast::types::NodeId<IdN = HAST::IdN>,
{
    fn eq(&self, other: &Self) -> bool {
        self.pos == other.pos
    }
}

impl<HAST: HyperAST> super::Node for self::NodeRef<'_, '_, HAST>
where
    HAST::IdN: std::fmt::Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> LendT<'t, HAST>: WithRoles,
    for<'t> LendT<'t, HAST>: WithPrecompQueries,
    HAST::IdN: hyperast::types::NodeId<IdN = HAST::IdN>,
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

    fn equal(&self, other: &Self, _text_provider: <Self as super::TextLending<'_>>::TP) -> bool {
        self.pos.node() == other.pos.node()
    }

    fn compare(&self, other: &Self) -> std::cmp::Ordering {
        self.pos.cmp(&other.pos)
    }

    fn text<'s, 'l>(
        &'s self,
        _text_provider: <Self as super::TextLending<'l>>::TP,
    ) -> super::BiCow<'s, 'l, str> {
        text(self.stores, &self.pos)
    }
}

impl<HAST: HyperAST> Node<'_, HAST>
where
    HAST::IdN: std::fmt::Debug + Copy,
{
    fn kind(&self) -> <HAST::TS as TypeStore>::Ty {
        kind(self.stores, &self.pos)
    }
}

pub(super) fn kind<HAST: HyperAST>(
    stores: &HAST,
    pos: &impl CursorHead<HAST::IdN, HAST::Idx>,
) -> <HAST::TS as TypeStore>::Ty {
    stores.resolve_type(&pos.node())
}

pub(super) fn resolve<'hast, HAST: HyperAST>(
    stores: &'hast HAST,
    pos: &impl CursorHead<HAST::IdN, HAST::Idx>,
) -> LendT<'hast, HAST> {
    let n = pos.node();
    use hyperast::types::NodeStore;
    let n = stores.node_store().resolve(&n);
    n
}

fn is_visible<HAST: HyperAST>(stores: &HAST, pos: &impl CursorHead<HAST::IdN, HAST::Idx>) -> bool {
    !kind(stores, pos).is_hidden()
}

fn symbol<HAST: HyperAST>(stores: &HAST, pos: &impl CursorHead<HAST::IdN, HAST::Idx>) -> Symbol {
    let n = pos.node();
    let t = stores.resolve_type(&n);
    use hyperast::types::NodeStore;
    let n = stores.node_store().resolve(&n);
    let id = stores.resolve_lang(&n).ts_symbol(t);
    id.into()
}

pub(super) fn text<'hast, 'l, HAST: HyperAST>(
    stores: &'hast HAST,
    pos: &impl CursorHead<HAST::IdN, HAST::Idx>,
) -> super::BiCow<'hast, 'l, str>
where
    HAST::IdN: hyperast::types::NodeId<IdN = HAST::IdN>,
{
    let id = pos.node();
    use hyperast::types::NodeStore;
    let n = stores.node_store().resolve(&id);
    if n.has_children() {
        let r = hyperast::nodes::TextSerializer::new(stores, id).to_string();
        return super::BiCow::Owned(r);
    }
    if let Some(l) = n.try_get_label() {
        let l = stores.label_store().resolve(l);
        return super::BiCow::A(l);
    }
    let ty = stores.resolve_type(&id);
    if !ty.is_named() {
        super::BiCow::A(ty.as_static_str())
        // ty.to_string().into()
    } else {
        super::BiCow::A("")
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
    let n = resolve(stores, pos);
    n.role_at::<<HAST::TS as RoleStore>::Role>(at)
}

struct SuperTypeIter<'a, 'hast, HAST: HyperASTShared> {
    pub stores: &'hast HAST,
    pub pos: structural_pos::RefNode<'a, HAST::IdN, HAST::Idx>,
}

impl<HAST: HyperAST> Iterator for SuperTypeIter<'_, '_, HAST>
where
    HAST::IdN: std::fmt::Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> LendT<'t, HAST>: WithRoles,
    for<'t> LendT<'t, HAST>: WithPrecompQueries,
{
    type Item = Symbol;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let p = self.pos.parent()?;
            let k = self.stores.resolve_type(&p);
            if !k.is_hidden() {
                return None;
            }
            if k.is_supertype() {
                let symbol = symbol(self.stores, &self.pos);
                assert!(self.pos.up());
                return Some(symbol);
            }
            assert!(self.pos.up());
        }
    }
}

fn goto_parent<HAST: HyperAST>(
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

fn goto_next_sibling_internal<HAST: HyperAST>(
    stores: &HAST,
    pos: &mut impl CursorHeadMove<HAST::IdN, HAST::Idx>,
) -> TreeCursorStep
where
    HAST::IdN: hyperast::types::NodeId<IdN = HAST::IdN>,
    HAST::IdN: Copy,
{
    use hyperast::types::NodeStore;
    let Some(p) = pos.parent() else {
        return TreeCursorStep::TreeCursorStepNone;
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
            return TreeCursorStep::TreeCursorStepNone;
        }
    };
    pos.inc(node);
    if kind(stores, pos).is_spaces() {
        return goto_next_sibling_internal(stores, pos);
    }
    if is_visible(stores, pos) {
        TreeCursorStep::TreeCursorStepVisible
    } else {
        TreeCursorStep::TreeCursorStepHidden
    }
}

fn goto_first_child_internal<HAST: HyperAST>(
    stores: &HAST,
    pos: &mut impl CursorHeadMove<HAST::IdN, HAST::Idx>,
) -> TreeCursorStep
where
    HAST::IdN: Copy,
    HAST::IdN: hyperast::types::NodeId<IdN = HAST::IdN>,
{
    use hyperast::types::NodeStore;
    let n = stores.node_store().resolve(&pos.node());
    use hyperast::types::Children;
    use hyperast::types::WithChildren;
    let Some(node) = n.child(&num::zero()) else {
        return TreeCursorStep::TreeCursorStepNone;
    };
    pos.down(node, num::zero());
    if kind(stores, pos).is_spaces() {
        return goto_next_sibling_internal(stores, pos);
    }
    if is_visible(stores, pos) {
        TreeCursorStep::TreeCursorStepVisible
    } else {
        TreeCursorStep::TreeCursorStepHidden
    }
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
            TreeCursorStep::TreeCursorStepNone => panic!(),
            TreeCursorStep::TreeCursorStepHidden => (),
            TreeCursorStep::TreeCursorStepVisible => break,
        }
    }
    child_by_role(stores, &mut pos, role).is_some()
}

fn child_by_role<'hast, HAST: HyperAST>(
    stores: &'hast HAST,
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
    if let TreeCursorStep::TreeCursorStepNone = goto_first_child_internal(stores, pos) {
        return None;
    }
    loop {
        if let Some(r) = role(stores, &mut pos.clone()) {
            if r == _role {
                return Some(());
            } else {
                if let TreeCursorStep::TreeCursorStepNone = goto_next_sibling_internal(stores, pos)
                {
                    return None;
                }
                continue;
            }
        }
        // do not go down
        if visible {
            if let TreeCursorStep::TreeCursorStepNone = goto_next_sibling_internal(stores, pos) {
                return None;
            }
        }
        // hidden node so can explore
        else {
            if child_by_role(stores, pos, _role).is_some() {
                return Some(());
            }
            if let TreeCursorStep::TreeCursorStepNone = goto_next_sibling_internal(stores, pos) {
                return None;
            }
        }
    }
}
