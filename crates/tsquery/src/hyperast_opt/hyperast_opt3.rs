//! # Optimized cursor
//! nodes are only persisted when captured
//! * optimizes current_status
//! * optimizes some redundant subtree resolutions
use super::CursorStatus;
use super::{has_child_with_field_id, text};
use crate::StatusLending;
use crate::{Status, Symbol, TreeCursorStep};
use hyperast::position::structural_pos::{self, CursorHead, CursorHeadMove};
use hyperast::types::{Childrn, HyperAST, LangRef, NodeStore as _, TypeStore};
use hyperast::types::{HyperASTShared, HyperType, LabelStore, Labeled, RoleStore, Tree};
use hyperast::types::{LendT, NodeId};
use hyperast::types::{WithChildren, WithPrecompQueries, WithRoles};

pub struct TreeCursor<'hast, HAST: HyperASTShared> {
    pub stores: &'hast HAST,
    pub pos: structural_pos::CursorWithPersistence<HAST::IdN, HAST::Idx>,
    pub p: structural_pos::PersistedNode<HAST::IdN, HAST::Idx>,
}

pub struct Node<'hast, HAST: HyperASTShared> {
    pub stores: &'hast HAST,
    pub pos: structural_pos::PersistedNode<HAST::IdN, HAST::Idx>,
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

impl<'hast, HAST: HyperAST> TreeCursor<'hast, HAST> {
    pub fn new(
        stores: &'hast HAST,
        mut pos: structural_pos::CursorWithPersistence<HAST::IdN, HAST::Idx>,
    ) -> Self {
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
    for<'t> hyperast::types::LendT<'t, HAST>: WithRoles,
    for<'t> hyperast::types::LendT<'t, HAST>: WithPrecompQueries,
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
    for<'t> hyperast::types::LendT<'t, HAST>: WithRoles,
    for<'t> hyperast::types::LendT<'t, HAST>: WithPrecompQueries,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
{
    type Node = self::Node<'hast, HAST>;

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

macro_rules! decomp_t {
    ($T:ty, $n:expr) => {
        <$T as TypeStore>::decompress_type($n, std::any::TypeId::of::<<$T as TypeStore>::Ty>())
    };
}

pub(super) fn current_status<'hast, HAST: HyperAST>(
    stores: &'hast HAST,
    pos: &structural_pos::CursorWithPersistence<HAST::IdN, HAST::Idx>,
) -> CursorStatus<<<HAST as HyperAST>::TS as RoleStore>::IdF>
where
    HAST::IdN: std::fmt::Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> hyperast::types::LendT<'t, HAST>: WithRoles,
    for<'t> hyperast::types::LendT<'t, HAST>: WithPrecompQueries,
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

impl<HAST: HyperAST> self::NodeRef<'_, '_, HAST>
where
    HAST::IdN: std::fmt::Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> hyperast::types::LendT<'t, HAST>: WithRoles,
    for<'t> hyperast::types::LendT<'t, HAST>: WithPrecompQueries,
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

impl<'hast, HAST: HyperAST> crate::TextLending<'_> for self::Node<'hast, HAST> {
    type TP = &'hast <HAST as HyperAST>::LS;
}

impl<HAST: HyperAST> PartialEq for self::Node<'_, HAST>
where
    HAST::IdN: std::fmt::Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> hyperast::types::LendT<'t, HAST>: WithRoles,
    for<'t> hyperast::types::LendT<'t, HAST>: WithPrecompQueries,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
{
    fn eq(&self, other: &Self) -> bool {
        self.pos == other.pos
    }
}

impl<HAST: HyperAST> crate::Node for self::Node<'_, HAST>
where
    HAST::IdN: std::fmt::Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> hyperast::types::LendT<'t, HAST>: WithRoles,
    for<'t> hyperast::types::LendT<'t, HAST>: WithPrecompQueries,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
{
    fn symbol(&self) -> Symbol {
        let n = self.pos.node();
        let t = self.stores.resolve_type(&n);
        let lang = t.get_lang();
        let id = lang.ts_symbol(t);
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
        text(self.stores, &self.pos)
    }
}

impl<'hast, HAST: HyperAST> crate::TextLending<'_> for self::NodeRef<'_, 'hast, HAST> {
    type TP = &'hast <HAST as HyperAST>::LS;
}

impl<HAST: HyperAST> PartialEq for self::NodeRef<'_, '_, HAST>
where
    HAST::IdN: std::fmt::Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> hyperast::types::LendT<'t, HAST>: WithRoles,
    for<'t> hyperast::types::LendT<'t, HAST>: WithPrecompQueries,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
{
    fn eq(&self, other: &Self) -> bool {
        self.pos == other.pos
    }
}

impl<HAST: HyperAST> crate::Node for self::NodeRef<'_, '_, HAST>
where
    HAST::IdN: std::fmt::Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> hyperast::types::LendT<'t, HAST>: WithRoles,
    for<'t> hyperast::types::LendT<'t, HAST>: WithPrecompQueries,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
{
    fn symbol(&self) -> Symbol {
        let n = self.pos.node();
        let t = self.stores.resolve_type(&n);
        let lang = t.get_lang();
        let id = lang.ts_symbol(t);
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

fn kind<HAST: HyperAST>(
    stores: &HAST,
    pos: &impl CursorHead<HAST::IdN, HAST::Idx>,
) -> <HAST::TS as TypeStore>::Ty {
    stores.resolve_type(&pos.node())
}

fn resolve<'hast, HAST: HyperAST>(
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

#[inline(always)]
fn goto_next_sibling_internal<HAST: HyperAST>(
    stores: &HAST,
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
        let k = decomp_t!(HAST::TS, &n);
        if k.is_hidden() {
            // dbg!(stores.resolve_type(&p));
            pos.up();
            return goto_next_sibling_internal(stores, pos);
        } else {
            return TreeCursorStep::None;
        }
    };
    pos.inc(node);
    let k = kind(stores, pos);
    if k.is_spaces() {
        return goto_next_sibling_internal(stores, pos);
    }
    if !k.is_hidden() {
        TreeCursorStep::Visible
    } else {
        TreeCursorStep::Hidden
    }
}

#[inline(always)]
fn goto_first_child_internal<HAST: HyperAST>(
    stores: &HAST,
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
    let k = kind(stores, pos);
    if k.is_spaces() {
        return goto_next_sibling_internal(stores, pos);
    }
    if !k.is_hidden() {
        TreeCursorStep::Visible
    } else {
        TreeCursorStep::Hidden
    }
}
