use super::cursor_utils::{has_child_with_field_id, kind, resolve, text};

use hyperast::position::structural_pos;
use hyperast::types;

use structural_pos::{CursorHead, CursorHeadMove};
use types::{HyperAST, HyperASTShared, LendT};
use types::{HyperType as _, Labeled as _, Tree as _};
use types::{RoleStore, TypeStore};
use types::{WithPrecompQueries, WithRoles};

use crate::Status;
use crate::WithField;
use crate::{BiCow, Point, Symbol};

impl<HAST: HyperAST + Copy> Clone for super::NodeRefNoRef<'_, HAST> {
    fn clone(&self) -> Self {
        Self {
            stores: self.stores,
            pos: self.pos.clone(),
        }
    }
}

#[cfg(feature = "tsg")]
impl<HAST: HyperAST + Copy> tree_sitter_graph::graph::SimpleNode for super::NodeRefNoRef<'_, HAST>
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
}

impl<HAST: HyperAST> super::NodeRefNoRef<'_, HAST>
where
    HAST::IdN: std::fmt::Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> LendT<'t, HAST>: WithRoles,
{
    pub(crate) fn compute_current_role(
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
            let k = kind(&self.stores, &self.pos);
            // dbg!(k);
            if k.is_supertype() {
                // dbg!();
                continue;
            }
            let n = resolve(&self.stores, &self.pos);
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

impl<HAST: HyperAST + Copy> Clone for super::NodeRefKNoRef<'_, HAST> {
    fn clone(&self) -> Self {
        Self {
            stores: self.stores,
            pos: self.pos.clone(),
            kind: self.kind,
        }
    }
}

#[cfg(feature = "tsg")]
impl<HAST: HyperAST + Copy> tree_sitter_graph::graph::SimpleNode for super::NodeRefKNoRef<'_, HAST>
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
}

impl<HAST: HyperAST> super::NodeRefKNoRef<'_, HAST>
where
    HAST::IdN: std::fmt::Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> hyperast::types::LendT<'t, HAST>: WithRoles,
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
            let k = kind(&self.stores, &self.pos);
            if k.is_supertype() {
                continue;
            }
            let n = resolve(&self.stores, &self.pos);
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

impl<HAST: HyperAST> crate::TextLending<'_> for super::NodeRefKNoRef<'_, HAST> {
    type TP = ();
}

impl<HAST: HyperAST> PartialEq for super::NodeRefKNoRef<'_, HAST>
where
    HAST::IdN: std::fmt::Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> hyperast::types::LendT<'t, HAST>: WithRoles,
    HAST::IdN: hyperast::types::NodeId<IdN = HAST::IdN>,
{
    fn eq(&self, other: &Self) -> bool {
        self.pos == other.pos
    }
}

impl<HAST: HyperAST + Copy> crate::Node for super::NodeRefKNoRef<'_, HAST>
where
    HAST::IdN: std::fmt::Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> hyperast::types::LendT<'t, HAST>: WithRoles,
    HAST::IdN: hyperast::types::NodeId<IdN = HAST::IdN>,
{
    fn symbol(&self) -> Symbol {
        let t = self.kind;
        let lang = t.get_lang();
        if t.is_directory() {
            return 0.into();
        }
        use hyperast::types::LangRef;
        let id = lang.ts_symbol(t);
        id.into()
    }

    fn is_named(&self) -> bool {
        self.kind.is_named()
    }

    fn str_symbol(&self) -> &str {
        self.kind.as_static_str()
    }

    fn start_point(&self) -> tree_sitter::Point {
        // TODO
        tree_sitter::Point { row: 0, column: 0 }
    }

    type IdF = <HAST::TS as RoleStore>::IdF;

    fn has_child_with_field_id(&self, field_id: Self::IdF) -> bool {
        if field_id == Default::default() {
            return false;
        }
        let role = HAST::TS::resolve_field(self.kind.get_lang(), field_id);
        let mut pos = self.pos.ext();
        if self.kind.is_supertype() {
            loop {
                use crate::TreeCursorStep;
                match super::cursor_utils2::goto_first_child_internal(&self.stores, &mut pos) {
                    TreeCursorStep::None => panic!(),
                    TreeCursorStep::Hidden => (),
                    TreeCursorStep::Visible => break,
                }
                if !kind(&self.stores, &pos).is_supertype() {
                    break;
                }
            }
        }
        super::cursor_utils2::child_by_role(&self.stores, &mut pos, role).is_some()
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
        text(&self.stores, &self.pos)
    }
}

impl<HAST: HyperAST, P> crate::TextLending<'_> for super::NodeNoRef<HAST, P> {
    type TP = ();
}

impl<HAST: HyperAST, P: Clone + PartialEq> PartialEq for super::NodeNoRef<HAST, P>
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

impl<HAST: HyperAST + Copy, P: Clone> Clone for super::NodeNoRef<HAST, P> {
    fn clone(&self) -> Self {
        Self {
            stores: self.stores,
            pos: self.pos.clone(),
        }
    }
}

impl<HAST: HyperAST + Copy> crate::Node for super::NodeNoRef<HAST>
where
    HAST::IdN: std::fmt::Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> LendT<'t, HAST>: WithRoles,
    for<'t> LendT<'t, HAST>: WithPrecompQueries,
    HAST::IdN: hyperast::types::NodeId<IdN = HAST::IdN>,
    // P: CursorHead<HAST::IdN, HAST::Idx>,
    // P: PartialEq,
    // P: structural_pos::Ext,
    // for<'t> P::E<'t>:
    //     Clone + CursorHeadMove<HAST::IdN, HAST::Idx> + CursorHead<HAST::IdN, HAST::Idx>,
{
    fn symbol(&self) -> Symbol {
        super::cursor_utils::symbol(&self.stores, &self.pos)
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
        has_child_with_field_id(&self.stores, self.pos.ext(), field_id)
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
        text(&self.stores, &self.pos)
    }
}

impl<HAST: HyperAST> crate::TextLending<'_> for super::NodeRefNoRef<'_, HAST> {
    type TP = ();
}

impl<HAST: HyperAST> PartialEq for super::NodeRefNoRef<'_, HAST>
where
    HAST::IdN: std::fmt::Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> LendT<'t, HAST>: WithRoles,
    HAST::IdN: hyperast::types::NodeId<IdN = HAST::IdN>,
{
    fn eq(&self, other: &Self) -> bool {
        self.pos == other.pos
    }
}

impl<HAST: HyperAST + Copy> crate::Node for super::NodeRefNoRef<'_, HAST>
where
    HAST::IdN: std::fmt::Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> LendT<'t, HAST>: WithRoles,
    HAST::IdN: hyperast::types::NodeId<IdN = HAST::IdN>,
{
    fn symbol(&self) -> Symbol {
        super::cursor_utils::symbol(&self.stores, &self.pos)
    }

    fn is_named(&self) -> bool {
        kind(&self.stores, &self.pos).is_named()
    }

    fn str_symbol(&self) -> &str {
        kind(&self.stores, &self.pos).as_static_str()
    }

    fn start_point(&self) -> tree_sitter::Point {
        // TODO
        tree_sitter::Point { row: 0, column: 0 }
    }

    type IdF = <HAST::TS as RoleStore>::IdF;

    fn has_child_with_field_id(&self, field_id: Self::IdF) -> bool {
        has_child_with_field_id(&self.stores, self.pos.ext(), field_id)
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
        text(&self.stores, &self.pos)
    }
}

impl<HAST: HyperAST> super::NodeNoRef<HAST>
where
    HAST::IdN: std::fmt::Debug + Copy,
{
    fn kind(&self) -> <HAST::TS as TypeStore>::Ty {
        kind(&self.stores, &self.pos)
    }
}
