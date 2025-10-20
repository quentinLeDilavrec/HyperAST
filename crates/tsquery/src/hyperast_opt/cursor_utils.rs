//! Utilities for cursor
//!
use hyperast::position::structural_pos;
use hyperast::types;

use types::{HyperAST, HyperASTShared, LendT};
use types::{HyperType as _, Labeled as _, Tree as _};
use types::{RoleStore, TypeStore};
use types::{WithPrecompQueries, WithRoles};

use structural_pos::{CursorHead, CursorHeadMove};

use crate::{BiCow, Point, Symbol, TreeCursorStep};
use crate::{StatusLending, TextLending};

pub fn kind<HAST: HyperAST>(
    stores: &HAST,
    pos: &impl CursorHead<HAST::IdN, HAST::Idx>,
) -> <HAST::TS as TypeStore>::Ty {
    stores.resolve_type(&pos.node())
}

pub fn resolve<'hast, HAST: HyperAST>(
    stores: &'hast HAST,
    pos: &impl CursorHead<HAST::IdN, HAST::Idx>,
) -> LendT<'hast, HAST> {
    let n = pos.node();
    use hyperast::types::NodeStore;
    let n = stores.node_store().resolve(&n);
    n
}

pub fn is_visible<HAST: HyperAST>(
    stores: &HAST,
    pos: &impl CursorHead<HAST::IdN, HAST::Idx>,
) -> bool {
    !kind(stores, pos).is_hidden()
}

pub fn symbol<HAST: HyperAST>(
    stores: &HAST,
    pos: &impl CursorHead<HAST::IdN, HAST::Idx>,
) -> Symbol {
    let id = pos.node();
    super::symbol(stores, id)
}

pub(super) fn text<'hast, 'l, HAST: HyperAST>(
    stores: &'hast HAST,
    pos: &impl CursorHead<HAST::IdN, HAST::Idx>,
) -> crate::BiCow<'hast, 'l, str>
where
    HAST::IdN: hyperast::types::NodeId<IdN = HAST::IdN>,
{
    let id = pos.node();
    use hyperast::types::NodeStore;
    let n = stores.node_store().resolve(&id);
    if n.has_children() {
        let r = hyperast::nodes::TextSerializer::new(stores, id).to_string();
        return crate::BiCow::Owned(r);
    }
    if let Some(l) = n.try_get_label() {
        use hyperast::types::LabelStore;
        let l = stores.label_store().resolve(l);
        return crate::BiCow::A(l);
    }
    let ty = stores.resolve_type(&id);
    if !ty.is_named() {
        crate::BiCow::A(ty.as_static_str())
        // ty.to_string().into()
    } else {
        crate::BiCow::A("")
    }
}

pub(super) fn role<'hast, HAST: HyperAST>(
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

pub(crate) struct SuperTypeIter<'a, 'hast, HAST: HyperASTShared> {
    pub stores: &'hast HAST,
    pub pos: structural_pos::RefNode<'a, HAST::IdN, HAST::Idx>,
}

impl<HAST: HyperAST> Iterator for SuperTypeIter<'_, '_, HAST>
where
    HAST::IdN: std::fmt::Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> LendT<'t, HAST>: WithRoles,
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

pub fn goto_parent<HAST: HyperAST>(
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

pub fn goto_next_sibling_internal<HAST: HyperAST>(
    stores: &HAST,
    pos: &mut impl CursorHeadMove<HAST::IdN, HAST::Idx>,
) -> TreeCursorStep
where
    HAST::IdN: hyperast::types::NodeId<IdN = HAST::IdN>,
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
    let k = kind(stores, pos);
    if k.is_spaces() {
        return goto_next_sibling_internal(stores, pos);
    }
    if super::is_visible(stores, k) {
        TreeCursorStep::Visible
    } else {
        TreeCursorStep::Hidden
    }
}

pub fn goto_first_child_internal<HAST: HyperAST>(
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

pub fn has_child_with_field_id<'hast, HAST: HyperAST>(
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
            TreeCursorStep::None => panic!(),
            TreeCursorStep::Hidden => (),
            TreeCursorStep::Visible => break,
        }
    }
    child_by_role(stores, &mut pos, role).is_some()
}

pub fn child_by_role<'hast, HAST: HyperAST>(
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
