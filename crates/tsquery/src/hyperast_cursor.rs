use crate::{CNLending, StatusLending};

use super::{Cursor, Node as _, Status, Symbol, TreeCursorStep};
use hyperast::position::TreePath;
use hyperast::position::TreePathMut;
use hyperast::types::{HyperAST, HyperASTShared, TypeStore};
use hyperast::types::{HyperType, LabelStore, Labeled, NodeStore, RoleStore, Tree};
use hyperast::types::{WithPrecompQueries, WithRoles};

pub type TreeCursor<'hast, HAST> = Node<'hast, HAST>;

pub struct Node<
    'hast,
    HAST: HyperASTShared,
    P = hyperast::position::StructuralPosition<
        <HAST as HyperASTShared>::IdN,
        <HAST as HyperASTShared>::Idx,
    >,
> {
    pub stores: &'hast HAST,
    pub pos: P,
}

#[derive(Clone, PartialEq, Eq)]
pub struct NodeR<P> {
    /// the offset in acc
    // offset: Idx,
    pub pos: P,
}

impl<HAST: HyperAST> PartialEq for Node<'_, HAST> {
    fn eq(&self, other: &Self) -> bool {
        self.pos == other.pos
    }
}

impl<'hast, HAST: HyperAST> Node<'hast, HAST> {
    pub fn new(
        stores: &'hast HAST,
        pos: hyperast::position::StructuralPosition<HAST::IdN, HAST::Idx>,
    ) -> Self {
        Self { stores, pos }
    }
}

impl<HAST: HyperASTShared, P: Clone> Clone for Node<'_, HAST, P> {
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

impl<'hast, HAST: HyperAST> CNLending<'_> for self::TreeCursor<'hast, HAST>
where
    HAST::IdN: std::fmt::Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> <HAST as hyperast::types::AstLending<'t>>::RT: WithRoles,
    for<'t> <HAST as hyperast::types::AstLending<'t>>::RT: WithPrecompQueries,
    HAST::IdN: hyperast::types::NodeId<IdN = HAST::IdN>,
{
    type NR = self::Node<'hast, HAST>;
}

impl<'hast, HAST: HyperAST> StatusLending<'_> for self::TreeCursor<'hast, HAST>
where
    HAST::IdN: std::fmt::Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> <HAST as hyperast::types::AstLending<'t>>::RT: WithRoles,
    for<'t> <HAST as hyperast::types::AstLending<'t>>::RT: WithPrecompQueries,
    HAST::IdN: hyperast::types::NodeId<IdN = HAST::IdN>,
{
    type Status = CursorStatus<<<HAST as HyperAST>::TS as RoleStore>::IdF>;
}

impl<'hast, HAST: HyperAST> super::Cursor for self::TreeCursor<'hast, HAST>
where
    HAST::IdN: std::fmt::Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> <HAST as hyperast::types::AstLending<'t>>::RT: WithRoles,
    for<'t> <HAST as hyperast::types::AstLending<'t>>::RT: WithPrecompQueries,
    HAST::IdN: hyperast::types::NodeId<IdN = HAST::IdN>,
{
    type Node = self::Node<'hast, HAST>;
    // type NodeRef<'a>
    //     = &'a self::Node<'hast, HAST>
    // where
    //     Self: 'a;

    fn goto_next_sibling_internal(&mut self) -> TreeCursorStep {
        self._goto_next_sibling_internal()
    }

    fn goto_first_child_internal(&mut self) -> TreeCursorStep {
        self._goto_first_child_internal()
    }

    fn goto_parent(&mut self) -> bool {
        loop {
            if self.pos.pop().is_none() {
                return false;
            }
            if self.pos.node().is_none() {
                return false;
            }
            if self.is_visible() {
                return true;
            }
        }
    }

    fn current_node(&self) -> <Self as CNLending<'_>>::NR {
        self.clone()
    }

    fn parent_is_error(&self) -> bool {
        // NOTE: maybe more efficient impl
        let mut s = self.clone();
        if !s.goto_parent() {
            return false;
        }
        s.symbol() == Symbol::ERROR
    }

    fn has_parent(&self) -> bool {
        let mut node = self.clone();
        node.goto_parent()
    }

    fn persist(&self) -> Self::Node {
        self.clone()
    }

    fn persist_parent(&self) -> Option<Self::Node> {
        let mut node = self.clone();
        node.goto_parent();
        Some(node)
    }

    #[inline]
    fn current_status(&self) -> <Self as StatusLending<'_>>::Status {
        let (role, field_id) = self.compute_current_role();
        let mut has_later_siblings = false;
        let mut has_later_named_siblings = false;
        let mut can_have_later_siblings_with_this_field = false;
        let mut s = self.clone();
        loop {
            if let TreeCursorStep::None = s.goto_next_sibling_internal() {
                break;
            }
            if role.is_some() && s.role() == role {
                can_have_later_siblings_with_this_field = true;
            }
            has_later_siblings = true;
            if s.kind().is_supertype() {
                has_later_named_siblings = true;
            }
            if s.is_visible() {
                has_later_siblings = true;
                use super::Node;
                if s.is_named() {
                    has_later_named_siblings = true;
                    break;
                }
            }
        }
        let mut supertypes = self.clone().super_types();
        if self.kind().is_supertype() {
            supertypes.push(self.symbol());
        }
        CursorStatus {
            has_later_siblings,
            has_later_named_siblings,
            can_have_later_siblings_with_this_field,
            field_id,
            supertypes,
        }
    }

    fn text_provider(&self) -> <Self::Node as super::TextLending<'_>>::TP {
        self.stores.label_store()
    }

    fn is_visible_at_root(&self) -> bool {
        assert!(self.pos.parent().is_none());
        self.is_visible()
    }

    fn wont_match(&self, needed: crate::Precomps) -> bool {
        if needed == 0 {
            return false;
        }
        use hyperast::types::NodeStore;
        let n = self.stores.node_store().resolve(self.pos.node().unwrap());
        n.wont_match_given_precomputed_queries(needed)
    }
}

impl<'hast, HAST: HyperAST> Node<'hast, HAST>
where
    HAST::IdN: std::fmt::Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> <HAST as hyperast::types::AstLending<'t>>::RT: WithRoles,
    HAST::IdN: hyperast::types::NodeId<IdN = HAST::IdN>,
{
    fn _goto_first_child_internal(&mut self) -> TreeCursorStep {
        use hyperast::types::NodeStore;
        let n = self.stores.node_store().resolve(self.pos.node().unwrap());
        use hyperast::types::Children;
        use hyperast::types::WithChildren;
        let Some(cs) = n.children() else {
            return TreeCursorStep::None;
        };
        let Some(node) = cs.get(num::zero()) else {
            return TreeCursorStep::None;
        };
        self.pos.goto(*node, num::zero());
        if self.kind().is_spaces() {
            return self._goto_next_sibling_internal();
        }
        if self.is_visible() {
            TreeCursorStep::Visible
        } else {
            TreeCursorStep::Hidden
        }
    }

    fn _goto_next_sibling_internal(&mut self) -> TreeCursorStep {
        use hyperast::types::NodeStore;
        let Some(p) = self.pos.parent() else {
            return TreeCursorStep::None;
        };
        let n = self.stores.node_store().resolve(p);
        use hyperast::types::Children;
        use hyperast::types::WithChildren;
        let Some(node) = n.child(self.pos.offset().unwrap()) else {
            if self.stores.resolve_type(p).is_hidden() {
                self.pos.pop();
                return self._goto_next_sibling_internal();
            } else {
                return TreeCursorStep::None;
            }
        };
        self.pos.inc(node);
        if self.kind().is_spaces() {
            return self._goto_next_sibling_internal();
        }
        if self.is_visible() {
            TreeCursorStep::Visible
        } else {
            TreeCursorStep::Hidden
        }
    }
}

impl<HAST: HyperAST> self::TreeCursor<'_, HAST>
where
    HAST::IdN: std::fmt::Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> <HAST as hyperast::types::AstLending<'t>>::RT: WithRoles,
    HAST::IdN: hyperast::types::NodeId<IdN = HAST::IdN>,
{
    fn role(&self) -> Option<<HAST::TS as RoleStore>::Role> {
        use hyperast::types::NodeStore;
        let n = self.stores.node_store().resolve(self.pos.parent().unwrap());
        n.role_at::<<HAST::TS as RoleStore>::Role>(self.pos.o().unwrap())
    }

    fn super_types(mut self) -> Vec<Symbol> {
        // TODO Might create efficiency issues, is it compiling well ?
        let mut result = vec![];
        loop {
            use super::Node;
            self.pos.pop();
            if self.pos.offset().is_none() {
                return result;
            }
            if self.is_visible() {
                return result;
            }
            if self.kind().is_supertype() {
                result.push(self.symbol())
            }
        }
    }

    fn compute_current_role(
        &self,
    ) -> (
        Option<<<HAST as HyperAST>::TS as RoleStore>::Role>,
        <<HAST as HyperAST>::TS as RoleStore>::IdF,
    ) {
        use hyperast::types::NodeStore;
        let mut p = self.clone();
        let lang;
        let role = loop {
            let Some((_, o)) = p.pos.pop() else {
                return (None, Default::default());
            };
            let Some(n) = p.pos.node() else {
                return (None, Default::default());
            };
            let n = self.stores.node_store().resolve(n);
            // dbg!(p.kind());
            if p.kind().is_supertype() {
                continue;
            }
            lang = p.kind().get_lang();
            break n.role_at::<<HAST::TS as RoleStore>::Role>(o - num::one());
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

impl<HAST: HyperAST> super::Node for self::Node<'_, HAST>
where
    HAST::IdN: std::fmt::Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> <HAST as hyperast::types::AstLending<'t>>::RT: WithRoles,
    HAST::IdN: hyperast::types::NodeId<IdN = HAST::IdN>,
{
    fn symbol(&self) -> Symbol {
        // TODO make something more efficient
        let n = self.pos.node().unwrap();
        let t = self.stores.resolve_type(n);
        if t.is_directory() {
            return 0.into();
        }
        use hyperast::types::LangRef;
        let id = t.get_lang().ts_symbol(t);
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
        if field_id == Default::default() {
            return false;
        }
        let role = HAST::TS::resolve_field(self.kind().get_lang(), field_id);
        let mut slf = self.clone();
        loop {
            if slf.kind().is_supertype() {
                match slf._goto_first_child_internal() {
                    TreeCursorStep::None => panic!(),
                    TreeCursorStep::Hidden => (),
                    TreeCursorStep::Visible => break,
                }
            } else {
                break;
            }
        }
        slf.child_by_role(role).is_some()
    }

    fn equal(&self, other: &Self, _text_provider: <Self as super::TextLending<'_>>::TP) -> bool {
        self.pos.node() == other.pos.node()
    }

    fn compare(&self, other: &Self) -> std::cmp::Ordering {
        self.pos.cmp(&other.pos)
    }

    fn text<'s, 'l>(
        &'s self,
        text_provider: <Self as super::TextLending<'l>>::TP,
    ) -> super::BiCow<'s, 'l, str> {
        let id = self.pos.node().unwrap();
        use hyperast::types::NodeStore;
        let n = self.stores.node_store().resolve(id);
        if n.has_children() {
            let r = hyperast::nodes::TextSerializer::new(self.stores, *id).to_string();
            return super::BiCow::Owned(r);
        }
        if let Some(l) = n.try_get_label() {
            let l = self.stores.label_store().resolve(l);
            // todo!()
            return super::BiCow::A(l);
        }
        super::BiCow::B("") // TODO check if it is the right behavior
    }
}

impl<HAST: HyperAST> Node<'_, HAST>
where
    HAST::IdN: std::fmt::Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> <HAST as hyperast::types::AstLending<'t>>::RT: WithRoles,
    HAST::IdN: hyperast::types::NodeId<IdN = HAST::IdN>,
{
    fn child_by_role(&mut self, role: <HAST::TS as RoleStore>::Role) -> Option<()> {
        // TODO what about multiple children with same role?
        // NOTE treesitter uses a bin tree for repeats
        let visible = self.is_visible();
        if let TreeCursorStep::None = self._goto_first_child_internal() {
            return None;
        }
        loop {
            if let Some(r) = self.role() {
                if r == role {
                    return Some(());
                } else {
                    if let TreeCursorStep::None = self._goto_next_sibling_internal() {
                        return None;
                    }
                    continue;
                }
            }
            // do not go down
            if visible {
                if let TreeCursorStep::None = self._goto_next_sibling_internal() {
                    return None;
                }
            }
            // hidden node so can explore
            else {
                if self.child_by_role(role).is_some() {
                    return Some(());
                }
                if let TreeCursorStep::None = self._goto_next_sibling_internal() {
                    return None;
                }
            }
        }
    }
}

impl<HAST: HyperAST> Node<'_, HAST>
where
    HAST::IdN: std::fmt::Debug + Copy,
{
    fn kind(&self) -> <HAST::TS as TypeStore>::Ty {
        self.stores.resolve_type(self.pos.node().unwrap())
    }
}

impl<HAST: HyperAST> Node<'_, HAST>
where
    HAST::IdN: std::fmt::Debug + Copy,
{
    fn is_visible(&self) -> bool {
        !self.kind().is_hidden()
    }

    pub(crate) fn goto_parent(&mut self) -> bool {
        loop {
            if self.pos.pop().is_none() {
                return false;
            }
            if self.pos.node().is_none() {
                return false;
            }
            if self.is_visible() {
                return true;
            }
        }
    }
}
