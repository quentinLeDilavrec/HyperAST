//! Attempt to integrate another query matchers compatible with hyperast.
//! The query matcher used here is largely inspired by tree_sitter (query.c).
//! Trying to make this one applicable directly on subtrees, ie. immediated/shallow
use std::fmt::Debug;
use std::hash::Hash;

use hyperast::position;
use hyperast::position::TreePathMut;
use hyperast::tree_gen;
use hyperast::tree_gen::WithLabel;
use hyperast::types;
use hyperast::types::{ETypeStore, NodeId};
use hyperast::types::{HyperAST, HyperASTShared, LendT};
use hyperast::types::{Role, RoleStore};
use hyperast::types::{WithRoles, WithSerialization, WithStats};

use crate::{CaptureId, StatusLending, hyperast_cursor::NodeR};

#[cfg(feature = "tsg")]
mod tsg_impl;

impl<HAST: HyperASTShared, Acc: WithLabel, Idx, P: Clone> From<Node<HAST, Acc, Idx, P>>
    for NodeR<P>
{
    fn from(value: Node<HAST, Acc, Idx, P>) -> Self {
        let pos = value.0.pos.clone();
        Self { pos }
    }
}

#[repr(transparent)]
pub struct Node<
    HAST: HyperASTShared,
    Acc: WithLabel,
    Idx = <HAST as HyperASTShared>::Idx,
    P = position::StructuralPosition<<HAST as HyperASTShared>::IdN, Idx>,
    L = <Acc as WithLabel>::L,
>(
    // NOTE actually a bad idea to directly wrap cursor_on_unbuild::Node,
    // the nodes go in tsg Graphs and by holding a reference to HAST it locks down everything
    // TODO find a way to extract the essentials from Node (to free Graph), the rest could be then part of the execution context.
    // Doing so will probably contribute to facilitating the staged storage of graph nodes and edges.
    pub crate::cursor_on_unbuild::Node<HAST, Acc, Idx, P, L>,
);
// pub use crate::cursor_on_unbuild::Node;

impl<'hast, 'acc, HAST: HyperASTShared + Clone, Acc> Clone for Node<HAST, &'acc Acc>
where
    &'acc Acc: WithLabel,
{
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<'hast, HAST: HyperASTShared, Acc: WithLabel> PartialEq for Node<HAST, Acc> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

type IdF = u16;

impl<'hast, 'acc, HAST: HyperAST, Acc> super::TextLending<'_> for self::Node<HAST, &'acc Acc>
where
    &'acc Acc: WithLabel,
{
    type TP = ();
}

impl<'hast, 'acc, HAST, Acc> crate::Node for self::Node<HAST, &'acc Acc>
where
    HAST: HyperAST + Copy,
    HAST::TS: ETypeStore<Ty2 = Acc::Type>,
    HAST::TS: types::RoleStore<IdF = IdF, Role = Role>,
    for<'t> LendT<'t, HAST>: types::WithRoles,
    HAST::IdN: Copy,
    Acc: tree_gen::WithChildren<HAST::IdN> + tree_gen::WithRole<Role> + types::Typed,
    &'acc Acc: WithLabel,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
{
    fn symbol(&self) -> crate::Symbol {
        self.0.symbol()
    }

    fn is_named(&self) -> bool {
        self.0.is_named()
    }

    fn str_symbol(&self) -> &str {
        self.0.str_symbol()
    }

    fn start_point(&self) -> tree_sitter::Point {
        self.0.start_point()
    }

    type IdF = <HAST::TS as types::RoleStore>::IdF;

    fn has_child_with_field_id(&self, field_id: Self::IdF) -> bool {
        self.0.has_child_with_field_id(field_id)
    }

    fn equal(&self, other: &Self, text_provider: <Self as super::TextLending<'_>>::TP) -> bool {
        self.0.equal(&other.0, text_provider)
    }

    fn compare(&self, other: &Self) -> std::cmp::Ordering {
        self.0.compare(&other.0)
    }

    fn text<'s, 'l>(
        &'s self,
        text_provider: <Self as super::TextLending<'l>>::TP,
    ) -> super::BiCow<'s, 'l, str> {
        self.0.text(text_provider)
    }
}

impl<'acc, HAST: HyperAST, Acc> crate::WithField for Node<HAST, &'acc Acc>
where
    &'acc Acc: WithLabel,
{
    type IdF = IdF;
}

impl<'hast, 'acc, HAST, Acc> crate::CNLending<'_> for Node<HAST, &'acc Acc>
where
    HAST: HyperAST + Copy,
    HAST::TS: ETypeStore<Ty2 = Acc::Type> + RoleStore<IdF = IdF, Role = Role>,
    HAST::IdN: Copy,
    for<'t> LendT<'t, HAST>: WithRoles,
    Acc: tree_gen::WithRole<Role>,
    Acc: tree_gen::WithChildren<HAST::IdN>,
    Acc: types::Typed,
    &'acc Acc: WithLabel,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
{
    type NR = Self;
}

impl<'hast, 'acc, HAST, Acc> crate::StatusLending<'_> for Node<HAST, &'acc Acc>
where
    HAST: HyperAST + Copy,
    HAST::TS: ETypeStore<Ty2 = Acc::Type> + RoleStore<IdF = IdF, Role = Role>,
    HAST::IdN: Copy,
    for<'t> LendT<'t, HAST>: WithRoles,
    Acc: tree_gen::WithRole<Role>,
    Acc: tree_gen::WithChildren<HAST::IdN>,
    Acc: types::Typed,
    &'acc Acc: WithLabel,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
{
    type Status = crate::cursor_on_unbuild::CursorStatus<<Self as crate::Node>::IdF>;
}

impl<'hast, 'acc, HAST, Acc> crate::Cursor for Node<HAST, &'acc Acc>
where
    HAST: HyperAST + Copy,
    HAST::TS: ETypeStore<Ty2 = Acc::Type> + RoleStore<IdF = IdF, Role = Role>,
    HAST::IdN: Copy,
    for<'t> LendT<'t, HAST>: WithRoles,
    Acc: tree_gen::WithRole<Role>,
    Acc: tree_gen::WithChildren<HAST::IdN>,
    Acc: types::Typed,
    &'acc Acc: WithLabel,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
{
    type Node = Self;
    // type NodeRef<'a>
    //     = &'a Self
    // where
    //     Self: 'a;

    fn goto_next_sibling_internal(&mut self) -> crate::TreeCursorStep {
        self.0.goto_next_sibling_internal()
    }

    fn goto_first_child_internal(&mut self) -> crate::TreeCursorStep {
        self.0.goto_first_child_internal()
    }

    fn goto_parent(&mut self) -> bool {
        self.0.goto_parent()
    }

    fn current_node(&self) -> Self {
        self.clone()
    }

    fn parent_is_error(&self) -> bool {
        self.0.parent_is_error()
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

    fn current_status(&self) -> <Self as StatusLending<'_>>::Status {
        self.0.current_status()
    }

    fn text_provider(&self) -> <Self::Node as crate::TextLending<'_>>::TP {
        self.0.text_provider()
    }

    fn is_visible_at_root(&self) -> bool {
        self.0.is_visible_at_root()
    }
}

impl<'hast, HAST: HyperASTShared, Acc: WithLabel> Node<HAST, Acc> {
    pub fn new(
        stores: HAST,
        acc: Acc,
        label: Option<Acc::L>,
        pos: position::StructuralPosition<HAST::IdN, HAST::Idx>,
    ) -> Self {
        Self(crate::cursor_on_unbuild::Node::new(stores, acc, label, pos))
    }
}

pub struct MyNodeErazing<HAST, Acc>(std::marker::PhantomData<(HAST, Acc)>);
impl<HAST, Acc> Default for MyNodeErazing<HAST, &Acc> {
    fn default() -> Self {
        Self(Default::default())
    }
}

pub struct QueryMatcher<TS, Acc> {
    pub query: crate::Query,
    _phantom: std::marker::PhantomData<(TS, Acc)>,
}

impl<TS, Acc> QueryMatcher<TS, Acc> {
    fn new(
        source: &str,
        language: &tree_sitter::Language,
    ) -> Result<Self, tree_sitter::QueryError> {
        let query = crate::Query::new(source, language.clone())?;

        Ok(Self {
            query,
            _phantom: std::marker::PhantomData,
        })
    }
    fn with_precomputed(
        source: &str,
        language: &tree_sitter::Language,
        precomputeds: impl crate::utils::ArrayStr,
    ) -> Result<Self, tree_sitter::QueryError> {
        let query = crate::Query::with_precomputed(source, language.clone(), precomputeds)?.1;

        Ok(Self {
            query,
            _phantom: std::marker::PhantomData,
        })
    }
}

impl<TS, Acc> Debug for QueryMatcher<TS, Acc> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.query)
    }
}

pub struct ExtendingStringQuery<Q = tree_sitter::Query, L = tree_sitter::Language> {
    pub(crate) query: Option<Q>,
    pub(crate) acc: String,
    pub(crate) precomputeds: Option<Box<dyn crate::utils::ArrayStr>>,
    pub(crate) language: Option<L>,
}

impl<Q, L> ExtendingStringQuery<Q, L> {
    fn empty() -> Self {
        Self {
            query: Default::default(),
            acc: Default::default(),
            precomputeds: Default::default(),
            language: None,
        }
    }
    pub fn new(
        language: L,
        precomputeds: Box<dyn crate::utils::ArrayStr>,
        capacity: usize,
    ) -> Self {
        Self {
            acc: String::with_capacity(capacity),
            precomputeds: Some(precomputeds),
            language: Some(language),
            ..Self::empty()
        }
    }
}

pub struct MyQMatch<
    'cursor,
    HAST: HyperASTShared,
    Acc: WithLabel,
    Idx = <HAST as HyperASTShared>::Idx,
    P = position::StructuralPosition<<HAST as HyperASTShared>::IdN, Idx>,
    L = <Acc as WithLabel>::L,
> {
    pub stores: HAST,
    pub b: &'cursor (),
    pub qm: crate::QueryMatch<Node<HAST, Acc, Idx, P, L>>,
    pub i: u16,
}

pub struct CapturedNodesIter<
    'cursor,
    HAST: HyperASTShared,
    Acc: WithLabel,
    Idx = <HAST as HyperASTShared>::Idx,
    P = position::StructuralPosition<<HAST as HyperASTShared>::IdN, Idx>,
> {
    stores: HAST,
    index: CaptureId,
    inner: &'cursor [crate::Capture<Node<HAST, Acc, Idx, P>>],
}

#[allow(type_alias_bounds)]
type Pos<HAST: HyperASTShared> =
    position::StructuralPosition<<HAST as HyperASTShared>::IdN, <HAST as HyperASTShared>::Idx>;

pub struct MyQMatches<
    'query,
    'cursor,
    It,
    HAST: HyperASTShared,
    Acc: WithLabel,
    Idx = <HAST as HyperASTShared>::Idx,
    P = position::StructuralPosition<<HAST as HyperASTShared>::IdN, Idx>,
    L = <Acc as WithLabel>::L,
> {
    pub(crate) q: &'query QueryMatcher<HAST, Acc>,
    pub(crate) cursor: &'cursor mut Vec<u16>,
    pub(crate) matchs: It,
    pub(crate) node: Node<HAST, Acc, Idx, P, L>,
}

impl<'cursor, 'acc, It, HAST, Acc> Iterator for MyQMatches<'_, 'cursor, It, HAST, &'acc Acc>
where
    HAST: HyperAST + Copy,
    It: Iterator<Item = crate::QueryMatch<Node<HAST, &'acc Acc>>>,
    &'acc Acc: WithLabel,
{
    type Item = self::MyQMatch<'cursor, HAST, &'acc Acc>;

    fn next(&mut self) -> Option<Self::Item> {
        let qm = self.matchs.next()?;
        let stores = self.node.0.stores;
        let i = self
            .q
            .query
            .enabled_pattern_index(qm.pattern_index)
            .unwrap();
        Some(self::MyQMatch {
            stores,
            b: &(),
            qm,
            i,
        })
    }
}
