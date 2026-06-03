//! Attempt to integrate another query matchers compatible with hyperast.
//! The query matcher used here is largely inspired by tree_sitter (query.c).
use std::{fmt::Debug, hash::Hash};

use hyperast::types;
use hyperast::types::{HyperAST, HyperASTShared, LendT, NodeId, RoleStore};
use hyperast::types::{WithPrecompQueries, WithRoles, WithSerialization, WithStats};

use crate::{ArrayStr, CaptureId, StatusLending, hyperast_cursor::NodeR};

#[cfg(feature = "tsg")]
mod tsg_impl;

impl<HAST: HyperASTShared, P: Clone> From<Node<'_, HAST, P>> for NodeR<P> {
    fn from(value: Node<'_, HAST, P>) -> Self {
        let pos = value.0.pos.clone();
        Self { pos }
    }
}

#[repr(transparent)]
pub struct Node<'tree, HAST: HyperASTShared, P = Pos<HAST>>(
    pub crate::hyperast_cursor::Node<'tree, HAST, P>,
);

impl<HAST: HyperASTShared, P: Clone> Clone for Node<'_, HAST, P> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<HAST: HyperAST> PartialEq for Node<'_, HAST> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<'hast, HAST: HyperAST> super::TextLending<'_> for Node<'hast, HAST> {
    type TP = &'hast <HAST as HyperAST>::LS;
}

impl<HAST: HyperAST> crate::Node for Node<'_, HAST>
where
    HAST::IdN: Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> LendT<'t, HAST>: WithRoles,
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

    type IdF = <HAST::TS as RoleStore>::IdF;

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

impl<HAST: HyperAST> crate::WithField for Node<'_, HAST>
where
    HAST::TS: RoleStore,
{
    type IdF = <HAST::TS as RoleStore>::IdF;
}

impl<HAST: HyperAST> crate::CNLending<'_> for Node<'_, HAST>
where
    HAST::IdN: NodeId<IdN = HAST::IdN>,
    HAST::IdN: Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> LendT<'t, HAST>: WithRoles + WithPrecompQueries,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
{
    type NR = Self;
}

impl<HAST: HyperAST> crate::StatusLending<'_> for Node<'_, HAST>
where
    HAST::IdN: NodeId<IdN = HAST::IdN>,
    HAST::IdN: Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> LendT<'t, HAST>: WithRoles,
{
    type Status = crate::hyperast_cursor::CursorStatus<<Self as crate::Node>::IdF>;
}

impl<HAST: HyperAST> crate::Cursor for Node<'_, HAST>
where
    HAST::IdN: NodeId<IdN = HAST::IdN>,
    HAST::IdN: Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> LendT<'t, HAST>: WithRoles + WithPrecompQueries,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
{
    type Node = Self;

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

impl<'tree, HAST: HyperAST> Node<'tree, HAST> {
    pub fn new(
        stores: &'tree HAST,
        pos: hyperast::position::StructuralPosition<HAST::IdN, HAST::Idx>,
    ) -> Self {
        Self(crate::hyperast_cursor::Node::new(stores, pos))
    }
}

pub struct MyNodeErazing<'hast, HAST>(std::marker::PhantomData<&'hast HAST>);
impl<HAST> Default for MyNodeErazing<'_, HAST> {
    fn default() -> Self {
        Self(Default::default())
    }
}

pub struct QueryMatcher<HAST> {
    pub query: crate::Query,
    _phantom: std::marker::PhantomData<HAST>,
}

impl<HAST> QueryMatcher<HAST> {
    fn new(
        source: &str,
        language: &tree_sitter::Language,
    ) -> Result<Self, tree_sitter::QueryError> {
        let query = crate::Query::new(source, language.clone())?;
        Ok(Self::with_query(query))
    }
    fn with_query(query: crate::Query) -> Self {
        let _phantom = Default::default();
        Self { query, _phantom }
    }
    fn with_precomputed(
        source: &str,
        language: &tree_sitter::Language,
        precomputeds: impl ArrayStr,
    ) -> Result<Self, tree_sitter::QueryError> {
        let query = crate::Query::with_precomputed(source, language.clone(), precomputeds)?.1;
        Ok(Self::with_query(query))
    }
}

impl<HAST> Debug for QueryMatcher<HAST> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.query)
    }
}
pub struct ExtendingStringQuery<Q = tree_sitter::Query, L = tree_sitter::Language> {
    pub(crate) query: Option<Q>,
    pub(crate) acc: String,
    pub(crate) precomputeds: Option<Box<dyn ArrayStr>>,
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
    pub fn new(language: L, precomputeds: Box<dyn ArrayStr>, capacity: usize) -> Self {
        Self {
            acc: String::with_capacity(capacity),
            precomputeds: Some(precomputeds),
            language: Some(language),
            ..Self::empty()
        }
    }
}

pub struct MyQMatch<'cursor, 'tree, HAST: HyperAST, P = Pos<HAST>> {
    stores: &'tree HAST,
    b: &'cursor (),
    qm: crate::QueryMatch<Node<'tree, HAST, P>>,
    i: u16,
}

#[allow(type_alias_bounds)]
type Pos<HAST: HyperASTShared> = hyperast::position::StructuralPosition<
    <HAST as HyperASTShared>::IdN,
    <HAST as HyperASTShared>::Idx,
>;

pub struct CapturedNodesIter<'hast, 'cursor, 'tree, HAST: HyperASTShared, P = Pos<HAST>> {
    stores: &'hast HAST,
    index: u32,
    inner: &'cursor [crate::Capture<Node<'tree, HAST, P>>],
}

pub struct MyQMatches<'query, 'cursor: 'query, 'tree: 'cursor, It, HAST: HyperASTShared> {
    q: &'query QueryMatcher<HAST>,
    cursor: &'cursor mut Vec<u16>,
    matchs: It,
    node: Node<'tree, HAST>,
}

impl<'query, 'cursor: 'query, 'tree: 'cursor, It, HAST: HyperAST> Iterator
    for MyQMatches<'query, 'cursor, 'tree, It, HAST>
where
    It: Iterator<Item = crate::QueryMatch<Node<'tree, HAST>>>,
{
    type Item = self::MyQMatch<'cursor, 'tree, HAST>;

    fn next(&mut self) -> Option<Self::Item> {
        dbg!();
        let qm = self.matchs.next()?;
        let stores = self.node.0.stores;
        let i = (self.q.query)
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
