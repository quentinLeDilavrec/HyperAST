use std::{fmt::Debug, hash::Hash};

use hyperast::types;
use hyperast::types::{HyperAST, HyperASTShared, LendT, NodeId, RoleStore};
use hyperast::types::{WithPrecompQueries, WithRoles, WithSerialization, WithStats};

use crate::{ArrayStr, CaptureId, StatusLending, hyperast_cursor::NodeR};

use tree_sitter_graph::{
    MatchLender, MatchLending, QueryWithLang,
    graph::{NNN, NodeLender, NodeLending, NodesLending},
};

use super::Pos;
use super::{CapturedNodesIter, ExtendingStringQuery, Node, QueryMatcher};
use super::{MyQMatch, MyQMatches};

impl<HAST> tree_sitter_graph::QueryWithLang for QueryMatcher<HAST> {
    type Lang = tree_sitter::Language;
    type I = u32;
}

impl<'a, HAST> tree_sitter_graph::graph::NodeLending<'a> for QueryMatcher<HAST>
where
    HAST: HyperAST,
    HAST::TS: types::RoleStore,
    <HAST::TS as types::RoleStore>::IdF: From<u16> + Into<u16>,
    for<'t> LendT<'t, HAST>: WithSerialization + WithStats + WithRoles,
    HAST::IdN: Debug + Copy + Hash,
    HAST::Idx: Copy + Hash,
    for<'t> LendT<'t, HAST>: WithPrecompQueries,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
{
    type SNode = Node<'a, HAST>;
}

impl<'a, HAST> tree_sitter_graph::MatchesLending<'a> for QueryMatcher<HAST>
where
    HAST: HyperAST,
    HAST::TS: types::RoleStore,
    <HAST::TS as types::RoleStore>::IdF: From<u16> + Into<u16>,
    for<'t> LendT<'t, HAST>: WithSerialization + WithStats + WithRoles,
    HAST::IdN: Debug + Copy + Hash,
    HAST::Idx: Copy + Hash,
    for<'t> LendT<'t, HAST>: WithPrecompQueries,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
{
    type Matches = super::MyQMatches<
        'a,
        'a,
        'a,
        crate::QueryCursor<'a, <Self as NodeLending<'a>>::SNode, <Self as NodeLending<'a>>::SNode>,
        HAST,
    >;
}

impl<HAST> tree_sitter_graph::GenQuery for QueryMatcher<HAST>
where
    HAST: HyperAST,
    HAST::TS: types::RoleStore,
    <HAST::TS as types::RoleStore>::IdF: From<u16> + Into<u16>,
    for<'t> LendT<'t, HAST>: WithSerialization + WithStats + WithRoles,
    HAST::IdN: Debug + Copy + Hash,
    HAST::Idx: Copy + Hash,
    for<'t> LendT<'t, HAST>: WithPrecompQueries,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
{
    type Ext = ExtendingStringQuery<Self, Self::Lang>;

    fn pattern_count(&self) -> usize {
        self.query.enabled_pattern_count()
    }

    fn capture_index_for_name(&self, name: &str) -> Option<u32> {
        self.query
            .capture_index_for_name(name)
            .map(|x| x.to_usize() as u32)
    }

    fn capture_quantifiers(
        &self,
        index: usize,
    ) -> impl std::ops::Index<usize, Output = tree_sitter::CaptureQuantifier> {
        let index = (self.query.enabled_pattern_map)
            .iter()
            .position(|x| *x as usize == index)
            .unwrap();
        self.query.capture_quantifiers(index)
    }

    fn capture_names(&self) -> &[&str] {
        todo!()
    }

    fn check(
        file: &mut tree_sitter_graph::ast::File<Self>,
    ) -> Result<(), tree_sitter_graph::checker::CheckError>
    where
        Self: Sized,
    {
        file.check2()
    }

    type Cursor = Vec<u16>;

    fn matches<'a>(
        &self,
        cursor: &mut Self::Cursor,
        node: &<Self as NodeLending<'a>>::SNode,
    ) -> <Self as tree_sitter_graph::MatchesLending<'a>>::Matches {
        let matchs = self
            .query
            .matches::<_, <Self as NodeLending<'_>>::SNode>(node.clone());
        // let matchs = self.query.matches_immediate(node.clone());
        // TODO find a way to avoid transmuting
        let node = node.clone();
        let node = unsafe { std::mem::transmute(node) };
        let matchs = unsafe { std::mem::transmute(matchs) };
        let q = unsafe { std::mem::transmute(self) };
        let cursor = unsafe { std::mem::transmute(cursor) };
        MyQMatches {
            q,
            cursor,
            matchs,
            node,
        }
    }
}

impl<HAST> tree_sitter_graph::ExtendedableQuery
    for ExtendingStringQuery<QueryMatcher<HAST>, tree_sitter::Language>
where
    HAST: HyperAST,
    for<'t> LendT<'t, HAST>: WithSerialization + WithStats + WithRoles,
    HAST::IdN: Debug + Copy + Hash,
    HAST::Idx: Copy + Hash,
    HAST::TS: types::RoleStore,
    <HAST::TS as types::RoleStore>::IdF: From<u16> + Into<u16>,
    for<'t> LendT<'t, HAST>: WithPrecompQueries,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
{
    type Query = QueryMatcher<HAST>;
    type Lang = tree_sitter::Language;

    fn as_ref(&self) -> Option<&Self::Query> {
        self.query.as_ref()
    }

    fn with_capacity(capacity: usize) -> Self {
        let acc = String::with_capacity(capacity);
        Self {
            acc,
            ..Self::empty()
        }
    }

    fn make_query(
        &mut self,
        language: &Self::Lang,
        source: &str,
    ) -> Result<Self::Query, tree_sitter::QueryError> {
        if let Some(l) = &self.language {
            // this impl cannot accept different languages
            assert_eq!(language, l);
        }
        self.acc += source;
        self.acc += "\n";
        dbg!(source);
        // QueryMatcher::new(source, language)
        if let Some(precomputeds) = &self.precomputeds {
            QueryMatcher::with_precomputed(source, language, precomputeds.as_ref())
        } else {
            QueryMatcher::new(source, language)
        }
    }

    fn make_main_query(&self, language: &Self::Lang) -> Self::Query {
        if let Some(l) = &self.language {
            // this impl cannot accept different languages
            // Moreover, given the existance of a main query, having multiple languages should be impossible.
            assert_eq!(language, l);
        }
        // QueryMatcher::new(&self.acc, language).unwrap()
        if let Some(precomputeds) = &self.precomputeds {
            QueryMatcher::with_precomputed(&self.acc, language, precomputeds.as_ref()).unwrap()
        } else {
            QueryMatcher::new(&self.acc, language).unwrap()
        }
    }
}

impl<HAST: HyperAST> tree_sitter_graph::graph::SimpleNode for Node<'_, HAST>
where
    HAST::IdN: Copy + Hash + Debug,
    HAST::Idx: Copy + Hash,
    for<'t> LendT<'t, HAST>: WithSerialization + WithStats,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
{
    fn id(&self) -> usize {
        use std::hash::Hash;
        use std::hash::Hasher;
        let mut hasher = std::hash::DefaultHasher::new();
        self.0.pos.hash(&mut hasher);
        hasher.finish() as usize
    }
}

impl<HAST: HyperAST> tree_sitter_graph::graph::SyntaxNode for Node<'_, HAST>
where
    HAST::IdN: Copy + Hash + Debug,
    HAST::Idx: Copy + Hash,
    for<'t> LendT<'t, HAST>: WithSerialization + WithStats,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
{
    fn kind(&self) -> &'static str {
        use hyperast::position::position_accessors::SolvedPosition;
        let n = self.0.pos.node();
        let n = self.0.stores.resolve_type(&n);
        use hyperast::types::HyperType;
        n.as_static_str()
    }

    fn start_position(&self) -> tree_sitter::Point {
        let conv =
            hyperast::position::PositionConverter::new(&self.0.pos).with_stores(self.0.stores);
        let pos: hyperast::position::row_col::RowCol<usize> =
            conv.compute_pos_post_order::<_, hyperast::position::row_col::RowCol<usize>>();
        // use hyperast::position::computing_offset_bottom_up::extract_position_it;
        // let p = extract_position_it(self.stores, self.pos.iter());
        tree_sitter::Point {
            row: pos.row(), //p.range().start,
            column: pos.col(),
        }
    }

    fn end_position(&self) -> tree_sitter::Point {
        todo!()
    }

    fn byte_range(&self) -> std::ops::Range<usize> {
        todo!()
    }

    fn range(&self) -> tree_sitter::Range {
        let r = self.byte_range();
        tree_sitter::Range {
            start_byte: r.start,
            end_byte: r.end,
            start_point: self.start_position(),
            end_point: self.end_position(),
        }
    }

    fn text(&self) -> String {
        use hyperast::position::position_accessors::SolvedPosition;
        hyperast::nodes::TextSerializer::new(self.0.stores, self.0.pos.node()).to_string()
    }

    fn named_child_count(&self) -> usize {
        todo!()
    }
}

impl<'tree, HAST: HyperAST> tree_sitter_graph::graph::SyntaxNodeExt for Node<'tree, HAST>
where
    HAST::IdN: Copy + Hash + Debug,
    HAST::Idx: Copy + Hash,
    for<'t> LendT<'t, HAST>: WithSerialization + WithStats,
    HAST::IdN: Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> LendT<'t, HAST>: WithRoles + WithPrecompQueries,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
{
    type Cursor = Vec<Self>;

    fn walk(&self) -> Self::Cursor {
        todo!()
    }

    fn named_children<'cursor>(
        &self,
        _cursor: &'cursor mut Self::Cursor,
    ) -> impl ExactSizeIterator<Item = Self>
    where
        'tree: 'cursor,
    {
        todo!();
        [].iter().cloned()
    }

    fn parent(&self) -> Option<Self>
    where
        Self: Sized,
    {
        let mut r = self.clone();
        if r.0.goto_parent() { Some(r) } else { None }
    }
}

impl<HAST: HyperAST> QueryWithLang for MyQMatch<'_, '_, HAST> {
    type Lang = tree_sitter::Language;
    type I = u32;
}

impl<'a, HAST: HyperAST> NodeLending<'a> for CapturedNodesIter<'_, '_, '_, HAST>
where
    HAST::IdN: Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> LendT<'t, HAST>: WithRoles + WithPrecompQueries,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
    HAST::IdN: Copy + Hash + Debug,
    HAST::Idx: Copy + Hash,
    for<'t> LendT<'t, HAST>: WithSerialization + WithStats,
{
    type SNode = Node<'a, HAST>;
}

impl<HAST: HyperAST> NodeLender for CapturedNodesIter<'_, '_, '_, HAST>
where
    HAST::IdN: Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> LendT<'t, HAST>: WithRoles + WithPrecompQueries,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
    HAST::IdN: Copy + Hash + Debug,
    HAST::Idx: Copy + Hash,
    for<'t> LendT<'t, HAST>: WithSerialization + WithStats,
{
    fn next(&mut self) -> Option<<Self as NodeLending<'_>>::SNode> {
        loop {
            if self.inner.is_empty() {
                return None;
            }
            let capture = &self.inner[0];
            self.inner = &self.inner[1..];
            if capture.index.to_usize() != self.index as usize {
                continue;
            }
            let node = capture.node.clone();
            return Some(node);
        }
    }
}

impl<'cursor, 'tree, 'a, HAST: HyperAST> NodesLending<'a> for MyQMatch<'cursor, 'tree, HAST>
where
    HAST::IdN: Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> LendT<'t, HAST>: WithRoles + WithPrecompQueries,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
    HAST::IdN: Copy + Hash + Debug,
    HAST::Idx: Copy + Hash,
    for<'t> LendT<'t, HAST>: WithSerialization + WithStats,
{
    type Nodes = CapturedNodesIter<'a, 'a, 'a, HAST>;
}

impl<HAST: HyperAST> tree_sitter_graph::graph::QMatch for MyQMatch<'_, '_, HAST>
where
    HAST::IdN: Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> LendT<'t, HAST>: WithRoles + WithPrecompQueries,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
    HAST::IdN: Copy + Hash + Debug,
    HAST::Idx: Copy + Hash,
    for<'t> LendT<'t, HAST>: WithSerialization + WithStats,
{
    type Simple = NodeR<Pos<HAST>>;

    fn nodes_for_capture_index(&self, index: Self::I) -> <Self as NodesLending<'_>>::Nodes {
        dbg!();
        // self.qm
        //     .nodes_for_capture_index(CaptureId::new(index))
        //     .cloned()
        CapturedNodesIter {
            stores: self.stores,
            index,
            inner: self.qm.captures.captures(),
        }
    }

    fn nodes_for_capture_indexi(&self, index: Self::I) -> Option<NNN<'_, '_, Self>> {
        todo!()
        // CapturedNodesIter {
        //     stores: self.stores,
        //     index,
        //     inner: self.qm.captures.captures(),
        // }
        // .next()
    }

    fn nodes_for_capture_indexii(
        &self,
        index: Self::I,
    ) -> impl NodeLender + NodeLending<'_, SNode = NNN<'_, '_, Self>> {
        CapturedNodesIter::<HAST> {
            stores: self.stores,
            index,
            inner: self.qm.captures.captures(),
        }
    }

    fn pattern_index(&self) -> usize {
        self.i as usize
    }

    fn syn_node_ref(&self, node: &NNN<'_, '_, Self>) -> tree_sitter_graph::graph::SyntaxNodeRef {
        tree_sitter_graph::graph::SyntaxNodeRef::new(node)
    }

    fn node(&self, s: Self::Simple) -> NNN<'_, '_, Self> {
        Node::<'_, HAST>(crate::hyperast_cursor::Node {
            stores: self.stores,
            pos: s.pos,
        })
    }
}

impl<'query, 'cursor: 'query, 'tree: 'cursor, It, HAST: HyperAST> MatchLending<'_>
    for MyQMatches<'query, 'cursor, 'tree, It, HAST>
where
    It: Iterator<Item = crate::QueryMatch<Node<'tree, HAST>>>,
    HAST::IdN: Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> LendT<'t, HAST>: WithRoles + WithPrecompQueries,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
    HAST::IdN: Copy + Hash + Debug,
    HAST::Idx: Copy + Hash,
    for<'t> LendT<'t, HAST>: WithSerialization + WithStats,
{
    type Match = self::MyQMatch<'cursor, 'tree, HAST>;
}

impl<'query, 'cursor: 'query, 'tree: 'cursor, It, HAST: HyperAST> MatchLender
    for MyQMatches<'query, 'cursor, 'tree, It, HAST>
where
    It: Iterator<Item = crate::QueryMatch<Node<'tree, HAST>>>,
    HAST::IdN: Debug + Copy,
    HAST::TS: RoleStore,
    for<'t> LendT<'t, HAST>: WithRoles + WithPrecompQueries,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
    HAST::IdN: Copy + Hash + Debug,
    HAST::Idx: Copy + Hash,
    for<'t> LendT<'t, HAST>: WithSerialization + WithStats,
{
    fn next(&mut self) -> Option<<Self as MatchLending<'_>>::Match> {
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

impl<'query, 'cursor: 'query, 'tree: 'cursor, It, HAST: HyperAST> QueryWithLang
    for MyQMatches<'query, 'cursor, 'tree, It, HAST>
{
    type Lang = tree_sitter::Language;
    type I = u32;
}
