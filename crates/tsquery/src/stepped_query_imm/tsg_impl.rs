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

use tree_sitter_graph::{
    MatchLender, MatchLending, MatchesLending, QueryWithLang,
    graph::{self, NNN, NodeLender, NodeLending, NodesLending},
};

use super::IdF;
use super::Pos;
use super::{CapturedNodesIter, ExtendingStringQuery, Node, QueryMatcher};
use super::{MyNodeErazing, MyQMatch, MyQMatches};

impl<P: Clone + Hash> graph::SimpleNode for NodeR<P> {
    fn id(&self) -> usize {
        use std::hash::Hasher;
        let mut hasher = std::hash::DefaultHasher::new();
        self.pos.hash(&mut hasher);
        hasher.finish() as usize
    }
}

impl<'acc, HAST: HyperASTShared, Acc: 'static> graph::Erzd for MyNodeErazing<HAST, &'acc Acc>
where
    &'acc Acc: WithLabel,
{
    type Original<'tree> = Node<HAST, &'acc Acc>;
}

impl<'acc, HAST: HyperASTShared, Acc: 'static> graph::LErazng for Node<HAST, &'acc Acc>
where
    &'acc Acc: WithLabel,
{
    type LErazing = MyNodeErazing<HAST, &'acc Acc>;
}

impl<TS, Acc> tree_sitter_graph::QueryWithLang for QueryMatcher<TS, &Acc> {
    type Lang = tree_sitter::Language;
    type I = CaptureId;
}

impl<'acc, HAST, Acc> NodeLending<'_> for QueryMatcher<HAST, &'acc Acc>
where
    HAST: types::HyperAST + Copy,
    HAST::IdN: Copy + Hash + Debug,
    HAST::Idx: Copy + Hash,
    for<'t> LendT<'t, HAST>: WithSerialization + WithStats + WithRoles,
    HAST::TS: ETypeStore<Ty2 = Acc::Type> + types::RoleStore<IdF = IdF, Role = Role>,
    Acc: tree_gen::WithRole<Role>,
    Acc: tree_gen::WithChildren<HAST::IdN>,
    Acc: types::Typed,
    &'acc Acc: WithLabel,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
{
    type SNode = Node<HAST, &'acc Acc>;
}

impl<'a, 'acc, HAST, Acc> MatchesLending<'a> for QueryMatcher<HAST, &'acc Acc>
where
    HAST: types::HyperAST + Copy,
    HAST::IdN: Copy + Hash + Debug,
    HAST::Idx: Copy + Hash,
    for<'t> LendT<'t, HAST>: WithSerialization + WithStats + WithRoles,
    HAST::TS: ETypeStore<Ty2 = Acc::Type> + types::RoleStore<IdF = IdF, Role = Role>,
    Acc: tree_gen::WithRole<Role>,
    Acc: tree_gen::WithChildren<HAST::IdN>,
    Acc: types::Typed,
    &'acc Acc: WithLabel,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
{
    type Matches = MyQMatches<
        'a,
        'a,
        crate::QueryCursor<'a, <Self as NodeLending<'a>>::SNode, <Self as NodeLending<'a>>::SNode>,
        HAST,
        &'acc Acc,
    >;
}

impl<'acc, HAST, Acc> tree_sitter_graph::GenQuery for QueryMatcher<HAST, &'acc Acc>
where
    HAST: types::HyperAST + Copy,
    HAST::IdN: Copy + Hash + Debug,
    HAST::Idx: Copy + Hash,
    for<'t> LendT<'t, HAST>: WithSerialization + WithStats + WithRoles,
    HAST::TS: ETypeStore<Ty2 = Acc::Type> + types::RoleStore<IdF = IdF, Role = Role>,
    Acc: tree_gen::WithRole<Role>,
    Acc: tree_gen::WithChildren<HAST::IdN>,
    Acc: types::Typed,
    &'acc Acc: WithLabel,
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
        let matchs = unsafe {
            std::mem::transmute::<crate::QueryCursor<'_, _, _>, crate::QueryCursor<'_, _, _>>(
                matchs,
            )
        };
        let q = unsafe { std::mem::transmute::<&_, &_>(self) };
        let cursor = unsafe { std::mem::transmute::<&mut _, &mut _>(cursor) };
        MyQMatches {
            q,
            cursor,
            matchs,
            node,
        }
    }

    fn from_str(
        language: Self::Lang,
        source: &str,
    ) -> Result<tree_sitter_graph::ast::File<Self>, tree_sitter_graph::ParseError>
    where
        Self: Sized,
    {
        let mut file = tree_sitter_graph::ast::File::<Self>::new(language);
        tree_sitter_graph::parser::Parser::<Self::Ext>::new(source).parse_into_file(&mut file)?;
        Self::check(&mut file)?;
        Ok(file)
    }
}

impl<'acc, HAST, Acc> tree_sitter_graph::ExtendedableQuery
    for ExtendingStringQuery<QueryMatcher<HAST, &'acc Acc>, tree_sitter::Language>
where
    HAST: HyperAST + Copy,
    HAST::IdN: Copy + Hash + Debug,
    HAST::Idx: Copy + Hash,
    for<'t> LendT<'t, HAST>: WithSerialization + WithStats + WithRoles,
    HAST::TS: ETypeStore<Ty2 = Acc::Type> + types::RoleStore<IdF = IdF, Role = Role>,
    Acc: tree_gen::WithChildren<HAST::IdN> + tree_gen::WithRole<Role> + types::Typed,
    &'acc Acc: WithLabel,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
{
    type Query = QueryMatcher<HAST, &'acc Acc>;
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
        let precomputeds = self.precomputeds.as_deref().unwrap();
        QueryMatcher::with_precomputed(source, language, precomputeds)
    }

    fn make_main_query(&self, language: &Self::Lang) -> Self::Query {
        if let Some(l) = &self.language {
            // this impl cannot accept different languages
            // Moreover, given the existance of a main query, having multiple languages should be impossible.
            assert_eq!(language, l);
        }
        // QueryMatcher::new(&self.acc, language).unwrap()
        let precomputeds = self.precomputeds.as_deref().unwrap();
        QueryMatcher::with_precomputed(&self.acc, language, precomputeds).unwrap()
    }
}

impl<'acc, HAST, Acc> graph::SimpleNode for Node<HAST, &'acc Acc>
where
    HAST: HyperAST + Copy,
    HAST::IdN: Hash + Copy + Debug,
    HAST::Idx: Hash,
    for<'t> LendT<'t, HAST>: WithSerialization + types::WithChildren + WithStats + WithRoles,
    HAST::TS: ETypeStore<Ty2 = Acc::Type> + types::RoleStore<IdF = IdF, Role = Role>,
    Acc: tree_gen::WithChildren<HAST::IdN> + tree_gen::WithRole<Role> + types::Typed,
    &'acc Acc: WithLabel,
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

impl<'acc, HAST, Acc> graph::SyntaxNode for Node<HAST, &'acc Acc>
where
    HAST: HyperAST + Copy,
    HAST::IdN: Hash + Copy + Debug,
    HAST::Idx: Hash,
    for<'t> LendT<'t, HAST>: WithSerialization + types::WithChildren + WithStats + WithRoles,
    HAST::TS: ETypeStore<Ty2 = Acc::Type> + types::RoleStore<IdF = IdF, Role = Role>,
    Acc: tree_gen::WithChildren<HAST::IdN> + tree_gen::WithRole<Role> + types::Typed,
    &'acc Acc: WithLabel,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
{
    fn kind(&self) -> &'static str {
        use types::HyperType;
        self.0.kind().as_static_str()
        // use hyperast::position::position_accessors::SolvedPosition;
        // let n = self.0.pos.node();
        // let n = self.0.stores.node_store.resolve(&n);
        // // TS::
        // let n = self.0.stores.type_store.resolve_type(&n);
        // n.as_static_str()
    }

    fn start_position(&self) -> tree_sitter::Point {
        // TODO compute the position
        // let conv =
        //     hyperast::position::PositionConverter::new(&self.0.pos).with_stores(&self.0.stores);

        // let conv: &hyperast::position::WithHyperAstPositionConverter<
        //     hyperast::position::StructuralPosition<_, _>,
        //     HAST,
        // > = unsafe { std::mem::transmute(&conv) };
        // let pos: hyperast::position::row_col::RowCol<usize> =
        //     conv.compute_pos_post_order::<_, hyperast::position::row_col::RowCol<usize>, HAST::IdN>();
        // // use hyperast::position::computing_offset_bottom_up::extract_position_it;
        // // let p = extract_position_it(self.stores, self.pos.iter());
        // tree_sitter::Point {
        //     row: pos.row() as usize, //p.range().start,
        //     column: pos.col() as usize,
        // }
        tree_sitter::Point { row: 0, column: 0 }
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
        use hyperast::position::TreePath;
        let stores: &HAST = &self.0.stores;
        if let Some(root) = self.0.pos.node() {
            hyperast::nodes::TextSerializer::new(stores, *root).to_string()
        } else {
            // log::error!("{}", self.kind());
            // use crate::Node;
            // self.0.text(())
            (self.0.label.as_ref())
                .map_or("aaa", |x| x.as_ref())
                .to_string()
        }
    }

    fn named_child_count(&self) -> usize {
        todo!()
    }
}

impl<'acc, HAST, Acc> graph::SyntaxNodeExt for Node<HAST, &'acc Acc>
where
    HAST: HyperAST + Copy,
    HAST::IdN: Copy + Hash + Debug,
    HAST::Idx: Copy + Hash,
    for<'t> LendT<'t, HAST>: WithSerialization + WithStats + WithRoles,
    HAST::TS: ETypeStore<Ty2 = Acc::Type> + types::RoleStore<IdF = IdF, Role = Role>,
    Acc: tree_gen::WithRole<Role>,
    Acc: tree_gen::WithChildren<HAST::IdN>,
    Acc: types::Typed,
    &'acc Acc: WithLabel,
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
        Self: 'cursor,
    {
        #[allow(unreachable_code)]
        vec![todo!()].into_iter()
    }

    fn parent(&self) -> Option<Self>
    where
        Self: Sized,
    {
        let mut r = self.clone();
        if r.0.pos.pop().is_some() {
            Some(r)
        } else {
            None
        }
    }
}

impl<HAST: HyperASTShared, Acc: WithLabel> QueryWithLang for MyQMatch<'_, HAST, Acc> {
    type Lang = tree_sitter::Language;
    type I = CaptureId;
}

impl<'acc, HAST: HyperAST, Acc> NodeLending<'_> for CapturedNodesIter<'_, HAST, &'acc Acc>
where
    HAST: HyperAST + Copy,
    HAST::IdN: Copy + Hash + Debug,
    HAST::Idx: Copy + Hash,
    for<'t> LendT<'t, HAST>: WithSerialization + WithStats + WithRoles,
    HAST::TS: ETypeStore<Ty2 = Acc::Type> + types::RoleStore<IdF = IdF, Role = Role>,
    Acc: tree_gen::WithRole<Role>,
    Acc: tree_gen::WithChildren<HAST::IdN>,
    Acc: types::Typed,
    &'acc Acc: WithLabel,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
{
    type SNode = Node<HAST, &'acc Acc>;
}

impl<'acc, HAST: HyperAST, Acc> NodeLender for CapturedNodesIter<'_, HAST, &'acc Acc>
where
    HAST: HyperAST + Copy,
    HAST::IdN: Copy + Hash + Debug,
    HAST::Idx: Copy + Hash,
    for<'t> LendT<'t, HAST>: WithSerialization + WithStats + WithRoles,
    HAST::TS: ETypeStore<Ty2 = Acc::Type> + types::RoleStore<IdF = IdF, Role = Role>,
    Acc: tree_gen::WithRole<Role>,
    Acc: tree_gen::WithChildren<HAST::IdN>,
    Acc: types::Typed,
    &'acc Acc: WithLabel,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
{
    fn next(&mut self) -> Option<<Self as NodeLending<'_>>::SNode> {
        loop {
            if self.inner.is_empty() {
                return None;
            }
            let capture = &self.inner[0];
            self.inner = &self.inner[1..];
            if capture.index != self.index {
                continue;
            }
            let node = capture.node.clone();
            return Some(node);
        }
    }
}

impl<'a, 'acc, HAST, Acc> NodesLending<'a> for MyQMatch<'_, HAST, &'acc Acc>
where
    HAST: HyperAST + Copy,
    HAST::TS: ETypeStore<Ty2 = Acc::Type> + types::RoleStore<IdF = IdF, Role = Role>,
    HAST::IdN: Copy + Hash + Debug,
    HAST::Idx: Copy + Hash,
    for<'t> LendT<'t, HAST>: WithSerialization + WithStats + WithRoles,
    HAST::TS: ETypeStore<Ty2 = Acc::Type> + types::RoleStore<IdF = IdF, Role = Role>,
    Acc: tree_gen::WithRole<Role>,
    Acc: tree_gen::WithChildren<HAST::IdN>,
    Acc: types::Typed,
    &'acc Acc: WithLabel,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
{
    type Nodes = CapturedNodesIter<'a, HAST, &'acc Acc>;
}

impl<'acc, HAST, Acc> graph::QMatch for MyQMatch<'_, HAST, &'acc Acc>
where
    HAST: HyperAST + Copy,
    HAST::TS: ETypeStore<Ty2 = Acc::Type> + types::RoleStore<IdF = IdF, Role = Role>,
    Acc: tree_gen::WithChildren<HAST::IdN> + tree_gen::WithRole<Role> + types::Typed,
    &'acc Acc: WithLabel,
    // syn_node_ref
    HAST::IdN: Copy + Hash + Debug,
    HAST::Idx: Copy + Hash,
    for<'t> LendT<'t, HAST>: WithSerialization + WithStats + WithRoles,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
{
    type Simple = NodeR<Pos<HAST>>;

    fn nodes_for_capture_index(&self, index: Self::I) -> <Self as NodesLending<'_>>::Nodes {
        CapturedNodesIter::<HAST, &'acc Acc> {
            stores: self.stores,
            index,
            inner: self.qm.captures.captures(),
        }
    }

    fn nodes_for_capture_indexi(&self, index: Self::I) -> Option<NNN<'_, '_, Self>> {
        CapturedNodesIter::<HAST, &'acc Acc> {
            stores: self.stores,
            index,
            inner: self.qm.captures.captures(),
        }
        .next()
    }

    fn nodes_for_capture_indexii(
        &self,
        index: Self::I,
    ) -> impl graph::NodeLender + graph::NodeLending<'_, SNode = NNN<'_, '_, Self>> {
        CapturedNodesIter::<HAST, &'acc Acc> {
            stores: self.stores,
            index,
            inner: self.qm.captures.captures(),
        }
    }

    fn pattern_index(&self) -> usize {
        self.i as usize
    }

    fn syn_node_ref(&self, node: &NNN<'_, '_, Self>) -> graph::SyntaxNodeRef {
        todo!()
    }

    fn node(&self, s: Self::Simple) -> NNN<'_, '_, Self> {
        todo!()
    }
}

impl<'cursor, 'acc, It, HAST, Acc> MatchLending<'_> for MyQMatches<'_, 'cursor, It, HAST, &'acc Acc>
where
    HAST: HyperAST + Copy,
    It: Iterator<Item = crate::QueryMatch<Node<HAST, &'acc Acc>>>,
    &'acc Acc: WithLabel,
    HAST: HyperAST + Copy,
    HAST::IdN: Copy + Hash + Debug,
    HAST::Idx: Copy + Hash,
    for<'t> LendT<'t, HAST>: WithSerialization + WithStats + WithRoles,
    HAST::TS: ETypeStore<Ty2 = Acc::Type> + types::RoleStore<IdF = IdF, Role = Role>,
    Acc: tree_gen::WithRole<Role>,
    Acc: tree_gen::WithChildren<HAST::IdN>,
    Acc: types::Typed,
    &'acc Acc: WithLabel,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
{
    type Match = super::MyQMatch<'cursor, HAST, &'acc Acc>;
}

impl<'acc, It, HAST, Acc> MatchLender for MyQMatches<'_, '_, It, HAST, &'acc Acc>
where
    HAST: HyperAST + Copy,
    It: Iterator<Item = crate::QueryMatch<Node<HAST, &'acc Acc>>>,
    &'acc Acc: WithLabel,
    HAST: HyperAST + Copy,
    HAST::IdN: Copy + Hash + Debug,
    HAST::Idx: Copy + Hash,
    for<'t> LendT<'t, HAST>: WithSerialization + WithStats + WithRoles,
    HAST::TS: ETypeStore<Ty2 = Acc::Type> + types::RoleStore<IdF = IdF, Role = Role>,
    Acc: tree_gen::WithRole<Role>,
    Acc: tree_gen::WithChildren<HAST::IdN>,
    Acc: types::Typed,
    &'acc Acc: WithLabel,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
{
    fn next(&mut self) -> Option<<Self as MatchLending<'_>>::Match> {
        let qm = self.matchs.next()?;
        let stores = self.node.0.stores;
        let i = (self.q.query)
            .enabled_pattern_index(qm.pattern_index)
            .unwrap();
        Some(super::MyQMatch {
            stores,
            b: &(),
            qm,
            i,
        })
    }
}

impl<'acc, It, HAST, Acc> QueryWithLang for super::MyQMatches<'_, '_, It, HAST, &'acc Acc>
where
    HAST: HyperAST + Copy,
    It: Iterator<Item = crate::QueryMatch<super::Node<HAST, &'acc Acc>>>,
    &'acc Acc: WithLabel,
{
    type Lang = tree_sitter::Language;
    type I = CaptureId;
}
