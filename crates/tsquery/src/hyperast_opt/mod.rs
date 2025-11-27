//! # Optimized cursor
//! nodes are only persisted when captured
use super::{Status, Symbol, TreeCursorStep};
use crate::StatusLending;
use hyperast::position::structural_pos;
use hyperast::types::{Childrn, HyperAST, LangRef, LendT, NodeStore as _, TypeStore};
use hyperast::types::{HyperASTShared, HyperType, LabelStore, Labeled, RoleStore, Tree};
use hyperast::types::{WithChildren, WithPrecompQueries, WithRoles};
use structural_pos::{CursorHead, CursorHeadMove, CursorWithPersistence, PersistedNode};

macro_rules! decomp_t {
    ($T:ty, $n:expr) => {
        <$T as TypeStore>::decompress_type($n, std::any::TypeId::of::<<$T as TypeStore>::Ty>())
    };
}

pub mod no_opt;

// pub mod hyperast_opt10;
pub mod opt3;
pub mod opt4;
pub mod opt5;
pub mod opt6_ancestors_subtree;
pub mod opt7_ancestors_hidden;
pub mod opt8_parent_error;
pub mod opt9_parent_types;
pub mod opt9_parent_types2;

#[doc(hidden)]
pub struct TreeCursor<
    'hast,
    HAST: HyperASTShared,
    P = opt9_parent_types::PersistCursor<'hast, HAST>,
> {
    pub stores: &'hast HAST,
    pub pos: P,
}

impl<'hast, HAST: HyperAST> TreeCursor<'hast, HAST> {
    pub fn new(stores: &'hast HAST, mut pos: CursorWithPersistence<HAST::IdN, HAST::Idx>) -> Self
    where
        HAST::IdN: Copy,
    {
        opt9_parent_types::tree_cursor(stores, pos)
    }
}

impl<HAST: HyperAST, P> crate::WithField for TreeCursor<'_, HAST, P>
where
    HAST::TS: RoleStore,
{
    type IdF = <HAST::TS as RoleStore>::IdF;
}

impl<'hast, HAST: HyperASTShared, P> TreeCursor<'hast, HAST, P> {
    pub fn bind(pos: P, stores: &'hast HAST) -> Self {
        Self { stores, pos }
    }
}

// pub struct TreeCursor<'hast, HAST: HyperASTShared> {
//     pub stores: &'hast HAST,
//     pub pos: CursorWithPersistence<HAST::IdN, HAST::Idx>,
//     pub p: PersistedNode<HAST::IdN, HAST::Idx>,
// }

pub struct Node<'hast, HAST: HyperASTShared> {
    pub stores: &'hast HAST,
    pub pos: PersistedNode<HAST::IdN, HAST::Idx>,
}

pub struct NodeRef<'a, 'hast, HAST: HyperASTShared> {
    pub stores: &'hast HAST,
    pub pos: structural_pos::RefNode<'a, HAST::IdN, HAST::Idx>,
}

pub struct NodeRefK<'a, 'hast, HAST: HyperAST> {
    pub stores: &'hast HAST,
    pub pos: structural_pos::RefNode<'a, HAST::IdN, HAST::Idx>,
    pub kind: <HAST::TS as TypeStore>::Ty,
}

struct ExtNodeRef<'a, 'hast, HAST: HyperASTShared> {
    pub stores: &'hast HAST,
    pub pos: structural_pos::ExtRefNode<'a, HAST::IdN, HAST::Idx>,
}

pub mod opt;

pub struct CursorStatus<IdF> {
    pub has_later_siblings: bool,
    pub has_later_named_siblings: bool,
    pub can_have_later_siblings_with_this_field: bool,
    pub field_id: IdF,
    pub supertypes: Vec<Symbol>,
}

mod impl_nodes;

mod cursor_utils;
mod cursor_utils2;
mod cursor_utils3;

use cursor_utils::SuperTypeIter;
use cursor_utils::text;
use cursor_utils::{goto_first_child_internal, goto_next_sibling_internal, goto_parent};
use cursor_utils::{has_child_with_field_id, role};
use cursor_utils::{kind, resolve};

pub fn symbol<HAST: HyperAST>(stores: &HAST, id: HAST::IdN) -> Symbol {
    let t = stores.resolve_type(&id);
    use hyperast::types::LangRef;
    let id = t.get_lang().ts_symbol(t);
    id.into()
}

pub fn is_visible<HAST: HyperAST>(stores: &HAST, k: <HAST::TS as TypeStore>::Ty) -> bool {
    !k.is_hidden()
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

pub(super) trait BitSet {
    fn max_size(&self) -> usize;
    fn set(&mut self, bit: usize, value: bool);
    fn bit(&self, bit: usize) -> bool;
}
impl BitSet for usize {
    fn max_size(&self) -> usize {
        std::mem::size_of::<usize>() * 8
    }
    fn set(&mut self, bit: usize, value: bool) {
        if value {
            *self |= 1 << bit;
        } else {
            *self &= !(1 << bit);
        }
    }
    fn bit(&self, bit: usize) -> bool {
        (*self & (1 << bit)) != 0
    }
}
impl BitSet for u128 {
    fn max_size(&self) -> usize {
        std::mem::size_of::<u128>() * 8
    }
    fn set(&mut self, bit: usize, value: bool) {
        if value {
            *self |= 1 << bit;
        } else {
            *self &= !(1 << bit);
        }
    }
    fn bit(&self, bit: usize) -> bool {
        (*self & (1 << bit)) != 0
    }
}

impl BitSet for Vec<u64> {
    fn max_size(&self) -> usize {
        usize::MAX
    }
    fn set(&mut self, bit: usize, value: bool) {
        while self.len() <= bit / 64 {
            self.push(0);
        }
        if value {
            self[bit / 64] |= 1 << (bit % 64);
        } else {
            self[bit / 64] &= !(1 << (bit % 64));
        }
    }
    fn bit(&self, bit: usize) -> bool {
        self.len() > bit / 64 && (self[bit / 64] & (1 << (bit % 64))) != 0
    }
}

///////// noref
///
///
mod impl_nodes_noref;

pub struct TreeCursorNoRef<HAST: HyperASTShared, P = opt9_parent_types2_noref::PersistCursor<HAST>>
{
    pub stores: HAST,
    pub pos: P,
}

impl<HAST: HyperAST> TreeCursorNoRef<HAST> {
    pub fn new(stores: HAST, mut pos: CursorWithPersistence<HAST::IdN, HAST::Idx>) -> Self
    where
        HAST::IdN: Copy,
    {
        opt9_parent_types2_noref::tree_cursor(stores, pos)
    }
}

impl<HAST: HyperASTShared, P> self::TreeCursorNoRef<HAST, P> {
    pub fn bind(pos: P, stores: HAST) -> Self {
        Self { stores, pos }
    }
}

pub struct NodeRefKNoRef<'a, HAST: HyperAST> {
    pub stores: HAST,
    pub pos: structural_pos::RefNode<'a, HAST::IdN, HAST::Idx>,
    pub kind: <HAST::TS as TypeStore>::Ty,
}

#[allow(type_alias_bounds)]
type Pos<HAST: HyperASTShared> =
    PersistedNode<<HAST as HyperASTShared>::IdN, <HAST as HyperASTShared>::Idx>;

pub struct NodeNoRef<HAST: HyperASTShared, P = Pos<HAST>> {
    pub stores: HAST,
    pub pos: P,
}

pub struct NodeRefNoRef<'a, HAST: HyperASTShared> {
    pub stores: HAST,
    pub pos: structural_pos::RefNode<'a, HAST::IdN, HAST::Idx>,
}

impl<HAST: HyperAST, P> crate::WithField for self::TreeCursorNoRef<HAST, P>
where
    HAST::TS: RoleStore,
{
    type IdF = <HAST::TS as RoleStore>::IdF;
}

mod opt9_parent_types2_noref;

#[cfg(feature = "tsg")]
pub mod tsg_impl {
    use std::fmt::Debug;
    use std::hash::Hash;

    use hyperast::tree_gen;
    use hyperast::types;
    use hyperast::types::WithSerialization;
    use hyperast::types::WithStats;

    use tree_sitter_graph::MatchLender;
    use tree_sitter_graph::MatchLending;
    use tree_sitter_graph::QueryWithLang;
    use tree_sitter_graph::graph;
    use tree_sitter_graph::graph::NodeLending;
    use tree_sitter_graph::graph::SimpleNode;
    use tree_sitter_graph::graph::SyntaxNode;
    use tree_sitter_graph::graph::SyntaxNodeExt;

    use crate::CaptureId;

    pub use super::NodeNoRef as Node;
    pub use super::TreeCursorNoRef as TreeCursor;
    use super::*;

    impl<'hast, HAST: HyperAST> SimpleNode for Node<HAST>
    where
        HAST::IdN: Copy,
        HAST::Idx: Copy + Hash,
    {
        fn id(&self) -> usize {
            use std::hash::Hash;
            use std::hash::Hasher;
            let mut hasher = std::hash::DefaultHasher::new();
            self.pos.ref_node().offsets().hash(&mut hasher);
            hasher.finish() as usize
        }
    }

    impl<'hast, HAST: HyperAST> SyntaxNode for Node<HAST>
    where
        HAST::IdN: Copy,
        HAST::Idx: Copy + Hash,
        for<'t> LendT<'t, HAST>: WithSerialization + WithStats,
        HAST::IdN: types::NodeId<IdN = HAST::IdN>,
    {
        fn kind(&self) -> &'static str {
            use hyperast::position::position_accessors::SolvedPosition;
            let n = self.pos.node();
            let n = self.stores.resolve_type(&n);
            use hyperast::types::HyperType;
            n.as_static_str()
        }

        fn start_position(&self) -> tree_sitter::Point {
            // let conv =
            //     hyperast::position::PositionConverter::new(&self.pos).with_stores(&self.stores);
            // let pos: hyperast::position::row_col::RowCol<usize> =
            //     conv.compute_pos_post_order::<_, hyperast::position::row_col::RowCol<usize>>();
            // // use hyperast::position::computing_offset_bottom_up::extract_position_it;
            // // let p = extract_position_it(self.stores, self.pos.iter());
            // tree_sitter::Point {
            //     row: pos.row(), //p.range().start,
            //     column: pos.col(),
            // }
            tree_sitter::Point {
                row: 0, //p.range().start,
                column: 0,
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
            hyperast::nodes::TextSerializer::new(&self.stores, self.pos.node()).to_string()
        }

        fn named_child_count(&self) -> usize {
            todo!()
        }
    }

    impl<HAST: HyperAST + Copy> SyntaxNodeExt for Node<HAST>
    where
        HAST::IdN: Copy,
        HAST::Idx: Copy + Hash,
        for<'t> LendT<'t, HAST>: WithSerialization + WithStats,
        HAST::IdN: types::NodeId<IdN = HAST::IdN>,
    {
        type Cursor = TreeCursor<HAST>;

        fn walk(&self) -> Self::Cursor {
            todo!()
        }

        fn named_children<'cursor>(
            &self,
            cursor: &'cursor mut Self::Cursor,
        ) -> impl ExactSizeIterator<Item = Self>
        where
            Self: 'cursor,
        {
            vec![todo!()].into_iter()
        }

        fn parent(&self) -> Option<Self>
        where
            Self: Sized,
        {
            todo!()
            // self.pos.parent();
            // self.pos.parent().map(|pos| Self {
            //     stores: self.stores,
            //     pos,
            // })
        }
    }

    pub struct QueryMatcher<HAST, P = Pos<HAST>> {
        pub query: crate::Query,
        pub _phantom: std::marker::PhantomData<(HAST, P)>,
    }
    impl<HAST, P> tree_sitter_graph::QueryWithLang for QueryMatcher<HAST, P> {
        type Lang = tree_sitter::Language;
        type I = CaptureId;
    }

    impl<'a, HAST> graph::NodeLending<'a> for QueryMatcher<HAST>
    where
        HAST: HyperAST + Copy,
        HAST::IdN: Copy,
        HAST::Idx: Copy + Hash,
        for<'t> LendT<'t, HAST>: WithSerialization + WithStats,
        HAST::IdN: types::NodeId<IdN = HAST::IdN>,
    {
        type SNode = Node<HAST>;
    }

    impl<'a, HAST> tree_sitter_graph::MatchesLending<'a> for QueryMatcher<HAST>
    where
        HAST: HyperAST + Copy,
        HAST::IdN: Copy,
        HAST::Idx: Copy + Hash,
        for<'t> LendT<'t, HAST>: WithSerialization + WithStats,
        HAST::IdN: types::NodeId<IdN = HAST::IdN>,
    {
        type Matches =
            MyQMatches<'a, crate::QueryCursor<'a, TreeCursorNoRef<HAST>, NodeNoRef<HAST>>, HAST>;
    }

    impl<HAST> tree_sitter_graph::GenQuery for QueryMatcher<HAST>
    where
        HAST: HyperAST + Copy,
        HAST::IdN: Debug + Copy + Hash,
        HAST::Idx: Copy + Hash,
        HAST::TS: RoleStore,
        for<'t> LendT<'t, HAST>: WithSerialization + WithStats,
        HAST::IdN: types::NodeId<IdN = HAST::IdN>,
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

        type Cursor = (); //TreeCursor<'hast, HAST>;

        fn matches<'a>(
            &self,
            cursor: &mut Self::Cursor,
            node: &<Self as graph::NodeLending<'a>>::SNode,
        ) -> <Self as tree_sitter_graph::MatchesLending<'a>>::Matches {
            todo!()
            // let matchs = self
            //     .query
            //     .matches::<_, <Self as NodeLending<'_>>::SNode>(node.clone());
            // // let matchs = self.query.matches_immediate(node.clone());
            // // TODO find a way to avoid transmuting
            // let node = node.clone();
            // let node = unsafe { std::mem::transmute(node) };
            // let matchs = unsafe { std::mem::transmute(matchs) };
            // let q = unsafe { std::mem::transmute(self) };
            // let cursor = unsafe { std::mem::transmute(cursor) };
            // MyQMatches {
            //     q,
            //     cursor,
            //     matchs,
            //     node,
            // }
        }
    }

    pub struct MyQMatches<'query, It, HAST: HyperASTShared, P = Pos<HAST>> {
        pub q: &'query QueryMatcher<HAST, P>,
        pub matchs: It,
        // pub node: Node<HAST, Pos<HAST>>,
    }

    impl<'a, 'query, It, HAST: HyperAST> QueryWithLang for MyQMatches<'query, It, HAST> {
        type Lang = tree_sitter::Language;
        type I = CaptureId;
    }

    impl<'a, 'query, It, HAST: HyperAST + Copy> MatchLending<'a> for MyQMatches<'query, It, HAST>
    where
        HAST::IdN: Copy,
        HAST::Idx: Copy + Hash,
        for<'t> LendT<'t, HAST>: WithSerialization + WithStats,
        HAST::IdN: types::NodeId<IdN = HAST::IdN>,
    {
        type Match = MyQMatch<HAST>;
    }

    impl<'query, It, HAST: HyperAST + Copy> MatchLender for MyQMatches<'query, It, HAST>
    where
        HAST::IdN: Copy,
        HAST::Idx: Copy + Hash,
        for<'t> LendT<'t, HAST>: WithSerialization + WithStats,
        HAST::IdN: types::NodeId<IdN = HAST::IdN>,
    {
        fn next(&mut self) -> Option<<Self as MatchLending<'_>>::Match> {
            todo!()
        }
    }

    pub struct MyQMatch<HAST: HyperAST, P = Pos<HAST>> {
        pub stores: HAST,
        pub qm: crate::QueryMatch<Node<HAST, P>>,
        pub i: CaptureId,
    }

    impl<'a, HAST: HyperAST, P> QueryWithLang for MyQMatch<HAST, P> {
        type Lang = tree_sitter::Language;
        type I = CaptureId;
    }

    impl<'a, HAST: HyperAST + Copy> graph::NodesLending<'a> for MyQMatch<HAST>
    where
        HAST::IdN: Copy,
        HAST::Idx: Copy + Hash,
        for<'t> LendT<'t, HAST>: WithSerialization + WithStats,
        HAST::IdN: types::NodeId<IdN = HAST::IdN>,
    {
        type Nodes = CapturedNodesIter<'a, HAST>;
    }

    impl<HAST: HyperAST + Copy> graph::QMatch for MyQMatch<HAST>
    where
        HAST::IdN: Copy,
        HAST::Idx: Copy + Hash,
        for<'t> LendT<'t, HAST>: WithSerialization + WithStats,
        HAST::IdN: types::NodeId<IdN = HAST::IdN>,
    {
        type Simple = Node<HAST>;

        fn nodes_for_capture_indexi(&self, index: Self::I) -> Option<graph::NNN<'_, '_, Self>> {
            use tree_sitter_graph::graph::NodeLender;
            CapturedNodesIter {
                stores: self.stores,
                index,
                inner: self.qm.captures.captures(),
            }
            .next()
        }

        fn nodes_for_capture_indexii(
            &self,
            index: Self::I,
        ) -> impl graph::NodeLender + NodeLending<'_, SNode = graph::NNN<'_, '_, Self>> {
            CapturedNodesIter {
                stores: self.stores,
                index,
                inner: self.qm.captures.captures(),
            }
        }

        fn nodes_for_capture_index(
            &self,
            index: Self::I,
        ) -> <Self as graph::NodesLending<'_>>::Nodes {
            CapturedNodesIter {
                stores: self.stores,
                index,
                inner: self.qm.captures.captures(),
            }
        }

        fn pattern_index(&self) -> usize {
            self.i.to_usize()
        }

        fn syn_node_ref(&self, node: &graph::NNN<'_, '_, Self>) -> graph::SyntaxNodeRef {
            tree_sitter_graph::graph::SyntaxNodeRef::new(node)
        }

        fn node(&self, s: Self::Simple) -> graph::NNN<'_, '_, Self> {
            s
        }
    }

    pub struct CapturedNodesIter<
        'x,
        HAST: HyperASTShared,
        P = PersistedNode<<HAST as HyperASTShared>::IdN, <HAST as HyperASTShared>::Idx>,
    > {
        stores: HAST,
        index: CaptureId,
        inner: &'x [crate::Capture<Node<HAST, P>>],
    }

    impl<'a, HAST: HyperAST + Copy> NodeLending<'a> for CapturedNodesIter<'_, HAST>
    where
        HAST::IdN: Copy,
        HAST::Idx: Copy + Hash,
        for<'t> LendT<'t, HAST>: WithSerialization + WithStats,
        HAST::IdN: types::NodeId<IdN = HAST::IdN>,
    {
        type SNode = Node<HAST>;
    }

    impl<HAST: HyperAST + Copy> graph::NodeLender for CapturedNodesIter<'_, HAST>
    where
        HAST::IdN: Copy,
        HAST::Idx: Copy + Hash,
        for<'t> LendT<'t, HAST>: WithSerialization + WithStats,
        HAST::IdN: types::NodeId<IdN = HAST::IdN>,
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
    type IdF = u16;

    impl<HAST> tree_sitter_graph::ExtendedableQuery
        for ExtendingStringQuery<QueryMatcher<HAST>, tree_sitter::Language>
    where
        HAST: HyperAST + Copy,
        HAST::IdN: Copy + Hash + Debug,
        HAST::Idx: Copy + Hash,
        HAST::TS: RoleStore,
        for<'t> LendT<'t, HAST>: WithSerialization + WithStats,
        HAST::IdN: types::NodeId<IdN = HAST::IdN>,
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
            let query = crate::Query::new(source, language.clone())?;
            Ok(Self::Query {
                query,
                _phantom: Default::default(),
            })
            // QueryMatcher::new(source, language)
            // TODO
            // let precomputeds = self.precomputeds.as_deref().unwrap();
            // // QueryMatcher::with_precomputed(source, language, precomputeds)
        }

        fn make_main_query(&self, language: &Self::Lang) -> Self::Query {
            if let Some(l) = &self.language {
                // this impl cannot accept different languages
                // Moreover, given the existance of a main query, having multiple languages should be impossible.
                assert_eq!(language, l);
            }
            let query = crate::Query::new(&self.acc, language.clone()).unwrap();
            Self::Query {
                query,
                _phantom: Default::default(),
            }
            // QueryMatcher::new(&self.acc, language).unwrap()
            // let precomputeds = self.precomputeds.as_deref().unwrap();
            // todo!()
            // QueryMatcher::with_precomputed(&self.acc, language, precomputeds).unwrap()
        }
    }
}
