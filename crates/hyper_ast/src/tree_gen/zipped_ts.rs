//! fully compress all subtrees
#![allow(unused)]
use num::ToPrimitive as _;
use std::fmt::Debug;
use std::marker::PhantomData;

use crate::compat::HashMap;
use crate::full::FullNode;
use crate::hashed::{IndexingHashBuilder, MetaDataHashsBuilder, SyntaxNodeHashs};
use crate::nodes::Space;
use crate::store::SimpleStores;
use crate::store::nodes::DefaultNodeStore as NodeStore;
use crate::store::nodes::compo;
use crate::store::nodes::legion::RawHAST;
use crate::store::nodes::legion::eq_node;
use crate::store::nodes::legion::{DedupMap, NodeIdentifier};
use crate::store::nodes::legion::{dyn_builder, subtree_builder};
use crate::tree_gen::handle_file_bounds;
use crate::tree_gen::{TsEnableTS, TsType};
use crate::types::LabelStore as _;
use crate::types::Role;
use crate::types::{ETypeStore, HyperType};

use super::TreeGen;
use super::parser::Node as _;
use super::parser::TreeCursor as _;
use super::utils_ts::TNode;
use super::utils_ts::TTreeCursor;
use super::utils_ts::make_leaf;
use super::{Accumulator, WithByteRange};
use super::{BasicAccumulator, SubTreeMetrics};
use super::{BasicGlobalData, GlobalData, Spaces, TotalBytesGlobalData as _};
use super::{Parents, PreResult};
use super::{RoleAcc, add_md_precomp_queries};
use super::{SpacedGlobalData, TextedGlobalData};
use super::{ZippedTreeGen, get_spacing};

pub type LabelIdentifier = crate::store::labels::DefaultLabelIdentifier;

pub struct TsTreeGen<'store, 'cache, TS, More = (), const HIDDEN_NODES: bool = false> {
    pub line_break: Vec<u8>,
    pub stores: &'store mut SimpleStores<TS>,
    pub dedup: Option<&'store mut DedupMap>,
    pub md_cache: &'cache mut MDCache,
    pub more: More,
}

pub type MDCache = HashMap<NodeIdentifier, DD>;

// NOTE only keep compute intensive metadata (where space/time tradeoff is worth storing)
// eg. decls refs, maybe hashes but not size and height
// * derived data: computation results from concrete code of node and its children
// they can be qualitative data .eg a hash or they can be quantitative .eg lines of code
pub struct DD {
    metrics: SubTreeMetrics<SyntaxNodeHashs<u32>>,
    precomp_queries: PrecompQueries,
}

impl<T> From<Local<T>> for DD {
    fn from(x: Local<T>) -> Self {
        DD {
            metrics: x.metrics,
            precomp_queries: x.precomp_queries,
        }
    }
}

pub type Global<'a> = SpacedGlobalData<'a>;

type PrecompQueries = u16;

#[derive(Debug, Clone)]
pub struct Local<T> {
    pub compressed_node: NodeIdentifier,

    // # debug
    pub _ty: T,

    // # directly bubbling derived data
    pub metrics: SubTreeMetrics<SyntaxNodeHashs<u32>>,
    pub role: Option<Role>,
    pub precomp_queries: PrecompQueries,
    // # by providing store could also fetch the ones not there
}

impl<T> Local<T> {
    fn acc(self, acc: &mut Acc<T>) {
        if self.metrics.size_no_spaces > 0 {
            acc.no_space.push(self.compressed_node)
        }
        if let Some(role) = self.role {
            let o = acc.simple.children.len();
            acc.role.acc(role, o);
        }
        acc.simple.push(self.compressed_node);
        acc.metrics.acc(self.metrics);
        acc.precomp_queries |= self.precomp_queries;
    }
}

pub struct Acc<T> {
    simple: BasicAccumulator<T, NodeIdentifier>,
    no_space: Vec<NodeIdentifier>,
    labeled: bool,
    start_byte: usize,
    end_byte: usize,
    metrics: SubTreeMetrics<SyntaxNodeHashs<u32>>,
    padding_start: usize,
    role: RoleAcc<Role>,

    // # non ts specific
    precomp_queries: PrecompQueries,
}

pub type FNode<T> = FullNode<BasicGlobalData, Local<T>>;
impl<T> Accumulator for Acc<T> {
    type Node = FNode<T>;
    fn push(&mut self, full_node: Self::Node) {
        full_node.local.acc(self);
    }
}

impl<T> WithByteRange for Acc<T> {
    fn has_children(&self) -> bool {
        !self.simple.children.is_empty()
    }

    fn begin_byte(&self) -> usize {
        self.start_byte
    }

    fn end_byte(&self) -> usize {
        self.end_byte
    }
}

impl<T: HyperType + Eq + Copy + Send + Sync> crate::types::Typed for Acc<T> {
    type Type = T;

    fn get_type(&self) -> Self::Type {
        self.simple.kind
    }
}

impl<T> super::WithChildren<NodeIdentifier> for Acc<T> {
    fn children(&self) -> &[NodeIdentifier] {
        &self.simple.children
    }
}

impl<T> super::WithRole<Role> for Acc<T> {
    fn role_at(&self, o: usize) -> Option<Role> {
        self.role
            .offsets
            .iter()
            .position(|x| *x as usize == o)
            .and_then(|x| self.role.roles.get(x))
            .cloned()
    }
}

impl<T: Debug> Debug for Acc<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Acc")
            .field("simple", &self.simple)
            .field("no_space", &self.no_space)
            .field("labeled", &self.labeled)
            .field("start_byte", &self.start_byte)
            .field("end_byte", &self.end_byte)
            .field("metrics", &self.metrics)
            .field("padding_start", &self.padding_start)
            .finish()
    }
}

impl<'acc, T> super::WithLabel for &'acc Acc<T> {
    type L = &'acc str;
}

impl<'store, 'cache, TS: TsEnableTS>
    TsTreeGen<'store, 'cache, TS, super::NoOpMore<TS, Acc<TS::Ty2>>, true>
where
    TS::Ty2: TsType,
{
    pub fn new(stores: &'store mut SimpleStores<TS>, md_cache: &'cache mut MDCache) -> Self {
        Self::with_preprocessing(stores, md_cache, Default::default())
    }
}

impl<'stores, 'cache, TS, More> TsTreeGen<'stores, 'cache, TS, More, true> {
    pub fn with_preprocessing(
        stores: &'stores mut SimpleStores<TS>,
        md_cache: &'cache mut MDCache,
        more: More,
    ) -> Self {
        Self {
            line_break: "\n".as_bytes().to_vec(),
            dedup: None,
            stores,
            md_cache,
            more,
        }
    }
    /// Replaces the default dedup map when deriving different data.
    /// Be cautious when replacing the default dedup map,
    /// as it breaks the referential equlity being equivalent to the structural equality.
    /// Otherwise, everything else is great !
    /// In the future use multiple dedup maps when we can guarantee valid nesting,
    ///   e.g., additional Derived Data on files can reuse subtrees of things inside files, but directories and files must be added without merging with the others
    ///    (it should also be possible to compute markers to provide similar guarantees, e.g. a DD only on classes can reuse children that do not contain classes).
    /// Could also make the eq consider the derived data, but to avoid breaking the incrementality we would still need some kind of marker for each context
    pub fn with_preprocessing_and_dedup(
        stores: &'stores mut SimpleStores<TS>,
        dedup: &'stores mut DedupMap,
        md_cache: &'cache mut MDCache,
        more: More,
    ) -> Self {
        Self {
            line_break: "\n".as_bytes().to_vec(),
            dedup: Some(dedup),
            stores,
            md_cache,
            more,
        }
    }
}

impl<'stores, 'cache, TS, More, const HIDDEN_NODES: bool>
    TsTreeGen<'stores, 'cache, TS, More, HIDDEN_NODES>
{
    pub fn set_line_break(self, line_break: Vec<u8>) -> Self {
        TsTreeGen {
            line_break,
            dedup: self.dedup,
            stores: self.stores,
            md_cache: self.md_cache,
            more: self.more,
        }
    }
}

impl<'stores, 'cache, TS, More> TsTreeGen<'stores, 'cache, TS, More, true> {
    pub fn without_hidden_nodes(self) -> TsTreeGen<'stores, 'cache, TS, More, false> {
        TsTreeGen {
            line_break: self.line_break,
            dedup: self.dedup,
            stores: self.stores,
            md_cache: self.md_cache,
            more: self.more,
        }
    }
}

impl<TS, More, const HIDDEN_NODES: bool> ZippedTreeGen for TsTreeGen<'_, '_, TS, More, HIDDEN_NODES>
where
    TS: TsEnableTS,
    TS::Ty2: TsType,
    More: super::More<SimpleStores<TS>, Acc = Acc<TS::Ty2>>,
{
    type Stores = SimpleStores<TS>;
    type Text = [u8];
    type Node<'b> = TNode<'b>;
    type TreeCursor<'b> = TTreeCursor<'b, HIDDEN_NODES>;

    fn stores(&mut self) -> &mut Self::Stores {
        self.stores
    }

    fn init_val(&mut self, text: &[u8], node: &Self::Node<'_>) -> Self::Acc {
        let kind = TS::obtain_type(node);
        let labeled = node.has_label();
        Acc {
            simple: BasicAccumulator::new(kind),
            no_space: vec![],
            labeled,
            start_byte: node.start_byte(),
            end_byte: node.end_byte(),
            metrics: Default::default(),
            padding_start: 0,
            role: Default::default(),
            precomp_queries: Default::default(),
        }
    }

    fn pre_skippable(
        &mut self,
        text: &Self::Text,
        cursor: &Self::TreeCursor<'_>,
        stack: &Parents<Self::Acc>,
        global: &mut Self::Global,
    ) -> PreResult<Self::Acc> {
        let node = cursor.node();
        let kind = TS::obtain_type(&node);
        if (!HIDDEN_NODES && kind.is_hidden()) || kind.is_repeat() {
            return PreResult::Ignore;
        }
        if node.0.is_missing() {
            log::trace!(
                "Missing node: {:?} {}-{}",
                kind,
                node.start_byte(),
                node.end_byte()
            );

            // must skip missing nodes, i.e., leafs added by tree-sitter to fix CST,
            // needed to avoid breaking invariant, as the node has no span:
            // `is_parent_hidden && parent.end_byte() <= acc.begin_byte()`
            return PreResult::Skip;
        }
        if kind.is_hidden() && node.start_byte() == node.end_byte() {
            log::trace!(
                "Ignoring empty hidden node: {:?} {}-{}",
                kind,
                node.start_byte(),
                node.end_byte()
            );
            return PreResult::Ignore;
        }
        let mut acc = self.pre(text, &node, stack, global);
        // TODO replace with wrapper
        if !stack.parent().is_some_and(|a| a.simple.kind.is_supertype()) {
            if let Some(r) = cursor.0.field_name() {
                if let Ok(r) = r.try_into() {
                    acc.role.current = Some(r);
                } else {
                    log::error!("cannot convert role: {}", r)
                }
            }
        }
        if kind.is_leaf() {
            acc.labeled = true;
            return PreResult::SkipChildren(acc);
        }
        PreResult::Ok(acc)
    }
    fn pre(
        &mut self,
        text: &[u8],
        node: &Self::Node<'_>,
        stack: &Parents<Self::Acc>,
        global: &mut Self::Global,
    ) -> Self::Acc {
        let kind = TS::obtain_type(node);
        Acc {
            labeled: node.has_label(),
            start_byte: node.start_byte(),
            end_byte: node.end_byte(),
            metrics: Default::default(),
            padding_start: global.sum_byte_length(),
            simple: BasicAccumulator::new(kind),
            no_space: vec![],
            role: Default::default(),
            precomp_queries: Default::default(),
        }
    }

    fn post(
        &mut self,
        mut acc_node: impl FnMut(<Self::Acc as Accumulator>::Node),
        global: &mut Self::Global,
        text: &[u8],
        acc: Self::Acc,
    ) -> <<Self as TreeGen>::Acc as Accumulator>::Node {
        let spacing = get_spacing(acc.padding_start, acc.start_byte, text);
        if global.sum_byte_length() < acc.end_byte {
            // It's an issue with TreeSitter and the grammar you are using, it rarely append, I have only seen C++ goes that bad.
            // Look at the Cpp generator for one way to handle this.
            panic!(
                "It's bad your skipping non-whitespace characters, you should choose a way of handling this."
            )
            // TODO for maximum resilience, should be handled by default, but it looks like a bug from TreeSitter.
        }
        if let Some(spacing) = spacing {
            acc_node(self.make_space(global, &spacing).into());
        }
        let label = if acc.labeled {
            let label = &text[acc.start_byte..acc.end_byte];
            std::str::from_utf8(label).ok().map(|x| x.to_string())
        } else {
            None
        };
        self.make(global, acc, label)
    }
}

impl<'store, TS, More, const HIDDEN_NODES: bool> TsTreeGen<'store, '_, TS, More, HIDDEN_NODES>
where
    TS: TsEnableTS,
    TS::Ty2: TsType,
    More: super::More<SimpleStores<TS>, Acc = Acc<TS::Ty2>>,
{
    pub(crate) fn make_space(
        &mut self,
        global: &<Self as TreeGen>::Global,
        spacing: &[u8],
    ) -> <<Self as TreeGen>::Acc as Accumulator>::Node {
        let kind = TS::Ty2::spaces();
        let interned_kind = TS::intern(kind);

        let spacing = std::str::from_utf8(spacing).unwrap().to_string();

        let dedup = &mut self.stores.node_store.dedup;
        let node_store = &mut self.stores.node_store.inner;
        let label_store = &mut self.stores.label_store;
        let line_break = &self.line_break;
        let (compressed_node, metrics) = make_leaf::<TS>(
            node_store,
            label_store,
            dedup,
            line_break,
            interned_kind,
            &spacing,
            |_| {},
        );

        FullNode {
            global: global.simple(),
            local: Local {
                compressed_node,
                metrics,
                role: None,
                precomp_queries: Default::default(),
                _ty: kind,
            },
        }
    }

    pub fn generate_file(
        &mut self,
        name: &[u8],
        text: &'store [u8],
        cursor: tree_sitter::TreeCursor,
    ) -> <<Self as TreeGen>::Acc as Accumulator>::Node {
        let mut global = Global::from(TextedGlobalData::new(Default::default(), text));
        let mut init = self.init_val(text, &TNode(cursor.node()));
        let mut xx = TTreeCursor(cursor);
        debug_assert_eq!(global.sum_byte_length(), init.padding_start);
        let mut acc = handle_file_bounds(self, text, xx, &mut global, init, Self::make_space);
        let label = Some(std::str::from_utf8(name).unwrap().to_owned());

        self.make(&mut global, acc, label)
    }
}

impl<'store, TS, More, const HIDDEN_NODES: bool> TreeGen
    for TsTreeGen<'store, '_, TS, More, HIDDEN_NODES>
where
    TS: TsEnableTS,
    TS::Ty2: TsType,
    More: super::More<SimpleStores<TS>, Acc = Acc<TS::Ty2>>,
{
    type Acc = Acc<TS::Ty2>;
    type Global = SpacedGlobalData<'store>;
    fn make(
        &mut self,
        global: &mut Self::Global,
        mut acc: Self::Acc,
        label: Option<String>,
    ) -> <<Self as TreeGen>::Acc as Accumulator>::Node {
        let type_store = self.stores.type_store;
        let label_store = &mut self.stores.label_store;
        let node_store = &mut self.stores.node_store;
        let more = &mut self.more;
        let kind = acc.simple.kind;
        let interned_kind = TS::intern(kind);
        let metrics = acc.metrics.finalize(&interned_kind, &label);

        let hashable = &metrics.hashs.most_discriminating();

        // Some notable type can contain very different labels,
        // they might benefit from a particular storing (like a blob storage, even using git's object database )
        // eg. acc.simple.kind == Type::Comment and acc.simple.kind.is_literal()
        let label_id = label.as_deref().map(|l| label_store.get_or_insert(l));

        let eq = eq_node(&interned_kind, label_id.as_ref(), &acc.simple.children);

        #[cfg(feature = "subtree-stats")]
        (node_store.inner.stats()).add_height_non_dedup(metrics.height);

        let dedup = self.dedup.as_mut();
        let dedup = dedup.map_or(&mut node_store.dedup, |x| &mut x.0);
        let insertion = node_store.inner.prepare_insertion(dedup, hashable, eq);

        let local = if let Some(compressed_node) = insertion.occupied_id() {
            let md = self.md_cache.get(&compressed_node).unwrap();
            debug_assert_eq!(metrics.height, md.metrics.height);
            debug_assert_eq!(metrics.size, md.metrics.size);
            debug_assert_eq!(metrics.size_no_spaces, md.metrics.size_no_spaces);
            debug_assert_eq!(metrics.hashs.build(), md.metrics.hashs);
            let metrics = md.metrics;
            let precomp_queries = md.precomp_queries;
            Local {
                compressed_node,
                metrics,
                role: acc.role.current,
                precomp_queries,
                _ty: kind,
            }
        } else {
            let mut metrics = metrics.map_hashs(|h| h.build());
            let own_line_count = super::newline_count(&label);
            metrics.line_count += own_line_count;

            let byte_len = (acc.end_byte - acc.start_byte).try_into().unwrap();
            let bytes_len = compo::BytesLen(byte_len);
            let children_is_empty = acc.simple.children.is_empty();

            let vacant = insertion.vacant();
            let node_store = &*vacant.1.1;
            let stores = SimpleStores {
                type_store,
                label_store: &*label_store,
                node_store,
            };

            let mut dyn_builder = subtree_builder::<TS>(interned_kind);
            dyn_builder.add(bytes_len);

            if More::ENABLED {
                acc.precomp_queries |= more.match_precomp_queries(stores, &acc, label.as_deref());
                add_md_precomp_queries(&mut dyn_builder, acc.precomp_queries);
            }

            let current_role = Option::take(&mut acc.role.current);
            acc.role.add_md(&mut dyn_builder);

            #[cfg(feature = "subtree-stats")]
            (vacant.1.1.stats()).add_height_dedup(metrics.height, metrics.hashs);
            let hashs = metrics.add_md_metrics(&mut dyn_builder, children_is_empty);
            hashs.persist(&mut dyn_builder);

            if acc.simple.children.len() != acc.no_space.len() {
                let children = acc.no_space;
                super::add_cs_no_spaces(&mut dyn_builder, children);
            }
            acc.simple
                .add_primary(&mut dyn_builder, interned_kind, label_id);

            let compressed_node = vacant.insert_built(dyn_builder.build());

            self.md_cache.insert(
                compressed_node,
                DD {
                    metrics,
                    precomp_queries: acc.precomp_queries,
                },
            );
            Local {
                compressed_node,
                metrics,
                role: current_role,
                precomp_queries: acc.precomp_queries,
                _ty: kind,
            }
        };

        FullNode {
            global: global.simple(),
            local,
        }
    }
}

// what I need is:
// a kind of automaton transmuting acc into a subtree,
// where each transmuter can own and require some fields
// and can add derived data to subtree

impl<TS: ETypeStore, More, const HIDDEN_NODES: bool> TsTreeGen<'_, '_, TS, More, HIDDEN_NODES>
where
    More: super::More<SimpleStores<TS>, Acc = Acc<TS::Ty2>>,
{
    fn custom_dd(
        stores: RawHAST<TS>,
        more: &More,
        dyn_builder: &mut dyn_builder::EntityBuilder,
        acc: &mut Acc<TS::Ty2>,
        label: Option<String>,
    ) {
        todo!("AAA"); //acc.precomp_queries |= more.match_precomp_queries(stores, &*acc, label.as_deref());
        if More::ENABLED {
            add_md_precomp_queries(dyn_builder, acc.precomp_queries);
        }
    }
}
