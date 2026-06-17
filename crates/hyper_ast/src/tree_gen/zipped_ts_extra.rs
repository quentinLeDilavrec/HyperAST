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
use crate::tree_gen::{LocalAcc, handle_file_bounds};
use crate::tree_gen::{TsEnableTS, TsType};
use crate::types::LabelStore as _;
use crate::types::Role;
use crate::types::{ETypeStore, HyperType};

use super::RoleAcc;
use super::TreeGen;
use super::ZippedTreeGen;
use super::add_md_precomp_queries;
use super::get_spacing;
use super::parser::Node as _;
use super::parser::TreeCursor as _;
use super::utils_ts::TNode;
use super::utils_ts::TTreeCursor;
use super::utils_ts::make_leaf;
use super::{Accumulator, WithByteRange};
use super::{BasicAccumulator, SubTreeMetrics};
use super::{BasicGlobalData, GlobalData, Spaces, TotalBytesGlobalData as _};
use super::{Parents, PreResult};
use super::{SpacedGlobalData, TextedGlobalData};

pub type LabelIdentifier = crate::store::labels::DefaultLabelIdentifier;

pub struct TsTreeGen<'store, 'cache, TS, Extra = (), const HIDDEN_NODES: bool = false> {
    pub line_break: Vec<u8>,
    pub stores: &'store mut SimpleStores<TS>,
    pub dedup: Option<&'store mut DedupMap>,
    pub more: &'cache mut Extra,
}

pub type Global<'a> = SpacedGlobalData<'a>;

type PrecompQueries = u16;

#[derive(Debug, Clone)]
pub struct Local {
    pub compressed_node: NodeIdentifier,

    // # directly bubbling derived data
    pub metrics: SubTreeMetrics<SyntaxNodeHashs<u32>>,
    pub role: Option<Role>,
}

impl Local {
    fn acc<T>(self, acc: &mut Acc<T>) {
        if self.metrics.size_no_spaces > 0 {
            acc.no_space.push(self.compressed_node)
        }
        if let Some(role) = self.role {
            let o = acc.simple.children.len();
            acc.role.acc(role, o);
        }
        acc.simple.push(self.compressed_node);
        acc.metrics.acc(self.metrics);
        // acc.extra += self.extra;
        // acc.precomp_queries |= self.precomp_queries;
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
    // precomp_queries: PrecompQueries,
}

pub type FNode<L> = FullNode<BasicGlobalData, L>;

impl<T> LocalAcc for Acc<T> {
    type Local = Local;
}

impl<T> Accumulator for Acc<T> {
    type Node = FNode<Local>;
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
            .field("role", &self.role)
            .finish()
    }
}

impl<'acc, T> super::WithLabel for &'acc Acc<T> {
    type L = &'acc str;
}

#[repr(transparent)]
pub struct NoOpExtra<T, Acc> {
    md_cache: HashMap<NodeIdentifier, ()>,
    _phantom: std::marker::PhantomData<(T, Acc)>,
}

impl<T, Acc> Default for NoOpExtra<T, Acc> {
    fn default() -> Self {
        Self {
            md_cache: HashMap::default(),
            _phantom: Default::default(),
        }
    }
}

pub trait Extra<HAST: crate::types::StoreRefAssoc, Acc: Accumulator> {
    type Acc: std::ops::Deref<Target = Acc>
        + std::ops::DerefMut
        + super::WithExtra
        + Accumulator<Node = Self::Node>
        + WithByteRange
        + From<Acc>;
    type Node: From<Acc::Node>;
    fn node(node: <Acc as Accumulator>::Node) -> Self::Node;
    fn node_with_acc(node: <Acc as Accumulator>::Node, acc: Self::Acc) -> Self::Node;
}

#[derive(Default)]
pub struct EmptyExtra;

impl std::ops::AddAssign for EmptyExtra {
    fn add_assign(&mut self, _: Self) {}
}

impl<HAST: crate::types::StoreRefAssoc, Acc> Extra<HAST, Acc> for NoOpExtra<HAST::TS, Acc>
where
    Acc: Accumulator,
    Acc: WithByteRange,
{
    type Acc = super::AccWithExtra<Acc, EmptyExtra>;
    type Node = <Self::Acc as Accumulator>::Node;

    fn node(node: <Acc as Accumulator>::Node) -> Self::Node {
        node.into()
    }

    fn node_with_acc(node: <Acc as Accumulator>::Node, _acc: Self::Acc) -> Self::Node {
        (node, EmptyExtra).into()
    }
}

// impl<Local> From<FNode<Local>> for FNode<Local, EmptyExtra> {
//     fn from(node: FNode<Local>) -> Self {
//         FNode {
//             global: node.global,
//             local: LocalWithExtra {
//                 local: node.local,
//                 extra: EmptyExtra,
//             },
//         }
//     }
// }

// impl<Acc, Local> From<(FNode<Local>, Acc)> for FNode<LocalWithExtra<Local, EmptyExtra>> {
//     fn from((node, acc): (FNode<Local>, Acc)) -> Self {
//         FNode {
//             global: node.global,
//             local: LocalWithExtra {
//                 local: node.local,
//                 extra: EmptyExtra,
//             },
//         }
//     }
// }

impl<'stores, 'cache, TS, More> TsTreeGen<'stores, 'cache, TS, More, true> {
    pub fn with_preprocessing(
        stores: &'stores mut SimpleStores<TS>,
        more: &'cache mut More,
    ) -> Self {
        Self {
            line_break: "\n".as_bytes().to_vec(),
            dedup: None,
            stores,
            more,
        }
    }
    /// Replaces the default dedup map when deriving different data.
    /// Be cautious when replacing the default dedup map,
    /// as it breaks the referential equality being equivalent to the structural equality.
    /// Otherwise, everything else is great !
    /// In the future use multiple dedup maps when we can guarantee valid nesting,
    ///   e.g., additional Derived Data on files can reuse subtrees of things inside files, but directories and files must be added without merging with the others
    ///    (it should also be possible to compute markers to provide similar guarantees, e.g. a DD only on classes can reuse children that do not contain classes).
    /// Could also make the eq consider the derived data, but to avoid breaking the incrementality we would still need some kind of marker for each context
    pub fn with_extra_and_dedup(
        stores: &'stores mut SimpleStores<TS>,
        dedup: &'stores mut DedupMap,
        more: &'cache mut More,
    ) -> Self {
        Self {
            line_break: "\n".as_bytes().to_vec(),
            dedup: Some(dedup),
            stores,
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
            more: self.more,
        }
    }
}

impl<TS, More, const HIDDEN_NODES: bool> ZippedTreeGen for TsTreeGen<'_, '_, TS, More, HIDDEN_NODES>
where
    TS: TsEnableTS,
    TS::Ty2: TsType,
    More: Extra<SimpleStores<TS>, Acc<TS::Ty2>>,
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
        }
        .into()
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
        }
        .into()
    }

    fn _post(
        &mut self,
        parent: &mut Self::Acc,
        global: &mut Self::Global,
        text: &Self::Text,
        acc: Self::Acc,
    ) {
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
            parent.push(self.make_space(global, &spacing).into());
        }
        let node = self.post(|n| parent.push(n), global, text, acc);
        parent.push(node);
    }

    fn post(
        &mut self,
        mut acc_node: impl FnMut(<Self::Acc as Accumulator>::Node),
        global: &mut Self::Global,
        text: &[u8],
        acc: Self::Acc,
    ) -> <<Self as TreeGen>::Acc as Accumulator>::Node {
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
    More: Extra<SimpleStores<TS>, Acc<TS::Ty2>>,
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
        More::node(FullNode {
            global: global.simple(),
            local: Local {
                compressed_node,
                metrics,
                role: None,
            },
        })
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
        // TODO what if making the file node was handled (at least in part) by the parent generator
        self.make(&mut global, acc, label)
    }
}

impl<'store, TS, More, const HIDDEN_NODES: bool> TreeGen
    for TsTreeGen<'store, '_, TS, More, HIDDEN_NODES>
where
    TS: TsEnableTS,
    TS::Ty2: TsType,
    More: Extra<SimpleStores<TS>, Acc<TS::Ty2>>,
{
    type Acc = More::Acc;
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

        if let Some(compressed_node) = insertion.occupied_id() {
            assert_eq!(super::newline_count(&label), 0);
            // let mut metrics = metrics.map_hashs(|h| h.build());
            // let own_line_count = super::newline_count(&label);
            // metrics.line_count += own_line_count;
            // let md = self.md_cache.get(&compressed_node).unwrap();
            // debug_assert_eq!(metrics.height, md.metrics.height);
            // debug_assert_eq!(metrics.size, md.metrics.size);
            // debug_assert_eq!(metrics.size_no_spaces, md.metrics.size_no_spaces);
            // debug_assert_eq!(metrics.hashs.build(), md.metrics.hashs);
            // let metrics = md.metrics;
            // let precomp_queries = md.precomp_queries;
            // Local {
            //     compressed_node,
            //     metrics: metrics.map_hashs(|h| h.build()),
            //     role: acc.role.current,
            // }

            More::node(FullNode {
                global: global.simple(),
                local: Local {
                    compressed_node,
                    metrics: metrics.map_hashs(|h| h.build()),
                    role: acc.role.current,
                },
            })
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

            // if More::ENABLED {
            //     acc.precomp_queries |= more.match_precomp_queries(stores, &acc, label.as_deref());
            //     add_md_precomp_queries(&mut dyn_builder, acc.precomp_queries);
            // }

            let current_role = Option::take(&mut acc.role.current);
            RoleAcc {
                current: None,
                roles: acc.role.roles.clone(),
                offsets: acc.role.offsets.clone(),
            }
            .add_md(&mut dyn_builder);

            #[cfg(feature = "subtree-stats")]
            (vacant.1.1.stats()).add_height_dedup(metrics.height, metrics.hashs);
            let hashs = metrics.add_md_metrics(&mut dyn_builder, children_is_empty);
            hashs.persist(&mut dyn_builder);

            if acc.simple.children.len() != acc.no_space.len() {
                let children = acc.no_space.clone();
                super::add_cs_no_spaces(&mut dyn_builder, children);
            }
            BasicAccumulator {
                kind: acc.simple.kind,
                children: acc.simple.children.clone(),
            }
            .add_primary(&mut dyn_builder, interned_kind, label_id);

            let compressed_node = vacant.insert_built(dyn_builder.build());

            More::node_with_acc(
                FullNode {
                    global: global.simple(),
                    local: Local {
                        compressed_node,
                        metrics,
                        role: current_role,
                    },
                },
                acc,
            )
        }
    }
}
