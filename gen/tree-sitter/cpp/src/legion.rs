//! fully compress all subtrees from a cpp CST
use std::collections::HashMap;
use std::fmt::Debug;

use hyperast::hashed::{IndexingHashBuilder, MetaDataHashsBuilder, SyntaxNodeHashs};
use hyperast::store::SimpleStores;
use hyperast::store::nodes::compo;
use hyperast::store::nodes::legion::DedupMap;
use hyperast::store::nodes::legion::NodeIdentifier;
use hyperast::store::nodes::legion::eq_node;
use hyperast::store::nodes::legion::subtree_builder;
use hyperast::tree_gen;
use hyperast::tree_gen::TotalBytesGlobalData as _;
use hyperast::tree_gen::TsType;
use hyperast::tree_gen::add_md_precomp_queries;
use hyperast::tree_gen::handle_file_bounds;
use hyperast::tree_gen::parser::Node as _;
use hyperast::tree_gen::parser::TreeCursor;
use hyperast::tree_gen::utils_ts::TTreeCursor;
use hyperast::tree_gen::{AccIndentation, Accumulator, WithByteRange};
use hyperast::tree_gen::{BasicAccumulator, RoleAcc, SubTreeMetrics};
use hyperast::tree_gen::{BasicGlobalData, NoOpMore};
use hyperast::tree_gen::{Parents, PreResult};
use hyperast::tree_gen::{SpacedGlobalData, TextedGlobalData};
use hyperast::tree_gen::{TreeGen, ZippedTreeGen};
use hyperast::tree_gen::{compute_indentation, get_spacing};

use hyperast::full::FullNode;
use hyperast::nodes::Space;
use hyperast::types;
use hyperast::types::LabelStore as _;
use hyperast::types::Role;

use crate::TNode;
use crate::TStore;
use crate::Type;
use crate::types::impls::CppEnabledTypeStore;

pub type LabelIdentifier = hyperast::store::labels::DefaultLabelIdentifier;

/// HIDDEN_NODES: enables recovering of hidden nodes from tree-sitter.
///   You should start without filtering out hidden nodes when intergrating/updating a grammar,
///   filtering hidden nodes adds complexity, thus might cause additional bugs
pub struct CppTreeGen<
    'stores,
    'cache,
    TS = TStore,
    S = SimpleStores<TS>,
    More = (),
    const HIDDEN_NODES: bool = true,
> {
    pub line_break: Vec<u8>,
    pub dedup: Option<&'stores mut DedupMap>,
    pub stores: &'stores mut S,
    pub md_cache: &'cache mut MDCache,
    pub more: More,
    pub _p: std::marker::PhantomData<TS>,
}

pub type MDCache = HashMap<NodeIdentifier, MD>;

// NOTE only keep compute intensive metadata (where space/time tradeoff is worth storing)
// eg. decls refs, maybe hashes but not size and height
// * metadata: computation results from concrete code of node and its children
// they can be qualitative metadata .eg a hash or they can be quantitative .eg lines of code
pub struct MD {
    pub metrics: SubTreeMetrics<SyntaxNodeHashs<u32>>,
    pub precomp_queries: PrecompQueries,
}

impl From<Local> for MD {
    fn from(x: Local) -> Self {
        MD {
            metrics: x.metrics,
            precomp_queries: x.precomp_queries,
        }
    }
}

impl MD {
    pub fn local(&self, compressed_node: NodeIdentifier) -> Local {
        let md = self;
        let metrics = md.metrics;
        let precomp_queries = md.precomp_queries;
        Local {
            compressed_node,
            metrics,
            role: None,
            precomp_queries,
            viz_cs_count: 0,
        }
    }
}

pub type Global<'a> = SpacedGlobalData<'a>;

/// TODO temporary placeholder
#[derive(Debug, Clone, Default)]
pub struct PartialAnalysis {}

type PrecompQueries = u16;

#[derive(Debug, Clone)]
pub struct Local {
    pub compressed_node: NodeIdentifier,
    pub metrics: SubTreeMetrics<SyntaxNodeHashs<u32>>,
    pub role: Option<Role>,
    pub precomp_queries: PrecompQueries,
    pub viz_cs_count: u32,
}

impl Local {
    fn acc(self, acc: &mut Acc) {
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
        acc.viz_cs_count = acc
            .viz_cs_count
            .checked_add(self.viz_cs_count)
            .expect("type of viz_cs_count is too small");
    }
}

pub struct Acc {
    pub(crate) simple: BasicAccumulator<Type, NodeIdentifier>,
    labeled: bool,
    metrics: SubTreeMetrics<SyntaxNodeHashs<u32>>,
    pub(crate) padding_start: usize,
    pub(crate) start_byte: usize,
    end_byte: usize,
    // field for derived data (more experimental and prone to changes)
    /// children that are not spaces
    no_space: Vec<NodeIdentifier>,
    /// At some point it will be used to make deduplication formatting independent
    indentation: tree_gen::Spaces,
    /// supports retrieval of roles
    role: RoleAcc<crate::Role>,
    /// aggregate of precomputed queries
    precomp_queries: PrecompQueries,
    /// number of visible children (by tree-sitter definition)
    viz_cs_count: u32,
}

pub type FNode = FullNode<BasicGlobalData, Local>;
impl Accumulator for Acc {
    type Node = FNode;
    fn push(&mut self, full_node: Self::Node) {
        // dbg!(self.simple.kind);
        full_node.local.acc(self);
    }
}

impl AccIndentation for Acc {
    fn indentation(&self) -> &tree_gen::Spaces {
        &self.indentation
    }
}

impl WithByteRange for Acc {
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

impl types::Typed for Acc {
    type Type = Type;

    fn get_type(&self) -> Self::Type {
        self.simple.kind
    }
}

impl hyperast::tree_gen::WithChildren<NodeIdentifier> for Acc {
    fn children(&self) -> &[NodeIdentifier] {
        &self.simple.children
    }
}

impl hyperast::tree_gen::WithRole<Role> for Acc {
    fn role_at(&self, o: usize) -> Option<Role> {
        self.role
            .offsets
            .iter()
            .position(|x| *x as usize == o)
            .and_then(|x| self.role.roles.get(x))
            .cloned()
    }
}

impl<'acc> hyperast::tree_gen::WithLabel for &'acc Acc {
    type L = &'acc str;
}

impl Debug for Acc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Acc")
            .field("simple", &self.simple)
            .field("no_space", &self.no_space)
            .field("labeled", &self.labeled)
            .field("start_byte", &self.start_byte)
            .field("end_byte", &self.end_byte)
            .field("metrics", &self.metrics)
            .field("padding_start", &self.padding_start)
            .field("indentation", &self.indentation)
            .finish()
    }
}

impl<TS, More, const HIDDEN_NODES: bool> ZippedTreeGen
    for CppTreeGen<'_, '_, TS, SimpleStores<TS>, More, HIDDEN_NODES>
where
    TS: CppEnabledTypeStore<Ty2 = Type>,
    More: tree_gen::Prepro<SimpleStores<TS>>
        + for<'s> tree_gen::PreproTSG<SimpleStores<TS>, Acc = Acc>,
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
        let parent_indentation = Space::try_format_indentation(&self.line_break)
            .unwrap_or_else(|| vec![Space::Space; self.line_break.len()]);
        let indent = compute_indentation(
            &self.line_break,
            text,
            node.start_byte(),
            0,
            &parent_indentation,
        );
        let labeled = node.has_label();
        Acc {
            simple: BasicAccumulator::new(kind),
            no_space: vec![],
            labeled,
            start_byte: node.start_byte(),
            end_byte: node.end_byte(),
            viz_cs_count: 0,
            metrics: Default::default(),
            padding_start: 0,
            indentation: indent,
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
        let Some(kind) = TS::try_obtain_type(&node) else {
            log::warn!("Failed to obtain type of cpp node");
            return PreResult::Skip;
        };
        if !HIDDEN_NODES {
            if kind.is_hidden() {
                return PreResult::Ignore;
            }
        }
        if kind.is_repeat() {
            // dbg!(kind);
            if stack.parent().unwrap().simple.children.len() < 1024
                && stack.parent().unwrap().viz_cs_count < 1024
            {
                return PreResult::Ignore;
            }
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
                "Empty hidden node: {:?} {}-{}",
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
        let parent_indentation = &stack.parent().unwrap().indentation();
        let kind = TS::obtain_type(node);
        let indent = if node.start_byte() < global.sum_byte_length() {
            eprintln!("kind: {:?}", kind);
            eprintln!("parent kind: {:?}", stack.parent().unwrap().simple.kind);
            let b = node.start_byte();
            let a = b.saturating_sub(100);
            let c = global.sum_byte_length();
            let d = c.saturating_add(100).min(text.len());
            eprintln!("{:?}", std::str::from_utf8(&text[a..b]).unwrap());
            eprintln!("{:?}", std::str::from_utf8(&text[b..c]).unwrap());
            eprintln!("{:?}", std::str::from_utf8(&text[c..d]).unwrap());
            panic!(
                "broken monotonicity invariant at {} which should not be smaller than {}",
                node.start_byte(),
                global.sum_byte_length()
            )
        } else {
            compute_indentation(
                &self.line_break,
                text,
                node.start_byte(),
                global.sum_byte_length(),
                parent_indentation,
            )
        };
        Acc {
            labeled: node.has_label(),
            start_byte: node.start_byte(),
            end_byte: node.end_byte(),
            metrics: Default::default(),
            padding_start: global.sum_byte_length(),
            indentation: indent,
            simple: BasicAccumulator::new(kind),
            viz_cs_count: 0,
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
        mut acc: Self::Acc,
    ) -> <Self::Acc as Accumulator>::Node {
        let spacing = get_spacing(acc.padding_start, acc.start_byte, text);
        if global.sum_byte_length() < acc.end_byte {
            // Only create an error node if tree-sitter is skipping non-whitespaces.
            // the error node takes the span to realign for next leaf node
            if tree_gen::try_get_spacing(global.sum_byte_length(), acc.end_byte, text).is_none() {
                let local = self.make_error(&text[global.sum_byte_length()..acc.end_byte]);
                acc.push(FullNode {
                    global: global.simple(),
                    local,
                });
                global.set_sum_byte_length(acc.end_byte);
            }
        }
        if let Some(spacing) = spacing {
            // debug_assert_ne!(parent.simple.children.len(), 0, "{:?}", parent.simple);
            acc_node(self.make_space(global, &spacing));
        }
        let label = if acc.labeled {
            std::str::from_utf8(&text[acc.start_byte..acc.end_byte])
                .ok()
                .map(|x| x.to_string())
        } else {
            None
        };
        self.make(global, acc, label)
    }
}

pub fn tree_sitter_parse(text: &[u8]) -> tree_sitter::Tree {
    hyperast::tree_gen::utils_ts::tree_sitter_parse(text, &crate::language())
}

impl<'store, 'cache, TS: CppEnabledTypeStore>
    CppTreeGen<'store, 'cache, TS, SimpleStores<TS>, NoOpMore<TS, Acc>, true>
{
    pub fn new(stores: &'store mut SimpleStores<TS>, md_cache: &'cache mut MDCache) -> Self {
        Self {
            line_break: "\n".as_bytes().to_vec(),
            dedup: None,
            stores,
            md_cache,
            more: Default::default(),
            _p: std::marker::PhantomData,
        }
    }
}

impl<'store, 'cache, TS, More> CppTreeGen<'store, 'cache, TS, SimpleStores<TS>, More, true> {
    pub fn without_hidden_nodes(
        self,
    ) -> CppTreeGen<'store, 'cache, TS, SimpleStores<TS>, More, false> {
        CppTreeGen {
            line_break: self.line_break,
            stores: self.stores,
            md_cache: self.md_cache,
            more: self.more,
            dedup: self.dedup,
            _p: std::marker::PhantomData,
        }
    }
}
impl<'stores, 'cache, TS: CppEnabledTypeStore + 'static, More>
    CppTreeGen<'stores, 'cache, TS, SimpleStores<TS>, More, true>
{
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
            _p: std::marker::PhantomData,
        }
    }
}

impl<'store, 'cache, TS, More, const HIDDEN_NODES: bool>
    CppTreeGen<'store, 'cache, TS, SimpleStores<TS>, More, HIDDEN_NODES>
where
    TS: CppEnabledTypeStore<Ty2 = Type>,
    More: tree_gen::Prepro<SimpleStores<TS>>
        + for<'s> tree_gen::PreproTSG<SimpleStores<TS>, Acc = Acc>,
{
    pub fn with_more<M>(
        self,
        more: M,
    ) -> CppTreeGen<'store, 'cache, TS, SimpleStores<TS>, M, HIDDEN_NODES> {
        CppTreeGen {
            line_break: self.line_break,
            dedup: self.dedup,
            stores: self.stores,
            md_cache: self.md_cache,
            more,
            _p: std::marker::PhantomData,
        }
    }

    pub fn set_line_break(self, line_break: Vec<u8>) -> Self {
        CppTreeGen {
            line_break,
            dedup: self.dedup,
            stores: self.stores,
            md_cache: self.md_cache,
            more: self.more,
            _p: std::marker::PhantomData,
        }
    }

    pub(crate) fn make_space(
        &mut self,
        global: &<Self as TreeGen>::Global,
        spacing: &[u8],
    ) -> <<Self as TreeGen>::Acc as Accumulator>::Node {
        FullNode {
            global: global.simple(),
            local: self.make_spacing(spacing),
        }
    }

    pub(crate) fn make_spacing(&mut self, spacing: &[u8]) -> Local {
        let kind = Type::Spaces;
        let interned_kind = TS::intern(kind);
        debug_assert_eq!(kind, TS::resolve(interned_kind));

        let spacing = std::str::from_utf8(spacing).unwrap().to_string();

        let dedup = &mut self.stores.node_store.dedup;
        let node_store = &mut self.stores.node_store.inner;
        let label_store = &mut self.stores.label_store;
        let line_break = &self.line_break;
        let (compressed_node, metrics) = tree_gen::utils_ts::make_leaf::<TS>(
            node_store,
            label_store,
            dedup,
            line_break,
            interned_kind,
            &spacing,
            |_| {},
        );
        Local {
            compressed_node,
            metrics,
            role: None,
            precomp_queries: Default::default(),
            viz_cs_count: 0,
        }
    }

    fn make_error(&mut self, text: &[u8]) -> Local {
        let kind = Type::ERROR;
        let interned_kind = TS::intern(kind);
        debug_assert_eq!(kind, TS::resolve(interned_kind));
        let text = std::str::from_utf8(text).unwrap().to_string();
        let dedup = &mut self.stores.node_store.dedup;
        let node_store = &mut self.stores.node_store.inner;
        let label_store = &mut self.stores.label_store;
        let line_break = &self.line_break;
        let (compressed_node, metrics) = tree_gen::utils_ts::make_leaf::<TS>(
            node_store,
            label_store,
            dedup,
            line_break,
            interned_kind,
            &text,
            |_| {},
        );
        Local {
            compressed_node,
            metrics,
            role: None,
            precomp_queries: Default::default(),
            viz_cs_count: 0,
        }
    }

    pub fn tree_sitter_parse(text: &[u8]) -> tree_sitter::Tree {
        tree_sitter_parse(text)
    }

    pub fn generate_file(
        &mut self,
        name: &[u8],
        text: &'store [u8],
        cursor: tree_sitter::TreeCursor,
    ) -> <<Self as TreeGen>::Acc as Accumulator>::Node {
        let mut global = Global::from(TextedGlobalData::new(Default::default(), text));
        let init = self.init_val(text, &TNode(cursor.node()));
        let xx = TTreeCursor(cursor);
        debug_assert_eq!(global.sum_byte_length(), init.padding_start);
        let mut acc = handle_file_bounds(self, text, xx, &mut global, init, Self::make_space);
        use hyperast::types::HyperType;
        if !acc.simple.kind.is_file() {
            log::warn!("ignoring parsing error at the root of the file");
            acc.simple.kind = Type::TranslationUnit;
        }
        let label = Some(std::str::from_utf8(name).unwrap().to_owned());
        self.make(&mut global, acc, label)
    }
}

impl<'stores, TS, More, const HIDDEN_NODES: bool> TreeGen
    for CppTreeGen<'stores, '_, TS, SimpleStores<TS>, More, HIDDEN_NODES>
where
    TS: CppEnabledTypeStore<Ty2 = Type>,
    More: tree_gen::Prepro<SimpleStores<TS>> + tree_gen::PreproTSG<SimpleStores<TS>, Acc = Acc>,
    TS::Ty2: hyperast::tree_gen::utils_ts::TsType,
{
    type Acc = Acc;
    type Global = SpacedGlobalData<'stores>;
    fn make(
        &mut self,
        global: &mut Self::Global,
        mut acc: Self::Acc,
        label: Option<String>,
    ) -> <Self::Acc as Accumulator>::Node {
        let node_store = &mut self.stores.node_store;
        let kind = acc.simple.kind;
        let interned_kind = TS::intern(kind);
        let metrics = acc.metrics.finalize(&interned_kind, &label);

        let hashable = &metrics.hashs.most_discriminating();

        let label_id = label
            .as_deref()
            .map(|l| self.stores.label_store.get_or_insert(l));
        let eq = eq_node(&interned_kind, label_id.as_ref(), &acc.simple.children);

        let dedup = &mut node_store.dedup;
        let dedup = self.dedup.as_mut().map_or(dedup, |x| &mut x.0);
        let insertion = node_store.inner.prepare_insertion(dedup, &hashable, eq);

        let local = if let Some(compressed_node) = insertion.occupied_id() {
            let md = self.md_cache.get(&compressed_node).unwrap();
            debug_assert_eq!(metrics.height, md.metrics.height);
            debug_assert_eq!(metrics.size, md.metrics.size);
            debug_assert_eq!(metrics.size_no_spaces, md.metrics.size_no_spaces);
            debug_assert_eq!(metrics.hashs.build(), md.metrics.hashs);
            let metrics = md.metrics;
            let precomp_queries = md.precomp_queries;
            let viz_cs_count = if acc.simple.kind.is_hidden() {
                acc.viz_cs_count
            } else {
                1
            };
            Local {
                compressed_node,
                metrics,
                role: acc.role.current,
                precomp_queries,
                viz_cs_count,
            }
        } else {
            let mut metrics = metrics.map_hashs(|h| h.build());
            let own_line_count = tree_gen::newline_count(&label);
            metrics.line_count += own_line_count;
            let byte_len = (acc.end_byte - acc.start_byte).try_into().unwrap();
            let bytes_len = compo::BytesLen(byte_len);
            let vacant = insertion.vacant();
            let node_store: &_ = vacant.1.1;
            let stores = SimpleStores {
                type_store: self.stores.type_store,
                label_store: &self.stores.label_store,
                node_store,
            };
            acc.precomp_queries |= self
                .more
                .match_precomp_queries(stores, &acc, label.as_deref());
            let children_is_empty = acc.simple.children.is_empty();

            let mut dyn_builder = subtree_builder::<TS>(interned_kind);
            dyn_builder.add(bytes_len);

            let current_role = Option::take(&mut acc.role.current);
            acc.role.add_md(&mut dyn_builder);
            if More::ENABLED {
                add_md_precomp_queries(&mut dyn_builder, acc.precomp_queries);
            }

            let hashs = metrics.add_md_metrics(&mut dyn_builder, children_is_empty);
            hashs.persist(&mut dyn_builder);

            if acc.simple.children.len() != acc.no_space.len() {
                let children = acc.no_space;
                tree_gen::add_cs_no_spaces(&mut dyn_builder, children);
            }
            let viz_cs_count = if acc.simple.kind.is_hidden() {
                acc.viz_cs_count
            } else {
                if acc.viz_cs_count != 0 {
                    dyn_builder.add(compo::VizCsCount(acc.viz_cs_count));
                }
                1
            };
            acc.simple
                .add_primary(&mut dyn_builder, interned_kind, label_id);

            let compressed_node = vacant.insert_built(dyn_builder.build());

            self.md_cache.insert(
                compressed_node,
                MD {
                    metrics,
                    precomp_queries: acc.precomp_queries,
                },
            );
            Local {
                compressed_node,
                metrics,
                role: current_role,
                precomp_queries: acc.precomp_queries,
                viz_cs_count,
            }
        };

        FullNode {
            global: global.simple(),
            local,
        }
    }
}
