//! fully compress all subtrees
#![allow(unused)]
use num::ToPrimitive as _;
use std::collections::HashMap;
use std::fmt::Debug;
use std::str::from_utf8;

use crate::filter::BloomSize;
use crate::full::FullNode;
use crate::hashed::{IndexingHashBuilder, MetaDataHashsBuilder, SyntaxNodeHashs};
use crate::store::SimpleStores;
use crate::store::nodes::DefaultNodeStore as NodeStore;
use crate::store::nodes::compo;
use crate::store::nodes::legion::{NodeIdentifier, dyn_builder, eq_node};
use crate::types::{HyperType, LabelStore as _};

use super::TotalBytesGlobalData as _;
use super::parser::Node as _;
use super::parser::TreeCursor;
use super::parser::Visibility;
use super::utils_ts::TTreeCursor;
use super::utils_ts::make_leaf;
use super::utils_ts::{TNode, TsEnableTS, TsType};
use super::{AccIndentation, Accumulator, WithByteRange};
use super::{BasicAccumulator, SubTreeMetrics};
use super::{BasicGlobalData, GlobalData};
use super::{Parents, PreResult};
use super::{SpacedGlobalData, TextedGlobalData};
use super::{compute_indentation, has_final_space};

pub type LabelIdentifier = crate::store::labels::DefaultLabelIdentifier;

pub struct TsTreeGen<'store, 'cache, TS, More = (), const HIDDEN_NODES: bool = false> {
    pub line_break: Vec<u8>,
    pub stores: &'store mut SimpleStores<TS>,
    pub md_cache: &'cache mut MDCache,
    pub more: More,
}

pub type MDCache = HashMap<NodeIdentifier, DD>;

pub struct DD {
    metrics: SubTreeMetrics<SyntaxNodeHashs<u32>>,
}

impl<T> From<Local<T>> for DD {
    fn from(x: Local<T>) -> Self {
        DD { metrics: x.metrics }
    }
}

pub type Global<'a> = SpacedGlobalData<'a>;

#[derive(Debug, Clone)]
pub struct Local<T> {
    pub compressed_node: NodeIdentifier,

    // # debug
    pub _ty: T,

    // # directly bubbling derived data
    pub metrics: SubTreeMetrics<SyntaxNodeHashs<u32>>,
    // # by providing store could also fetch the ones not there
}

impl<T> Local<T> {
    fn acc(self, acc: &mut Acc<T>) {
        acc.simple.push(self.compressed_node);
        acc.metrics.acc(self.metrics);
    }
}

pub struct Acc<T> {
    // # primary
    simple: BasicAccumulator<T, NodeIdentifier>,
    labeled: bool,
    // size: u32,
    // hash: u32,
    // # debug
    _next: T,
    metrics: SubTreeMetrics<SyntaxNodeHashs<u32>>, // contains important hash
    // support
    start_byte: usize,
    end_byte: usize,
    padding_start: usize,
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

impl<T: Debug> Debug for Acc<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Acc")
            .field("simple", &self.simple)
            .field("labeled", &self.labeled)
            .field("start_byte", &self.start_byte)
            .field("end_byte", &self.end_byte)
            .field("metrics", &self.metrics)
            .field("padding_start", &self.padding_start)
            .finish()
    }
}

impl<T> super::WithChildren<NodeIdentifier> for Acc<T> {
    fn children(&self) -> &[NodeIdentifier] {
        &self.simple.children
    }
}

impl<T> super::WithRole<crate::types::Role> for Acc<T> {
    fn role_at(&self, o: usize) -> Option<crate::types::Role> {
        todo!()
        // self.role
        //     .offsets
        //     .iter()
        //     .position(|x| *x as usize == o)
        //     .and_then(|x| self.role.roles.get(x))
        //     .cloned()
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
        Self {
            line_break: "\n".as_bytes().to_vec(),
            stores,
            md_cache,
            more: Default::default(),
        }
    }
}

pub trait ZippedTreeGen: TreeGen
where
    Self::Global: super::TotalBytesGlobalData,
{
    // # results
    // type Node1;
    type Stores;
    // # source
    type Text: ?Sized;
    type Node<'a>: super::parser::Node;
    type TreeCursor<'a>: super::parser::TreeCursor<N = Self::Node<'a>>;

    fn init_val(&mut self, text: &Self::Text, node: &Self::Node<'_>) -> Self::Acc;

    fn pre_skippable(
        &mut self,
        text: &Self::Text,
        cursor: &Self::TreeCursor<'_>,
        stack: &Parents<Self::Acc>,
        global: &mut Self::Global,
    ) -> PreResult<Self::Acc> {
        PreResult::Ok(self.pre(text, &cursor.node(), stack, global))
    }

    fn pre(
        &mut self,
        text: &Self::Text,
        node: &Self::Node<'_>,
        stack: &Parents<Self::Acc>,
        global: &mut Self::Global,
    ) -> Self::Acc;

    fn acc(&mut self, parent: &mut Self::Acc, full_node: <Self::Acc as Accumulator>::Node) {
        parent.push(full_node);
    }

    fn post(
        &mut self,
        parent: &mut Self::Acc,
        global: &mut Self::Global,
        text: &Self::Text,
        acc: Self::Acc,
    ) -> <<Self as TreeGen>::Acc as Accumulator>::Node;

    fn stores(&mut self) -> &mut Self::Stores;

    fn r#gen(
        &mut self,
        text: &Self::Text,
        stack: &mut Parents<Self::Acc>,
        cursor: &mut Self::TreeCursor<'_>,
        global: &mut Self::Global,
    );
}

#[derive(PartialEq, Eq)]
pub(crate) enum Has {
    Down,
    Up,
    Right,
}

impl<TS, More, const HIDDEN_NODES: bool> ZippedTreeGen for TsTreeGen<'_, '_, TS, More, HIDDEN_NODES>
where
    TS: TsEnableTS,
    TS::Ty2: TsType,
    More: for<'t> super::More<SimpleStores<TS>, Acc = Acc<TS::Ty2>>,
{
    type Stores = SimpleStores<TS>;
    type Text = [u8];
    type Node<'b> = TNode<'b>;
    type TreeCursor<'b> = TTreeCursor<'b, HIDDEN_NODES>;

    fn r#gen(
        &mut self,
        text: &Self::Text,
        stack: &mut Parents<Self::Acc>,
        cursor: &mut Self::TreeCursor<'_>,
        global: &mut Self::Global,
    ) {
        let mut has = Has::Down;
        loop {
            dbg!(cursor.0.node().kind());
            if has != Has::Up {
                if let Some(visibility) = cursor.goto_first_child_extended() {
                    has = Has::Down;
                    self._pre(global, text, cursor, stack, &mut has, visibility);
                    continue;
                }
            }
            if let Some(visibility) = cursor.goto_next_sibling_extended() {
                has = Has::Right;
                global.right();
                self._post(stack, global, text);
                self._pre(global, text, cursor, stack, &mut has, visibility);
                dbg!()
            } else if cursor.goto_parent() {
                has = Has::Up;
                self._post(stack, global, text);
            } else {
                dbg!();
                break;
            }
        }
    }

    fn stores(&mut self) -> &mut Self::Stores {
        self.stores
    }

    fn init_val(&mut self, _text: &[u8], node: &Self::Node<'_>) -> Self::Acc {
        let kind = TS::obtain_type(node);
        let labeled = node.has_label();
        Acc {
            simple: BasicAccumulator {
                kind,
                children: vec![],
            },
            labeled,
            start_byte: node.start_byte(),
            end_byte: node.end_byte(),
            metrics: Default::default(),
            padding_start: 0,
            _next: TS::Ty2::spaces(),
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
            return PreResult::Skip;
        };
        if HIDDEN_NODES && (kind.is_repeat() || kind.is_hidden()) {
            return PreResult::Ignore;
        }
        if node.0.is_missing() {
            dbg!("missing");
            return PreResult::Skip;
        }
        let mut acc = self.pre(text, &node, stack, global);
        // TODO replace with wrapper
        if !stack.parent().is_some_and(|a| a.simple.kind.is_supertype()) {
            if let Some(r) = cursor.0.field_name() {
                if let Ok(r) = TryInto::<crate::types::Role>::try_into(r) {
                    // acc.role.current = Some(r);
                    log::warn!("not retrieving roles");
                } else {
                    log::error!("cannot convert role: {}", r)
                }
            }
        }
        PreResult::Ok(acc)
    }

    fn pre(
        &mut self,
        text: &[u8],
        node: &Self::Node<'_>,
        _stack: &Parents<Self::Acc>,
        global: &mut Self::Global,
    ) -> Self::Acc {
        println!(
            "`{}`",
            from_utf8(&text[node.start_byte()..node.end_byte()]).unwrap()
        );
        dbg!(global.sum_byte_length()..node.start_byte());
        println!(
            "`{}`",
            from_utf8(&text[global.sum_byte_length()..node.start_byte()]).unwrap()
        );
        let kind = TS::obtain_type(node);
        Acc {
            simple: BasicAccumulator {
                kind,
                children: vec![],
            },
            labeled: node.has_label(),
            start_byte: node.start_byte(),
            end_byte: node.end_byte(),
            metrics: Default::default(),
            padding_start: global.sum_byte_length(),
            _next: TS::Ty2::spaces(),
        }
    }

    fn post(
        &mut self,
        parent: &mut Self::Acc,
        global: &mut Self::Global,
        text: &[u8],
        acc: Self::Acc,
    ) -> <<Self as TreeGen>::Acc as Accumulator>::Node {
        let spacing = get_spacing(acc.padding_start, acc.start_byte, text);
        if let Some(spacing) = spacing {
            let local = self.make_spacing(spacing);
            debug_assert_ne!(parent.simple.children.len(), 0, "{:?}", parent.simple);
            parent.push(FullNode {
                global: global.simple(),
                local,
            });
        }
        let label = if acc.labeled {
            from_utf8(&text[acc.start_byte..acc.end_byte])
                .ok()
                .map(|x| x.to_string())
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
    More: for<'t> super::More<SimpleStores<TS>, Acc = Acc<TS::Ty2>>,
{
    fn make_spacing(&mut self, spacing: Vec<u8>) -> Local<TS::Ty2> {
        let kind = TS::Ty2::spaces();
        let interned_kind = TS::intern(kind);

        let spacing = from_utf8(&spacing).unwrap().to_string();

        let dedup = &mut self.stores.node_store.dedup;
        let node_store = &mut self.stores.node_store.inner;
        let label_store = &mut self.stores.label_store;
        let line_break = &self.line_break;
        let (compressed_node, metrics) = super::utils_ts::make_leaf::<TS>(
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
            _ty: kind,
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

        let spacing = get_spacing(init.padding_start, init.start_byte, text);
        if let Some(spacing) = spacing {
            global.down();
            global.set_sum_byte_length(init.start_byte);
            init.push(FullNode {
                global: global.simple(),
                local: self.make_spacing(spacing),
            });
            global.right();
        }
        let mut stack = init.into();

        self.r#gen(text, &mut stack, &mut xx, &mut global);

        let mut acc = stack.finalize();

        if has_final_space(&0, global.sum_byte_length(), text) {
            let spacing = get_spacing(global.sum_byte_length(), text.len(), text);
            if let Some(spacing) = spacing {
                global.right();
                acc.push(FullNode {
                    global: global.simple(),
                    local: self.make_spacing(spacing),
                });
            }
        }
        let label = Some(from_utf8(name).unwrap().to_owned());

        self.make(&mut global, acc, label)
    }

    fn _pre(
        &mut self,
        global: &mut SpacedGlobalData<'store>,
        text: &[u8],
        cursor: &mut <Self as ZippedTreeGen>::TreeCursor<'_>,
        stack: &mut Parents<Acc<TS::Ty2>>,
        has: &mut Has,
        visibility: Visibility,
    ) {
        global.down();
        match self.pre_skippable(text, cursor, stack, global) {
            PreResult::Skip => {
                stack.push(super::P::BothHidden);
                *has = Has::Up;
                global.up();
            }
            PreResult::Ignore => todo!(),
            PreResult::SkipChildren(_) => todo!(),
            PreResult::Ok(acc) => {
                global.set_sum_byte_length(acc.begin_byte());
                stack.push(super::P::Visible(acc))
            }
        }
    }

    fn _post(
        &mut self,
        stack: &mut Parents<Acc<TS::Ty2>>,
        global: &mut SpacedGlobalData<'store>,
        text: &[u8],
    ) {
        let acc = stack.pop().unwrap();
        use crate::tree_gen::P;
        let acc = match acc {
            P::ManualyHidden => todo!(),
            P::BothHidden => return,
            P::Hidden(_) => todo!(),
            P::Visible(acc) => acc,
        };
        global.set_sum_byte_length(acc.end_byte());
        global.up();
        let parent = stack.parent_mut().unwrap();
        let full_node = self.post(parent, global, text, acc);
        self.acc(parent, full_node);
    }
}

pub fn get_spacing(padding_start: usize, pos: usize, text: &[u8]) -> Option<Vec<u8>> {
    // TODO change debug assert to assert if you want to strictly enforce spaces, issues with other char leaking is often caused by "bad" grammar.
    if padding_start != pos {
        let spaces = &text[padding_start..pos];
        // let spaces = Space::format_indentation(spaces);
        let mut bslash = false;
        spaces.iter().for_each(|x| {
            if bslash && (*x == b'\n' || *x == b'\r') {
                bslash = false
            } else if *x == b'\\' {
                debug_assert!(!bslash);
                bslash = true
            } else {
                debug_assert!(
                    *x == b' ' || *x == b'\n' || *x == b'\t' || *x == b'\r',
                    "{} {} {:?}",
                    x,
                    padding_start,
                    from_utf8(spaces).unwrap()
                )
            }
        });
        debug_assert!(
            !bslash,
            "{}",
            from_utf8(&text[padding_start.saturating_sub(100)..pos + 50]).unwrap()
        );
        let spaces = spaces.to_vec();
        // let spaces = Space::replace_indentation(parent_indentation, &spaces);
        // TODO put back the relativisation later, can pose issues when computing len of a subtree (contextually if we make the optimisation)
        Some(spaces)
    } else {
        None
    }
}

pub trait TreeGen {
    /// Container holding data waiting to be added to the HyperAST
    /// Note: needs WithByteRange to handle hidden node properly, it allows to go back up without using the cursor. When Treesitter is "fixed" change that
    type Acc: Accumulator + WithByteRange;
    /// Container holding global data used during generation.
    ///
    /// Useful for transient data needed during generation,
    /// this way you avoid cluttering [TreeGen::Acc].
    ///
    /// WARN make sure it does not leaks contextual data in subtrees.
    type Global: GlobalData;
    fn make(
        &mut self,
        global: &mut Self::Global,
        acc: Self::Acc,
        label: Option<String>,
    ) -> <<Self as TreeGen>::Acc as Accumulator>::Node;
}

impl<'stores, TS, More, const HIDDEN_NODES: bool> TreeGen
    for TsTreeGen<'stores, '_, TS, More, HIDDEN_NODES>
where
    TS: TsEnableTS,
    TS::Ty2: TsType,
    More: for<'t> super::More<SimpleStores<TS>, Acc = Acc<TS::Ty2>>,
{
    type Acc = Acc<TS::Ty2>;
    type Global = SpacedGlobalData<'stores>;
    fn make(
        &mut self,
        global: &mut Self::Global,
        acc: Self::Acc,
        label: Option<String>,
    ) -> <<Self as TreeGen>::Acc as Accumulator>::Node {
        let kind = acc.simple.kind;
        let interned_kind = TS::intern(kind);
        let metrics = acc.metrics.finalize(&interned_kind, &label);

        let hashable = &metrics.hashs.most_discriminating();

        let label_id = label
            .as_deref()
            .map(|l| self.stores.label_store.get_or_insert(l));
        let eq = eq_node(&interned_kind, label_id.as_ref(), &acc.simple.children);

        let insertion = self.stores.node_store.prepare_insertion(hashable, eq);

        let local = if let Some(compressed_node) = insertion.occupied_id() {
            let md = self.md_cache.get(&compressed_node).unwrap();
            debug_assert_eq!(metrics.height, md.metrics.height);
            debug_assert_eq!(metrics.size, md.metrics.size);
            debug_assert_eq!(metrics.size_no_spaces, md.metrics.size_no_spaces);
            debug_assert_eq!(metrics.hashs.build(), md.metrics.hashs);
            let metrics = md.metrics;
            Local {
                compressed_node,
                metrics,
                _ty: kind,
            }
        } else {
            let mut metrics = metrics.map_hashs(|h| h.build());
            let own_line_count = super::newline_count(&label);
            metrics.line_count += own_line_count;
            let byte_len = (acc.end_byte - acc.start_byte).try_into().unwrap();
            let bytes_len = compo::BytesLen(byte_len);
            let vacant = insertion.vacant();
            let mut dyn_builder = dyn_builder::EntityBuilder::new();

            let children_is_empty = acc.simple.children.is_empty();
            dyn_builder.add(bytes_len);

            let hashs = metrics.add_md_metrics(&mut dyn_builder, children_is_empty);
            hashs.persist(&mut dyn_builder);

            acc.simple
                .add_primary(&mut dyn_builder, interned_kind, label_id);

            let compressed_node = vacant.insert_built(dyn_builder.build());

            self.md_cache.insert(compressed_node, DD { metrics });
            Local {
                compressed_node,
                metrics,
                _ty: kind,
            }
        };

        FullNode {
            global: global.simple(),
            local,
        }
    }
}
