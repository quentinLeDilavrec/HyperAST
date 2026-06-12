//! fully compress all subtrees from a typescript CST
use std::collections::HashMap;
use std::fmt::Debug;

use hyperast::full::FullNode;
use hyperast::hashed::SyntaxNodeHashs;
use hyperast::hashed::{IndexingHashBuilder, MetaDataHashsBuilder};
use hyperast::nodes::Space;
use hyperast::store::SimpleStores;
use hyperast::store::nodes::compo;
use hyperast::store::nodes::legion::NodeIdentifier;
use hyperast::store::nodes::legion::{eq_node, subtree_builder};
use hyperast::tree_gen;
use hyperast::tree_gen::BasicGlobalData;
use hyperast::tree_gen::GlobalData as _;
use hyperast::tree_gen::parser::Node as _;
use hyperast::tree_gen::parser::TreeCursor;
use hyperast::tree_gen::utils_ts::TTreeCursor;
use hyperast::tree_gen::{AccIndentation, Accumulator, WithByteRange};
use hyperast::tree_gen::{BasicAccumulator, SubTreeMetrics};
use hyperast::tree_gen::{Parents, PreResult};
use hyperast::tree_gen::{SpacedGlobalData, TextedGlobalData};
use hyperast::tree_gen::{TreeGen, ZippedTreeGen};
use hyperast::tree_gen::{compute_indentation, get_spacing, has_final_space};
use hyperast::types::LabelStore as LabelStoreTrait;

use crate::TNode;
use crate::Type;
use crate::types::TsEnabledTypeStore;

pub type LabelIdentifier = hyperast::store::labels::DefaultLabelIdentifier;

pub struct TsTreeGen<'store, 'cache, TS> {
    pub line_break: Vec<u8>,
    pub stores: &'store mut SimpleStores<TS>,
    pub md_cache: &'cache mut MDCache,
}

pub type MDCache = HashMap<NodeIdentifier, MD>;

// NOTE only keep compute intensive metadata (where space/time tradeoff is worth storing)
// eg. decls refs, maybe hashes but not size and height
// * metadata: computation results from concrete code of node and its children
// they can be qualitative metadata .eg a hash or they can be quantitative .eg lines of code
pub struct MD {
    metrics: SubTreeMetrics<SyntaxNodeHashs<u32>>,
}

impl From<Local> for MD {
    fn from(x: Local) -> Self {
        MD { metrics: x.metrics }
    }
}

pub type Global<'a> = SpacedGlobalData<'a>;

/// TODO temporary placeholder
#[derive(Debug, Clone, Default)]
pub struct PartialAnalysis {}

#[derive(Debug, Clone)]
pub struct Local {
    pub compressed_node: NodeIdentifier,
    pub metrics: SubTreeMetrics<SyntaxNodeHashs<u32>>,
}

impl Local {
    fn acc(self, acc: &mut Acc) {
        if self.metrics.size_no_spaces > 0 {
            acc.no_space.push(self.compressed_node)
        }
        acc.simple.push(self.compressed_node);
        acc.metrics.acc(self.metrics);
    }
}

pub struct Acc {
    simple: BasicAccumulator<Type, NodeIdentifier>,
    no_space: Vec<NodeIdentifier>,
    labeled: bool,
    start_byte: usize,
    end_byte: usize,
    metrics: SubTreeMetrics<SyntaxNodeHashs<u32>>,
    padding_start: usize,
    indentation: tree_gen::Spaces,
}

pub type FNode = FullNode<BasicGlobalData, Local>;
impl Accumulator for Acc {
    type Node = FNode;
    fn push(&mut self, full_node: Self::Node) {
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

impl<'store, 'cache, TS: TsEnabledTypeStore> ZippedTreeGen for TsTreeGen<'store, 'cache, TS> {
    type Stores = SimpleStores<TS>;
    type Text = [u8];
    type Node<'b> = TNode<'b>;
    type TreeCursor<'b> = TTreeCursor<'b>;

    fn stores(&mut self) -> &mut Self::Stores {
        &mut self.stores
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
            simple: BasicAccumulator {
                kind,
                children: vec![],
            },
            no_space: vec![],
            labeled,
            start_byte: node.start_byte(),
            end_byte: node.end_byte(),
            metrics: Default::default(),
            padding_start: 0,
            indentation: indent,
        }
    }
    fn pre_skippable(
        &mut self,
        text: &Self::Text,
        cursor: &Self::TreeCursor<'_>,
        stack: &Parents<Self::Acc>,
        global: &mut Self::Global,
    ) -> hyperast::tree_gen::PreResult<Self::Acc> {
        let node = cursor.node();
        if node.0.is_missing() {
            return PreResult::Skip;
        }
        let Some(_kind) = TS::try_obtain_type(&node) else {
            return PreResult::Skip;
        };
        let acc = self.pre(text, &node, stack, global);
        log::warn!("not retrieving roles");
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
        let indent = compute_indentation(
            &self.line_break,
            text,
            node.start_byte(),
            global.sum_byte_length(),
            &parent_indentation,
        );
        Acc {
            labeled: node.has_label(),
            start_byte: node.start_byte(),
            end_byte: node.end_byte(),
            metrics: Default::default(),
            padding_start: global.sum_byte_length(),
            indentation: indent,
            simple: BasicAccumulator {
                kind,
                children: vec![],
            },
            no_space: vec![],
        }
    }

    fn post(
        &mut self,
        parent: &mut Self::Acc,
        global: &mut Self::Global,
        text: &[u8],
        acc: Self::Acc,
    ) -> <<Self as TreeGen>::Acc as Accumulator>::Node {
        let spacing = get_spacing(
            acc.padding_start,
            acc.start_byte,
            text,
            parent.indentation(),
        );
        if let Some(spacing) = spacing {
            parent.push(FullNode {
                global: global.simple(),
                local: self.make_spacing(spacing),
            });
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

impl<'store, 'cache, TS: TsEnabledTypeStore> TsTreeGen<'store, 'cache, TS> {
    fn make_spacing(&mut self, spacing: Vec<u8>) -> Local {
        let kind = Type::Spaces;
        let interned_kind = TS::intern(kind);
        debug_assert_eq!(kind, TS::resolve(interned_kind));

        let spacing = std::str::from_utf8(&spacing).unwrap().to_string();

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
        }
    }

    pub fn new(
        stores: &'store mut <Self as ZippedTreeGen>::Stores,
        md_cache: &'cache mut MDCache,
    ) -> TsTreeGen<'store, 'cache, TS> {
        TsTreeGen::<'store, 'cache, TS> {
            line_break: "\n".as_bytes().to_vec(),
            stores,
            md_cache,
        }
    }

    pub fn tree_sitter_parse(text: &[u8]) -> Result<tree_sitter::Tree, tree_sitter::Tree> {
        let mut parser = tree_sitter::Parser::new();
        let language = crate::language();
        parser.set_language(&language).unwrap();
        let tree = parser.parse(text, None).unwrap();
        if tree.root_node().has_error() {
            Err(tree)
        } else {
            Ok(tree)
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

        let spacing = get_spacing(
            init.padding_start,
            init.start_byte,
            text,
            init.indentation(),
        );
        if let Some(spacing) = spacing {
            global.down();
            init.start_byte = 0;
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
            let spacing = get_spacing(
                global.sum_byte_length(),
                text.len(),
                text,
                acc.indentation(),
            );
            if let Some(spacing) = spacing {
                global.right();
                acc.push(FullNode {
                    global: global.simple(),
                    local: self.make_spacing(spacing),
                });
            }
        }
        let label = Some(std::str::from_utf8(name).unwrap().to_owned());
        let full_node = self.make(&mut global, acc, label);
        full_node
    }
}

impl<'stores, 'cache, TS: TsEnabledTypeStore> TreeGen for TsTreeGen<'stores, 'cache, TS> {
    type Acc = Acc;
    type Global = SpacedGlobalData<'stores>;
    fn make(
        &mut self,
        global: &mut Self::Global,
        acc: Self::Acc,
        label: Option<String>,
    ) -> <<Self as TreeGen>::Acc as Accumulator>::Node {
        let node_store = &mut self.stores.node_store;
        let label_store = &mut self.stores.label_store;
        let interned_kind = TS::intern(acc.simple.kind);
        let metrics = acc.metrics.finalize(&interned_kind, &label);
        let hashable = &metrics.hashs.most_discriminating();

        let label_id = label.as_deref().map(|l| label_store.get_or_insert(l));
        let eq = eq_node(&interned_kind, label_id.as_ref(), &acc.simple.children);

        let insertion = node_store.prepare_insertion(&hashable, eq);

        let local = if let Some(compressed_node) = insertion.occupied_id() {
            let md = self.md_cache.get(&compressed_node).unwrap();
            debug_assert_eq!(metrics.height, md.metrics.height);
            debug_assert_eq!(metrics.size, md.metrics.size);
            debug_assert_eq!(metrics.size_no_spaces, md.metrics.size_no_spaces);
            debug_assert_eq!(metrics.hashs.build(), md.metrics.hashs);
            Local {
                compressed_node,
                metrics: md.metrics,
            }
        } else {
            let mut metrics = metrics.map_hashs(|h| h.build());
            let own_line_count = tree_gen::newline_count(&label);
            metrics.line_count += own_line_count;

            let mut dyn_builder = subtree_builder::<TS>(interned_kind);
            let children_is_empty = acc.simple.children.is_empty();
            let hashs = metrics.add_md_metrics(&mut dyn_builder, children_is_empty);
            hashs.persist(&mut dyn_builder);

            let bytes_len = (acc.end_byte - acc.start_byte).try_into().unwrap();
            dyn_builder.add(compo::BytesLen(bytes_len));
            if acc.simple.children.len() != acc.no_space.len() {
                let children = acc.no_space;
                tree_gen::add_cs_no_spaces(&mut dyn_builder, children);
            }
            acc.simple
                .add_primary(&mut dyn_builder, interned_kind, label_id);

            let compressed_node = insertion.vacant().insert_built(dyn_builder.build());

            Local {
                compressed_node,
                metrics,
            }
        };

        let full_node = FullNode {
            global: global.simple(),
            local,
        };
        full_node
    }
}
